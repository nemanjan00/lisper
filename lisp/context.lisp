;;;; lisp/context.lisp -- compactor-style context management.
;;
;; The model keeps the context small from the start: it collapses
;; finished stretches of *messages* with context_rewind (its own summary
;; plus an index stands in for them), offloads big tool results with
;; context_offload, and finds every original back with context_search /
;; context_restore.  Nothing is deleted: the session .jsonl keeps every
;; message, and offloaded/collapsed originals are held in
;; *ctx-originals*.  Entry ids ([entry N]) are added to the request copy
;; in *before-request*; stored messages stay clean.  Threshold notes
;; (edge-triggered) and todo-done nudges arrive the same way.
;;
;; Loaded by init.lisp after the tools; (context-start) installs the
;; hook, watcher, tools and the context pane.

(in-package :lisping)

;;; ---------------------------------------------------------------- state

(defvar *ctx-ids* (make-hash-table :test #'eq :weakness :key)
  "message object -> entry id (integer).  Weak on the message: stubs and
dropped messages GC away instead of leaking an id each request.")
(defvar *ctx-id-messages* (make-hash-table :weakness :value)
  "entry id -> message object.  Weak on the value, for the same reason.")
(defvar *ctx-next-id* 0)
(defvar *ctx-originals* (make-hash-table)
  "entry id -> (kind . value): :content for an offloaded string, :message
for a message object removed by a rewind.")
(defvar *ctx-stretches* nil
  "List of (from-id to-id short-summary marker-message count est-tokens).")
(defvar *ctx-offloaded-count* 0)
(defvar *ctx-window* 1048576 "Context window in tokens (K3: 1M).")
(defvar *ctx-budget* 200000
  "Working budget in tokens: cost, latency and quality suffer long
before the window is full, so thresholds are percents of THIS.")
(defvar *ctx-note-at* 70 "Percent of the budget at which to note.")
(defvar *ctx-near-at* 80 "Percent at which the note becomes an instruction.")
(defvar *ctx-noted-percent* 0 "Last noted percent (ratchets +5; resets on drops).")
(defvar *ctx-pending-note* nil "Milestone note waiting to be injected.")
(defvar *ctx-last-prompt-tokens* nil "prompt_tokens of the previous request.")
(defvar *ctx-last-visible-count* nil "message count when last measured.")
(defvar *ctx-requests-since-collapse* 99
  "Requests since the last rewind/offload; the truncation check stays quiet for 2.")
(defvar *ctx-low-pt-streak* 0
  "Consecutive requests with prompt_tokens far below the local estimate.")
(defvar *ctx-result-max-chars* 20000 "Tool results longer than this offload at once.")
(defvar *ctx-pointer-head* 2000 "Chars kept from the start of an offloaded result.")
(defvar *ctx-pointer-tail* 1000 "… and from the end.")
(defvar *ctx-piece-max* 16000 "Most context_restore reads at once.")
(defvar *ctx-search-limit* 10)
(defvar *ctx-index-max* 40 "Lines a rewind index lists at most.")
(defvar *ctx-autoloaded* nil)
(progn
  (defvar *ctx-chars-per-token* 3.5
    "Chars per token, calibrated against prompt_tokens after each request.")
  (defvar *ctx-prev-request-chars* nil "Char count of the request just sent.")
  (defvar *ctx-tools-chars* nil "Chars of the tool specs, cached from the last request body.")
  (defvar *ctx-last-est* 0 "Token estimate of the last request sent.")

  (defun ctx-est-tokens (string)
    (max 1 (round (length string) *ctx-chars-per-token*)))

  (defun ctx-system-prompt-text ()
    (let ((sp lisping-core:*system-prompt*))
      (if (functionp sp) (funcall sp) (or sp ""))))

  (defun ctx-message-chars (m)
    "Chars in one message: content, reasoning_content (sent back too),
and tool-call arguments — which live at tc -> function -> arguments."
    (+ (let ((c (gethash "content" m))) (if (stringp c) (length c) 0))
       (let ((r (gethash "reasoning_content" m))) (if (stringp r) (length r) 0))
       (loop for tc across (or (gethash "tool_calls" m) #())
             sum (let ((f (gethash "function" tc)))
                   (if (hash-table-p f)
                       (let ((a (gethash "arguments" f)))
                         (if (stringp a) (length a) 0))
                       0))))))
(defun ctx-session-log ()
  "The current session's jsonl path, or NIL."
  (and (find-symbol "*SESSION-LOG*" :lisping-core)
       (boundp (find-symbol "*SESSION-LOG*" :lisping-core))
       (symbol-value (find-symbol "*SESSION-LOG*" :lisping-core))))

(defun ctx-entry-id (msg)
  "Stable entry id (integer) for a message object, assigning on first use."
  (or (gethash msg *ctx-ids*)
      (let ((id (incf *ctx-next-id*)))
        (setf (gethash msg *ctx-ids*) id
              (gethash id *ctx-id-messages*) msg)
        id)))

(defun ctx-parse-id (x)
  "Accept \"12\", \"e12\", \"E12\" or the integer 12 → 12, or NIL."
  (cond ((integerp x) x)
        ((stringp x)
         (let ((s (string-trim " " x)))
           (when (and (plusp (length s)) (member (char s 0) '(#\e #\E)))
             (setf s (subseq s 1)))
           (ignore-errors (parse-integer s))))
        (t nil)))

(defun ctx-parse-int (x)
  (cond ((integerp x) x)
        ((stringp x) (ignore-errors (parse-integer x)))
        (t nil)))

(defun ctx-head-p (prefix string)
  "STRING starts with PREFIX (safe on short strings)."
  (and (stringp string)
       (let ((n (min (length prefix) (length string))))
         (string= prefix string :end2 n))))

(defun ctx-short (text)
  (let ((s (string-trim " " (substitute #\Space #\Newline (or text "")))))
    (subseq s 0 (min 60 (length s)))))

(defun ctx-copy-message (msg)
  "A same-test copy of a message hash table (no COPY-HASH-TABLE here)."
  (let ((m (make-hash-table :test #'equal)))
    (maphash (lambda (k v) (setf (gethash k m) v)) msg)
    m))

(defun ctx-message-label (msg)
  "e<id> (<role>): first 60 content chars — one index line."
  (let* ((id (ctx-entry-id msg))
         (role (or (gethash "role" msg) "?"))
         (content (gethash "content" msg))
         (text (if (stringp content)
                   (string-trim " " (substitute #\Space #\Newline content))
                   "")))
    (format nil "e~D (~A): ~A" id role (subseq text 0 (min 60 (length text))))))

(defun ctx-synthetic-p (m)
  "True for a message the SYSTEM wrote (x-kind set), not the user or
the model.  Looks up the core's SYNTHETIC-MESSAGE-P at runtime so this
file still loads on cores that predate it."
  (let ((s (find-symbol "SYNTHETIC-MESSAGE-P" :lisping-core)))
    (and s (fboundp s) (funcall s m))))

(defun ctx-latest-user-index ()
  "Index of the latest real user message — the current task; rewinds and
offloads must never touch it or anything after it.  System-written
messages (x-kind) and the older text-marked markers don't count."
  (position-if (lambda (m)
                 (and (equal (gethash "role" m) "user")
                      (stringp (gethash "content" m))
                      (not (ctx-synthetic-p m))
                      (not (ctx-head-p "[core]" (gethash "content" m)))
                      (not (ctx-head-p "[boot]" (gethash "content" m)))
                      (not (ctx-head-p "[collapsed" (gethash "content" m)))
                      (not (ctx-head-p "[context:" (gethash "content" m)))))
               *messages* :from-end t))

;;; ---------------------------------------------------------------- tools
;;; Every result starts with [ctx] so the auto-offloader never touches
;;; the compactor's own results.

(defun ctx-replace-range (start end marker)
  "Replace *messages* positions [start, end] (inclusive) with MARKER."
  (setf *messages*
        (append (subseq *messages* 0 start)
                (list marker)
                (subseq *messages* (1+ end)))))

(defun ctx-marker-text (stretch from-id to-id summary est)
  (with-output-to-string (s)
    (format s "[collapsed with context_rewind: ~D entries, e~D … e~D, ~D messages, ~DK est tokens]~%~A~%"
            (length stretch) from-id to-id (length stretch) (round est 1000) summary)
    (format s "~%[index of what this replaced: find a detail in it with context_search {\"text\": ...}, or read one entry back with context_restore {\"entry\": \"e~D\"}]~%"
            from-id)
    (loop for m in stretch
          for k from 0
          while (< k *ctx-index-max*)
          do (format s "- ~A~%" (ctx-message-label m)))
    (when (> (length stretch) *ctx-index-max*)
      (format s "- … ~D more, find them with context_search~%"
              (- (length stretch) *ctx-index-max*)))))

(defun ctx-snap-range (i j)
  "Expand [I,J] to whole tool-call units, so no tool call loses its
result (the core would fill the hole with a false \"interrupted\"
stub): back up over a leading run of tool results to the assistant
that owns them; a trailing assistant carries its tool results along."
  (let ((n (length *messages*)))
    (loop while (and (> i 0)
                     (equal (gethash "role" (nth i *messages*)) "tool"))
          do (decf i))
    (loop while (and (< j (1- n))
                     (equal (gethash "role" (nth j *messages*)) "assistant")
                     (gethash "tool_calls" (nth j *messages*)))
          do (incf j))
    (values i j)))

(defun ctx-do-rewind (from-msg to-msg summary)
  (let* ((i (position from-msg *messages* :test #'eq))
         (j (position to-msg *messages* :test #'eq))
         (guard (ctx-latest-user-index)))
    (cond
      ((or (null i) (null j))
       "[ctx] ERROR: entries found, but no longer in the visible conversation (already collapsed? use context_search).")
      ((and guard (>= (min i j) guard))
       "[ctx] ERROR: that stretch is the current task — rewind only FINISHED work before the latest user message.")
      (t
       (when (> i j) (rotatef i j))
       (multiple-value-setq (i j) (ctx-snap-range i j))
       (when (and guard (> j (1- guard))) (setf j (1- guard)))
       (when (and guard (>= i guard))
         (return-from ctx-do-rewind
           "[ctx] ERROR: snapped to whole tool-call units, the stretch reaches into the current task — rewind less, or finish the task first."))
       (let* ((stretch (subseq *messages* i (1+ j)))
              (from-id (ctx-entry-id (first stretch)))
              (to-id (ctx-entry-id (car (last stretch))))
              (est (reduce #'+ (mapcar (lambda (m) (ctx-est-tokens (or (gethash "content" m) "")))
                                       stretch)))
              (marker (obj :role "user" :content (ctx-marker-text stretch from-id to-id summary est))))
         ;; Provenance: the core strips x-kind before the request and
         ;; counts x-kind'd messages as system-written, so this marker
         ;; can never become "the current task" and block later rewinds.
         (setf (gethash "x-kind" marker) "collapse")
         (dolist (m stretch)
           (setf (gethash (ctx-entry-id m) *ctx-originals*) (cons :message m)))
         (push (list from-id to-id (ctx-short summary) marker (length stretch) est)
               *ctx-stretches*)
         (ctx-replace-range i j marker)
         (setf *ctx-requests-since-collapse* 0)
         (pane-dirty "context")
         (format nil "[ctx] collapsed entries e~D … e~D (~D messages, ~DK est tokens); takes effect from the next request."
                 from-id to-id (length stretch) (round est 1000)))))))

(defun ctx-tool-rewind (args)
  "context_rewind {from, to?, summary}"
  (let* ((from (ctx-parse-id (arg args "from")))
         (to (ctx-parse-id (arg args "to")))
         (summary (or (arg args "summary") ""))
         (from-msg (and from (gethash from *ctx-id-messages*)))
         (to-msg (and (or to from) (gethash (or to from) *ctx-id-messages*))))
    (cond
      ((null from-msg) "[ctx] ERROR: unknown from entry — see [entry N] prefixes in tool results.")
      ((null to-msg) "[ctx] ERROR: unknown to entry.")
      (t (ctx-do-rewind from-msg to-msg summary)))))

(defun ctx-tool-offload (args)
  "context_offload {entry, text} — replace one message with the model's
one-line pointer text."
  (let* ((id (ctx-parse-id (arg args "entry")))
         (text (or (arg args "text") ""))
         (msg (and id (gethash id *ctx-id-messages*))))
    (cond
      ((null msg) "[ctx] ERROR: unknown entry.")
      ((null (position msg *messages* :test #'eq))
       "[ctx] ERROR: entry is not in the visible conversation (already collapsed? use context_search).")
      (t
       (let ((content (gethash "content" msg)))
         (cond
           ((not (stringp content)) "[ctx] ERROR: entry has no text content to offload.")
           ((ctx-head-p "[offloaded" content) "[ctx] ERROR: entry is already offloaded.")
           ((and (equal (gethash "role" msg) "user")
                 (eq msg (nth (ctx-latest-user-index) *messages*)))
            "[ctx] ERROR: the latest user message (the current task) stays verbatim.")
           (t
            (setf (gethash id *ctx-originals*) (cons :content content))
            (setf (gethash "content" msg)
                  (format nil "~A — [offloaded ~D chars; context_restore {\"entry\": \"e~D\"} puts it back]"
                          (ctx-short text) (length content) id))
            (incf *ctx-offloaded-count*)
            (setf *ctx-requests-since-collapse* 0)
            (pane-dirty "context")
            (format nil "[ctx] offloaded entry e~D (~D chars); takes effect from the next request."
                    id (length content)))))))))

(defun ctx-piece (text id offset length)
  "Read [offset, offset+length) of TEXT, capped at *ctx-piece-max*."
  (let* ((off (max 0 (or offset 0)))
         (end (min (length text) (+ off (min (or length *ctx-piece-max*) *ctx-piece-max*)))))
    (format nil "[ctx] entry e~D chars [~D, ~D):~%~A" id off end (subseq text off end))))

(defun ctx-tool-restore (args)
  "context_restore {entry, offset?, length?} — with offset/length, read a
piece of the original; without them, put the original back into the
visible conversation (an offloaded result at its place; a collapsed
entry as a quoted message, so it can never orphan a tool call)."
  (let* ((id (ctx-parse-id (arg args "entry")))
         (offset (ctx-parse-int (arg args "offset")))
         (length (ctx-parse-int (arg args "length")))
         (rec (and id (gethash id *ctx-originals*))))
    (cond
      ((null id) "[ctx] ERROR: unknown entry.")
      ((null rec)
       ;; Not offloaded/collapsed: maybe a visible message — read a piece.
       (let ((msg (and id (gethash id *ctx-id-messages*))))
         (cond
           ((null msg)
            "[ctx] ERROR: no original kept for that entry; it may be from before this boot. Try context_search over the session log.")
           ((or offset length)
            (ctx-piece (or (gethash "content" msg) "") id offset length))
           (t (format nil "[ctx] entry e~D is already in the conversation verbatim." id)))))
      (t
       (let* ((kind (car rec))
              (value (cdr rec))
              (text (if (eq kind :content) value (or (gethash "content" value) ""))))
         (cond
           ((or offset length)
            (ctx-piece text id offset length))
           ((eq kind :content)
            (let ((msg (gethash id *ctx-id-messages*)))
              (setf (gethash "content" msg) text)
              (remhash id *ctx-originals*)
              (decf *ctx-offloaded-count*)
              (pane-dirty "context")
              (format nil "[ctx] restored entry e~D (~D chars) into the conversation, at its original place."
                      id (length text))))
           (t
            (let ((cell (find-if (lambda (st) (<= (first st) id (second st))) *ctx-stretches*)))
              (cond
                ((null cell)
                 "[ctx] ERROR: collapse record for that entry is gone (image restarted?). Use context_search over the session log.")
                (t
                 (let* ((marker (fourth cell))
                        (at (position marker *messages* :test #'eq))
                        (role (gethash "role" value)))
                   (if (null at)
                       "[ctx] ERROR: collapse marker no longer in the conversation."
                       (progn
                         (let ((quoted (obj :role "user"
                                            :content (format nil "[restored entry e~D, was a ~A message]~%~A"
                                                             id role text))))
                           ;; x-kind: a restored entry is system-written;
                           ;; it must not become "the current task".
                           (setf (gethash "x-kind" quoted) "restored")
                           (setf *messages*
                                 (append (subseq *messages* 0 (1+ at))
                                         (list quoted)
                                         (subseq *messages* (1+ at)))))
                         (remhash id *ctx-originals*)
                         (pane-dirty "context")
                         (format nil "[ctx] re-inserted entry e~D as a quoted message after its collapse marker; takes effect from the next request."
                                 id))))))))))))))

(defun ctx-snippet (text pos w)
  "W chars of TEXT around POS, newlines flattened, ellipsis at cuts."
  (let* ((flat (substitute #\Space #\Newline text))
         (len (length flat))
         (start (max 0 (min pos (max 0 (- len 1)) (- pos (floor w 3)))))
         (end (min len (+ start w))))
    (setf start (max 0 (- end (min w len))))
    (concatenate 'string (if (plusp start) "…" "")
                 (subseq flat start end)
                 (if (< end len) "…" ""))))

(defun ctx-search-log (path needle limit hits)
  "Search a session jsonl for NEEDLE; push (label . snippet) onto HITS."
  (when (and path (probe-file path) (< (length hits) limit))
    (ignore-errors
      (with-open-file (s path :direction :input)
        (loop for line = (read-line s nil nil)
              for n from 1
              while (and line (< (length hits) limit))
              do (let* ((low (string-downcase line))
                        (pos (search needle low)))
                   (when pos
                     (push (cons (format nil "log ~A:~D" (file-namestring path) n)
                                 (ctx-snippet line pos 120))
                           hits))))))))

(defun ctx-search-finish (hits needle)
  (with-output-to-string (s)
    (format s "[ctx] ~D hit~:P for ~S.~%" (length hits) needle)
    (dolist (h (reverse hits))
      (format s "~%~A~%  ~A" (car h) (cdr h)))
    (format s "~%~%[read one back with context_restore {\"entry\": \"eN\"} — optionally with offset and length for a piece; session-log hits live in the jsonl file]~%")))

(defun ctx-tool-search (args)
  "context_search {text, limit?}"
  (let* ((needle0 (or (arg args "text") ""))
         (limit (min (max 1 (or (ctx-parse-int (arg args "limit")) *ctx-search-limit*)) 50))
         (hits '()))
    (cond
      ((zerop (length (string-trim " " needle0)))
       "[ctx] ERROR: text is required.")
      (t
       (let ((needle (string-downcase (string-trim " " needle0))))
         ;; visible conversation
         (dolist (m *messages*)
           (when (>= (length hits) limit) (return))
           (let* ((text (gethash "content" m))
                  (low (and (stringp text) (string-downcase text)))
                  (pos (and low (search needle low))))
             (when pos
               (push (cons (format nil "e~D (visible ~A)" (ctx-entry-id m) (gethash "role" m))
                           (ctx-snippet text pos 120))
                     hits))))
         ;; offloaded / collapsed originals kept this boot
         (dolist (cell (sort (loop for id being the hash-keys of *ctx-originals*
                                   collect (cons id (gethash id *ctx-originals*)))
                             #'< :key #'car))
           (when (>= (length hits) limit) (return))
           (let* ((id (car cell))
                  (rec (cdr cell))
                  (text (if (eq (car rec) :content)
                            (cdr rec)
                            (or (gethash "content" (cdr rec)) "")))
                  (pos (search needle (string-downcase text))))
             (when pos
               (push (cons (format nil "e~D (~A)" id
                                   (if (eq (car rec) :content) "offloaded" "collapsed"))
                           (ctx-snippet text pos 120))
                     hits))))
         ;; the full session logs, current and previous
         (ctx-search-log (ctx-session-log) needle limit hits)
         (ctx-search-log lisping-core:*previous-session* needle limit hits)
         (ctx-search-finish hits (string-trim " " needle0)))))))

;;; ------------------------------------------------------- before-request

(defun ctx-prefix-message (msg id)
  "Shallow copy of MSG with [entry N] prefixed to string content."
  (if (and (stringp (gethash "content" msg))
           (member (gethash "role" msg) '("tool" "user") :test #'equal)
           (not (ctx-head-p "[entry " (gethash "content" msg))))
      (let ((m (ctx-copy-message msg)))
        (setf (gethash "content" m)
              (concatenate 'string (format nil "[entry ~D] " id) (gethash "content" msg)))
        m)
      msg))

(defun ctx-auto-offload (msg)
  "Offload an over-long tool result in place (original kept in the
registry).  Returns T if it offloaded."
  (let ((content (gethash "content" msg)))
    (when (and (equal (gethash "role" msg) "tool")
               (stringp content)
               (> (length content) *ctx-result-max-chars*)
               (not (ctx-head-p "[ctx]" content))
               (not (ctx-head-p "[offloaded" content)))
      (let ((id (ctx-entry-id msg)))
        (setf (gethash id *ctx-originals*) (cons :content content))
        (setf (gethash "content" msg)
              (concatenate
               'string
               (subseq content 0 *ctx-pointer-head*)
               (format nil "~%~%[offloaded middle ~D of ~D chars; read the rest with context_restore {\"entry\": \"e~D\", \"offset\": ~D}]~%~%"
                       (- (length content) *ctx-pointer-head* *ctx-pointer-tail*)
                       (length content) id *ctx-pointer-head*)
               (subseq content (max *ctx-pointer-head* (- (length content) *ctx-pointer-tail*)))))
        (incf *ctx-offloaded-count*)
        (setf *ctx-requests-since-collapse* 0)
        (pane-dirty "context")
        t))))

(defun ctx-biggest-finished-results (k)
  "The K biggest tool results before the current task, as (id . est)."
  (let* ((guard (ctx-latest-user-index))
         (candidates (loop for m in *messages*
                           for i from 0
                           for c = (gethash "content" m)
                           when (and (equal (gethash "role" m) "tool")
                                     (stringp c)
                                     (or (null guard) (< i guard))
                                     (not (ctx-head-p "[offloaded" c)))
                           collect (cons (ctx-entry-id m) (ctx-est-tokens c)))))
    (subseq (sort candidates #'> :key #'cdr) 0 (min k (length candidates)))))

(defun ctx-threshold-note (percent)
  "A note when the context passes a threshold, or NIL.  Edge-triggered:
after noting at P, the next note comes at P+5, so the request is never
peppered with repeats."
  (when (and (>= percent *ctx-note-at*)
             (>= percent (+ *ctx-noted-percent* 5)))
    (setf *ctx-noted-percent* percent)
    (with-output-to-string (s)
      (if (>= percent *ctx-near-at*)
          (format s "[context: ~D% of ~:D tokens (past ~D%)] Free space BEFORE going on: collapse the biggest finished stretches with context_rewind {from, to, summary}, offload big results with context_offload {entry, text}.~%"
                  percent *ctx-window* *ctx-near-at*)
          (format s "[context: ~D% of ~:D tokens] The biggest finished parts:~%"
                  percent *ctx-window*))
      (dolist (c (ctx-biggest-finished-results 3))
        (format s "- e~D ≈ ~D est tokens: context_offload {\"entry\": \"e~D\"}~%"
                (car c) (cdr c) (car c)))
      (format s "Collapse finished stretches with context_rewind {from, to, summary}; summaries keep what later work needs verbatim (paths, ids, decisions).~%"))))

(defun ctx-emit-watch (event)
  "Notice finished tasks (a todo marked done) for the next nudge; retire
the nudge only when a response actually arrives (clearing it at
injection would lose it on a failed request)."
  (case (getf event :type)
    (:tool-result
     (when (and (equal (getf event :name) "todo")
                (ctx-head-p "done:" (getf event :text)))
       (setf *ctx-pending-note*
             (format nil "[context: a task is done] ~A The work for it above is finished; once nothing below needs it verbatim, collapse it with context_rewind {from, to, summary} (summaries keep paths, ids, decisions later work needs), or offload its big results with context_offload {entry, text}."
                     (ctx-short (getf event :text))))))
    (:assistant
     (setf *ctx-pending-note* nil)))
  event)
(defun ctx-local-estimate ()
  "Estimated tokens of the next request: system prompt, tool specs, and
every message (content + reasoning_content + tool-call arguments),
at the calibrated chars-per-token.  The endpoint's prompt_tokens of the
PREVIOUS request recalibrates the ratio, so this tracks reality."
  ;; *MESSAGES* is a list, but "tool_calls" arrives from the wire as a
  ;; VECTOR — iterate it with ACROSS, never mapcar/for-in.  (A mapcar
  ;; over it was the "#(...) is not of type LIST" that locked out every
  ;; request.)
  (round (+ (length (ctx-system-prompt-text))
            (or *ctx-tools-chars* 0)
            (loop for m in *messages* sum (ctx-message-chars m)))
         *ctx-chars-per-token*))
(defun ctx-before-request (body)
  "The compactor's view of every request: assign entry ids, offload
over-long tool results (originals kept), prefix [entry N] marks on tool
results and user messages, and inject threshold / truncation /
milestone notes.  Only the request copy is prefixed; stored messages
stay clean.  Also calibrates the token estimate: PT (reported usage of
the request just answered) against the chars we counted for it."
  (let ((messages (gethash "messages" body)))
    (when (vectorp messages)
      ;; Chars of THIS request: system prompt + tool specs + messages.
      (let ((tools (gethash "tools" body)))
        (setf *ctx-tools-chars*
              (if tools (length (json-encode tools)) 0)))
      (let ((chars (+ (length (ctx-system-prompt-text))
                      *ctx-tools-chars*
                      (loop for m across messages sum (ctx-message-chars m)))))
        (let ((notes '()))
          (loop for m across messages
                unless (equal (gethash "role" m) "system")
                do (ctx-entry-id m)
                   (ctx-auto-offload m))
          (let* ((usage *last-usage*)
                 (pt (and (hash-table-p usage)
                          (integerp (gethash "prompt_tokens" usage))
                          (gethash "prompt_tokens" usage)))
                 ;; Calibrate: PT describes the request whose chars we
                 ;; counted last time (*CTX-PREV-REQUEST-CHARS*).
                 (dummy (when (and pt *ctx-prev-request-chars*
                                   (plusp *ctx-prev-request-chars*) (plusp pt))
                          (setf *ctx-chars-per-token*
                                (min 8.0 (max 1.5 (/ (float *ctx-prev-request-chars*) pt))))))
                 (est (max 1 (round chars *ctx-chars-per-token*)))
                 (basis (max (or pt 0) est))
                 (percent (round (* 100 basis) *ctx-budget*))
                 (low-pt (and pt (plusp est) (< pt (* 7/10 est)))))
            (declare (ignore dummy))
            (setf *ctx-prev-request-chars* chars
                  *ctx-last-est* est)
            ;; Ratchet reset: a collapse dropped us below the threshold —
            ;; let the nudges fire again.
            (when (< percent *ctx-note-at*)
              (setf *ctx-noted-percent* 0))
            (setf *ctx-last-prompt-tokens* pt
                  *ctx-last-visible-count* (length *messages*))
            ;; Truncation check: the endpoint reports far fewer
            ;; prompt_tokens than the local estimate of the SAME request,
            ;; twice running, and not right after a collapse.
            (incf *ctx-requests-since-collapse*)
            (if low-pt
                (incf *ctx-low-pt-streak*)
                (setf *ctx-low-pt-streak* 0))
            (when (and (>= *ctx-low-pt-streak* 2)
                       (>= *ctx-requests-since-collapse* 2))
              (push (format nil "[context: TRUNCATION LIKELY] prompt_tokens ~D is under 70% of the local estimate ~:D two requests running — the endpoint may be silently dropping the middle. Rewind the biggest finished stretch NOW: context_rewind {from, to, summary}."
                            pt est)
                    notes))
            (let ((note (ctx-threshold-note percent)))
              (when note (push note notes))))
          (when *ctx-pending-note*
            (push *ctx-pending-note* notes))
          (setf (gethash "messages" body)
                (coerce (append (loop for m across messages
                                      collect (ctx-prefix-message m (ctx-entry-id m)))
                                (mapcar (lambda (n)
                                          (let ((m (obj :role "user" :content n)))
                                            (setf (gethash "x-kind" m) "note")
                                            m))
                                        (reverse notes)))
                        'vector))
          (pane-dirty "context")
          body)))))
;;; ----------------------------------------------------------- the pane

(defun pane-context-render (pane w h)
  "Context size and what the compactor has collapsed, for the pane.
Returns chunk LISTS (a chunk is (text . attr))."
  (declare (ignore pane w))
  (flet ((line (text attr) (list (cons text attr))))
    (let* ((usage *last-usage*)
           (pt (and (hash-table-p usage) (gethash "prompt_tokens" usage)))
           (est-visible (reduce #'+ (mapcar (lambda (m) (ctx-est-tokens (or (gethash "content" m) "")))
                                            *messages*)))
           (lines (list (line (format nil "context: ~:[?~;~:D~] / ~:D tokens (~D%)"
                                      pt pt *ctx-window*
                                      (if pt (round (* 100 pt) *ctx-window*) 0))
                              7)
                        (line (format nil "visible: ~D messages (~DK est)"
                                      (length *messages*) (round est-visible 1000))
                              8))))
      (dolist (st *ctx-stretches*)
        (push (line (format nil "rewind e~D…e~D: ~A (~DK)"
                            (first st) (second st) (third st) (round (sixth st) 1000))
                    8)
              lines))
      (when (plusp *ctx-offloaded-count*)
        (push (line (format nil "offloaded results: ~D" *ctx-offloaded-count*) 8) lines))
      (when *ctx-pending-note*
        (push (line "milestone note: pending" 3) lines))
      (setf lines (nreverse lines))
      (subseq lines 0 (min h (length lines))))))

;;; --------------------------------------------------------------- start

(defun ctx-init-panes ()
  "Re-register the context pane after every PANES-INIT (the init wipes
the registry and resets the layout, so this must run again on each UI
restart — hooked via *PANES-INIT-HOOKS*).  Idempotent."
  (block ctx-init-panes
    (unless (pane-get "context")
      (pane-register "context" :title "context" :kind :buffer))
    (setf (pane-render (pane-get "context")) #'pane-context-render)
    (unless (member "context" (layout-pane-names) :test #'equal)
      (setf *layout* (list :vsplit 0.85 *layout* (list 'leaf "context")))
      (panes-save)
      (panes-mark-all-dirty)
      (tui-dirty))))

(defun context-start ()
  "Install the compactor: before_request hook, the emit observer, the
four context_* tools, the context pane (via the panes-init hook, so a
UI restart cannot drop it), and the system-prompt guideline.
Idempotent."
  (pushnew 'ctx-init-panes *panes-init-hooks*)
  (ctx-init-panes)
  (unless *ctx-autoloaded*
    (setf *ctx-autoloaded* t)
    (pushnew 'ctx-before-request *before-request*)
    ;; The todo-done watcher lives on *observers*, not an *emit* wrapper:
    ;; tui-start rebinds *emit* at every UI (re)start and would drop it.
    (pushnew 'ctx-emit-watch lisping-core:*observers*)
    (when (and (stringp *system-prompt*)
               (not (search "[context:compactor]" *system-prompt*)))
      (setf *system-prompt*
            (concatenate 'string *system-prompt* "
- [context:compactor] Keep your context small from the start: as soon as a part of the work is done (files read and understood, a step completed), collapse it with context_rewind {from, to, summary}, keeping in the summary what later work needs verbatim (paths, line numbers, ids, decisions). Big tool results you no longer need: context_offload {entry, text}. Tool results and user messages carry [entry N] ids; name them in the tools. Nothing is deleted: context_search {text} finds any original, even in earlier sessions' logs, and context_restore {entry} reads it back.")))
    (register-tool "context_rewind"
                   "Collapse a finished stretch of the conversation (from entry `from` through entry `to`, inclusive; `to` defaults to `from`) replacing it with your summary plus an index of what it replaced. Rewind only FINISHED work before the latest user message. The originals stay in the session log; context_restore/context_search reach them. The summary should keep what later work needs verbatim (paths, line numbers, ids, decisions) and end with a Keywords: line."
                   (obj :type "object" :properties
                        (obj :from (obj :type "string" :description "entry id, as in [entry N] (e.g. \"12\" or \"e12\")")
                             :to (obj :type "string" :description "entry id; defaults to `from`")
                             :summary (obj :type "string" :description "your summary of the stretch, ending with a Keywords: line"))
                        :required (vector "from" "summary"))
                   #'ctx-tool-rewind)
    (register-tool "context_offload"
                   "Replace one message (usually a big tool result) in the conversation with your one-line pointer text. The original stays retrievable with context_restore {entry}. Do not offload the latest user message or anything you still need verbatim."
                   (obj :type "object" :properties
                        (obj :entry (obj :type "string" :description "entry id")
                             :text (obj :type "string" :description "one line saying what this was and what matters about it"))
                        :required (vector "entry" "text"))
                   #'ctx-tool-offload)
    (register-tool "context_restore"
                   "With offset/length: read that character range of the original entry (no change to the conversation). Without them: put the original back into the conversation (an offloaded result at its place; a collapsed entry as a quoted message after its collapse marker)."
                   (obj :type "object" :properties
                        (obj :entry (obj :type "string" :description "entry id")
                             :offset (obj :type "string" :description "character offset into the original")
                             :length (obj :type "string" :description "how many characters to read, at most 16000"))
                        :required (vector "entry"))
                   #'ctx-tool-restore)
    (register-tool "context_search"
                   "Full-text search over the visible conversation, offloaded/collapsed originals, and the current and previous session logs. Each hit comes with a snippet and how to read it back."
                   (obj :type "object" :properties
                        (obj :text (obj :type "string" :description "text to find (words, not meaning)")
                             :limit (obj :type "string" :description "max hits, default 10"))
                        :required (vector "text"))
                   #'ctx-tool-search))
  t)

