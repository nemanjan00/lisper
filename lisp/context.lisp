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

(defvar *ctx-next-id* 0)

(defvar *ctx-originals* (make-hash-table)
  "entry id -> (kind . value): :content for an offloaded string, :message
for a message object removed by a rewind.  Persisted to the session's
ctx-originals.json after every mutation (see CTX-SAVE-STATE).")

(defvar *ctx-stretches* nil
  "List of (from-id to-id short-summary marker-id count est-tokens).
marker-id is the collapse marker message's x-id; persisted with the
originals.")

(defvar *ctx-offloaded-count* 0)

(defun ctx-model-window (&optional (model lisping-core:*model*))
  "Context window in tokens for MODEL (by name): Kimi K-series 1M,
Claude Sonnet/Opus/Haiku 200k, anything unknown 200k (the safe side)."
  (let ((m (string-downcase (or model ""))))
    (cond ((search "kimi" m) 1048576)
          ((search "claude" m) 200000)
          (t 200000))))

(defvar *ctx-window* (ctx-model-window)
  "Context window in tokens, from the model name (see CTX-MODEL-WINDOW).")

(defvar *ctx-budget* (min 200000 (floor (* 3 *ctx-window*) 4))
  "Working budget in tokens: cost, latency and quality suffer long
before the window is full, so thresholds are percents of THIS.  At most
200k, and 3/4 of the window on small-window models.")

(defvar *ctx-note-at* 70 "Percent of the budget at which to note.")

(defvar *ctx-near-at* 80 "Percent at which the note becomes an instruction.")

(defvar *ctx-task-note-at* 60
  "Percent of the budget below which finishing a todo triggers no nudge
(a context edit far back re-caches everything after it: only worth it
when it cuts a lot).")

(defvar *ctx-min-cut-percent* 20
  "Smallest share of the context a context edit should free; smaller
edits cost more cache than they save.")

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
  "The current session's jsonl path, or NIL. (*SESSION-LOG* is internal
to the core, hence the ::.)"
  (and (boundp 'lisping-core::*session-log*)
       lisping-core::*session-log*))

(defun ctx-entry-id (msg)
  "Stable entry id (integer) for MSG: stored ON the message as x-id.
The core strips x-* keys from requests but saves them with the session,
so the message<->id mapping survives a restart.  Reading an existing
x-id bumps *CTX-NEXT-ID* past it, so a state file older than the last
id assignment can never hand out a live message's id."
  (let ((id (gethash "x-id" msg)))
    (cond (id (setf *ctx-next-id* (max *ctx-next-id* id))
               id)
          (t (setf (gethash "x-id" msg) (incf *ctx-next-id*))))))

(defun ctx-find-message (id)
  "The message in *messages* whose x-id is ID, or NIL."
  (and id (find-if (lambda (m) (eql (gethash "x-id" m) id)) *messages*)))

;;; ------------------------------------------------------- persistence

(defun ctx-originals-file ()
  (lisping-core:session-path "ctx-originals.json"))

(defun ctx-save-state ()
  "Persist *CTX-ORIGINALS*, *CTX-STRETCHES* and *CTX-NEXT-ID* to the
session dir, so entry ids still resolve after a restart."
  (ignore-errors
    (let ((f (ctx-originals-file)))
      (when f
        (with-open-file (s f :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
          (write-string
           (json-encode
            (obj :next-id *ctx-next-id*
                 :originals (coerce
                             (loop for id being the hash-keys of *ctx-originals*
                                   using (hash-value rec)
                                   collect (obj :id id
                                                :kind (if (eq (car rec) :content) "content" "message")
                                                :value (cdr rec)))
                             'vector)
                 :stretches (coerce
                             (loop for st in *ctx-stretches*
                                   collect (obj :from (first st) :to (second st)
                                                :short (third st) :marker (fourth st)
                                                :count (fifth st) :est (sixth st)))
                             'vector)))
           s))))))

(defun ctx-load-state ()
  "Reload the persisted originals and stretches; rebuild *CTX-NEXT-ID*
as the largest id seen anywhere (CTX-ENTRY-ID also ratchets it up
whenever it meets a higher x-id)."
  (let ((f (ctx-originals-file)))
    (when (and f (probe-file f))
      (let ((data (ignore-errors
                    (json-decode
                     (with-open-file (s f)
                       (let ((out (make-string-output-stream)))
                         (loop for ch = (read-char s nil nil)
                               while ch do (write-char ch out))
                         (get-output-stream-string out)))))))
        (when (hash-table-p data)
          (clrhash *ctx-originals*)
          (loop for rec across (or (gethash "originals" data) #())
                do (setf (gethash (gethash "id" rec) *ctx-originals*)
                         (cons (if (equal (gethash "kind" rec) "content") :content :message)
                               (gethash "value" rec))))
          (setf *ctx-stretches*
                (loop for st across (or (gethash "stretches" data) #())
                      collect (list (gethash "from" st) (gethash "to" st)
                                    (gethash "short" st) (gethash "marker" st)
                                    (gethash "count" st) (gethash "est" st)))
                *ctx-offloaded-count* (loop for rec being the hash-values of *ctx-originals*
                                            count (eq (car rec) :content))
                *ctx-next-id*
                (max (or (gethash "next-id" data) 0)
                     (or (loop for id being the hash-keys of *ctx-originals* maximize id) 0)
                     (or (loop for st in *ctx-stretches* maximize (second st)) 0)
                     (or (loop for m in *messages*
                               maximize (or (gethash "x-id" m) 0)) 0))))))))

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
the model."
  (lisping-core:synthetic-message-p m))

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
    (format s "[collapsed with context_rewind: ~D entries, e~D … e~D, ~D messages, ~A est tokens]~%~A~%"
            (length stretch) from-id to-id (length stretch) (ctx-fmt-k est) summary)
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
that owns them; carry EVERY following tool result along (parallel
calls produce several)."
  (let ((n (length *messages*)))
    (loop while (and (> i 0)
                     (equal (gethash "role" (nth i *messages*)) "tool"))
          do (decf i))
    (loop while (and (< j (1- n))
                     (equal (gethash "role" (nth (1+ j) *messages*)) "tool"))
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
              ;; Full estimate — content + reasoning + tool-call args,
              ;; via CTX-MESSAGE-CHARS — measured BEFORE the stretch is
              ;; replaced. The content-only estimate skipped tool-call
              ;; arguments and whole-K rounding printed "(0K)".
              (est (round (reduce #'+ (mapcar #'ctx-message-chars stretch))
                          *ctx-chars-per-token*))
              (marker (obj :role "user" :content (ctx-marker-text stretch from-id to-id summary est))))
         ;; Provenance: the core strips x-kind before the request and
         ;; counts x-kind'd messages as system-written, so this marker
         ;; can never become "the current task" and block later rewinds.
         (setf (gethash "x-kind" marker) "collapse")
         (dolist (m stretch)
           (setf (gethash (ctx-entry-id m) *ctx-originals*) (cons :message m)))
         (push (list from-id to-id (ctx-short summary)
                     (ctx-entry-id marker) (length stretch) est)
               *ctx-stretches*)
         (ctx-replace-range i j marker)
         (setf *ctx-requests-since-collapse* 0)
         (ctx-save-state)
         (pane-dirty "context")
         (format nil "[ctx] collapsed entries e~D … e~D (~D messages, ~A est tokens); takes effect from the next request."
                 from-id to-id (length stretch) (ctx-fmt-k est)))))))

(defun ctx-tool-rewind (args)
  "context_rewind {from, to?, summary}"
  (let* ((from (ctx-parse-id (arg args "from")))
         (to (ctx-parse-id (arg args "to")))
         (summary (or (arg args "summary") ""))
         (from-msg (and from (ctx-find-message from)))
         (to-msg (and (or to from) (ctx-find-message (or to from)))))
    (cond
      ((null from-msg) "[ctx] ERROR: unknown from entry — see [entry N] prefixes in tool results.")
      ((null to-msg) "[ctx] ERROR: unknown to entry.")
      (t (ctx-do-rewind from-msg to-msg summary)))))

(defun ctx-tool-offload (args)
  "context_offload {entry, text} — replace one message with the model's
one-line pointer text."
  (let* ((id (ctx-parse-id (arg args "entry")))
         (text (or (arg args "text") ""))
         (msg (and id (ctx-find-message id))))
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
            (ctx-save-state)
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
       (let ((msg (and id (ctx-find-message id))))
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
            (let ((msg (ctx-find-message id)))
              (setf (gethash "content" msg) text)
              (remhash id *ctx-originals*)
              (decf *ctx-offloaded-count*)
              (ctx-save-state)
              (pane-dirty "context")
              (format nil "[ctx] restored entry e~D (~D chars) into the conversation, at its original place."
                      id (length text))))
           (t
            (let ((cell (find-if (lambda (st) (<= (first st) id (second st))) *ctx-stretches*)))
              (cond
                ((null cell)
                 "[ctx] ERROR: collapse record for that entry is gone (image restarted?). Use context_search over the session log.")
                (t
                 (let* ((marker (ctx-find-message (fourth cell)))
                        (at (and marker (position marker *messages* :test #'eq)))
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
                         (ctx-save-state)
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
        (ctx-save-state)
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

(defun ctx-current-percent ()
  "Percent of *CTX-BUDGET* the context takes now (last reported
prompt_tokens or the local estimate, whichever is larger)."
  (round (* 100 (max (or *ctx-last-prompt-tokens* 0) (or *ctx-last-est* 0)))
         *ctx-budget*))

(defun ctx-threshold-note (percent)
  "A note when the context passes a threshold, or NIL.  Edge-triggered:
after noting at P, the next note comes at P+5, so the request is never
peppered with repeats.  PERCENT is of *CTX-BUDGET* (the working
budget), not of the whole window."
  (when (and (>= percent *ctx-note-at*)
             (>= percent (+ *ctx-noted-percent* 5)))
    (setf *ctx-noted-percent* percent)
    (with-output-to-string (s)
      (if (>= percent *ctx-near-at*)
          (format s "[context: ~D% of the ~:D-token budget (past ~D%)] Free space BEFORE going on, in ONE batch: a single context_rewind over the biggest finished stretch (plus offloads of its big results) that frees at least ~~~D% of the context.~%"
                  percent *ctx-budget* *ctx-near-at* *ctx-min-cut-percent*)
          (format s "[context: ~D% of the ~:D-token budget] The biggest finished parts:~%"
                  percent *ctx-budget*))
      (dolist (c (ctx-biggest-finished-results 3))
        (format s "- e~D ≈ ~D est tokens~%" (car c) (cdr c)))
      (format s "Collapse in one go with context_rewind {from, to, summary} only if it frees ~D%+ of the context; skip small edits (each re-caches everything after it). Summaries keep what later work needs verbatim (paths, ids, decisions).~%"
              *ctx-min-cut-percent*))))

(progn
  (defvar *ctx-note-in-flight* nil
    "T when the pending note was injected into a request; cleared when
the response to that request arrives — as text (:assistant) or as tool
calls with no text (:tool-call).")

  (defun ctx-emit-watch (event)
    "Notice finished tasks (a todo marked done) for the next nudge;
retire the nudge only when a response actually arrives (clearing it at
injection would lose it on a failed request)."
    (case (getf event :type)
      (:tool-result
       ;; Edits far back re-cache everything after them (1.25x write vs
       ;; 0.1x read), so a finished task only earns a nudge once the
       ;; context is big enough for one batched collapse to pay back.
       (when (and (equal (getf event :name) "todo")
                  (ctx-head-p "done:" (getf event :text))
                  (>= (ctx-current-percent) *ctx-task-note-at*))
         (setf *ctx-pending-note*
               (format nil "[context: a task is done] ~A The context is at ~D% of the budget. If the finished work is a big share of it (~D%+), collapse it in ONE context_rewind covering everything finished (summaries keep paths, ids, decisions later work needs); otherwise leave it, small edits cost more cache than they save."
                       (ctx-short (getf event :text))
                       (ctx-current-percent) *ctx-min-cut-percent*))))
      ;; A response arrived.  Only retire the note if it was actually
      ;; injected into the request this response answers; a :tool-call
      ;; can also arrive interleaved in the same turn that SET the note.
      (:assistant
       (setf *ctx-pending-note* nil
             *ctx-note-in-flight* nil))
      (:tool-call
       (when *ctx-note-in-flight*
         (setf *ctx-pending-note* nil
               *ctx-note-in-flight* nil))))
    event))
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

(defvar *ctx-args-trim-min* 18000
  "Chars of stubbable old tool-call arguments that must pile up before
CTX-TRIM-CALL-ARGS touches them (each trim re-caches the tail).")

(defun ctx-trimmable-arg-chars (guard limit)
  "Total chars of tool-call arguments over LIMIT before index GUARD."
  (let ((sum 0))
    (loop for m in *messages* for i from 0 while (< i guard)
          do (dolist (tc (coerce (or (gethash "tool_calls" m) #()) 'list))
               (let* ((f (gethash "function" tc))
                      (args (and f (gethash "arguments" f))))
                 (when (and args (> (length args) limit)
                            (not (search "\"_trimmed\"" args :end2 (min 40 (length args)))))
                   (incf sum (length args))))))
    sum))

(defun ctx-trim-call-args (&optional (limit 1500))
  "Stub out huge tool_call ARGUMENTS and old reasoning_content of
assistant messages.  The calls are done, their results follow them,
trace.log has the full text — but each untrimmed blob rides every
request forever (an edit_files batch can be 15k chars; old thinking
blocks added up to 300k chars in one session).
Tool args are trimmed in finished territory (before the latest real
user message).  reasoning_content is dropped anywhere except the live
edge (the last 6 messages): old thinking is never needed again, and
the guard does not move while only synthetic (task/note) messages
arrive, so it would grow without bound in a long task."
  (let* ((guard (ctx-latest-user-index))
         (edge (- (length *messages*) 6))
         (trimmed 0)
         ;; Stubbing old tool args edits far back, which re-caches
         ;; everything after it: wait until it frees enough chars
         ;; (~*CTX-ARGS-TRIM-MIN* ≈ 5k tokens) to be worth one bust.
         (args-ok (>= (ctx-trimmable-arg-chars guard limit) *ctx-args-trim-min*)))
    (loop for m in *messages* for i from 0
          do (when (and args-ok (< i guard))
               (dolist (tc (coerce (or (gethash "tool_calls" m) #()) 'list))
                 (let* ((f (gethash "function" tc))
                        (args (and f (gethash "arguments" f))))
                   (when (and args (> (length args) limit)
                              (not (search "\"_trimmed\"" args :end2 (min 40 (length args)))))
                     (setf (gethash "arguments" f)
                           (format nil "{\"_trimmed\":\"~D chars of args; work finished, trace.log has it\"}"
                                   (length args)))
                     (incf trimmed)))))
             (when (< i edge)
               (let ((rc (gethash "reasoning_content" m)))
                 (when (and (stringp rc) (> (length rc) 800))
                   (remhash "reasoning_content" m)
                   (incf trimmed)))))
    trimmed))

(defun ctx-before-request (body)
  "The compactor's view of every request: assign entry ids, offload
over-long tool results (originals kept), prefix [entry N] marks on tool
results and user messages, and inject threshold / truncation /
milestone notes.  Only the request copy is prefixed; stored messages
stay clean.  Also calibrates the token estimate: PT (reported usage of
the request just answered) against the chars we counted for it."
  (ctx-trim-call-args)
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
                 ;; LOW-PT = PT came in far under the local estimate of
                 ;; that same request (truncation suspected): do NOT
                 ;; recalibrate from it — PT then reflects the
                 ;; truncation, not the true size, and the ratio would
                 ;; adapt the evidence away. The ratio also stays inside
                 ;; sane bounds (2.5–5 chars/token) no matter what.
                 (prev-est (and *ctx-prev-request-chars*
                                (plusp *ctx-prev-request-chars*)
                                (round *ctx-prev-request-chars*
                                       *ctx-chars-per-token*)))
                 (low-pt (and pt prev-est (< pt (* 7/10 prev-est))))
                 (dummy (when (and pt prev-est (plusp pt) (not low-pt))
                          (setf *ctx-chars-per-token*
                                (min 5.0 (max 2.5 (/ (float *ctx-prev-request-chars*) pt))))))
                 (est (max 1 (round chars *ctx-chars-per-token*)))
                 (basis (max (or pt 0) est))
                 (percent (round (* 100 basis) *ctx-budget*)))
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
            (push *ctx-pending-note* notes)
            (setf *ctx-note-in-flight* t))
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

(defun ctx-fmt-k (n)
  "Compact token count: 246 -> \"246\", 3954 -> \"3.9k\", 32768 -> \"32k\".
For approximate counts (rewind sizes) where rounding to whole K would
print 0."
  (cond ((< n 1000) (format nil "~D" n))
        ((< n 10000) (format nil "~,1Fk" (/ n 1000)))
        ((< n 1000000) (format nil "~Dk" (round n 1000)))
        (t (format nil "~,1FM" (/ n 1000000)))))

(defun ctx-fmt-size (n)
  "Human token counts for the context line: 1048576 -> \"1M\", 8192 -> \"8K\";
anything not an exact multiple keeps the full ~:D form."
  (cond ((and (>= n 1048576) (zerop (mod n 1048576)))
         (format nil "~DM" (/ n 1048576)))
        ((and (>= n 1024) (zerop (mod n 1024)))
         (format nil "~DK" (/ n 1024)))
        (t (format nil "~:D" n))))

(defvar *ctx-compact-width* 70
  "Context panes narrower than this (a phone) use the dense view.")

(defun ctx-compact-lines (w h)
  "Dense context view for a narrow pane: a gauge bar of the budget, one
status line, then one truncated line per rewind (newest first as kept in
*ctx-stretches*).  Chunk lists, at most H of them, none wider than W."
  (let* ((usage *last-usage*)
         (pt (and (hash-table-p usage)
                  (integerp (gethash "prompt_tokens" usage))
                  (gethash "prompt_tokens" usage)))
         (tokens (or pt *ctx-last-est*))
         (pct (if (plusp *ctx-budget*) (round (* 100 tokens) *ctx-budget*) 0))
         (colour (cond ((>= pct 90) 1) ((>= pct 70) 3) (t 2)))
         (bar-w (max 4 (min 24 (- w 16))))
         (filled (min bar-w (round (* bar-w (min 100 pct)) 100)))
         (lines '()))
    (flet ((add (&rest chunks) (push chunks lines))
           (fit (s n) (str-truncate s (max 1 n))))
      (add (cons (fit (format nil "~A~A " (if pt "" "~") (ctx-fmt-k tokens)) (- w 1)) 7)
           (cons (make-string filled :initial-element #\█) colour)
           (cons (make-string (- bar-w filled) :initial-element #\░) 8)
           (cons (format nil " ~D%" pct) colour))
      (add (cons (fit (format nil "~D msgs~A~A"
                              (length *messages*)
                              (if (plusp *ctx-offloaded-count*)
                                  (format nil " · ~D off" *ctx-offloaded-count*) "")
                              (if *ctx-pending-note* " · note" ""))
                      w)
                 8))
      (dolist (st *ctx-stretches*)
        (let ((head (format nil "e~D-~D ~A " (first st) (second st) (ctx-fmt-k (sixth st)))))
          (add (cons head 6)
               (cons (fit (substitute #\Space #\Newline (or (third st) ""))
                          (- w (length head)))
                     8)))))
    (let ((out (nreverse lines)))
      (subseq out 0 (min h (length out))))))

(defun pane-context-render (pane w h)
  "Context size and what the compactor has collapsed, for the pane.
Returns chunk LISTS (a chunk is (text . attr)).  Main number is the
REAL prompt_tokens of the last request; the local estimate is only
shown as what has grown since.  Narrow panes get the dense view."
  (declare (ignore pane))
  (when (< w *ctx-compact-width*)
    (return-from pane-context-render (ctx-compact-lines w h)))
  (labels ((wrap (text width)
             ;; Word-wrap TEXT to WIDTH columns; continuation lines get a
             ;; two-space hanging indent.  Over-long words are split.
             (let ((out '()) (cur "") (width (max 8 width)))
               (flet ((flush () (when (plusp (length cur))
                                  (push cur out)
                                  (setf cur "  "))))
                 (dolist (word (let ((ws '()) (start 0))
                                 (loop for i from 0 to (length text)
                                       do (when (or (= i (length text))
                                                    (char= (char text i) #\Space))
                                            (when (> i start)
                                              (push (subseq text start i) ws))
                                            (setf start (1+ i))))
                                 (nreverse ws)))
                   (loop while (> (length word) (- width 2))
                         do (flush)
                            (push (concatenate 'string cur (subseq word 0 (- width (length cur))))
                                  out)
                            (setf word (subseq word (- width (length cur)))
                                  cur "  "))
                   (cond ((zerop (length cur)) (setf cur word))
                         ((<= (+ (length cur) 1 (length word)) width)
                          (setf cur (if (string= cur "  ")
                                        (concatenate 'string cur word)
                                        (concatenate 'string cur " " word))))
                         (t (flush)
                            (setf cur (concatenate 'string cur word)))))
                 (when (plusp (length (string-trim " " cur))) (push cur out)))
               (or (nreverse out) (list ""))))
           (line (text attr)
             ;; One logical entry -> its wrapped chunk-lines.
             (mapcar (lambda (s) (list (cons s attr))) (wrap text (1- w)))))
    (let* ((usage *last-usage*)
           (pt (and (hash-table-p usage)
                    (integerp (gethash "prompt_tokens" usage))
                    (gethash "prompt_tokens" usage)))
           (est *ctx-last-est*)
           (lines
            (append
             (line
              (if pt
                  (format nil "context: ~A tokens (~D% of ~A, ~D% of budget)"
                          (ctx-fmt-k pt)
                          (round (* 100 pt) *ctx-window*) (ctx-fmt-size *ctx-window*)
                          (round (* 100 pt) *ctx-budget*))
                  (format nil "context: ~A est tokens" (ctx-fmt-k est)))
              7)
             (line
              (format nil "+~A est since · ~D messages"
                      (ctx-fmt-k (if pt (max 0 (- est pt)) 0))
                      (length *messages*))
              8)))
           (extra '()))
      (dolist (st *ctx-stretches*)
        (setf extra (append (reverse (line (format nil "rewind e~D…e~D: ~A (~A)"
                                                   (first st) (second st) (third st)
                                                   (ctx-fmt-k (sixth st)))
                                           8))
                            extra)))
      (when (plusp *ctx-offloaded-count*)
        (setf extra (append (reverse (line (format nil "offloaded results: ~D" *ctx-offloaded-count*) 8))
                            extra)))
      (when *ctx-pending-note*
        (setf extra (append (reverse (line "milestone note: pending" 3)) extra)))
      (setf lines (append lines (nreverse extra)))
      (subseq lines 0 (min h (length lines))))));;; --------------------------------------------------------------- start

(defun ctx-init-panes ()
  "Re-register the context pane after every PANES-INIT (the init wipes
the registry and resets the layout, so this must run again on each UI
restart — hooked via *PANES-INIT-HOOKS*).  Idempotent."
  (block ctx-init-panes
    (unless (pane-get "context")
      (pane-register "context" :title "context" :kind :buffer))
    (setf (pane-render (pane-get "context")) 'pane-context-render)
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
    (ctx-load-state)
    (pushnew 'ctx-before-request *before-request*)
    ;; The todo-done watcher lives on *observers*, not an *emit* wrapper:
    ;; tui-start rebinds *emit* at every UI (re)start and would drop it.
    (pushnew 'ctx-emit-watch lisping-core:*observers*)
    (when (and (stringp *system-prompt*)
               (not (search "[context:compactor]" *system-prompt*)))
      (setf *system-prompt*
            (concatenate 'string *system-prompt* "
- [context:compactor] Keep your context lean, but edit it rarely: a rewind/offload far back makes the provider re-cache everything after it (about 12x the price of a cache read), so small edits never pay back. Batch: wait until finished work is at least ~20% of the context or you are near the budget, then collapse it all in ONE context_rewind {from, to, summary}, keeping in the summary what later work needs verbatim (paths, line numbers, ids, decisions). Big tool results are trimmed automatically when they arrive; context_offload {entry, text} is for a rare huge old one. Tool results and user messages carry [entry N] ids; name them in the tools. Nothing is deleted: context_search {text} finds any original, even in earlier sessions' logs, and context_restore {entry} reads it back.")))
    (when (and (stringp *system-prompt*)
               (not (search "[eval:docs]" *system-prompt*)))
      (setf *system-prompt*
            (concatenate 'string *system-prompt* "
- [eval:docs] eval returns the forms' values as the tool result (\"=> value, ...\", or an error backtrace); with `file` it returns a status line (\"Wrote and loaded ...; committed\", or NOT SAVED with the boot problems). Example: {\"code\":\"(+ 1 2)\"} => 3.")))
    (register-tool "context_rewind"
                   "Collapse a finished stretch of the conversation — entries `from` through `to` inclusive (`to` defaults to `from`; ids are the N in [entry N] marks, e.g. \"12\" or \"e12\") — replacing it with your `summary` plus an index of what it replaced. Rewind only FINISHED work before the latest user message. The originals stay in the session log: context_search finds them, context_restore reads them back. The summary keeps what later work needs verbatim (paths, line numbers, ids, decisions) and ends with a Keywords: line. Example: {\"from\":\"40\",\"to\":\"52\",\"summary\":\"Weather pane done, lisp/panes.lisp ...\\nKeywords: weather pane\"}. Returns a confirmation naming the collapsed range."
                   (obj :type "object" :properties
                        (obj :from (obj :type "string" :description "entry id, as in [entry N] (e.g. \"12\" or \"e12\")")
                             :to (obj :type "string" :description "entry id; defaults to `from`")
                             :summary (obj :type "string" :description "your summary of the stretch, ending with a Keywords: line"))
                        :required (vector "from" "summary"))
                   #'ctx-tool-rewind)
    (register-tool "context_offload"
                   "Replace one message (usually a big tool result) in the conversation with your one-line pointer `text` — what the entry was and what matters about it. `entry` is the id from its [entry N] mark. The original stays retrievable with context_restore. Do not offload the latest user message or anything you still need verbatim. Returns a confirmation. Example: {\"entry\":\"57\",\"text\":\"pane drag test output: ratio moved and saved\"}."
                   (obj :type "object" :properties
                        (obj :entry (obj :type "string" :description "entry id")
                             :text (obj :type "string" :description "one line saying what this was and what matters about it"))
                        :required (vector "entry" "text"))
                   #'ctx-tool-offload)
    (register-tool "context_restore"
                   "Read back an original entry; `entry` is the id from [entry N]. With `offset` and `length` (characters; `length` at most 16000): read just that range of the original — the conversation is unchanged. Without them: put the original back into the conversation (an offloaded result at its place; a collapsed entry as a quoted message after its collapse marker). Returns the original text (possibly truncated to `length`). Example: {\"entry\":\"57\",\"offset\":\"0\",\"length\":\"4000\"}."
                   (obj :type "object" :properties
                        (obj :entry (obj :type "string" :description "entry id")
                             :offset (obj :type "string" :description "character offset into the original")
                             :length (obj :type "string" :description "how many characters to read, at most 16000"))
                        :required (vector "entry"))
                   #'ctx-tool-restore)
    (register-tool "context_search"
                   "Full-text search over the visible conversation, offloaded/collapsed originals, and the current and previous session logs. `text` is literal words, not meaning; `limit` caps the hits (default 10). Returns one line per hit — where the text lives (entry id or log file) plus a snippet; read the original back with context_restore. Example: {\"text\":\"panes-save\",\"limit\":\"5\"}."
                   (obj :type "object" :properties
                        (obj :text (obj :type "string" :description "text to find (words, not meaning)")
                             :limit (obj :type "string" :description "max hits, default 10"))
                        :required (vector "text"))
                   #'ctx-tool-search))
  t)

