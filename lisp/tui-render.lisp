;;;; lisp/tui-render.lisp -- turning agent events into chat lines and
;;;; compositing chat, status bar and input line into the grid.
;;;; Draws only from the redraw thread; takes *tui-lock* while reading.

(in-package :lisping)

(declaim (ftype function tui-draw-separator
                tui-tool-call-line tui-tool-summary tui-chat-result
                tui-replay-history panes-ensure-canvas panes-draw))

(declaim (special *todos* *panes-canvas*))

;;; ---------------------------------------------------------------
;;; Frame state and perf counters. Only the redraw thread touches these.
;;; The previous frame's grid is kept so frames can be diffed: a full
;;; redraw (~18KB) happens only when *TUI-PREV-GRID* is NIL — on UI
;;; start, on resize, on C-b r — never per keystroke.

(defvar *tui-prev-grid* nil "Grid of the last frame written to the tty.")

(defvar *tui-prev-cursor* nil "Last (row . col) the cursor was placed at.")

(defvar *tui-clock-minute* nil "Minute value drawn in the status bar.")

(defvar *tui-frames-total* 0 "TUI-DRAW calls, including skipped ones.")

(defvar *tui-frames-skipped* 0 "Frames where nothing needed writing.")

(defvar *tui-frames-full* 0 "Frames written as full renders.")

(defvar *tui-bytes-total* 0 "Bytes written to the tty by TUI-DRAW.")

(defun tui-perf ()
  "Plist of frame counters for measuring redraw cost."
  (list :frames *tui-frames-total*
        :skipped *tui-frames-skipped*
        :full *tui-frames-full*
        :bytes *tui-bytes-total*))

(defun tui-force-redraw ()
  "Repaint from scratch (C-b r): fresh panes canvas, every pane dirty,
no diff against the previous frame.  Dropping only *tui-prev-grid* is
not enough — it would faithfully re-blit a corrupted *panes-canvas*."
  (tui-drop-frame))

;;; ---------------------------------------------------------------
;;; Hooks: events in, chat buffer out.

(defun tui-install-hooks ()
  (setf *emit* 'tui-emit))

(defvar *tui-display-error-times* nil
  "Recent universal-times of display (per-event formatting) errors, for
the flood detector in TUI-DISPLAY-ERROR.")

(defun tui-display-error (ui condition)
  "One event failed to render: log it, note it in the chat, stay alive.
Ten in a minute means the renderer itself is broken — escalate to
TUI-CRASH so the supervised-restart path engages."
  (tui-log-crash condition)
  (let ((now (get-universal-time)))
    (setf *tui-display-error-times*
          (remove-if (lambda (t0) (> (- now t0) 60)) *tui-display-error-times*))
    (push now *tui-display-error-times*)
    (if (>= (length *tui-display-error-times*) 10)
        (tui-crash (format nil "display error flood: ~A" condition))
        (ignore-errors
          (sb-thread:with-recursive-lock (*tui-lock*)
            (when (eq *tui* ui)
              (uibuf-append (tui-ui-chat ui)
                            (list (cons (format nil "[display error: ~A]" condition) 8))
                            :fresh t)))
          (tui-dirty))))
  nil)

(defun tui-emit (event)
  "Receives every agent event.  A failure to FORMAT one event must not
take the UI down (21:26: an unlucky eval arg string killed the whole
display): log it, count it, show a '[display error]' line in the chat
and keep going.  Only a flood of display errors (10 in a minute)
escalates to TUI-CRASH, which stops and supervises a restart."
  (let ((ui *tui*))
    (if (null ui)
        (terminal-emit event)
        (handler-bind ((serious-condition #'tui-log-crash-at-signal))
          (handler-case (tui-log-event ui event)
            (serious-condition (c)
              (when (eq *tui* ui) (tui-display-error ui c))))))))


(defun tui-show-user (ui text)
  "Log a user message unless it is already on screen. Upgrades the dimmed
queued echo of the same text instead of duplicating it."
  (sb-thread:with-recursive-lock (*tui-lock*)
    (when (and (eq *tui* ui)
               (not (equal text (tui-ui-shown-user ui))))
      (when (member text *tui-queued-text* :test #'equal)
        (uibuf-remove-matching
         (tui-ui-chat ui)
         (lambda (ln)
           (string= (chunks-text (uline-chunks ln))
                    (concatenate 'string "you (queued)> " text))))
        (setf *tui-queued-text* (delete text *tui-queued-text*
                                        :test #'equal :count 1)))
      (setf (tui-ui-shown-user ui) text
            (tui-ui-pending-user ui) nil
            (tui-ui-scroll ui) 0)
      (uibuf-append (tui-ui-chat ui)
                    (list (cons "you> " (ui-attr 2 :bright t))
                          (cons text (ui-attr 2 :bright t)))
                    :fresh t)))
  (tui-dirty))

(defun tui-show-queued (ui text)
  "Echo a just-submitted message dimmed, ahead of the model picking it
up; TUI-SHOW-USER replaces this line when the :user event arrives.
Queued messages can pile up (the core holds them until the turn ends),
so *TUI-QUEUED-TEXT* is a list, oldest first."
  (when (and (eq *tui* ui) (stringp text) (plusp (length text))
             (not (member text *tui-queued-text* :test #'equal)))
    (sb-thread:with-recursive-lock (*tui-lock*)
      (when (eq *tui* ui)
        (setf *tui-queued-text* (append *tui-queued-text* (list text)))
        (uibuf-append (tui-ui-chat ui)
                      (list (cons "you (queued)> " 8)
                            (cons text 8))
                      :fresh t)))
    (tui-dirty)))

(defun tui-log-event (ui event)
  (let ((type (getf event :type)))
    (case type
      (:assistant (tui-chat-add ui (ui-parse-ansi (getf event :text))))
      (:thinking (tui-chat-add ui (list (cons (getf event :text) 8))))
      (:tool-call
       (tui-chat-add ui (tui-tool-call-line (getf event :name)
                                            (getf event :args))))
      (:tool-result (tui-chat-result ui (getf event :name) (getf event :text)))
      (:error (tui-chat-add ui (ui-parse-ansi (getf event :text))))
      (:info (tui-chat-add ui (list (cons (getf event :text) 8))))
      (:queued (tui-show-queued ui (getf event :text)))
      (:user (let ((text (getf event :text)))
               (unless (equal text (tui-ui-pending-user ui))
                 (tui-show-user ui text))))
      (t nil))))

(defun tui-chat-note-new (n)
  "N new wrapped lines arrived in the chat. With the user scrolled up,
raise the scroll offset by N so the reading position stays put, and
count toward the \"↓ N new\" marker; at the bottom (scroll 0) the chat
stays at the bottom and nothing is counted."
  (let ((p (pane-of-kind :chat)))
    (when (and p (plusp n) (plusp (pane-scroll p)))
      (sb-thread:with-recursive-lock (*tui-lock*)
        (incf (pane-scroll p) n)
        (incf *tui-chat-new* n)
        (setf (pane-needs-redraw p) 1)))))

(defun tui-chat-add (ui chunks)
  "Append CHUNKS to the chat buffer, one buffer line per text line:
embedded newlines must never reach the grid."
  (when (eq *tui* ui)
    (sb-thread:with-recursive-lock (*tui-lock*)
      (when (eq *tui* ui)
        (let ((added 0))
          (dolist (line (tui-chunk-lines chunks))
            (uibuf-append (tui-ui-chat ui) line :fresh t)
            (incf added (max 1 (length (ui-wrap-chunks
                                        line (or *tui-chat-pane-width* 80))))))
          (tui-chat-note-new added))))
    (pane-dirty "chat")))

(defun tui-chunk-lines (chunks)
  "Split (text . attr) CHUNKS into a list of chunk-lists, one per line."
  (let ((out nil)
        (cur nil))
    (dolist (ck chunks)
      (let ((s (car ck))
            (a (cdr ck))
            (start 0))
        (loop
          (let ((p (position #\Newline s :start start)))
            (if p
                (progn
                  (push (cons (subseq s start p) a) cur)
                  (push (nreverse cur) out)
                  (setf cur nil)
                  (setf start (1+ p)))
                (progn
                  (push (cons (subseq s start) a) cur)
                  (return)))))))
    (when cur (push (nreverse cur) out))
    (nreverse out)))

(defun first-line (s)
  (subseq s 0 (or (position #\Newline s) (length s))))

(defun str-truncate (s n)
  "Cut S to N chars. A cut result ends with … (the caller renders it
dim) so a shortened line never passes for a complete one."
  (if (> (length s) n)
      (concatenate 'string (subseq s 0 (max 0 (1- n))) "…")
      s))

(defun tui-short-args (args)
  (let ((s (cond ((and (hash-table-p args) (stringp (gethash "code" args)))
                  (first-line (gethash "code" args)))
                 ((and (hash-table-p args) (stringp (gethash "command" args)))
                  (gethash "command" args))
                 ((hash-table-p args) (json-encode args))
                 ((stringp args) args)
                 (t (princ-to-string args)))))
    (str-truncate s 90)))

(defun tui-chat-result (ui name text)
  "Short form of a tool result, set apart from the conversation: dim,
indented. Up to 2 lines are shown; longer output collapses into a
single \"<- N lines\" count line so machine output (especially screen
captures of the UI itself) never reads as chat."
  (declare (ignore name))
  (when (stringp text)
    (let* ((lines (tui-chunk-lines (ui-parse-ansi (str-truncate text 2000))))
           (n (length lines)))
      (cond ((> n 2)
             (tui-chat-add ui (list (cons (format nil "  <- ~D lines" n) 8))))
            (t
             (dolist (line lines)
               (tui-chat-add ui (list (cons (concatenate 'string
                                                         "  " (chunks-text line))
                                            8)))))))))

;;; ---------------------------------------------------------------
;;; Tool-line summaries: what a call actually did, in a glance.

(defun tui-unwrap (form)
  "Descend through wrapping forms (progn/let/flet/handler-case/...) to
the first real form inside — \"(progn\" alone says nothing."
  (loop while (and (consp form) (symbolp (car form))
                   (member (car form) '(progn let let* flet labels handler-case
                                        multiple-value-progn unwind-protect)))
        do (setf form (cond ((or (eq (car form) 'progn)
                                 (eq (car form) 'multiple-value-progn))
                             (and (cdr form) (cadr form)))
                            ((eq (car form) 'unwind-protect) (cadr form))
                            (t (and (cddr form) (caddr form))))))
  form)

(defun tui-collect-defs (form &optional acc)
  "Names of definitions anywhere in FORM: 'defun foo', 'defmacro
bar', ... ACCUM is accumulated in reverse.  Same dotted-pair rule as
TUI-COLLECT-CALLS: never dolist a raw subform."
  (cond ((not (consp form)) acc)
        ((and (symbolp (car form))
              (member (car form) '(defun defmacro defvar defparameter defstruct)))
         (cons (format nil "~A ~A" (car form)
                       (let ((n (cadr form)))
                         (if (consp n) (car n) n)))
               acc))
        ((and (symbolp (car form))
              (member (car form) '(progn let let* labels flet handler-case)))
         (loop for x on (cdr form) while (consp x)
               do (setf acc (tui-collect-defs (car x) acc)))
         acc)
        (t acc)))


(defun tui-collect-calls (form &optional acc (depth 0))
  "Function symbols called by FORM, unique, discovery order, for
summaries like 'tui-start, sleep, shell'.  Never DOLISTs a subform:
eval args may hold alists whose CDRs are source STRINGS, and LISTP is
vacuous on conses (true for dotted pairs) — walk the cdr chain with
'for x on' instead, stopping at the first atom (the 21:26 UI kill)."
  (cond ((or (not (consp form)) (> depth 4)) acc)
        ((and (symbolp (car form))
              (not (member (car form) '(quote lambda function block return-from
                                       prog1 multiple-value-progn))))
         (unless (member (car form) acc) (setf acc (cons (car form) acc)))
         (loop for x on (cdr form) while (consp x)
               do (setf acc (tui-collect-calls (car x) acc (1+ depth))))
         acc)
        (t
         (loop for x on form while (consp x)
               do (setf acc (tui-collect-calls (car x) acc (1+ depth))))
         acc)))



(defun tui-eval-summary (code)
  "One line describing an eval's CODE: the definitions it makes, else
the functions it calls, else its first line."
  (let ((form (let ((*package* (find-package :lisping)))
                (ignore-errors (read-from-string code nil nil)))))
    (if (not form)
        (str-truncate (first-line code) 60)
        (let* ((form (tui-unwrap form))
               (defs (reverse (tui-collect-defs form)))
               (calls (reverse (tui-collect-calls form))))
          (cond (defs (format nil "~{~A~^, ~}"
                              (mapcar #'string-downcase
                                      (subseq defs 0 (min 3 (length defs))))))
                (calls (format nil "~{~A~^, ~}"
                               (mapcar #'string-downcase
                                       (subseq calls 0 (min 3 (length calls))))))
                (t (str-truncate (first-line (princ-to-string form)) 60)))))))

(defun tui-tool-summary (name args)
  "One short phrase saying what a tool call did: edit_file -> the
path; eval -> what it defines or calls; otherwise the short args."
  (cond ((string= name "edit_file")
         (format nil "edit ~A" (if (hash-table-p args)
                                   (or (gethash "path" args) "?")
                                   "?")))
        ((string= name "eval")
         (format nil "eval: ~A" (tui-eval-summary
                                 (if (hash-table-p args)
                                     (or (gethash "code" args) "")
                                     ""))))
        (t (format nil "~A: ~A" name (tui-short-args args)))))

(defun tui-tool-call-line (name args)
  "Chunk-list for a tool-call line: dim and indented, set apart from
the conversation.  Summarizing must never kill the emit path: on any
error fall back to the eval code's first line (or just the name)."
  (list (cons (format nil "  -> ~A"
                      (handler-case (tui-tool-summary name args)
                        (serious-condition ()
                          (let ((code (and (hash-table-p args)
                                           (gethash "code" args))))
                            (if (stringp code)
                                (format nil "eval: ~A"
                                        (str-truncate (first-line code) 60))
                                (format nil "~A" name))))))
              8)))


(defun tui-tool-name (tc)
  "Tool name of a *messages* tool_call entry (OpenAI layout: under
\"function\"; tolerate a flat \"name\" too)."
  (let ((fn (and (hash-table-p tc) (gethash "function" tc))))
    (or (and (hash-table-p fn) (gethash "name" fn))
        (and (hash-table-p tc) (gethash "name" tc)))))

(defun tui-replay-history (ui)
  "Rebuild the chat buffer from LISPING-CORE:*MESSAGES* so a UI
restart keeps the conversation: user messages become you> lines,
assistant text becomes chat lines, tool_calls and tool results become
the same dim indented lines the live events produce. System/developer
and empty messages are skipped."
  (let ((names (make-hash-table :test 'equal)))
    ;; tool_call_id -> tool name, for naming result lines.
    (dolist (m lisping-core:*messages*)
      (let ((tcs (gethash "tool_calls" m)))
        (when tcs
          (dotimes (i (length tcs))
            (let ((tc (aref tcs i)))
              (when (and (tui-tool-name tc) (gethash "id" tc))
                (setf (gethash (gethash "id" tc) names) (tui-tool-name tc))))))))
    (dolist (m lisping-core:*messages*)
      (let ((role (or (gethash "role" m) ""))
            (content (gethash "content" m)))
        (cond ((string= role "user")
               (when (and (stringp content) (plusp (length content)))
                 (tui-show-user ui content)))
              ((string= role "assistant")
               (when (and (stringp content) (plusp (length content)))
                 (tui-chat-add ui (ui-parse-ansi content)))
               (let ((tcs (gethash "tool_calls" m)))
                 (when tcs
                   (dotimes (i (length tcs))
                     (let ((tc (aref tcs i)))
                       (tui-chat-add ui (tui-tool-call-line
                                         (tui-tool-name tc)
                                         (when (hash-table-p (gethash "function" tc))
                                           (ignore-errors
                                             (json-decode (or (gethash "arguments"
                                                                       (gethash "function" tc))
                                                              "{}")))))))))))
              ((string= role "tool")
               (when (stringp content)
                 (tui-chat-result ui (gethash (gethash "tool_call_id" m) names)
                                  content))))))
    ;; The replay ends at the newest message; a queued echo would be stale.
    (setf *tui-queued-text* nil)))

(defun chunks-text (chunks)
  (with-output-to-string (out)
    (dolist (ck chunks) (write-string (car ck) out))))

;;; ---------------------------------------------------------------
;;; Drawing. Only the redraw thread calls TUI-DRAW.

(defun tui-minute-changed-p ()
  "True when the wall-clock minute differs from the one on screen."
  (/= (or *tui-clock-minute* -1)
      (nth-value 1 (decode-universal-time (get-universal-time)))))

(defun tui-seconds-to-next-minute ()
  "Sleep budget for the redraw wait: wake when the clock may tick over."
  (- 60 (nth-value 0 (decode-universal-time (get-universal-time)))))

(defun tui-redraw-loop (ui)
  (loop while (and *tui* (eq *tui* ui))
        do (handler-bind ((serious-condition #'tui-log-crash-at-signal))
             (handler-case
                 (progn
                   ;; Wait for work.  CONDITION-WAIT with :TIMEOUT can
                   ;; return NIL WITHOUT re-acquiring the lock (the
                   ;; timeout covers re-acquisition, per its docstring).
                   ;; The next CONDITION-WAIT must then NOT run inside
                   ;; this WITH-RECURSIVE-LOCK: it would release a mutex
                   ;; the thread does not hold and kill the UI ("not
                   ;; holding #<MUTEX>", tui-crash.log 20:20:49).  Bail
                   ;; out of the lock scope entirely and re-acquire on
                   ;; the next iteration instead.
                   (block wait-done
                     (loop
                       (sb-thread:with-recursive-lock (*tui-lock*)
                         (loop while (and *tui* (eq *tui* ui) (zerop *tui-dirty*))
                               do (if (sb-thread:condition-wait *tui-cv* *tui-lock*
                                                                :timeout 2)
                                      ;; Wake every 2 s: TUI-DRAW runs
                                      ;; TUI-MAYBE-UPDATE-SIZE (a cheap
                                      ;; counter/time check — stty only
                                      ;; when SIGWINCH fired or 2 s
                                      ;; passed), so a terminal resize is
                                      ;; noticed even when nothing else
                                      ;; dirties the TUI.  Also the
                                      ;; once-a-minute clock tick.
                                      (when (and (zerop *tui-dirty*)
                                                 (tui-minute-changed-p))
                                        (incf *tui-dirty*))
                                      (return-from wait-done)))
                         ;; Consume the flag BEFORE drawing: emitters
                         ;; append state first and bump the counter under
                         ;; the same lock, so nothing can be lost; a bump
                         ;; during the draw schedules exactly one more
                         ;; pass.
                         (when (and *tui* (eq *tui* ui))
                           (setf *tui-dirty* 0))
                         (return-from wait-done))))
                   (when (and *tui* (eq *tui* ui)) (tui-draw ui)))
               (serious-condition (c)
                 (when (eq *tui* ui) (tui-crash c)))))))

(defun tui-draw (ui)
  (tui-maybe-update-size ui)
  (sb-thread:with-recursive-lock (*tui-lock*)
    (when (and *tui* (eq *tui* ui))
      (let* ((w (tui-ui-width ui))
             (h (tui-ui-height ui))
             ;; Panes own rows 0..pane-h-1 (each with its own top
             ;; border); then the queue rows, input, status.
             (pane-h (max 3 (- h 2 (tui-queue-rows ui))))
             (g (ui-grid-make w h)))
        (panes-ensure-canvas w pane-h)
        (panes-draw *panes-canvas*)
        (replace (uigrid-chars g) (uigrid-chars *panes-canvas*))
        (replace (uigrid-attrs g) (uigrid-attrs *panes-canvas*))
        (tui-draw-queue g ui pane-h w)
        (tui-draw-status g ui (1- h) w)
        (let* ((input-row (- h 2))
               ;; TUI-DRAW-INPUT returns the cursor column: with
               ;; horizontal scroll it is not prompt-len + cursor + 1.
               (col (tui-draw-input g ui input-row w))
               ;; The cursor position in the frame is 1-based (ANSI).
               (row (1+ input-row))
               (full (or (null *tui-prev-grid*)
                         (/= (uigrid-w *tui-prev-grid*) w)
                         (/= (uigrid-h *tui-prev-grid*) h)))
               (body (ui-grid-diff-render (and (not full) *tui-prev-grid*) g)))
          (incf *tui-frames-total*)
          (cond ((and (zerop (length body))
                      (equal *tui-prev-cursor* (cons row col)))
                 ;; Nothing changed: write nothing at all.
                 (incf *tui-frames-skipped*))
                (t
                 (when full (incf *tui-frames-full*))
                 ;; One synchronized update per frame: the terminal shows
                 ;; it atomically, so the cursor does not flicker. ?25h
                 ;; re-shows the cursor hidden by ?25l at UI start — it
                 ;; must be inside the sync wrap, after positioning.
                 (let ((frame (format nil "~C[?2026h~A~C[~D;~DH~C[?25h~C[?2026l"
                                      #\Esc body #\Esc row col #\Esc #\Esc)))
                   (incf *tui-bytes-total* (length frame))
                   (tty-write frame))))
          (setf *tui-prev-grid* g
                *tui-prev-cursor* (cons row col)
                *tui-clock-minute* (nth-value 1 (decode-universal-time
                                                 (get-universal-time)))))))))

(defun tui-queue-rows (ui)
  "Rows the queue display takes above the input line (0 when the queue
is empty): one per waiting message, at most 4."
  (min 4 (length (lisping-core:queued-messages))))

(defun tui-draw-queue (g ui y w)
  "Draw the waiting user messages stuck above the input line, oldest
first, selected one highlighted (TUI-UI-QUEUE-SEL), the one loaded in
the input line marked (TUI-UI-QUEUE-EDIT). Y is the first queue row —
the panes end there."
  (let* ((queued (lisping-core:queued-messages))
         (n (length queued)))
    (when (plusp n)
      (let* ((rows (tui-queue-rows ui))
             ;; The newest messages sit closest to the input line.
             (shown (last queued rows))
             (first-i (- n (length shown)))) ; 0-based index of SHOWN's first
        (loop for text in shown for k from 0
              for i = (+ first-i k)
              for row = (+ y k)
              for sel = (eql (tui-ui-queue-sel ui) i)
              for editing = (and (tui-ui-queue-edit ui)
                                 (eq (tui-ui-queue-edit ui) text))
              for attr = (cond (sel (ui-attr 0 :bg 6))
                               (editing (ui-attr 6))
                               (t 8))
              do (ui-grid-write g 0 row
                                (str-truncate
                                 (format nil "~A~D: ~A"
                                         (cond (sel "→ ") (editing "✎ ") (t "  "))
                                         (1+ i) text)
                                 w)
                                attr w))))))

(defun tui-draw-separator (g y w)
  "Thin dim line between the chat pane and the input row."
  (dotimes (x w) (ui-grid-put g x y #\─ 8)))

(defun tui-visible-lines (pane buf width height)
  "Top-first chunk-lists fitting HEIGHT rows, scrolled up the pane's
scroll offset. The offset is clamped to [0, n-HEIGHT] (n = wrapped line
count) and written back to the pane: when content fits, scrolling does
nothing; the offset can never eat the newest lines or build up past
the top."
  (let* ((src (uibuf-lines buf))
         (n0 (length src))
         (recent (loop for i from (max 0 (- n0 400)) below n0
                       collect (aref src i)))
         (all (loop for ln in recent
                    append (or (ui-wrap-chunks (uline-chunks ln) width)
                               (list nil))))
         (n (length all))
         (s (max 0 (min (pane-scroll pane) (max 0 (- n height))))))
    (setf (pane-scroll pane) s)
    (let* ((end (max 0 (- n s)))
           (start (max 0 (- end height))))
      (subseq all start end))))

(defun tui-input-window (len cursor avail hscroll)
  "Horizontal window of the input text for AVAIL screen columns (what
remains of the row after the prompt), given the current HSCROLL.
Returns (values start count left right new-hscroll): draw COUNT chars
starting at START; LEFT/RIGHT call for a dim </> overflow marker at that
edge. The window scrolls minimally to keep CURSOR visible; while
scrolling, one column per edge is reserved for the markers. The cursor
never lands on a marker column. Pure: the caller stores NEW-HSCROLL
back into the UI's input-hscroll slot."
  (if (or (<= len avail) (< avail 3))
      (values 0 (min len avail) nil (> len avail) 0)
      (let* ((inner (- avail 2))
             (hs (max 0 (min hscroll cursor (- len inner)))))
        (when (> (- cursor hs) (1- inner))
          (setf hs (1+ (- cursor inner))))
        (let ((end (min len (+ hs inner))))
          (values hs (- end hs) (> hs 0) (< end len) hs)))))

(defun tui-draw-input (g ui y w)
  "Draw prompt and the horizontally scrolled input line. Returns the
terminal column (1-based) the cursor belongs on."
  (let* ((prefix (tui-ui-prefix ui))
         (pattr (if prefix (ui-attr 3 :bright t) (ui-attr 2 :bright t)))
         (prompt (if prefix "*> " "> "))
         (text (coerce (tui-ui-input ui) 'string))
         (plen (length prompt))
         (cursor (tui-ui-input-cursor ui)))
    (ui-grid-write g 0 y prompt pattr w)
    (multiple-value-bind (start count left right hs)
        (tui-input-window (length text) cursor (- w plen)
                          (tui-ui-input-hscroll ui))
      (setf (tui-ui-input-hscroll ui) hs)
      (let ((x plen))
        (when left (ui-grid-put g x y #\‹ 8) (incf x))
        (ui-grid-write g x y (subseq text start (+ start count)) 7 w)
        (when right (ui-grid-put g (1- w) y #\› 8))
        (+ x (- cursor start) 1)))))

(defun tui-held-count ()
  "User messages the core is holding until the current turn ends.
Returns an integer."
  (typecase lisping-core:*held*
    (integer lisping-core:*held*)
    (list (length lisping-core:*held*))
    (t 0)))

(defun tui-draw-status (g ui y w)
  (let ((attr (ui-attr 7 :bright t :bg 4)))
    (dotimes (x w) (ui-grid-put g x y #\Space attr))
    (ui-grid-write g 1 y (format nil " lisping-ai ~A [~D todo~:P open] ~A~@[ [~D held]~]"
                                 *model*
                                 (count-if-not #'cdr *todos*)
                                 *pane-focus*
                                 (let ((h (tui-held-count)))
                                   (unless (zerop h) h)))
                   attr (1- w))
    (let* ((now (multiple-value-list (decode-universal-time (get-universal-time))))
           (time (format nil "~2,'0d:~2,'0d " (nth 2 now) (nth 1 now)))
           (hint (if (tui-ui-prefix ui)
                     "C-b cancel | q quit | r redraw | Left/Right pane | 1-9 pane"
                     "C-b prefix")))
      (ui-grid-write g (max 1 (- w (length hint) (length time) 1)) y
                     (concatenate 'string hint " " time) attr w))))
