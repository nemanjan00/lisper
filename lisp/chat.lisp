;;;; lisp/chat.lisp -- the chat as Claude Code draws it.
;;;;
;;;; The chat buffer (a UIBUF) holds ULINEs; the lines of this file are
;;;; CLINEs (a ULINE plus HANG, FILL, GEN, TAG).  A plain cline is one
;;;; logical line wrapped with a hanging indent.  A cline with GEN makes
;;;; its rows at draw time from live state (a tool run that was expanded,
;;;; a user message wrapped under its bar), so expanding never rewrites
;;;; the buffer.  Everything that reads the chat (drawing, /find, the
;;;; click hit-test) goes through CHAT-ROWS, so they all see the same rows
;;;; and CHAT-ROWS also says which tool run or call each row belongs to.
;;;;
;;;; Assistant text goes through lisp/markdown.lisp.  Tool calls between
;;;; two texts form ONE run, shown as one line ("▸ 6 tool calls (eval ×6)");
;;;; a run expands to one line per call, a call to its arguments and
;;;; output (at most *CHAT-OUTPUT-CAP* lines).  Failed calls are red and
;;;; always listed.  Click a row, Enter on the selected one, Ctrl-O (with
;;;; nothing selected: all runs).  Tool output and user text are shown
;;;; as they are; only their length on screen is capped.

(in-package :lisping)

(declaim (special *chat-diffs* *chat-diff-show*))

(declaim (ftype function diff-after-result chat-diff-rows chat-call-diff-p
                tui-tool-summary tui-chunk-lines tui-chat-note-new
                chunks-text first-line str-truncate layout-leaves panes-layout-now
                pane-dirty tui-dirty chat-close-run chat-run-rows))

(declaim (special *panes-canvas* *tui-chat-pane-width*))

(defstruct (cline (:include uline)) hang fill gen tag)

(defstruct tcall name args summary result error-p expanded)

(defstruct trun calls expanded)

(defvar *chat-pending* nil "Calls still waiting for their result, oldest first.")

(defvar *chat-selected* nil "The trun or tcall last clicked (Enter toggles it).")

(defvar *chat-row-owners* (vector) "Owner (trun/tcall/NIL) of each row the chat pane drew.")

(defvar *chat-run* nil "The open tool run (calls since the last text), or NIL.")

(defparameter *chat-output-cap* 20)

(defparameter *chat-args-cap* 12)

(defun chat-blank-p (ln)
  (every (lambda (c) (char= c #\Space)) (chunks-text (uline-chunks ln))))

(defun uibuf-push-line (buf ln)
  "Append the ULINE LN to BUF, honouring the line cap."
  (let ((v (uibuf-lines buf)))
    (vector-push-extend ln v)
    (setf (uibuf-last-attr buf) -1)
    (let ((excess (- (length v) (uibuf-cap buf))))
      (when (plusp excess)
        (replace v v :start2 excess)
        (decf (fill-pointer v) excess)))))

;;; ------------------------------------------------------------ rows

(defun chat-pad-row (row width fill)
  (let ((len (length (chunks-text row))))
    (if (< len width)
        (append row (list (cons (make-string (- width len) :initial-element #\Space) fill)))
        row)))

(defun chat-wrap (chunks width hang fill)
  (let ((rows (or (ui-wrap-chunks chunks width hang) (list nil))))
    (if fill
        (mapcar (lambda (r) (chat-pad-row r width fill)) rows)
        rows)))

(defun chat-uline-rows (ln width)
  "Rows of LN at WIDTH: a list of (chunks . owner)."
  (if (typep ln 'cline)
      (let ((gen (cline-gen ln)))
        (if gen
            (loop for (chunks hang owner) in (funcall gen width)
                  append (mapcar (lambda (r) (cons r owner))
                                 (chat-wrap chunks width hang nil)))
            (mapcar (lambda (r) (cons r nil))
                    (chat-wrap (uline-chunks ln) width (cline-hang ln) (cline-fill ln)))))
      (mapcar (lambda (r) (cons r nil))
              (chat-wrap (uline-chunks ln) width nil nil))))

(defun chat-rows (ulines width)
  "All rows of ULINES: (values chunk-lists owners), top first."
  (let ((rows nil) (owners nil))
    (dolist (ln ulines)
      (dolist (ro (chat-uline-rows ln width))
        (push (car ro) rows)
        (push (cdr ro) owners)))
    (values (nreverse rows) (nreverse owners))))

;;; ---------------------------------------------------- adding lines

(defun chat-add-lines (ui lines)
  "Append the CLINEs LINES to UI's chat (one lock hold), count the new rows
for the scroll marker."
  (when (eq *tui* ui)
    (sb-thread:with-recursive-lock (*tui-lock*)
      (when (eq *tui* ui)
        (let ((added 0)
              (w (or *tui-chat-pane-width* 80)))
          (dolist (ln lines)
            (uibuf-push-line (tui-ui-chat ui) ln)
            (incf added (length (chat-uline-rows ln w))))
          (tui-chat-note-new added))))
    (pane-dirty "chat")))

(defun chat-block-start (ui)
  "A blank line unless the chat is empty or already ends in one."
  (let* ((v (uibuf-lines (tui-ui-chat ui)))
         (n (length v)))
    (when (and (plusp n) (not (chat-blank-p (aref v (1- n)))))
      (uibuf-push-line (tui-ui-chat ui)
                       (make-cline :chunks (list (cons " " 7)))))))

(defun chat-add-assistant (ui text)
  "Assistant TEXT as markdown, behind a ● marker, set off by blank lines."
  (when (and (eq *tui* ui) (stringp text))
    (chat-close-run)
    (multiple-value-bind (lines hangs)
        (md-lines (chunks-text (ui-parse-ansi text)))
      (when lines
        (sb-thread:with-recursive-lock (*tui-lock*)
          (chat-block-start ui))
        (let ((first t) (out nil))
          (loop for chunks in lines for hang in hangs
                do (let* ((blank (md-blank-p chunks))
                          (code (and (not blank)
                                     (every (lambda (c) (eql (cdr c) *md-block-attr*)) chunks)))
                          (prefix (cond (blank nil)
                                        (first (cons "● " 7))
                                        (t (cons "  " (if code *md-block-attr* 7))))))
                     (unless blank (setf first nil))
                     (push (make-cline :chunks (if prefix (cons prefix chunks) chunks)
                                       :hang (+ hang 2)
                                       :fill (and code *md-block-attr*))
                           out)))
          (chat-add-lines ui (nreverse out)))))))

(defun chat-user-gen (text)
  (lambda (width)
    (let ((rows nil)
          (attr (ui-attr 7 :bright t)))
      (dolist (line (ui-split-lines (substitute #\Space #\Tab (remove #\Return text))))
        (dolist (r (or (ui-wrap-chunks (list (cons (if (zerop (length line)) " " line) attr))
                                       (max 1 (- width 2)))
                       (list nil)))
          (push (list (cons (cons "▌ " 2) r) 0 nil) rows)))
      (nreverse rows))))

(defun chat-add-user (ui text)
  (when (eq *tui* ui)
    (chat-close-run)
    (sb-thread:with-recursive-lock (*tui-lock*)
      (chat-block-start ui))
    (chat-add-lines ui (list (make-cline :chunks (list (cons text 7))
                                         :gen (chat-user-gen text))))))

;;; ------------------------------------------------------- tool runs

(defun chat-close-run () (setf *chat-run* nil))

(defun chat-error-result-p (text)
  (and (stringp text)
       (or (eql 0 (search "ERROR (" text))
           (eql 0 (search "NOT SAVED" text))
           (eql 0 (search "Unknown tool" text)))))

(defun chat-call-summary (name args)
  (handler-case (tui-tool-summary name args)
    (serious-condition ()
      (let ((code (and (hash-table-p args) (gethash "code" args))))
        (if (stringp code)
            (format nil "eval: ~A" (str-truncate (first-line code) 60))
            (format nil "~A" name))))))

(defun chat-add-call (ui name args)
  "A tool call: joins the open run, or opens one (its own blank-line block)."
  (when (eq *tui* ui)
    (sb-thread:with-recursive-lock (*tui-lock*)
      (when (eq *tui* ui)
        (let ((call (make-tcall :name (or name "?") :args args
                                :summary (chat-call-summary (or name "?") args))))
          (setf *chat-pending* (append *chat-pending* (list call)))
          (if *chat-run*
              (setf (trun-calls *chat-run*) (append (trun-calls *chat-run*) (list call)))
              (let ((run (make-trun :calls (list call))))
                (setf *chat-run* run)
                (chat-block-start ui)
                (uibuf-push-line (tui-ui-chat ui)
                                 (make-cline :chunks (list (cons "  ▸ tool calls" 8))
                                             :tag run
                                             :gen (lambda (w) (declare (ignore w))
                                                    (chat-run-rows run)))))))))
    (pane-dirty "chat")))

(defun chat-add-result (ui name text)
  "A tool result: fills the oldest pending call of that name."
  (when (and (eq *tui* ui) (stringp text))
    (sb-thread:with-recursive-lock (*tui-lock*)
      (let ((call (or (find name *chat-pending* :key #'tcall-name :test #'equal)
                      (first *chat-pending*)))
            (diff (ignore-errors (diff-after-result text))))
        (when (and call diff) (setf (gethash call *chat-diffs*) diff))
        (cond (call
               (setf *chat-pending* (remove call *chat-pending* :count 1)
                     (tcall-result call) text
                     (tcall-error-p call) (chat-error-result-p text)))
              (t ;; a result with no call on record: a run of its own
               (chat-add-call ui name nil)
               (let ((c (car (last (trun-calls *chat-run*)))))
                 (setf *chat-pending* (remove c *chat-pending*)
                       (tcall-result c) text
                       (tcall-error-p c) (chat-error-result-p text)))))))
    (pane-dirty "chat")))

(defun chat-hl (owner chunks)
  "CHUNKS with a grey background when OWNER is the selected item."
  (if (and owner (eq owner *chat-selected*))
      (mapcar (lambda (c) (cons (car c) (logior (logand (cdr c) (lognot 240) (lognot 128)) 16 128)))
              chunks)
      chunks))

(defun chat-count-lines (text)
  (if (zerop (length text)) 0 (1+ (count #\Newline (string-right-trim '(#\Newline) text)))))

(defun chat-head-lines (text n)
  "(values first-N-lines-text total-lines)."
  (let ((total (chat-count-lines text)) (pos 0))
    (if (<= total n)
        (values text total)
        (progn (dotimes (k n) (setf pos (1+ (position #\Newline text :start pos))))
               (values (subseq text 0 (1- pos)) total)))))

(defun chat-call-rows (call)
  "Rows (chunks hang owner) of one call: its line, and when expanded its
arguments and output."
  (let* ((err (tcall-error-p call))
         (text (tcall-result call))
         (n (and text (chat-count-lines text)))
         (rows nil))
    (push (list (chat-hl call
                         (list (cons (if err "    ✗ " "    ● ") (if err 1 2))
                               (cons (tcall-summary call) (if err 1 7))
                               (cons (cond ((null text) " …")
                                           ((> n 1) (format nil " → ~D lines" n))
                                           (t (format nil " → ~A"
                                                      (str-truncate
                                                       (string-left-trim "=> " (first-line text)) 60))))
                                     (if err 1 8))))
                6 call)
          rows)
    (when (tcall-expanded call)
      (let* ((args (tcall-args call))
             (code (and (hash-table-p args) (gethash "code" args)))
             (src (cond ((stringp code) code)
                        ((hash-table-p args) (json-encode args))
                        (t ""))))
        (multiple-value-bind (shown total) (chat-head-lines src *chat-args-cap*)
          (dolist (l (ui-split-lines shown))
            (push (list (list (cons "      " 7) (cons l 8)) 6 call) rows))
          (when (> total *chat-args-cap*)
            (push (list (list (cons (format nil "      … +~D lines" (- total *chat-args-cap*)) 8)) 6 call)
                  rows))))
      (when text
        (multiple-value-bind (shown total) (chat-head-lines text *chat-output-cap*)
          (dolist (l (tui-chunk-lines (ui-parse-ansi shown)))
            (push (list (append (list (cons "      ◂ " 2))
                                (if err (list (cons (chunks-text l) 1)) l))
                        8 call)
                  rows))
          (when (> total *chat-output-cap*)
            (push (list (list (cons (format nil "      … +~D lines" (- total *chat-output-cap*)) 8)) 6 call)
                  rows)))))
    (dolist (r (ignore-errors (chat-diff-rows call))) (push r rows))
    (nreverse rows)))

(defun chat-run-names (calls)
  (let ((counts nil))
    (dolist (c calls)
      (let ((cell (assoc (tcall-name c) counts :test #'equal)))
        (if cell (incf (cdr cell)) (setf counts (append counts (list (cons (tcall-name c) 1)))))))
    (format nil "~{~A~^, ~}"
            (mapcar (lambda (cell) (if (> (cdr cell) 1)
                                       (format nil "~A ×~D" (car cell) (cdr cell))
                                       (car cell)))
                    counts))))

(defun chat-run-rows (run)
  (let* ((calls (trun-calls run))
         (n (length calls))
         (errs (remove-if-not #'tcall-error-p calls))
         (pending (find-if-not #'tcall-result calls))
         (rows nil))
    (push (list (chat-hl run
                         (append (list (cons (if (trun-expanded run) "  ▾" "  ▸") 4)
                                       (cons (format nil " ~D tool call~:P (~A)" n (chat-run-names calls)) 8))
                                 (when errs (list (cons (format nil " · ~D failed" (length errs)) 1)))
                                 (when pending (list (cons " …" 3)))))
                4 run)
          rows)
    (dolist (c (if (trun-expanded run) calls (remove-if-not (lambda (c) (or (tcall-error-p c) (chat-call-diff-p c))) calls)))
      (dolist (r (chat-call-rows c)) (push r rows)))
    (nreverse rows)))

;;; ---------------------------------------------------------- toggling

(defun chat-toggle (owner)
  (typecase owner
    (trun (setf (trun-expanded owner) (not (trun-expanded owner))))
    (tcall (setf (tcall-expanded owner) (not (tcall-expanded owner)))))
  (pane-dirty "chat")
  (tui-dirty)
  t)

(defun chat-all-runs ()
  (when *tui*
    (loop for ln across (uibuf-lines (tui-ui-chat *tui*))
          when (and (typep ln 'cline) (trun-p (cline-tag ln)))
            collect (cline-tag ln))))

(defun chat-toggle-all ()
  "Ctrl-O: collapse every run if any is open, else open them all."
  (let* ((runs (chat-all-runs))
         (open (some #'trun-expanded runs)))
    (dolist (r runs)
      (setf (trun-expanded r) (not open))
      (when open (dolist (c (trun-calls r)) (setf (tcall-expanded c) nil))))
    (pane-dirty "chat")
    (tui-dirty)
    t))

(defun chat-ctrl-o ()
  (if *chat-selected* (chat-toggle *chat-selected*) (chat-toggle-all)))

(defun chat-click (x0 y0)
  "A left click at the 0-based cell: toggles the tool run/call drawn there.
True when it hit one."
  (when *panes-canvas*
    (let ((rect (find "chat" (layout-leaves (panes-layout-now) 0 0
                                            (uigrid-w *panes-canvas*) (uigrid-h *panes-canvas*))
                      :key #'first :test #'equal)))
      (when rect
        (destructuring-bind (name rx ry rw rh) rect
          (declare (ignore name))
          (let ((idx (- y0 ry 1)))
            (when (and (<= rx x0) (< x0 (+ rx rw)) (<= 0 idx) (< idx (1- rh)))
              (let ((owner (and (< idx (length *chat-row-owners*))
                                (aref *chat-row-owners* idx))))
                (setf *chat-selected* owner)
                (if owner (chat-toggle owner) (progn (pane-dirty "chat") nil))))))))))
