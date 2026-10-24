;;; history.lisp — input history (Up/Down recall of earlier sent messages).
;;;
;;; Every submitted line is appended to ~/.lisper/history.jsonl (one JSON
;;; string per line, shared by all sessions) and kept in *HISTORY*, oldest
;;; first.  Up on the first line of the input (when no queued message is
;;; waiting to be selected) steps back through it; Down steps forward and
;;; finally restores the draft that was being typed.  Submitting ends the
;;; browse.  Called from TUI-NORMAL-KEY / TUI-SUBMIT-INPUT in tui-input.lisp.

(defvar *history* nil "Sent messages, oldest first (a list).")
(defvar *history-pos* nil "Index into *HISTORY* while browsing, else NIL.")
(defvar *history-draft* nil "The input text before browsing started.")
(defvar *history-max* 500 "Entries kept in memory and loaded at start.")
(defvar *history-max-chars* 4000 "Longer messages (big pastes) are not remembered.")

(defun history-file ()
  (lisping-core:home-path "history.jsonl"))

(defun history-load ()
  "Read the last *HISTORY-MAX* entries from the history file."
  (let ((entries nil))
    (ignore-errors
     (when (probe-file (history-file))
       (with-open-file (in (history-file) :external-format :utf-8)
         (loop for line = (read-line in nil) while line
               do (let ((v (ignore-errors (lisping-core:json-decode line))))
                    (when (and (stringp v) (plusp (length v)))
                      (push v entries)))))))
    (setf *history* (reverse (subseq entries 0 (min *history-max* (length entries))))
          *history-pos* nil *history-draft* nil)
    (length *history*)))

(defun history-add (text)
  "Remember TEXT (unless empty, huge or the same as the last entry) and
end any browse."
  (setf *history-pos* nil *history-draft* nil)
  (when (and (plusp (length text))
             (<= (length text) *history-max-chars*)
             (not (equal text (car (last *history*)))))
    (setf *history* (append *history* (list text)))
    (when (> (length *history*) *history-max*)
      (setf *history* (last *history* *history-max*)))
    (ignore-errors
     (with-open-file (out (history-file) :direction :output :if-exists :append
                                         :if-does-not-exist :create :external-format :utf-8)
       (write-string (lisping-core:json-encode text) out)
       (terpri out)))
    t))

(defun history-set-input (ui text)
  "Put TEXT in the input line, cursor at its end."
  (let ((in (tui-ui-input ui)))
    (setf (fill-pointer in) 0)
    (loop for c across text do (vector-push-extend c in))
    (setf (tui-ui-input-cursor ui) (length in)
          (tui-ui-input-hscroll ui) 0)))

(defun history-prev (ui)
  "Older entry into the input.  True when there was one to show."
  (when *history*
    (cond ((null *history-pos*)
           (setf *history-draft* (coerce (copy-seq (tui-ui-input ui)) 'simple-string)
                 *history-pos* (1- (length *history*))))
          ((plusp *history-pos*) (decf *history-pos*))
          (t (return-from history-prev t)))   ; already the oldest
    (history-set-input ui (nth *history-pos* *history*))
    t))

(defun history-next (ui)
  "Newer entry, or the draft after the newest.  True when browsing."
  (when *history-pos*
    (incf *history-pos*)
    (cond ((< *history-pos* (length *history*))
           (history-set-input ui (nth *history-pos* *history*)))
          (t (history-set-input ui (or *history-draft* ""))
             (setf *history-pos* nil *history-draft* nil)))
    t))

(defun history-start ()
  "Called from init.lisp: load the saved history."
  (unless lisping-core:*test-boot* (history-load)))
