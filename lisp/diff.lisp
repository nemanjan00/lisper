;;;; lisp/diff.lisp -- show what a file-writing tool changed, under its call.
;;;;
;;;; Every edit the harness makes is a git commit.  When a tool result says
;;;; "committed" and HEAD moved since the last look, the change is
;;;; `git diff -U2 BASE HEAD`.  The diff is kept per tool call (weak table)
;;;; and drawn by chat-call-rows at draw time, red and green, capped; a
;;;; big rewrite shows only its stat line.  Display only: the model never
;;;; sees it.  /diff on|off toggles.

(in-package :lisping)

(defvar *chat-diffs* (make-hash-table :test 'eq :weakness :key)
  "tcall -> raw `git diff` text of the commit that call made.")

(defvar *chat-diff-show* t "Draw diffs under file-writing tool calls.")

(defparameter *chat-diff-cap* 24 "Body lines of diff shown per call.")

(defparameter *chat-diff-big* 80 "A file with more changed lines than this shows only its stat.")

(defvar *diff-base* nil "HEAD at the last committed tool result.")

(defun chat-call-diff-p (call)
  "True when CALL has a diff to draw."
  (and *chat-diff-show* (gethash call *chat-diffs*) t))

(defun diff-head ()
  "Current HEAD of the harness repo, or \"\"."
  (or (ignore-errors
       (string-trim '(#\Newline #\Space)
                    (values (shell "git rev-parse HEAD 2>/dev/null"
                                   :directory lisping-core:*root*))))
      ""))

(defun diff-after-result (text)
  "Raw diff of the commit TEXT's tool made, or NIL.  Moves the base."
  (when (and (stringp text) (search "committed" text) (not (search "NOT SAVED" text)))
    (let ((head (diff-head)) (base *diff-base*))
      (when (plusp (length head)) (setf *diff-base* head))
      (when (and *chat-diff-show* base (plusp (length head)) (string/= base head))
        (let ((d (ignore-errors
                  (values (shell (format nil "git diff -U2 --no-color ~A ~A" base head)
                                 :directory lisping-core:*root*)))))
          (and d (plusp (length d)) d))))))

(defun diff-sections (text)
  "Split raw diff TEXT into sections: (path adds dels body-lines)."
  (let ((secs nil) (cur nil) (in-body nil))
    (flet ((close-cur ()
             (when cur
               (setf (fourth cur) (nreverse (fourth cur)))
               (push cur secs))))
      (dolist (l (ui-split-lines text))
        (cond ((eql 0 (search "diff --git " l))
               (close-cur)
               (setf cur (list (let ((p (search " b/" l :from-end t)))
                                 (if p (subseq l (+ p 3)) l))
                               0 0 nil)
                     in-body nil))
              ((null cur))
              ((eql 0 (search "@@" l)) (setf in-body t) (push l (fourth cur)))
              ((not in-body))
              ((and (plusp (length l)) (char= (char l 0) #\+)) (incf (second cur)) (push l (fourth cur)))
              ((and (plusp (length l)) (char= (char l 0) #\-)) (incf (third cur)) (push l (fourth cur)))
              ((and (plusp (length l)) (char= (char l 0) #\\)))
              (t (push l (fourth cur)))))
      (close-cur))
    (nreverse secs)))

(defun diff-line-color (l)
  (cond ((eql 0 (search "@@" l)) 4)
        ((and (plusp (length l)) (char= (char l 0) #\+)) 2)
        ((and (plusp (length l)) (char= (char l 0) #\-)) 1)
        (t 8)))

(defun chat-diff-rows (call)
  "Rows (chunks hang owner) of CALL's diff, or NIL."
  (let ((raw (and *chat-diff-show* (gethash call *chat-diffs*))))
    (when raw
      (let ((rows nil) (left *chat-diff-cap*) (hidden 0))
        (dolist (s (diff-sections raw))
          (destructuring-bind (path adds dels body) s
            (push (list (list (cons "      ± " 4) (cons path 7)
                              (cons (format nil " +~D" adds) 2)
                              (cons (format nil " -~D" dels) 1))
                        8 call)
                  rows)
            (if (> (+ adds dels) *chat-diff-big*)
                nil
                (dolist (l body)
                  (if (plusp left)
                      (progn (decf left)
                             (push (list (list (cons "        " 7)
                                               (cons (if (zerop (length l)) " " l) (diff-line-color l)))
                                         8 call)
                                   rows))
                      (incf hidden))))))
        (when (plusp hidden)
          (push (list (list (cons (format nil "        … +~D lines" hidden) 8)) 8 call) rows))
        (nreverse rows)))))

(defun slash-diff (ui args)
  (declare (ignore ui))
  (let ((a (string-downcase (string-trim " " args))))
    (cond ((string= a "on") (setf *chat-diff-show* t) (slash-say "diff: on"))
          ((string= a "off") (setf *chat-diff-show* nil) (slash-say "diff: off"))
          ((string= a "") (slash-say (format nil "diff: ~A" (if *chat-diff-show* "on" "off"))))
          (t (slash-say "usage: /diff on|off")))
    (pane-dirty "chat")
    (when *tui* (tui-dirty))
    t))

(defun slash-diff-args (ui partial)
  (declare (ignore ui))
  (remove-if-not (lambda (s) (eql 0 (search (string-downcase partial) s))) '("on" "off")))

(defun diff-start ()
  "Remember HEAD as the base and install /diff. Idempotent."
  (unless lisping-core:*test-boot*
    (setf *diff-base* (diff-head))
    (define-slash "diff" 'slash-diff "show file diffs under edits on|off" 'slash-diff-args)
    t))
