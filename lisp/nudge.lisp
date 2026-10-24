;;; nudge.lisp — todo nudge (todo 25).
;;;
;;; When a turn ends (:idle) with open todos, nothing held, and no
;;; ask_user question open, wait a few seconds (so a user typing right
;;; then wins) and send a synthetic message telling the agent to carry
;;; on.  Loop guard: at most *NUDGE-MAX* nudges in a row without the
;;; todo list changing; after that nudging pauses (status bar: "nudges
;;; paused") until the user types or a todo changes.  /nudge on|off.

(in-package :lisping)

(defvar *nudge-enabled* t "Master switch (/nudge on|off).")
(defvar *nudge-delay* 5 "Seconds to wait after :idle before nudging.")
(defconstant +nudge-max+ 2)
(defvar *nudge-count* 0 "Nudges sent in a row without a todo change.")
(defvar *nudge-sig* nil "Todo signature at the last nudge.")
(defvar *nudge-paused* nil "True once the loop guard has stopped nudging.")
(defvar *nudge-generation* 0
  "Bumped by every event except :idle; a waiting nudge that sees it change
gives up (something happened meanwhile).")

(defun nudge-open-todos () (remove-if #'cdr *todos*))

(defun nudge-signature ()
  "What counts as 'a todo changed': texts, done flags, the current item."
  (list (mapcar (lambda (i) (cons (car i) (and (cdr i) t))) *todos*)
        (let ((c (todo-current))) (and c (car c)))))

(defun nudge-message ()
  (let* ((open (nudge-open-todos))
         (cur (todo-current)))
    (format nil "[nudge] ~D todo~:P open; current: ~A. Continue, or ask_user if you need the user."
            (length open) (str-truncate (car (or cur (first open))) 160))))

(defun nudge-user-typed-p (event)
  "True for a :user event the user wrote (not a nudge or other synthetic
message).  The core tags system messages with x-kind; the text prefix is
the fallback when the message isn't found."
  (null (getf event :kind)))

(defun nudge-ready-p ()
  "All the conditions for sending a nudge, right now."
  (and *nudge-enabled* (not *nudge-paused*)
       (nudge-open-todos)
       (not (and (boundp '*ask-active*) (symbol-value '*ask-active*)))
       (zerop (length (lisping-core:queued-messages)))
       (eq (activity-kind *activity*) :idle)))

(defun nudge-fire (gen)
  "Called after the wait: send the nudge if nothing happened since GEN."
  (when (and (= gen *nudge-generation*) (nudge-ready-p))
    (let ((sig (nudge-signature)))
      (cond ((not (equal sig *nudge-sig*))
             (setf *nudge-count* 0 *nudge-sig* sig))
            ((>= *nudge-count* +nudge-max+)
             (setf *nudge-paused* t)
             (when *tui* (tui-dirty))
             (return-from nudge-fire nil)))
      (incf *nudge-count*)
      (lisping-core:send-user-message (nudge-message) :kind "nudge"))))

(defun nudge-schedule ()
  (let ((gen *nudge-generation*))
    (sb-thread:make-thread
     (lambda ()
       (sleep *nudge-delay*)
       (ignore-errors (nudge-fire gen)))
     :name "nudge")))

(defun nudge-observe (event)
  "Observer on the kernel's events."
  (ignore-errors
   (case (getf event :type)
     (:idle
      (when (and *nudge-enabled* (not *nudge-paused*)
                 (nudge-open-todos)
                 (eql 0 (or (getf event :held) 0)))
        (nudge-schedule)))
     (:user
      (incf *nudge-generation*)
      (when (nudge-user-typed-p event)
        (setf *nudge-count* 0 *nudge-paused* nil)
        (when *tui* (tui-dirty))))
     (t (incf *nudge-generation*)
        (nudge-todo-changed))))
  nil)

(defun nudge-todo-changed ()
  "Hook for todo edits: a changed list lifts the pause."
  (when (and *nudge-paused* (not (equal (nudge-signature) *nudge-sig*)))
    (setf *nudge-paused* nil *nudge-count* 0)
    (when *tui* (tui-dirty))))

(defun nudge-segment ()
  "Status bar segment (TEXT FG BG), or NIL."
  (cond ((not *nudge-enabled*) nil)
        (*nudge-paused* (list "nudges paused" 0 3))))

(defun slash-nudge (ui args)
  (declare (ignore ui))
  (let ((a (string-downcase (string-trim " " args))))
    (cond ((string= a "on") (setf *nudge-enabled* t *nudge-paused* nil *nudge-count* 0)
           (slash-say "todo nudge: on"))
          ((string= a "off") (setf *nudge-enabled* nil)
           (slash-say "todo nudge: off"))
          ((string= a "")
           (slash-say (format nil "todo nudge: ~A~@[ (paused: ~A nudges without a todo change; type or change a todo to resume)~]"
                              (if *nudge-enabled* "on" "off")
                              (and *nudge-paused* *nudge-count*))))
          (t (slash-say "usage: /nudge on|off")))
    (when *tui* (tui-dirty))
    t))

(defun slash-nudge-args (ui partial)
  (declare (ignore ui))
  (remove-if-not (lambda (s) (eql 0 (search (string-downcase partial) s))) '("on" "off")))

(defun nudge-start ()
  "Install the observer, the todo-change hook and /nudge. Idempotent."
  (unless lisping-core:*test-boot*
    (pushnew 'nudge-observe lisping-core:*observers*)
    (define-slash "nudge" 'slash-nudge "todo nudge on|off" 'slash-nudge-args)
    t))
