;;;; lisp/goal.lisp -- /goal and /loop: work that goes on by itself.
;;;;
;;;;   /goal TEXT        keep working until TEXT is achieved: after every
;;;;                     turn the agent is re-prompted (through nudge.lisp,
;;;;                     which shares its loop guard: a few re-prompts in a
;;;;                     row without progress pause it, typing resumes it)
;;;;                     until it calls goal_done {evidence}.
;;;;   /goal             show the goal      /goal clear   drop it
;;;;   /loop [5m] TEXT   send TEXT as a "loop" message every interval while
;;;;                     the agent is idle (the interval counts from the end
;;;;                     of the last run); the agent may change the delay
;;;;                     with loop_next {interval} or end it with loop_stop.
;;;;   /loop             show it            /loop stop    end it
;;;; Both are saved in the session dir (goal.json) and survive restarts.
;;;; The status bar shows "⚑ goal" and "⟳ 5m".  A todo nudge defers to an
;;;; active goal (the goal re-prompt carries the todo state).
;;;;
;;;; The three tools are registered only at a real boot (*booting*): a
;;;; change to the tool list re-caches the whole prompt, so loading this
;;;; file into a live image must never touch it.

(in-package :lisping)

(declaim (special *nudge-paused* *nudge-count* *nudge-sig* *activity*))

(defvar *goal* nil "Active goal: plist (:text :set :reprompts), or NIL.")
(defvar *goal-history* nil "Finished goals, newest first: (text set done-time evidence).")
(defvar *goal-tool-calls* 0 "Tool calls made while a goal was active (its progress counter).")
(defparameter *goal-max-reprompts* 25 "Re-prompts per goal before it pauses for good.")

(defvar *loop* nil "Active loop: plist (:prompt :interval :next-at :runs), or NIL.")
(defvar *loop-running* nil "True from the moment a loop message is sent until the turn ends.")
(defvar *loop-generation* 0)
(defparameter *loop-min-interval* 30)
(defparameter *loop-default-interval* 600)

;;; ------------------------------------------------------ persistence

(defun goal-file () (lisping-core:session-path "goal.json"))

(defun goal-save ()
  (ignore-errors
    (let ((f (goal-file)))
      (when f
        (with-open-file (s f :direction :output :if-exists :supersede :if-does-not-exist :create)
          (write-string
           (json-encode
            (apply #'obj
                   (append
                    (when *goal*
                      (list :goal-text (getf *goal* :text) :goal-set (getf *goal* :set)
                            :goal-reprompts (or (getf *goal* :reprompts) 0)))
                    (when *loop*
                      (list :loop-prompt (getf *loop* :prompt) :loop-interval (getf *loop* :interval)
                            :loop-next-at (getf *loop* :next-at) :loop-runs (or (getf *loop* :runs) 0)))
                    (list :history
                          (coerce (loop for h in *goal-history*
                                        collect (obj :text (first h) :set (second h)
                                                     :done (third h) :evidence (fourth h)))
                                  'vector)))))
           s))))))

(defun goal-load ()
  (let ((f (goal-file)))
    (when (and f (probe-file f))
      (let ((d (ignore-errors (json-decode (read-file f)))))
        (when (hash-table-p d)
          (setf *goal* (when (stringp (gethash "goal-text" d))
                         (list :text (gethash "goal-text" d)
                               :set (or (gethash "goal-set" d) (get-universal-time))
                               :reprompts (or (gethash "goal-reprompts" d) 0)))
                *loop* (when (stringp (gethash "loop-prompt" d))
                         (list :prompt (gethash "loop-prompt" d)
                               :interval (or (gethash "loop-interval" d) *loop-default-interval*)
                               :next-at (or (gethash "loop-next-at" d) (get-universal-time))
                               :runs (or (gethash "loop-runs" d) 0)))
                *goal-history* (loop for h across (or (gethash "history" d) #())
                                     collect (list (gethash "text" h) (gethash "set" h)
                                                   (gethash "done" h) (gethash "evidence" h)))))))))

;;; ------------------------------------------------------------- goal

(defun goal-active-p () (and *goal* t))

(defun goal-progress () *goal-tool-calls*)

(defun goal-capped-p ()
  (and *goal* (>= (or (getf *goal* :reprompts) 0) *goal-max-reprompts*)))

(defun goal-stalled-p ()
  (and *goal* (or *nudge-paused* (goal-capped-p))))

(defun goal-first-message (text)
  (format nil "[goal] ~A~%Work on this until it is achieved. Keep the todo list for the steps. When it is achieved, call goal_done {evidence} with concrete evidence (what you ran and what you saw); do not call it earlier. Use ask_user only if you are really blocked."
          text))

(defun goal-reprompt-message ()
  "The text a nudge sends while a goal is active (also counts the re-prompt)."
  (incf (getf *goal* :reprompts 0))
  (goal-save)
  (let* ((open (remove-if #'cdr *todos*))
         (cur (todo-current)))
    (format nil "[goal] Still active: ~A~%~D todo~:P open~@[; current: ~A~]. Keep going. When the goal is achieved, call goal_done {evidence} with concrete evidence (what you ran and saw). Ask the user (ask_user) only if you are really blocked."
            (getf *goal* :text) (length open)
            (and (or cur (first open)) (str-truncate (car (or cur (first open))) 160)))))

(defun goal-set (text)
  (setf *goal* (list :text text :set (get-universal-time) :reprompts 0)
        *goal-tool-calls* 0
        *nudge-count* 0 *nudge-paused* nil *nudge-sig* nil)
  (goal-save)
  (when *tui* (tui-dirty))
  (lisping-core:send-user-message (goal-first-message text) :kind "goal"))

(defun goal-clear (&optional (why "cleared"))
  (when *goal*
    (push (list (getf *goal* :text) (getf *goal* :set) (get-universal-time) why) *goal-history*))
  (setf *goal* nil *nudge-paused* nil)
  (goal-save)
  (when *tui* (tui-dirty)))

(defun slash-goal (ui args)
  (declare (ignore ui))
  (let ((a (string-trim " " (or args ""))))
    (cond ((member a '("clear" "off" "stop" "cancel") :test #'string-equal)
           (if *goal*
               (progn (goal-clear) (slash-say "goal cleared"))
               (slash-say "no active goal")))
          ((zerop (length a))
           (slash-say (if *goal*
                          (format nil "goal: ~A~%~D re-prompt~:P so far~:[~; · paused (type to resume)~]"
                                  (getf *goal* :text) (or (getf *goal* :reprompts) 0) (goal-stalled-p))
                          "no active goal. /goal TEXT sets one; the agent keeps working until it calls goal_done.")))
          (t (goal-set a)
             (slash-say (format nil "goal set: ~A" (str-truncate a 120)))))
    t))

(defun goal-done-tool (args)
  (let ((evidence (string-trim " " (or (arg args "evidence") ""))))
    (cond ((null *goal*) "There is no active goal.")
          ((< (length evidence) 12)
           "Not accepted: give concrete evidence (what you ran and what you saw) in `evidence`.")
          (t (let ((text (getf *goal* :text)))
               (goal-clear evidence)
               (lisping-core:emit :info :text (format nil "✓ goal achieved: ~A — ~A"
                                                      (str-truncate text 80) (str-truncate evidence 200)))
               "Goal marked done. Stop working on it and tell the user briefly what you found.")))))

;;; -------------------------------------------------------------- loop

(defun goal-parse-duration (s)
  "\"30s\" \"5m\" \"2h\" \"1d\" (a bare number is minutes) -> seconds, or NIL."
  (let* ((s (string-downcase (string-trim " " (or s ""))))
         (n (length s)))
    (when (plusp n)
      (let* ((unit (char s (1- n)))
             (mult (cond ((digit-char-p unit) 60)
                         (t (case unit (#\s 1) (#\m 60) (#\h 3600) (#\d 86400) (t nil)))))
             (num (if (digit-char-p unit) s (subseq s 0 (1- n))))
             (v (ignore-errors (parse-integer num))))
        (when (and mult v (plusp v)) (* v mult))))))

(defun goal-fmt-duration (secs)
  (cond ((>= secs 86400) (format nil "~Dd" (round secs 86400)))
        ((>= secs 3600) (format nil "~Dh" (round secs 3600)))
        ((>= secs 60) (format nil "~Dm" (round secs 60)))
        (t (format nil "~Ds" secs))))

(defun loop-message ()
  (format nil "[loop] ~A~%(run ~D of a loop every ~A. If there is nothing to do, say so in one line. loop_next {interval} sets the delay before the next run, loop_stop ends the loop.)"
          (getf *loop* :prompt) (or (getf *loop* :runs) 1) (goal-fmt-duration (getf *loop* :interval))))

(defun loop-tick ()
  "Send the next loop message when it is due and the agent is idle."
  (when (and *loop* (not *loop-running*)
             (<= (or (getf *loop* :next-at) 0) (get-universal-time))
             (not (lisping-core:turn-busy-p))
             (zerop (length (lisping-core:queued-messages)))
             (eq (activity-kind *activity*) :idle)
             (not (and (boundp '*ask-active*) (symbol-value '*ask-active*))))
    (setf *loop-running* t)
    (incf (getf *loop* :runs 0))
    (goal-save)
    (lisping-core:send-user-message (loop-message) :kind "loop")))

(defun loop-stop ()
  (setf *loop* nil *loop-running* nil)
  (goal-save)
  (when *tui* (tui-dirty)))

(defun slash-loop (ui args)
  (declare (ignore ui))
  (let* ((a (string-trim " " (or args "")))
         (sp (position #\Space a))
         (first-word (if sp (subseq a 0 sp) a))
         (secs (goal-parse-duration first-word)))
    (cond ((member a '("stop" "off" "clear" "cancel") :test #'string-equal)
           (if *loop*
               (progn (loop-stop) (slash-say "loop stopped"))
               (slash-say "no active loop")))
          ((zerop (length a))
           (slash-say (if *loop*
                          (format nil "loop every ~A (~D run~:P): ~A"
                                  (goal-fmt-duration (getf *loop* :interval)) (or (getf *loop* :runs) 0)
                                  (getf *loop* :prompt))
                          "no active loop. /loop [5m] PROMPT sends PROMPT every interval while idle (default 10m); /loop stop ends it.")))
          (t (let* ((interval (max *loop-min-interval* (or secs *loop-default-interval*)))
                    (prompt (string-trim " " (if secs (if sp (subseq a (1+ sp)) "") a))))
               (if (zerop (length prompt))
                   (slash-say "usage: /loop [INTERVAL like 30s 5m 2h] PROMPT")
                   (progn
                     (setf *loop* (list :prompt prompt :interval interval
                                        :next-at (get-universal-time) :runs 0)
                           *loop-running* nil)
                     (goal-save)
                     (when *tui* (tui-dirty))
                     (slash-say (format nil "loop started: every ~A, first run now: ~A"
                                        (goal-fmt-duration interval) (str-truncate prompt 100))))))))
    t))

(defun loop-next-tool (args)
  (let ((secs (goal-parse-duration (arg args "interval"))))
    (cond ((null *loop*) "There is no active loop.")
          ((null secs) "Give `interval` like 30s, 5m, 2h or 1d.")
          (t (let ((secs (max *loop-min-interval* secs)))
               (setf (getf *loop* :interval) secs)
               (goal-save)
               (when *tui* (tui-dirty))
               (format nil "The next loop run comes ~A after this turn ends." (goal-fmt-duration secs)))))))

(defun loop-stop-tool (args)
  (declare (ignore args))
  (if *loop*
      (progn (loop-stop)
             (lisping-core:emit :info :text "⟳ loop stopped by the agent")
             "Loop stopped.")
      "There is no active loop."))

;;; --------------------------------------------------- events and UI

(defun goal-observe (event)
  (ignore-errors
   (case (getf event :type)
     (:tool-call
      (when (and *goal* (not (equal (getf event :name) "goal_done")))
        (incf *goal-tool-calls*)))
     (:info
      ;; Esc / interrupt: the user stopped the turn, so do not push on.
      (when (and *goal* (equal (getf event :text) "turn cancelled"))
        (setf *nudge-paused* t)
        (when *tui* (tui-dirty))))
     (:idle
      (when *loop-running*
        (setf *loop-running* nil)
        (when *loop*
          (setf (getf *loop* :next-at) (+ (get-universal-time) (getf *loop* :interval)))
          (goal-save)
          (when *tui* (tui-dirty)))))))
  nil)

(defun goal-segments ()
  "Status bar segments (TEXT FG BG) for the goal and the loop."
  (append (when *goal*
            (if (goal-stalled-p)
                (list (list "⚑ goal paused" 0 3))
                (list (list "⚑ goal" 0 5))))
          (when *loop*
            (list (list (format nil "⟳ ~A" (goal-fmt-duration (getf *loop* :interval))) 0 6)))))

(defun goal-context-lines ()
  "Lines for the context pane."
  (append (when *goal*
            (list (format nil "goal: ~A (~D re-prompt~:P~:[~; · paused~])"
                          (getf *goal* :text) (or (getf *goal* :reprompts) 0) (goal-stalled-p))))
          (when *loop*
            (list (format nil "loop every ~A (~D run~:P): ~A"
                          (goal-fmt-duration (getf *loop* :interval)) (or (getf *loop* :runs) 0)
                          (getf *loop* :prompt))))))

(defun goal-start ()
  "Called from init.lisp (after nudge-start).  Loads the saved goal and
loop, installs the observer, /goal, /loop and the timer thread; the three
tools only at a real boot."
  (unless lisping-core:*test-boot*
    (goal-load)
    (pushnew 'goal-observe lisping-core:*observers*)
    (define-slash "goal" 'slash-goal "work until a goal is achieved (/goal TEXT | clear)")
    (define-slash "loop" 'slash-loop "repeat a prompt while idle (/loop [5m] PROMPT | stop)")
    (when (and (boundp 'lisping-core::*booting*) (symbol-value 'lisping-core::*booting*))
      (register-tool "goal_done"
                     "Mark the active /goal as achieved. Call it only when the goal is really met, with `evidence`: what you ran and what you saw. The goal re-prompts stop."
                     (obj :type "object" :properties
                          (obj :evidence (obj :type "string" :description "concrete evidence the goal is met"))
                          :required (vector "evidence"))
                     'goal-done-tool)
      (register-tool "loop_next"
                     "Inside a /loop run: set the delay before the next run, e.g. \"20m\", \"90s\", \"2h\" (at least 30s). The delay counts from the end of this turn."
                     (obj :type "object" :properties
                          (obj :interval (obj :type "string" :description "30s, 5m, 2h or 1d"))
                          :required (vector "interval"))
                     'loop-next-tool)
      (register-tool "loop_stop"
                     "End the active /loop (the work it repeats is finished or pointless)."
                     (obj :type "object" :properties (obj))
                     'loop-stop-tool))
    (let ((gen (incf *loop-generation*)))
      (sb-thread:make-thread
       (lambda ()
         (loop while (= gen *loop-generation*)
               do (sleep 2)
                  (ignore-errors (loop-tick))))
       :name "loop"))
    t))
