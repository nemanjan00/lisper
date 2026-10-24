;;; activity.lisp — activity indicator in the status bar (todo 22).
;;;
;;; One segment right after the name: what the agent is doing right now.
;;;   thinking  ⠹ thinking 12s       (a model call is in flight)
;;;   tool      ⠹ eval 3s            (a tool is running)
;;;   retry     ↻ retry in 7s (#2)   (a failed call waits to be retried)
;;;   idle      idle                 (turn over, waiting for the user)
;;;
;;; State comes from events (an observer, so a UI restart can't drop it):
;;; :user and :thinking = a model call, :tool-call / :tool-result bracket
;;; a tool, :error with :retry-in = retry wait, :idle = turn over.
;;; Animation does NOT come from events: a ticker thread wakes ~8x/s
;;; while something is active and repaints only the status row (a copy
;;; of the last frame with a fresh status bar, diffed against it).

(in-package :lisping)

(defstruct activity
  (kind :idle)     ; :idle :thinking :tool :retry
  name             ; tool name
  (since 0)        ; internal-real-time when this state began
  until            ; internal-real-time a retry wait ends
  attempt)         ; retry attempt number

(defvar *activity* (make-activity) "The current activity state.")

(defparameter *activity-spinner* "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏")

(defun activity-now () (get-internal-real-time))

(defun activity-seconds (ticks)
  (/ ticks (float internal-time-units-per-second)))

(defun activity-set (kind &key name until attempt)
  (setf *activity* (make-activity :kind kind :name name :since (activity-now)
                                  :until until :attempt attempt)))

(defun activity-observe (event)
  "Observer: follow the agent's state from its events."
  (ignore-errors
   (case (getf event :type)
     ((:user :thinking) (activity-set :thinking))
     (:tool-call (activity-set :tool :name (getf event :name)))
     (:tool-result (activity-set :thinking))
     (:error (let ((wait (getf event :retry-in)))
               (when (numberp wait)
                 (activity-set :retry
                               :until (+ (activity-now)
                                         (round (* wait internal-time-units-per-second)))
                               :attempt (getf event :attempt)))))
     (:idle (activity-set :idle)))
   ;; The todo pane shows the spinner / busy highlight: repaint it on
   ;; every state change, so it appears at once and never stays on
   ;; after the turn is over (the ticker only runs while busy).
   (when (member (getf event :type) '(:user :thinking :tool-call :tool-result :idle :error))
     (when (fboundp 'pane-dirty) (funcall 'pane-dirty "todo")))
   (when *tui* (tui-dirty)))
  nil)

(defun activity-animated-p ()
  "True while the status segment changes with time."
  (not (eq (activity-kind *activity*) :idle)))

(defun activity-spinner ()
  (let ((n (length *activity-spinner*)))
    (char *activity-spinner*
          (mod (floor (activity-seconds (activity-now)) 0.12) n))))

(defun activity-segment ()
  "The status segment (TEXT FG BG) for the current state."
  (let* ((a *activity*)
         (elapsed (floor (activity-seconds (- (activity-now) (activity-since a))))))
    (ecase (activity-kind a)
      ;; Integer colours only: TUI-DRAW-STATUS uses UI-ATTR directly.
      (:idle (list "idle" 7 4))
      (:thinking (list (format nil "~A thinking ~Ds" (activity-spinner) elapsed) 0 3))
      (:tool (list (format nil "~A ~A ~Ds" (activity-spinner)
                           (str-truncate (or (activity-name a) "tool") 24) elapsed)
                   0 6))
      (:retry (list (format nil "↻ retry in ~Ds~@[ (#~A)~]"
                            (max 0 (ceiling (activity-seconds
                                             (- (activity-until a) (activity-now)))))
                            (activity-attempt a))
                    7 1)))))

;;; ---------------------------------------------------------------
;;; cheap repaint: the status row only

(defun tui-tick-status (ui)
  "Repaint just the status row onto a copy of the last frame."
  (sb-thread:with-recursive-lock (*tui-lock*)
    (let ((prev *tui-prev-grid*))
      (when (and *tui* (eq *tui* ui) prev
                 (= (uigrid-w prev) (tui-ui-width ui))
                 (= (uigrid-h prev) (tui-ui-height ui))
                 *tui-prev-cursor*)
        (let* ((w (uigrid-w prev)) (h (uigrid-h prev))
               (g (make-uigrid :w w :h h
                               :chars (copy-seq (uigrid-chars prev))
                               :attrs (copy-seq (uigrid-attrs prev)))))
          (tui-draw-status g ui (1- h) w)
          (let ((body (ui-grid-diff-render prev g)))
            (when (plusp (length body))
              (tty-write (format nil "~C[?2026h~A~C[~D;~DH~C[?25h~C[?2026l"
                                 #\Esc body #\Esc (car *tui-prev-cursor*)
                                 (cdr *tui-prev-cursor*) #\Esc #\Esc))
              (setf *tui-prev-grid* g))))))))

(defvar *activity-ticker* nil "The ticker thread.")

(defun activity-ticker-loop (me)
  (let ((n 0))
    (loop while (eq *activity-ticker* me)
          do (sleep 0.12)
             (when (and *tui* (activity-animated-p))
               (handler-case
                   (progn (tui-tick-status *tui*)
                          ;; The todo pane's working-on spinner: ~4 repaints/s.
                          (when (zerop (mod (incf n) 2))
                            (pane-dirty "todo")))
                 (serious-condition () nil))))))

(defun activity-start ()
  "Install the observer and start the ticker. Idempotent."
  (unless lisping-core:*test-boot*
    (pushnew 'activity-observe lisping-core:*observers*)
    (setf *activity-ticker* nil)        ; retire an old ticker
    (let ((cell (list nil)))
      (declare (ignorable cell))
      (setf *activity-ticker*
            (sb-thread:make-thread
             (lambda ()
               (sleep 0.05)
               (activity-ticker-loop sb-thread:*current-thread*))
             :name "activity-ticker")))
    t))
