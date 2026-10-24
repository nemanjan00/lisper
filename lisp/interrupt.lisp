;;; interrupt.lisp — the TUI side of stopping the running turn.
;;;
;;; The core does the work: LISPING-CORE:CANCEL-TURN (from any thread)
;;; abandons the model call or tool run, kills a shell command's process
;;; group, stubs unfinished tool calls, adds the "[core] The user
;;; cancelled this turn here." note and emits :info "turn cancelled" and
;;; :idle.  LISPING-CORE:TURN-BUSY-P says whether a turn runs.
;;;
;;; Here: Esc Esc / Ctrl-C on an empty input / /stop call it, and the todo
;;; nudge is paused until the user types again (a nudge right after a
;;; cancel would undo it).  The core functions are looked up at call time
;;; so this file also loads in an image booted before they existed.

(defvar *interrupt-esc-at* 0
  "Internal real time of the last Esc that armed an interrupt.")

(defvar *interrupt-esc-window* 2
  "Seconds within which a second Esc interrupts the turn.")

(defun interrupt-core-fn (name)
  (let ((s (find-symbol name :lisping-core)))
    (and s (fboundp s) (symbol-function s))))

(defun interrupt-busy-p ()
  "True while the agent works on a turn."
  (let ((f (interrupt-core-fn "TURN-BUSY-P")))
    (and f (funcall f))))

(defun interrupt-turn ()
  "Stop the running turn.  T when a turn was running, NIL otherwise."
  (let ((f (interrupt-core-fn "CANCEL-TURN")))
    (when (and f (funcall f))
      (when (boundp '*nudge-paused*) (setf (symbol-value '*nudge-paused*) t))
      (when *tui* (tui-dirty))
      t)))

(defun interrupt-esc ()
  "Esc while busy: the first press arms, a second within the window interrupts."
  (when (interrupt-busy-p)
    (let ((now (get-internal-real-time)))
      (cond ((< (- now *interrupt-esc-at*)
                (* *interrupt-esc-window* internal-time-units-per-second))
             (setf *interrupt-esc-at* 0)
             (interrupt-turn))
            (t (setf *interrupt-esc-at* now)
               (ignore-errors
                (emit :info :text "Press Esc again to interrupt the turn (or Ctrl-C on an empty input)."))
               t)))))

(defun slash-stop (ui args)
  (declare (ignore ui args))
  (unless (interrupt-turn)
    (slash-say "Nothing is running.")))

(defun interrupt-start ()
  "Called from init.lisp: register /stop."
  (define-slash "stop" 'slash-stop "interrupt the running turn (also Esc Esc, Ctrl-C on empty input)"))
