;;; interrupt.lisp — stop the running turn (todo: Esc / Ctrl-C / /stop).
;;;
;;; The core has no cancel, so this wraps LISPING-CORE::RUN-TURN: the
;;; wrapper binds *IN-TURN* and establishes a CATCH.  INTERRUPT-TURN then
;;; interrupts the agent thread with a function that throws to the catch
;;; (only when *IN-TURN* is true in that thread, so an interrupt that
;;; lands while the agent waits for a message does nothing).  The throw
;;; unwinds out of a model call, a retry wait or a tool run; the core then
;;; saves the session and emits :idle as after any turn.  A tool call that
;;; never got its result is stubbed by the core's pair-tool-results.
;;;
;;; Child processes of the interrupted tool (a long `shell`) are killed
;;; too, except tasks and MCP servers.  A synthetic user message of kind
;;; "interrupt" tells the model what happened, and the todo nudge is
;;; paused until the user types again.

(defvar *in-turn* nil
  "True inside the wrapped RUN-TURN (bound per thread).")

(defvar *interrupt-esc-at* 0
  "Internal real time of the last Esc that armed an interrupt.")

(defvar *interrupt-esc-window* 2
  "Seconds within which a second Esc interrupts the turn.")

(defun interrupt-agent-thread ()
  (find "agent" (sb-thread:list-all-threads)
        :key #'sb-thread:thread-name :test #'equal))

(defun interrupt-busy-p ()
  "True while the agent is working on a turn."
  (and (boundp '*activity*)
       (not (eq (activity-kind *activity*) :idle))))

(defun interrupt-run-turn-wrapper (fn &rest args)
  "Encapsulation of RUN-TURN: run it inside a catch; on a cancel, tell
the model."
  (let ((result (let ((*in-turn* t))
                  (catch 'turn-cancel (apply fn args)))))
    (when (eq result :cancelled)
      (ignore-errors
       (let ((m (lisping-core:obj
                 :role "user"
                 :content "[interrupt] The user interrupted your turn: the running model call or tool was stopped, and a tool call may have no result. Wait for their next message before continuing.")))
         (setf (gethash "x-kind" m) "interrupt")
         (lisping-core::add-message m))))
    result))

(defun interrupt-excluded-pids ()
  "Pids of children we must not kill: tasks and MCP servers."
  (let ((pids nil))
    (ignore-errors
     (maphash (lambda (k task) (declare (ignore k))
                (when (task-pid task) (push (task-pid task) pids))
                (when (task-process task)
                  (push (sb-ext:process-pid (task-process task)) pids)))
              *tasks*))
    (ignore-errors
     (maphash (lambda (k srv) (declare (ignore k))
                (let ((p (mcps-process srv)))
                  (when p (push (sb-ext:process-pid p) pids))))
              *mcp-servers*))
    pids))

(defun interrupt-kill-children ()
  "SIGTERM the image's other child processes (an interrupted tool's
shell command); returns the pids signalled."
  (let* ((me (sb-posix:getpid))
         (out (or (ignore-errors (lisping-core:shell (format nil "pgrep -P ~D" me))) ""))
         (skip (interrupt-excluded-pids))
         (killed nil))
    (with-input-from-string (s out)
      (loop for line = (read-line s nil) while line
            do (let ((pid (ignore-errors (parse-integer (string-trim " " line)))))
                 (when (and pid (not (member pid skip)))
                   (ignore-errors (sb-posix:kill pid 15))
                   (push pid killed)))))
    killed))

(defun interrupt-turn ()
  "Stop the running turn.  Returns T when an interrupt was sent, NIL
when nothing is running."
  (let ((th (interrupt-agent-thread)))
    (when (and th (interrupt-busy-p))
      (sb-thread:interrupt-thread
       th (lambda () (when *in-turn* (throw 'turn-cancel :cancelled))))
      (when (boundp '*nudge-paused*) (setf (symbol-value '*nudge-paused*) t))
      ;; After the unwind has had a moment: kill what the tool left behind.
      (sb-thread:make-thread
       (lambda () (sleep 0.3) (ignore-errors (interrupt-kill-children)))
       :name "interrupt-kill")
      (ignore-errors (emit :info :text "Interrupting the turn..."))
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
               (ignore-errors (emit :info :text "Press Esc again to interrupt the turn (or Ctrl-C on an empty input)."))
               t)))))

(defun slash-stop (ui args)
  (declare (ignore ui args))
  (unless (interrupt-turn)
    (slash-say "Nothing is running.")))

(defun interrupt-install ()
  "Wrap RUN-TURN (idempotent)."
  (sb-ext:without-package-locks
    (ignore-errors (sb-int:unencapsulate 'lisping-core::run-turn 'interrupt))
    (sb-int:encapsulate 'lisping-core::run-turn 'interrupt 'interrupt-run-turn-wrapper)))

(defun interrupt-start ()
  "Called from init.lisp: install the wrapper (not in the boot check,
which has no agent) and register /stop."
  (unless lisping-core:*test-boot* (interrupt-install))
  (define-slash "stop" 'slash-stop "interrupt the running turn (also Esc Esc, Ctrl-C on empty input)"))
