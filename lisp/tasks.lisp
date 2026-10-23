;;;; lisp/tasks.lisp -- background tasks and monitors.
;;;;
;;;; bg_start runs a shell command detached (no stdin; stdout+stderr go to
;;;; the session's tmp/task-NAME.log) and returns immediately; a watcher thread
;;;; reports the exit as a synthetic x-kind "task" message, which the held
;;;; queue delivers when the current turn ends.  monitor_start is a bg task
;;;; plus a reader thread that tails the log and forwards matching lines as
;;;; x-kind "monitor" messages, batched (~1s) and rate-limited so a chatty
;;;; command can't flood the agent.  TASKS-START (called from init.lisp)
;;;; registers the tools and the "tasks" pane; TUI-STOP kills everything
;;;; via *TUI-STOP-HOOKS* so restarts leave no orphans.

(in-package :lisping)

(defstruct (task (:constructor %make-task (name cmd)))
  name cmd process stream file
  (start (get-universal-time))
  (status :running)                     ; :running | :exited
  exit-code
  (monitor nil)                         ; t: forward matching output lines
  pattern (max-rate 20)
  (reader-stop nil)
  (batch nil) (batch-start 0) (dropped 0) (sent-times nil))

(defvar *tasks* (make-hash-table :test 'equal) "name -> TASK.")

;;; Forward references: BG-START starts threads running these, defined below.
(declaim (ftype function task-watch monitor-read-loop monitor-flush tasks-init-panes))

(defun task-list ()
  (sort (loop for v being the hash-values of *tasks* collect v)
        #'< :key #'task-start))

(defun task-file-for (name)
  (lisping-core:session-path
   (format nil "tmp/task-~A.log"
           (with-output-to-string (s)
             (loop for c across name
                   do (write-char (if (or (alphanumericp c) (find c "-_")) c #\-) s))))))

(defun task-line-count (task)
  (or (ignore-errors
        (with-open-file (s (task-file task))
          (loop while (read-line s nil nil) count 1)))
      0))

(defun task-status-string (task)
  (if (eq (task-status task) :running)
      "running"
      (format nil "exited ~A" (task-exit-code task))))

(defun tool-arg-int (v)
  "A tool argument that should be an integer: JSON gives integers, but
accept a string too."
  (cond ((integerp v) v)
        ((and (stringp v) (plusp (length v))) (parse-integer v :junk-allowed t))
        (t nil)))

;;; ---------------------------------------------------------------
;;; start / watch / stop / inspect

(defun bg-start (name cmd &key monitor pattern max-rate)
  "Start CMD as background task NAME and return a status string.
Output (stdout+stderr) goes to the session's tmp/task-NAME.log.  With
MONITOR, also start a reader forwarding matching lines to the agent."
  (cond ((zerop (length (string-trim " " name))) "bg_start: name is empty")
        ((zerop (length (string-trim " " cmd))) "bg_start: cmd is empty")
        ((let ((old (gethash name *tasks*)))
           (and old (eq (task-status old) :running)))
         (format nil "task ~S is already running (bg_stop it first)" name))
        (t
         (let* ((file (task-file-for name))
                (stream (open file :direction :output :if-exists :supersede
                              :if-does-not-exist :create))
                (proc (handler-case
                          (sb-ext:run-program "/bin/sh" (list "-c" cmd)
                                              :wait nil :output stream :error :output)
                        (error (e)
                          (close stream)
                          (return-from bg-start
                            (format nil "bg_start ~S failed to start: ~A" name e)))))
                (task (%make-task name cmd)))
           (setf (task-process task) proc
                 (task-stream task) stream
                 (task-file task) file
                 (task-monitor task) monitor
                 (task-pattern task) (and pattern (plusp (length pattern)) pattern)
                 (task-max-rate task) (max 1 (or max-rate 20)))
           (setf (gethash name *tasks*) task)
           (sb-thread:make-thread (lambda () (task-watch task))
                                  :name (format nil "task-~A" name))
           (when monitor
             (sb-thread:make-thread (lambda () (monitor-read-loop task))
                                    :name (format nil "monitor-~A" name)))
           (tasks-init-panes)
           (pane-dirty "tasks")
           (format nil "~A ~S started (pid ~D), output: ~A"
                   (if monitor "monitor" "task") name (sb-ext:process-pid proc)
                   (enough-namestring file lisping-core:*root*))))))

(defun task-watch (task)
  "Wait for TASK's process to exit, record the result, and tell the agent
with a synthetic message (kind \"task\" — delivered when the turn ends)."
  (sb-ext:process-wait (task-process task))
  (setf (task-exit-code task) (sb-ext:process-exit-code (task-process task))
        (task-status task) :exited)
  (ignore-errors (close (task-stream task)))
  (setf (task-reader-stop task) t)      ; a monitor reader drains, then exits
  (pane-dirty "tasks")
  (send-user-message
   (format nil "[task ~A] ~A, ~D line~:P"
           (task-name task) (task-status-string task) (task-line-count task))
   :kind "task"))

(defun bg-stop (name)
  "SIGTERM task NAME (a no-op with a message if it already exited)."
  (let ((task (gethash name *tasks*)))
    (cond ((null task) (format nil "no task ~S (bg_list shows names)" name))
          ((eq (task-status task) :running)
           (ignore-errors (sb-ext:process-kill (task-process task) 15))
           (format nil "stopped ~S (SIGTERM); the exit message follows" name))
          (t (format nil "task ~S already ~A" name (task-status-string task))))))

(defun bg-output (name tail)
  "The last TAIL lines (default 50) of task NAME's output file."
  (let ((task (gethash name *tasks*)))
    (if (null task)
        (format nil "no task ~S (bg_list shows names)" name)
        (let* ((all (or (ignore-errors
                          (with-open-file (s (task-file task))
                            (loop for l = (read-line s nil nil) while l collect l)))
                        '()))
               (n (max 1 (or tail 50)))
               (show (last all (min n (length all)))))
          (format nil "~A [~A, ~D line~:P total]~%~{~A~%~}"
                  name (task-status-string task) (length all) show)))))

(defun bg-list ()
  (let ((tasks (task-list)))
    (if (null tasks)
        "no background tasks"
        (with-output-to-string (s)
          (dolist (task tasks)
            (format s "~A: ~A, ~Ds, ~D line~:P"
                    (task-name task) (task-status-string task)
                    (- (get-universal-time) (task-start task))
                    (task-line-count task))
            (when (task-monitor task)
              (format s ", monitor pattern ~S, max ~D/min, ~D dropped"
                      (task-pattern task) (task-max-rate task) (task-dropped task)))
            (format s "~%  ~A~%" (task-cmd task)))))))

;;; ---------------------------------------------------------------
;;; monitor: tail the log, forward matching lines batched + rate-limited

(defun monitor-match-p (task line)
  "PATTERN is a case-insensitive substring (no regex: cl-ppcre isn't
loaded); no pattern matches every line."
  (let ((p (task-pattern task)))
    (or (null p) (search p line :test #'char-equal))))

(defun monitor-read-loop (task)
  "Tail TASK's output file.  The stop flag is only acted on at EOF, so
output written just before the process exits is still forwarded."
  (handler-case
      (with-open-file (s (task-file task))
        (loop
          (let ((line (read-line s nil nil)))
            (cond (line
                   (when (monitor-match-p task line)
                     (unless (task-batch task)
                       (setf (task-batch-start task) (get-internal-real-time)))
                     (push (str-truncate line 300) (task-batch task))))
                  ((task-reader-stop task)
                   (monitor-flush task :force t)
                   (return))
                  (t
                   (monitor-flush task)
                   (sleep 0.25))))))
    (error () nil)))

(defun monitor-flush (task &key force)
  "Send the batched lines once the batch is ~1s old (or FORCE), unless
over MAX-RATE messages in the last 60s — then drop them and count."
  (when (and (task-batch task)
             (or force
                 (>= (- (get-internal-real-time) (task-batch-start task))
                     internal-time-units-per-second)))
    (let ((now (get-universal-time)))
      (setf (task-sent-times task)
            (remove-if (lambda (tm) (> (- now tm) 60)) (task-sent-times task)))
      (if (< (length (task-sent-times task)) (task-max-rate task))
          (progn
            (push now (task-sent-times task))
            (let ((text (with-output-to-string (s)
                          (format s "[monitor ~A]" (task-name task))
                          (dolist (l (reverse (task-batch task)))
                            (format s "~%~A" l))
                          (when (plusp (task-dropped task))
                            (format s "~%(+~D line~:P dropped: rate limit)"
                                    (task-dropped task))))))
              (setf (task-dropped task) 0)
              (send-user-message text :kind "monitor")))
          (incf (task-dropped task) (length (task-batch task))))
      (setf (task-batch task) nil
            (task-batch-start task) 0))))

;;; ---------------------------------------------------------------
;;; pane + lifecycle

(defun pane-tasks-render (pane w h)
  (declare (ignore pane))
  (let ((lines
         (let ((tasks (task-list)))
           (if (null tasks)
               (list (list (cons "(no background tasks)" 8)))
               (loop for task in tasks
                     collect (list (cons (str-truncate
                                          (format nil "~A ~A ~Ds ~DL~@[ ~S~]"
                                                  (task-name task)
                                                  (task-status-string task)
                                                  (- (get-universal-time) (task-start task))
                                                  (task-line-count task)
                                                  (task-pattern task))
                                          w)
                                         (if (eq (task-status task) :running) 7 8))))))))
    (subseq lines (max 0 (- (length lines) h)))))

(defun tasks-init-panes ()
  "Register the tasks pane and put it right of the context pane.
On *PANES-INIT-HOOKS* (appended — it splits the context pane's leaf, so
it must run after CTX-INIT-PANES) and called once by TASKS-START."
  (unless (pane-get "tasks")
    (pane-register "tasks" :title "tasks" :kind :tasks))
  (unless (member "tasks" (layout-pane-names) :test #'equal)
    (setf *layout*
          (if (member "context" (layout-pane-names) :test #'equal)
              (layout-split-replace *layout* "context" :hsplit 0.68 "tasks")
              (list :vsplit 0.85 *layout* (list 'leaf "tasks"))))
    (panes-save)
    (panes-mark-all-dirty)
    (when *tui* (tui-dirty))))

(defun tasks-stop-all ()
  "SIGTERM every running task; runs from TUI-STOP, so on restarts too."
  (dolist (task (task-list))
    (when (eq (task-status task) :running)
      (ignore-errors (sb-ext:process-kill (task-process task) 15)))))

;;; ---------------------------------------------------------------
;;; tools

(defun bg-start-tool (args)
  (bg-start (or (lisping-core::arg args "name") "")
            (or (lisping-core::arg args "cmd") "")))

(defun bg-list-tool (args)
  (declare (ignore args))
  (bg-list))

(defun bg-output-tool (args)
  (bg-output (or (lisping-core::arg args "name") "")
             (tool-arg-int (lisping-core::arg args "tail"))))

(defun bg-stop-tool (args)
  (bg-stop (or (lisping-core::arg args "name") "")))

(defun monitor-start-tool (args)
  (bg-start (or (lisping-core::arg args "name") "")
            (or (lisping-core::arg args "cmd") "")
            :monitor t
            :pattern (lisping-core::arg args "pattern")
            :max-rate (tool-arg-int (lisping-core::arg args "max_rate"))))

(defun monitor-stop-tool (args)
  "Stopping a monitor is stopping its task; the exit message follows."
  (bg-stop (or (lisping-core::arg args "name") "")))

(defun tasks-start ()
  "Register the task/monitor tools, the tasks pane, and the stop hook.
Idempotent."
  (setf *panes-init-hooks* (remove 'tasks-init-panes *panes-init-hooks*))
  (setf *panes-init-hooks* (append *panes-init-hooks* (list 'tasks-init-panes)))
  (setf *tui-stop-hooks* (remove 'tasks-stop-all *tui-stop-hooks*))
  (setf *tui-stop-hooks* (append *tui-stop-hooks* (list 'tasks-stop-all)))
  (tasks-init-panes)
  (lisping-core::register-tool
   "bg_start" "Start a background task running shell CMD named NAME (detached; no stdin; stdout+stderr to the session's tmp/task-NAME.log) and return immediately. When it exits, a '[task NAME] exited CODE, N lines' message arrives. Returns e.g. \"task \\\"build\\\" started (pid 1234), output: .../sessions/main/tmp/task-build.log\". Example: {\"name\":\"build\",\"cmd\":\"make -j4\"}. See also bg_list, bg_output, bg_stop."
   (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj "name" (lisping-core::obj "type" "string")
                                       "cmd" (lisping-core::obj "type" "string"))
        "required" #("name" "cmd"))
   'bg-start-tool)
  (lisping-core::register-tool
   "bg_list" "List background tasks: name, status (running / exited CODE), age, output lines, command; monitors also show pattern, rate limit and dropped-line count. Example: {} => \"build: running, 12s, 40 lines\\n  make -j4\"."
   (lisping-core::obj "type" "object" "properties" (lisping-core::obj))
   'bg-list-tool)
  (lisping-core::register-tool
   "bg_output" "Return the last TAIL lines (default 50) of task NAME's output file, headed by status and total line count. Example: {\"name\":\"build\",\"tail\":20} => \"build [running, 40 lines total]\\n...\"."
   (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj "name" (lisping-core::obj "type" "string")
                                       "tail" (lisping-core::obj "type" "integer"))
        "required" #("name"))
   'bg-output-tool)
  (lisping-core::register-tool
   "bg_stop" "SIGTERM background task NAME. The '[task NAME] exited ...' message still follows. Example: {\"name\":\"build\"} => \"stopped \\\"build\\\" (SIGTERM); the exit message follows\"."
   (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj "name" (lisping-core::obj "type" "string"))
        "required" #("name"))
   'bg-stop-tool)
  (lisping-core::register-tool
   "monitor_start" "Start background task NAME running CMD and watch its output: lines containing PATTERN (case-insensitive substring; omit to match every line) are batched over ~1s into '[monitor NAME] ...' messages to you, at most MAX_RATE messages per minute (default 20; overflow is dropped and counted). Ends when the command exits or monitor_stop/bg_stop runs. Returns like bg_start. Example: {\"name\":\"temp\",\"cmd\":\"cat /dev/ttyUSB0\",\"pattern\":\"TEMP\",\"max_rate\":12}."
   (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj "name" (lisping-core::obj "type" "string")
                                       "cmd" (lisping-core::obj "type" "string")
                                       "pattern" (lisping-core::obj "type" "string")
                                       "max_rate" (lisping-core::obj "type" "integer"))
        "required" #("name" "cmd"))
   'monitor-start-tool)
  (lisping-core::register-tool
   "monitor_stop" "Stop monitor NAME (kills its task and reader; the task exit message still follows). Example: {\"name\":\"temp\"} => \"stopped \\\"temp\\\" (SIGTERM); the exit message follows\"."
   (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj "name" (lisping-core::obj "type" "string"))
        "required" #("name"))
   'monitor-stop-tool)
  t)
