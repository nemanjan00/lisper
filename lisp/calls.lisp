;;;; lisp/calls.lisp -- call tools from code: loops and parallel fan-out.
;;;;
;;;; Inside one `eval` the model can run many tool calls and get back only
;;;; what it chooses to return, so a sweep over 50 MCP items costs one turn
;;;; and one short result instead of 50 round trips:
;;;;
;;;;   (loop for i from 1 to 10 collect (call-tool "mcp__srv__get" :index i))
;;;;   (call-tools-parallel (loop for i from 1 to 10
;;;;                              collect (list "mcp__srv__get" :index i)))
;;;;
;;;; A call goes through the core's RUN-TOOL-CALL, so it behaves like a call
;;;; the model made itself: *before-tool-call* hooks can refuse it, results
;;;; of untrusted tools (MCP, web) stay fenced, and the UI shows it.
;;;; Plain functions, not a registered tool: the tool list stays as it is.

(in-package :lisping)

(defvar *call-depth* 0 "Nesting of CALL-TOOL inside CALL-TOOL.")

(defparameter *call-max-depth* 3 "A tool that calls tools that call tools stops here.")

(defparameter *call-max-parallel* 8 "Most calls in flight at once.")

(defvar *call-counter* 0)

(defun call-args-obj (args)
  "ARGS as a JSON object: a hash table as is, else a plist (:key value ...
or \"key\" value ...), keys as the tool names them (keywords downcased)."
  (if (hash-table-p args)
      args
      (let ((o (obj)))
        (loop for (k v) on args by #'cddr
              do (setf (gethash (if (symbolp k) (string-downcase (symbol-name k)) k) o) v))
        o)))

(defun call-tool (name &rest args)
  "Run tool NAME with ARGS (a plist, or one hash table) and return its
result string, as the model would see it. Errors come back as the tool's
\"ERROR ...\" text, they are not signalled."
  (when (>= *call-depth* *call-max-depth*)
    (return-from call-tool (format nil "ERROR: call-tool nested more than ~D deep" *call-max-depth*)))
  (let* ((args (if (and args (hash-table-p (first args))) (first args) (call-args-obj args)))
         (call (obj :id (format nil "call-~D" (incf *call-counter*))
                    :function (obj :name name :arguments (json-encode args))))
         (*call-depth* (1+ *call-depth*))
         (msg (lisping-core::run-tool-call call)))
    (gethash "content" msg)))

(defun call-tools-parallel (calls &key (limit *call-max-parallel*))
  "CALLS is a list of (name . args-plist). Run them in threads, at most
LIMIT at once, and return the result strings in the same order."
  (let* ((n (length calls))
         (results (make-array n :initial-element nil))
         (depth *call-depth*)
         (pending (loop for c in calls for i from 0 collect (cons i c)))
         (running nil))
    (flet ((start (item)
             (destructuring-bind (i name . args) item
               (push (sb-thread:make-thread
                      (lambda ()
                        (let ((*call-depth* depth) (*package* (find-package :lisping)))
                          (setf (aref results i)
                                (handler-case (apply #'call-tool name args)
                                  (serious-condition (c) (format nil "ERROR (~S): ~A" (type-of c) c))))))
                      :name (format nil "call ~A" name))
                     running)))
           (reap ()
             (setf running (remove-if-not #'sb-thread:thread-alive-p running))))
      (loop while (or pending running)
            do (reap)
               (loop while (and pending (< (length running) (max 1 limit)))
                     do (start (pop pending)))
               (when (or pending running) (sleep 0.02)))
      (coerce results 'list))))
