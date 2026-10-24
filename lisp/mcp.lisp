;;; mcp.lisp — MCP (Model Context Protocol) client, stdio transport (todo 17).
;;;
;;; Servers come from ~/.lisper/mcp.json (Claude Desktop format:
;;; {"mcpServers": {"name": {"command", "args", "env"}}}), optionally
;;; overridden per session by (session-path "mcp.json"); a server entry may
;;; also carry "timeout" (seconds per tool call, default 120), "cwd" and
;;; "disabled": true.  Each server is spawned with run-program, which makes
;;; it a process-group leader, so killing -PID takes npx/node children too.
;;; JSON-RPC 2.0, one message per line on stdin/stdout; stderr goes to
;;; tmp/mcp-NAME.log in the session directory.
;;;
;;; Every server tool is registered as the normal tool NAME__TOOL.  A dead
;;; or hung server never hangs the agent: requests carry timeouts, a dead
;;; server is restarted on its next use (and the result says so).

(defvar *mcp-servers* (make-hash-table :test 'equal)
  "Server name -> mcp-server.")

(defvar *mcp-init-timeout* 90 "Seconds to wait for a server's handshake (npx may download).")
(defvar *mcp-list-timeout* 30)
(defvar *mcp-protocol-version* "2025-06-18")

(defstruct (mcp-server (:conc-name mcps-))
  name command args env cwd (timeout 120)
  process thread (status :stopped) last-error
  (next-id 0)
  (pending (make-hash-table))
  (lock (sb-thread:make-mutex :name "mcp pending"))
  (wlock (sb-thread:make-mutex :name "mcp write"))
  (slock (sb-thread:make-mutex :name "mcp start"))
  tool-names (hung 0) (ever-ready nil))

(defstruct (mcp-pending (:conc-name mcpp-))
  (sem (sb-thread:make-semaphore)) result error)

;;; ---------------------------------------------------------------- JSON

(defun mcp-get (h key)
  "KEY of hash table H, or NIL when H is no table, KEY missing or null."
  (and (hash-table-p h)
       (let ((v (gethash key h)))
         (and (not (eq v :null)) v))))

(defun mcp-vec-list (v)
  (cond ((null v) nil) ((listp v) v) ((vectorp v) (coerce v 'list))))

(defun mcp-log-path (srv)
  (let ((p (lisping-core:session-path
            (format nil "tmp/mcp-~A.log" (mcps-name srv)))))
    (ensure-directories-exist p)
    p))

(defun mcp-log (srv fmt &rest args)
  "Append a line about SRV to its log file."
  (ignore-errors
    (with-open-file (o (mcp-log-path srv) :direction :output :if-exists :append
                       :if-does-not-exist :create :external-format :utf-8)
      (format o "[mcp] ~?~%" fmt args))))

;;; ------------------------------------------------------------ transport

(defun mcp-alive-p (srv)
  (let ((p (mcps-process srv)))
    (and p (sb-ext:process-alive-p p))))

(defun mcp-send (srv obj)
  "Write OBJ as one JSON line to SRV's stdin."
  (sb-thread:with-mutex ((mcps-wlock srv))
    (let ((s (sb-ext:process-input (mcps-process srv))))
      (write-string (lisping-core:json-encode obj) s)
      (write-char #\Newline s)
      (finish-output s))))

(defun mcp-fail-pending (srv why)
  (sb-thread:with-mutex ((mcps-lock srv))
    (maphash (lambda (id p)
               (declare (ignore id))
               (setf (mcpp-error p) why)
               (sb-thread:signal-semaphore (mcpp-sem p)))
             (mcps-pending srv))
    (clrhash (mcps-pending srv))))

(defun mcp-deliver (srv msg)
  (let* ((id (gethash "id" msg))
         (p (sb-thread:with-mutex ((mcps-lock srv))
              (prog1 (gethash id (mcps-pending srv))
                (remhash id (mcps-pending srv))))))
    (when p
      (let ((err (mcp-get msg "error")))
        (if err
            (setf (mcpp-error p)
                  (format nil "~A: ~A" (or (mcp-get err "code") "error")
                          (or (mcp-get err "message") "(no message)")))
            (setf (mcpp-result p) (gethash "result" msg))))
      (sb-thread:signal-semaphore (mcpp-sem p)))))

(defun mcp-answer-request (srv msg)
  "A request FROM the server: answer ping and roots/list, refuse the rest."
  (let ((id (gethash "id" msg)) (method (gethash "method" msg)))
    (ignore-errors
      (mcp-send srv
                (cond ((equal method "ping")
                       (lisping-core::obj "jsonrpc" "2.0" "id" id "result" (lisping-core::obj)))
                      ((equal method "roots/list")
                       (lisping-core::obj "jsonrpc" "2.0" "id" id
                                          "result" (lisping-core::obj "roots" #())))
                      (t (lisping-core::obj "jsonrpc" "2.0" "id" id
                                            "error" (lisping-core::obj "code" -32601
                                                                       "message" "method not supported"))))))))

(defun mcp-handle-line (srv line)
  (let ((msg (and (plusp (length (string-trim " 	" line)))
                  (ignore-errors (lisping-core:json-decode line)))))
    (cond ((not (hash-table-p msg))
           (mcp-log srv "non-JSON on stdout: ~A" (subseq line 0 (min 200 (length line)))))
          ((and (gethash "method" msg) (nth-value 1 (gethash "id" msg)))
           (mcp-answer-request srv msg))
          ((gethash "method" msg)
           (when (equal (gethash "method" msg) "notifications/tools/list_changed")
             (sb-thread:make-thread
              (lambda () (ignore-errors (mcp-refresh-tools srv)))
              :name "mcp-refresh")))
          ((nth-value 1 (gethash "id" msg)) (mcp-deliver srv msg)))))

(defun mcp-reader-loop (srv proc)
  (let ((in (sb-ext:process-output proc)))
    (handler-case
        (loop for line = (read-line in nil nil)
              while line do (mcp-handle-line srv line))
      (error (e) (mcp-log srv "reader error: ~A" e)))
    (when (eq (mcps-process srv) proc)
      (setf (mcps-status srv) :dead)
      (unless (mcps-last-error srv) (setf (mcps-last-error srv) "server exited"))
      (mcp-fail-pending srv "server exited"))))

(defun mcp-request (srv method params &optional (timeout 30))
  "Send a request and wait.  Returns (values result error-string)."
  (let* ((p (make-mcp-pending))
         (id (sb-thread:with-mutex ((mcps-lock srv))
               (let ((id (incf (mcps-next-id srv))))
                 (setf (gethash id (mcps-pending srv)) p)
                 id))))
    (handler-case
        (mcp-send srv (lisping-core::obj "jsonrpc" "2.0" "id" id "method" method
                                         "params" (or params (lisping-core::obj))))
      (error (e)
        (sb-thread:with-mutex ((mcps-lock srv)) (remhash id (mcps-pending srv)))
        (return-from mcp-request (values nil (format nil "send failed: ~A" e)))))
    (cond ((sb-thread:wait-on-semaphore (mcpp-sem p) :timeout timeout)
           (values (mcpp-result p) (mcpp-error p)))
          (t (sb-thread:with-mutex ((mcps-lock srv)) (remhash id (mcps-pending srv)))
             (ignore-errors
               (mcp-send srv (lisping-core::obj
                              "jsonrpc" "2.0" "method" "notifications/cancelled"
                              "params" (lisping-core::obj "requestId" id
                                                          "reason" "client timeout"))))
             (values nil (format nil "timeout after ~Ds" timeout))))))

(defun mcp-notify (srv method)
  (ignore-errors
    (mcp-send srv (lisping-core::obj "jsonrpc" "2.0" "method" method))))

;;; ------------------------------------------------------- process control

(defun mcp-kill (srv)
  "Stop SRV's process group: close stdin, SIGTERM, SIGKILL after ~2s."
  (let ((p (mcps-process srv)))
    (when p
      (let ((pid (sb-ext:process-pid p)))
        (ignore-errors (close (sb-ext:process-input p)))
        (ignore-errors (sb-posix:kill (- pid) 15))
        (loop repeat 20
              while (handler-case (progn (sb-posix:kill (- pid) 0) t) (error () nil))
              do (sleep 0.1))
        (when (handler-case (progn (sb-posix:kill (- pid) 0) t) (error () nil))
          (ignore-errors (sb-posix:kill (- pid) 9)))))
    (setf (mcps-process srv) nil
          (mcps-status srv) :stopped)
    (mcp-fail-pending srv "server stopped")))

(defun mcp-spawn (srv)
  "Start the process; returns NIL or an error string."
  (handler-case
      (let* ((env (append (loop for (k . v) in (mcps-env srv) collect (format nil "~A=~A" k v))
                          (sb-ext:posix-environ)))
             (proc (sb-ext:run-program
                    (mcps-command srv) (mcps-args srv)
                    :search t :wait nil :input :stream :output :stream
                    :error (mcp-log-path srv) :if-error-exists :append
                    :environment env
                    :directory (or (mcps-cwd srv)
                                   (and lisping-core:*cwd* (namestring lisping-core:*cwd*)))
                    :external-format '(:utf-8 :replacement #\?))))
        (clrhash (mcps-pending srv))
        (setf (mcps-process srv) proc (mcps-status srv) :starting
              (mcps-last-error srv) nil)
        (mcp-log srv "spawned ~A ~{~A~^ ~} pid ~D" (mcps-command srv) (mcps-args srv)
                 (sb-ext:process-pid proc))
        (setf (mcps-thread srv)
              (sb-thread:make-thread (lambda () (mcp-reader-loop srv proc))
                                     :name (format nil "mcp-reader-~A" (mcps-name srv))))
        nil)
    (error (e) (format nil "cannot start ~A: ~A" (mcps-command srv) e))))

;;; ------------------------------------------------------------- tools

(defun mcp-tool-name (srv tool)
  (let ((raw (format nil "~A__~A" (mcps-name srv) tool)))
    (subseq (substitute-if #\_ (lambda (c) (not (or (alphanumericp c) (char= c #\_) (char= c #\-))))
                           raw)
            0 (min 64 (length raw)))))

(defun mcp-block-text (b)
  (let ((type (mcp-get b "type")))
    (cond ((equal type "text") (or (mcp-get b "text") ""))
          ((member type '("image" "audio") :test #'equal)
           (format nil "[~A ~A, ~D chars base64]" type (or (mcp-get b "mimeType") "?")
                   (length (or (mcp-get b "data") ""))))
          ((equal type "resource")
           (let ((r (mcp-get b "resource")))
             (or (mcp-get r "text")
                 (format nil "[resource ~A ~A]" (mcp-get r "uri") (or (mcp-get r "mimeType") "")))))
          ((equal type "resource_link")
           (format nil "[resource link ~A~@[ (~A)~]]" (mcp-get b "uri") (mcp-get b "name")))
          (t (lisping-core:json-encode b)))))

(defun mcp-result-text (result)
  "Tool-call RESULT as text; an isError result is marked."
  (let* ((blocks (mcp-vec-list (mcp-get result "content")))
         (text (format nil "~{~A~^~%~}" (mapcar #'mcp-block-text blocks))))
    (when (and (zerop (length text)) (mcp-get result "structuredContent"))
      (setf text (lisping-core:json-encode (mcp-get result "structuredContent"))))
    (if (and (mcp-get result "isError") (not (eq (mcp-get result "isError") :false)))
        (format nil "MCP tool error: ~A" text)
        text)))

(defun mcp-call-tool (srv tool args)
  "Run TOOL on SRV with ARGS (a hash table); starts/restarts the server."
  (multiple-value-bind (ok note) (mcp-ensure srv)
    (if (not ok)
        (format nil "MCP server ~A is not available: ~A" (mcps-name srv) note)
        (multiple-value-bind (result err)
            (mcp-request srv "tools/call"
                         (lisping-core::obj "name" tool
                                            "arguments" (or args (lisping-core::obj)))
                         (mcps-timeout srv))
          (cond (err
                 (when (search "timeout" err)
                   (when (>= (incf (mcps-hung srv)) 2)
                     (mcp-log srv "two timeouts in a row, killing the server")
                     (mcp-kill srv)))
                 (format nil "~@[~A~%~]MCP ~A/~A failed: ~A~:[~; (server killed, restarts on next use)~]"
                         note (mcps-name srv) tool err (eq (mcps-status srv) :stopped)))
                (t (setf (mcps-hung srv) 0)
                   (format nil "~@[~A~%~]~A" note (mcp-result-text result))))))))

(defun mcp-register-tool (srv def)
  "Register server tool DEF (a hash table) as a harness tool; its Lisp name."
  (let* ((tool (mcp-get def "name"))
         (name (mcp-tool-name srv tool))
         (schema (let ((s (mcp-get def "inputSchema")))
                   (if (hash-table-p s) s
                       (lisping-core::obj "type" "object" "properties" (lisping-core::obj)))))
         (desc (format nil "~A~%(MCP server ~A, tool ~A)"
                       (or (mcp-get def "description") (mcp-get def "title") "")
                       (mcps-name srv) tool)))
    (lisping-core::register-tool
     name desc schema
     (lambda (args) (mcp-call-tool srv tool args)))
    name))

(defun mcp-list-tools (srv)
  "All tool definitions of SRV (following nextCursor): (values list error)."
  (let ((all nil) (cursor nil))
    (loop
      (multiple-value-bind (result err)
          (mcp-request srv "tools/list"
                       (if cursor (lisping-core::obj "cursor" cursor) (lisping-core::obj))
                       *mcp-list-timeout*)
        (when err (return-from mcp-list-tools (values nil err)))
        (setf all (append all (mcp-vec-list (mcp-get result "tools"))))
        (setf cursor (mcp-get result "nextCursor"))
        (unless cursor (return (values all nil)))))))

(defun mcp-refresh-tools (srv)
  "Re-list SRV's tools, register new ones and unregister vanished ones."
  (multiple-value-bind (defs err) (mcp-list-tools srv)
    (if err
        (progn (mcp-log srv "tools/list failed: ~A" err) err)
        (let ((names (loop for d in defs when (mcp-get d "name")
                           collect (mcp-register-tool srv d))))
          (dolist (old (set-difference (mcps-tool-names srv) names :test #'string=))
            (ignore-errors (lisping-core::unregister-tool old)))
          (setf (mcps-tool-names srv) names)
          nil))))

;;; --------------------------------------------------------- lifecycle

(defun mcp-handshake (srv)
  "initialize + initialized + tool listing; NIL or an error string."
  (multiple-value-bind (result err)
      (mcp-request srv "initialize"
                   (lisping-core::obj
                    "protocolVersion" *mcp-protocol-version*
                    "capabilities" (lisping-core::obj)
                    "clientInfo" (lisping-core::obj "name" "lisping-ai" "version" "1"))
                   *mcp-init-timeout*)
    (declare (ignore result))
    (or (and err (format nil "initialize failed: ~A" err))
        (progn (mcp-notify srv "notifications/initialized")
               (mcp-refresh-tools srv)))))

(defun mcp-ensure (srv)
  "Make SRV ready, (re)starting it if needed.  Returns (values ok note):
NOTE says that it was restarted, or why it failed."
  (sb-thread:with-mutex ((mcps-slock srv))
    (if (and (eq (mcps-status srv) :ready) (mcp-alive-p srv))
        (values t nil)
        (let ((was (mcps-ever-ready srv)))
          (when (mcps-process srv) (mcp-kill srv))
          (let ((err (or (mcp-spawn srv)
                         (mcp-handshake srv))))
            (cond (err
                   (setf (mcps-last-error srv) err)
                   (mcp-log srv "start failed: ~A" err)
                   (mcp-kill srv)
                   (setf (mcps-status srv) :dead)
                   (values nil err))
                  (t (setf (mcps-status srv) :ready (mcps-ever-ready srv) t
                           (mcps-hung srv) 0)
                     (values t (and was (format nil "[MCP server ~A was restarted]"
                                                (mcps-name srv)))))))))))

;;; ---------------------------------------------------------- config

(defun mcp-read-config-file (path)
  "mcpServers table of the JSON file PATH, or NIL."
  (when (probe-file path)
    (handler-case
        (mcp-get (lisping-core:json-decode (lisping-core:read-file path)) "mcpServers")
      (error (e) (lisping-core::emit :info :text (format nil "mcp: bad ~A: ~A" path e)) nil))))

(defun mcp-config ()
  "Merged server table (home file, then session overrides): list of
(name . entry-hash), disabled ones dropped."
  (let ((merged nil))
    (dolist (path (list (lisping-core:home-path "mcp.json")
                        (lisping-core:session-path "mcp.json")))
      (let ((table (mcp-read-config-file path)))
        (when table
          (maphash (lambda (k v)
                     (setf merged (cons (cons k v) (remove k merged :key #'car :test #'string=))))
                   table))))
    (remove-if (lambda (e) (or (not (hash-table-p (cdr e)))
                               (mcp-get (cdr e) "disabled")
                               (not (stringp (mcp-get (cdr e) "command")))))
               (reverse merged))))

(defun mcp-make-server (name entry)
  (make-mcp-server
   :name name
   :command (mcp-get entry "command")
   :args (mapcar #'princ-to-string (mcp-vec-list (mcp-get entry "args")))
   :env (let ((e (mcp-get entry "env")) (r nil))
          (when (hash-table-p e)
            (maphash (lambda (k v) (push (cons k (princ-to-string v)) r)) e))
          (nreverse r))
   :cwd (mcp-get entry "cwd")
   :timeout (let ((tm (mcp-get entry "timeout"))) (if (realp tm) (max 1 (round tm)) 120))))

(defun mcp-stop-all ()
  (let ((servers (loop for s being the hash-values of *mcp-servers* collect s)))
    (dolist (s servers)
      (dolist (n (mcps-tool-names s)) (ignore-errors (lisping-core::unregister-tool n)))
      (setf (mcps-tool-names s) nil))
    (mapc (lambda (s) (ignore-errors (mcp-kill s))) servers)
    (clrhash *mcp-servers*)
    (length servers)))

(defun mcp-start-server-async (srv)
  (sb-thread:make-thread
   (lambda () (ignore-errors (mcp-ensure srv)))
   :name (format nil "mcp-start-~A" (mcps-name srv))))

(defun mcp-load ()
  "(Re)read the config and start every server in the background."
  (mcp-stop-all)
  (loop for (name . entry) in (mcp-config)
        for srv = (mcp-make-server name entry)
        do (setf (gethash name *mcp-servers*) srv)
           (mcp-start-server-async srv))
  (hash-table-count *mcp-servers*))

;;; ------------------------------------------------------- status, UI

(defun mcp-status-line (srv)
  (format nil "~A: ~(~A~), ~D tool~:P~@[ — ~A~]"
          (mcps-name srv) (if (and (eq (mcps-status srv) :ready) (not (mcp-alive-p srv)))
                              :dead (mcps-status srv))
          (length (mcps-tool-names srv))
          (and (member (mcps-status srv) '(:dead :stopped)) (mcps-last-error srv))))

(defun mcp-status-text ()
  (let ((names (sort (loop for k being the hash-keys of *mcp-servers* collect k) #'string<)))
    (if names
        (format nil "~{~A~^~%~}"
                (mapcar (lambda (n) (mcp-status-line (gethash n *mcp-servers*))) names))
        "no MCP servers configured (~/.lisper/mcp.json, {\"mcpServers\": {name: {command, args, env}}})")))

(defun mcp-restart (name)
  (let ((srv (gethash name *mcp-servers*)))
    (if (null srv)
        (format nil "no MCP server ~S" name)
        (progn (sb-thread:with-mutex ((mcps-slock srv)) (mcp-kill srv))
               (multiple-value-bind (ok note) (mcp-ensure srv)
                 (if ok (mcp-status-line srv) (format nil "restart failed: ~A" note)))))))

(defun mcp-command (args)
  "ARGS: \"\" / \"list\", \"reload\", or \"restart NAME\"; returns text."
  (let* ((args (string-trim " " args))
         (sp (position #\Space args))
         (verb (if sp (subseq args 0 sp) args))
         (rest (if sp (string-trim " " (subseq args sp)) "")))
    (cond ((member verb '("" "list" "status") :test #'string=) (mcp-status-text))
          ((string= verb "reload")
           (format nil "reloading: ~D server~:P started~%~A" (mcp-load) (mcp-status-text)))
          ((string= verb "restart") (mcp-restart rest))
          (t "usage: /mcp [list | reload | restart NAME]"))))

(defun slash-mcp (ui args)
  (declare (ignore ui))
  (slash-say (mcp-command args)))

(defun slash-mcp-args (ui partial)
  (declare (ignore ui))
  (loop for c in (append '("list" "reload")
                         (loop for k being the hash-keys of *mcp-servers*
                               collect (format nil "restart ~A" k)))
        when (and (<= (length partial) (length c))
                  (string-equal partial c :end2 (length partial)))
          collect c))

(defun mcp-tool-fn (args)
  (mcp-command (format nil "~A ~A" (or (lisping-core::arg args "action") "list")
                       (or (lisping-core::arg args "name") ""))))

(defun mcp-start ()
  "Called from init.lisp: tools, /mcp, stop hook, and the servers."
  (setf *tui-stop-hooks* (remove 'mcp-stop-all *tui-stop-hooks*))
  (setf *tui-stop-hooks* (append *tui-stop-hooks* (list 'mcp-stop-all)))
  (define-slash "mcp" 'slash-mcp "MCP servers: list, reload, restart NAME" 'slash-mcp-args)
  (lisping-core::register-tool
   "mcp" "Manage MCP servers: action list (default), reload (re-read ~/.lisper/mcp.json and restart all), restart (with name). Each server's tools appear as SERVER__TOOL."
   (lisping-core::obj "type" "object"
                      "properties" (lisping-core::obj "action" (lisping-core::obj "type" "string")
                                                      "name" (lisping-core::obj "type" "string")))
   'mcp-tool-fn)
  (unless lisping-core:*test-boot* (mcp-load)))
