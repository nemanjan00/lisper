;;;; core.lisp: the frozen kernel of lisping-ai.
;;;;
;;;; Everything here is deliberately small: an agent loop, one tool (eval),
;;;; an API to register more tools, and Emacs-style persistence: files that
;;;; init.lisp loads at boot, kept in git. The AI builds everything else itself.

(require :sb-concurrency)

(defpackage :lisping-core
  (:use :cl)
  (:export #:main
           ;; tools
           #:register-tool #:unregister-tool #:*tools* #:arg
           ;; files
           #:home-path #:session-path #:*home* #:*session* #:*cwd* #:session-save #:session-forget #:home-commit #:read-file #:session-list #:session-switch
           #:load-file #:write-files-and-load #:test-boot #:*test-boot* #:*observers* #:synthetic-message-p #:commit #:restart-image #:*boot-errors* #:*previous-session*
           ;; conversation and I/O
           #:*system-prompt* #:*messages* #:*inbox* #:*emit* #:emit
           #:send-user-message #:*before-request* #:*held* #:*deliver-mid-turn* #:queued-messages #:edit-queued #:pin-queued #:release-queued
           ;; the model call, replaceable (e.g. to stream)
           #:*complete* #:*last-usage* #:default-complete #:chat-url #:api-headers #:*terminal-thread* #:*queue-thread*
           ;; settings
           #:*model* #:*api-base* #:*max-tokens* #:*eval-timeout* #:*output-limit* #:*root*
           ;; helpers
           #:obj #:json-encode #:json-decode #:http-post-json #:shell))

(defpackage :lisping
  (:use :cl :lisping-core))

(in-package :lisping-core)

(declaim (ftype function emit client-error-p complete-once without-x-kind synthetic-message-p session-save note-file session-name-p)) ; used before their definitions further down

(defvar *root* (make-pathname :name nil :type nil :version nil :defaults *load-truename*)
  "The harness's code: core.lisp, init.lisp and the files it loads, .git. Runtime
state lives in *HOME*, not here.")

(defvar *home* nil
  "Your state directory, outside the code repo: LISPER_HOME, else ~/.lisper/. Holds
sessions/<id>/ (conversation snapshot, transcripts, trace, queue), tmp/, and your
own data files (notes, todos, layout).")
(defvar *session* "main" "This process's session id (LISPER_SESSION). One process is one session.")
(defvar *cwd* nil
  "Working directory for shell and project work (LISPER_CWD, else the session's
saved cwd, else *ROOT*). Saved with the session.")

(defun home-path (name)
  "NAME inside *HOME* (directories are created on demand)."
  (let ((path (merge-pathnames name *home*))) (ensure-directories-exist path) path))
(defun session-dir () (merge-pathnames (format nil "sessions/~A/" *session*) *home*))
(defun session-path (name)
  "NAME inside this session's directory."
  (let ((path (merge-pathnames name (session-dir)))) (ensure-directories-exist path) path))

;;; ------------------------------------------------------------------
;;; Settings

(defvar *api-base* nil)
(defvar *api-key* nil)
(defvar *model* nil)
(defvar *max-tokens* 32000)
(defvar *eval-timeout* 300 "Seconds an eval tool call may run before it is interrupted.")
(defvar *output-limit* 20000 "Characters of tool output returned to the model.")

(defun read-dotenv (path)
  "Return a hash table of KEY=VALUE lines from PATH (missing file -> empty)."
  (let ((table (make-hash-table :test #'equal)))
    (with-open-file (in path :if-does-not-exist nil :external-format :utf-8)
      (when in
        (loop for line = (read-line in nil)
              while line
              do (let* ((text (string-trim '(#\Space #\Tab #\Return) line))
                        (eq (position #\= text)))
                   (when (and eq (plusp eq) (char/= (char text 0) #\#))
                     (setf (gethash (string-trim " " (subseq text 0 eq)) table)
                           (string-trim "\"'" (string-trim " " (subseq text (1+ eq))))))))))
    table))

(defun configure ()
  (let* ((path (or (sb-ext:posix-getenv "LISPING_DOTENV")
                   (namestring (merge-pathnames ".env" *root*))))
         (env (read-dotenv path)))
    (flet ((setting (name default)
             (or (sb-ext:posix-getenv name) (gethash name env) default)))
      (setf *api-base* (string-right-trim "/" (setting "OPENAI_BASE_URL" "https://api.kimi.com/coding/v1"))
            *api-key* (setting "OPENAI_API_KEY" nil)
            *model* (setting "MODEL" "kimi-k2-7-code")))
    (unless *api-key*
      (error "No OPENAI_API_KEY in the environment or in ~A" path))))

;;; ------------------------------------------------------------------
;;; JSON. Objects are equal hash tables, arrays are vectors, true is T,
;;; false is NIL, null is :NULL. The encoder also writes :FALSE and :TRUE,
;;; non-empty lists as arrays, and other symbols as lowercase strings.

(defun obj (&rest keys-and-values)
  "Build a JSON object: (obj :type \"object\" \"x\" 1). Keyword keys are downcased."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash (if (symbolp key) (string-downcase (symbol-name key)) key) table) value))
    table))

(defun write-json-string (string stream)
  (write-char #\" stream)
  (loop for c across string
        do (case c
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (t (if (< (char-code c) 32)
                    (format stream "\\u~4,'0X" (char-code c))
                    (write-char c stream)))))
  (write-char #\" stream))

(defun write-json (x stream)
  (cond ((or (eq x t) (eq x :true)) (write-string "true" stream))
        ((or (null x) (eq x :false)) (write-string "false" stream))
        ((eq x :null) (write-string "null" stream))
        ((stringp x) (write-json-string x stream))
        ((integerp x) (format stream "~D" x))
        ((realp x) (format stream "~F" (float x 1d0)))
        ((hash-table-p x)
         (write-char #\{ stream)
         (let ((first t))
           (maphash (lambda (k v)
                      (unless first (write-char #\, stream))
                      (setf first nil)
                      (write-json-string (if (symbolp k) (string-downcase (symbol-name k)) (princ-to-string k)) stream)
                      (write-char #\: stream)
                      (write-json v stream))
                    x))
         (write-char #\} stream))
        ((or (vectorp x) (consp x))
         (write-char #\[ stream)
         (let ((first t))
           (map nil (lambda (v)
                      (unless first (write-char #\, stream))
                      (setf first nil)
                      (write-json v stream))
                x))
         (write-char #\] stream))
        ((symbolp x) (write-json-string (string-downcase (symbol-name x)) stream))
        (t (error "Can't encode ~S as JSON" x))))

(defun json-encode (x &optional stream)
  "Encode X as JSON to STREAM, or return a string when STREAM is NIL."
  (if stream
      (write-json x stream)
      (with-output-to-string (s) (write-json x s))))

(defun json-decode (string)
  "Parse a JSON STRING."
  (let ((pos 0)
        (len (length string)))
    (labels ((fail (what) (error "JSON parse error at ~D: ~A" pos what))
             (peek () (when (< pos len) (char string pos)))
             (next () (if (< pos len) (prog1 (char string pos) (incf pos)) (fail "unexpected end")))
             (skip-ws ()
               (loop while (and (< pos len) (member (char string pos) '(#\Space #\Tab #\Newline #\Return)))
                     do (incf pos)))
             (literal (word value)
               (let ((end (+ pos (length word))))
                 (unless (and (<= end len) (string= word string :start2 pos :end2 end))
                   (fail "bad literal"))
                 (setf pos end)
                 value))
             (hex4 ()
               (when (> (+ pos 4) len) (fail "short \\u escape"))
               (prog1 (parse-integer string :start pos :end (+ pos 4) :radix 16)
                 (incf pos 4)))
             (read-string ()
               (with-output-to-string (out)
                 (loop
                   (let ((c (next)))
                     (cond ((char= c #\") (return))
                           ((char= c #\\)
                            (let ((e (next)))
                              (case e
                                (#\n (write-char #\Newline out))
                                (#\t (write-char #\Tab out))
                                (#\r (write-char #\Return out))
                                (#\b (write-char (code-char 8) out))
                                (#\f (write-char (code-char 12) out))
                                (#\u (let ((code (hex4)))
                                       (when (and (<= #xD800 code #xDBFF)
                                                  (< (1+ pos) len)
                                                  (char= (char string pos) #\\)
                                                  (char= (char string (1+ pos)) #\u))
                                         (incf pos 2)
                                         (setf code (+ #x10000 (ash (- code #xD800) 10) (- (hex4) #xDC00))))
                                       (write-char (code-char code) out)))
                                (t (write-char e out)))))
                           (t (write-char c out)))))))
             (read-number ()
               (let* ((start pos)
                      (end (or (position-if-not (lambda (c) (find c "+-0123456789.eE")) string :start pos) len))
                      (text (subseq string start end)))
                 (when (= start end) (fail "unexpected character"))
                 (setf pos end)
                 (if (find-if (lambda (c) (find c ".eE")) text)
                     (let ((*read-default-float-format* 'double-float)
                           (*read-eval* nil))
                       (coerce (read-from-string text) 'double-float))
                     (parse-integer text))))
             (read-value ()
               (skip-ws)
               (case (peek)
                 ((nil) (fail "unexpected end"))
                 (#\{ (next) (read-object))
                 (#\[ (next) (read-array))
                 (#\" (next) (read-string))
                 (#\t (literal "true" t))
                 (#\f (literal "false" nil))
                 (#\n (literal "null" :null))
                 (t (read-number))))
             (read-object ()
               (let ((table (make-hash-table :test #'equal)))
                 (skip-ws)
                 (when (eql (peek) #\})
                   (next)
                   (return-from read-object table))
                 (loop
                   (skip-ws)
                   (unless (eql (next) #\") (fail "expected a key"))
                   (let ((key (read-string)))
                     (skip-ws)
                     (unless (eql (next) #\:) (fail "expected :"))
                     (setf (gethash key table) (read-value)))
                   (skip-ws)
                   (case (next)
                     (#\,)
                     (#\} (return table))
                     (t (fail "expected , or }"))))))
             (read-array ()
               (let ((items '()))
                 (skip-ws)
                 (when (eql (peek) #\])
                   (next)
                   (return-from read-array (vector)))
                 (loop
                   (push (read-value) items)
                   (skip-ws)
                   (case (next)
                     (#\,)
                     (#\] (return (coerce (nreverse items) 'vector)))
                     (t (fail "expected , or ]")))))))
      (prog1 (read-value)
        (skip-ws)
        (when (< pos len) (fail "trailing characters"))))))

;;; ------------------------------------------------------------------
;;; Processes and HTTP (through curl, so the core needs no libraries)

(defun shell (command &key (directory (or *cwd* *root*)))
  "Run COMMAND with sh -c in DIRECTORY (default: *CWD*, the session's working
directory). Return (values output exit-code)."
  (let* ((out (make-string-output-stream))
         (process (sb-ext:run-program "/bin/sh" (list "-c" command)
                                      :directory (namestring directory)
                                      :output out :error :output :input nil
                                      :external-format :utf-8 :wait t)))
    (values (get-output-stream-string out) (sb-ext:process-exit-code process))))

(defun tmp-path (name type)
  (home-path (format nil "tmp/~A-~D-~D.~A" name (get-universal-time) (random 1000000) type)))

(defun curl-quote (string)
  (with-output-to-string (s)
    (write-char #\" s)
    (loop for c across string
          do (when (find c "\"\\") (write-char #\\ s))
             (write-char c s))
    (write-char #\" s)))

(define-condition http-error (error)
  ((status :initarg :status :reader http-error-status)
   (url :initarg :url :reader http-error-url)
   (body :initarg :body :reader http-error-body))
  (:report (lambda (c stream)
             (format stream "HTTP ~A from ~A: ~A"
                     (http-error-status c) (http-error-url c) (http-error-body c)))))

(defun http-post-json (url body &key headers (timeout 600))
  "POST BODY encoded as JSON to URL. Return (values decoded-response status).
Signals an error for HTTP status 400 and up. HEADERS are \"Name: value\" strings."
  (let ((request (tmp-path "request" "json")))
    (with-open-file (out request :direction :output :if-exists :supersede :external-format :utf-8)
      (json-encode body out))
    (unwind-protect
         (let* ((config (with-output-to-string (s)
                          (format s "url = ~A~%" (curl-quote url))
                          (dolist (h (list* "Content-Type: application/json" "Accept: application/json" headers))
                            (format s "header = ~A~%" (curl-quote h)))
                          (format s "data-binary = ~A~%" (curl-quote (format nil "@~A" (namestring request))))
                          (format s "max-time = ~D~%" timeout)
                          (format s "write-out = \"\\n%{http_code}\"~%")))
                (out (make-string-output-stream))
                (err (make-string-output-stream))
                (process (sb-ext:run-program "curl" '("-sS" "-K" "-") :search t
                                             :input (make-string-input-stream config)
                                             :output out :error err
                                             :external-format :utf-8 :wait t))
                (raw (get-output-stream-string out))
                (split (position #\Newline raw :from-end t))
                (text (if split (subseq raw 0 split) ""))
                (status (if split (parse-integer raw :start (1+ split) :junk-allowed t) nil)))
           (unless (zerop (sb-ext:process-exit-code process))
             (error "curl failed: ~A" (get-output-stream-string err)))
           (when (or (null status) (>= status 400))
             (error 'http-error :status status :url url :body (subseq text 0 (min 2000 (length text)))))
           (values (json-decode text) status))
      (ignore-errors (delete-file request)))))

;;; ------------------------------------------------------------------
;;; Files, the way Emacs does it: at boot the core loads init.lisp, which loads
;;; the rest. Evaluating code changes the running image only; what lasts is
;;; what is in the files. git keeps their history.

(defvar *booting* nil "True while init.lisp is loaded at boot.")
(defvar *test-boot* nil
  "True only inside TEST-BOOT's fresh process, which shares the files with the running
image (and *HOME*). Skip side effects (writing files, starting things) when it's true.")
(defvar *boot-errors* '() "Problems reported while loading init.lisp at the last boot.")
(defvar *load-problems* nil "Problems collected by LOAD-FILE, including nested loads.")
(defvar *reported-conditions* nil
  "Conditions already reported during the current load: a warning from a nested
load-file also reaches the outer one's handlers, and should be listed once.")

(defun init-path () (merge-pathnames "init.lisp" *root*))
(defun resume-path () (session-path "resume.txt"))

(defun read-text (path)
  "The contents of PATH as a string, or NIL if it doesn't exist."
  (with-open-file (in path :if-does-not-exist nil :external-format :utf-8)
    (when in
      (let ((text (make-string (file-length in))))
        (subseq text 0 (read-sequence text in))))))

(defun write-text (path text)
  (when (and (equalp (pathname-type (pathname path)) "lisp") (find (code-char 0) text))
    (error "Refusing to write ~A: it contains NUL characters (often from a string made with (make-string (file-length ...)) on a UTF-8 file, where bytes outnumber characters)." path))
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
    (write-string text out)))

(defun form-label (form)
  (if (and (consp form) (symbolp (car form)))
      (format nil "~(~A~)~@[ ~A~]" (car form)
              (and (consp (cdr form)) (atom (cadr form)) (cadr form)))
      (let ((text (prin1-to-string form)))
        (subseq text 0 (min 60 (length text))))))

(defun shell-quote (string)
  (with-output-to-string (s)
    (write-char #\' s)
    (loop for c across string
          do (if (char= c #\') (write-string "'\\''" s) (write-char c s)))
    (write-char #\' s)))

(defun git (command &key (directory *root*))
  "Run a git COMMAND in DIRECTORY (the code repo by default), retrying while
another session's git holds the index lock. Return (values output exit-code)."
  (loop repeat 8
        do (multiple-value-bind (out code) (shell command :directory directory)
             (unless (and (/= code 0) (search "index.lock" out))
               (return-from git (values out code))))
           (sleep 0.4))
  (shell command :directory directory))

(defun staged-tree (paths)
  "Build exactly the tree a path-limited commit of PATHS would make (HEAD plus the
current contents of PATHS) in a temporary index, and check it out into a
temporary directory. Return (values directory tree-hash), or NIL if git can't.
Booting that directory checks precisely what gets committed: another session's
uncommitted files in the working tree are not in it."
  (let* ((stamp (format nil "~D-~D" (get-universal-time) (random 1000000)))
         (index (namestring (home-path (format nil "tmp/index-~A" stamp))))
         (dir (namestring (home-path (format nil "tmp/tree-~A/" stamp))))
         (env (format nil "GIT_INDEX_FILE=~A " (shell-quote index))))
    (multiple-value-bind (out code)
        (git (format nil "~Agit read-tree HEAD && ~Agit add -- ~{~A ~}&& ~Agit checkout-index -a --prefix=~A && ~Agit write-tree"
                     env env (mapcar #'shell-quote paths) env (shell-quote dir) env))
      (ignore-errors (delete-file index))
      (cond ((eql code 0)
             ;; As in the hook: the settings file and an empty state directory.
             (when (probe-file (merge-pathnames ".env" *root*))
               (shell (format nil "ln -s ~A ~A" (shell-quote (namestring (merge-pathnames ".env" *root*)))
                              (shell-quote (concatenate 'string dir ".env")))))
             (ensure-directories-exist (concatenate 'string dir "state/"))
             (values dir (string-trim '(#\Space #\Newline)
                                      (subseq out (1+ (or (position #\Newline (string-right-trim '(#\Newline) out) :from-end t) -1))))))
            (t (remove-tree dir) nil)))))

(defun remove-tree (dir)
  (when (and dir (or (search "/tmp/tree-" dir) (search "/tmp/home-" dir)))
    (shell (format nil "rm -rf ~A" (shell-quote dir)))))

(defun checked-test-boot (paths extra)
  "TEST-BOOT on the tree a commit of PATHS would hold. Return (values problems
tree-hash); the hash is NIL when that tree couldn't be built and the working
tree was booted instead."
  (multiple-value-bind (dir tree) (staged-tree paths)
    (unwind-protect
         (values (test-boot :timeout 60 :extra extra :directory (or dir *root*)) tree)
      (remove-tree dir))))

(defun commit (message &optional path checked)
  "Commit under the project directory: just PATH when given (so the commit holds
exactly the file that was checked), else everything. Does nothing during boot.
A git pre-commit hook boot-checks every commit, however it's made; CHECKED is
the hash of a tree that just booted cleanly (from STAGED-TREE), and the hook
skips its own boot when that is exactly the tree being committed.
Returns NIL on success, else git's output explaining why nothing was committed."
  (unless *booting*
    (multiple-value-bind (out code)
        (progn (git (if path (format nil "git add -- ~A" (shell-quote path)) "git add -A"))
               (git (format nil "~Agit commit -q -m ~A~@[ -- ~A~]"
                            (if checked (format nil "LISPER_CHECKED_TREE=~A " checked) "") (shell-quote message)
                            (and path (shell-quote path)))))
        (unless (or (eql code 0) (search "nothing to commit" out) (search "no changes added" out))
          (string-trim '(#\Space #\Newline) out)))))

(defun commit-note (failure)
  (if failure (format nil "NOT committed (the file is written and loaded): ~A" failure) "committed"))

(defun skip-to-next-toplevel-form (in start)
  "Move IN past the form starting at START, to the next line that begins with (."
  (file-position in start)
  (read-line in nil)
  (loop
    (let ((here (file-position in))
          (line (read-line in nil)))
      (when (or (null line) (and (plusp (length line)) (char= (char line 0) #\()))
        (file-position in here)
        (return line)))))

(defun restart-image (&optional resume-message)
  "Exit so the supervisor restarts the image. RESUME-MESSAGE is sent to you after boot,
followed by any user messages that were still held, waiting for the turn to end."
  (let ((held (and (boundp '*held*) (symbol-value '*held*))))
    (when held
      (setf resume-message
            (format nil "~@[~A~%~%~]Messages that were waiting when the image restarted, oldest first:~{~%- ~A~}"
                    resume-message
                    (mapcar (lambda (item) (if (consp item) (car item) item)) held)))))
  (ignore-errors (session-save))
  (when resume-message
    (with-open-file (out (resume-path) :direction :output :if-exists :supersede
                                       :if-does-not-exist :create :external-format :utf-8)
      (write-string resume-message out)))
  (sb-ext:exit :code 42 :abort t))

;;; ------------------------------------------------------------------
;;; Tools

(defstruct tool name description schema fn)

(defvar *tools* (make-hash-table :test #'equal) "Tool name -> TOOL.")

(defun register-tool (name description schema fn)
  "Expose FN to the model as tool NAME. SCHEMA is a JSON Schema for the arguments,
as a JSON string or an OBJ. FN gets the arguments as a hash table (see ARG) and
returns a string or any printable value. Pass a symbol as FN so redefining the
function updates the tool."
  (setf (gethash name *tools*)
        (make-tool :name name :description description
                   :schema (if (stringp schema) (json-decode schema) schema)
                   :fn fn))
  name)

(defun unregister-tool (name)
  (remhash name *tools*))

(defun arg (args key &optional default)
  "Look up KEY in a tool's ARGS hash table."
  (multiple-value-bind (value found) (gethash key args)
    (if found value default)))

(defun limit-output (text)
  (if (> (length text) *output-limit*)
      (format nil "~A~%... [truncated ~D of ~D characters]"
              (subseq text 0 *output-limit*) (- (length text) *output-limit*) (length text))
      text))

(defun user-backtrace ()
  "The current backtrace, cut off where the harness's own frames begin."
  (let* ((text (with-output-to-string (s)
                 (sb-debug:print-backtrace :stream s :count 25)))
         (cut (or (search "LISPING-CORE::EVAL-TOPLEVEL" text) (search "LISPING-CORE::EVAL-CODE" text)))
         (line (and cut (position #\Newline text :end cut :from-end t))))
    (if line (subseq text 0 (1+ line)) text)))

(defun similar-names (name candidates what &optional preferred)
  "Up to 8 CANDIDATES sharing parts of NAME; names in PREFERRED rank higher."
  (let* ((parts (loop with start = 0
                      for end = (position #\- name :start start)
                      for part = (subseq name start end)
                      when (>= (length part) 3) collect part
                      while end do (setf start (1+ end))))
         (scored (loop for candidate in (remove-duplicates candidates :test #'string=)
                       for score = (+ (if (search name candidate) 3 0)
                                      (if (member candidate preferred :test #'string=) 1 0)
                                      (count-if (lambda (part) (search part candidate)) parts))
                       when (plusp score) collect (cons score candidate)))
         (best (subseq (stable-sort scored #'> :key #'car) 0 (min 8 (length scored)))))
    (when best
      (format nil "Similar ~A: ~{~(~A~)~^, ~}" what (mapcar #'cdr best)))))

(defun error-hint (condition)
  "A hint for an error the model often makes (a contrib not loaded, a misremembered
symbol, a function in another package), or NIL."
  (let ((text (princ-to-string condition)))
    (flet ((between (prefix suffix)
             (let ((start (search prefix text)))
               (when start
                 (let* ((from (+ start (length prefix)))
                        (end (search suffix text :start2 from)))
                   (when end (string-trim "\"" (subseq text from end))))))))
      (let ((missing-package (between "Package " " does not exist"))
            (missing-symbol (or (between "Symbol " " not found in the ")
                                (between "The symbol " " is not external in the ")))
            (home (or (between " not found in the " " package")
                      (between " is not external in the " " package"))))
        (cond ((typep condition 'end-of-file)
               "a form is missing a closing paren: the reader reached the end while still inside it")
              ((search "unmatched close parenthesis" text)
               "there is one closing paren too many here")
              (missing-package
               (if (probe-file (merge-pathnames (format nil "contrib/~(~A~).fasl" missing-package)
                                                (sb-int:sbcl-homedir-pathname)))
                   (format nil "~A is an SBCL contrib that isn't loaded yet: (require :~(~A~))"
                           missing-package missing-package)
                   (similar-names missing-package (mapcar #'package-name (list-all-packages)) "packages")))
              ((and missing-symbol home (find-package home))
               (let ((package (find-package home))
                     (names '())
                     (external '()))
                 (do-symbols (s package)
                   (when (eq (symbol-package s) package) (push (symbol-name s) names)))
                 (do-external-symbols (s package)
                   (push (symbol-name s) names)
                   (push (symbol-name s) external))
                 (let ((elsewhere (loop for p in (list-all-packages)
                                        for s = (find-symbol missing-symbol p)
                                        when (and s (not (eq (symbol-package s) package))
                                                  (or (fboundp s) (boundp s)))
                                          collect (let ((*package* (find-package :keyword)))
                                                    (prin1-to-string s)))))
                   (cond ((find-symbol missing-symbol package)
                          (format nil "It exists but isn't exported: ~(~A::~A~)" home missing-symbol))
                         (elsewhere
                          (format nil "It's defined in another package: ~(~{~A~^, ~}~)"
                                  (remove-duplicates elsewhere :test #'string=)))
                         (t (similar-names missing-symbol names (format nil "symbols in ~A" home) external))))))
              ((typep condition 'undefined-function)
               (let ((name (cell-error-name condition)))
                 (when (symbolp name)
                   (let ((*package* (find-package :lisping))
                         (others '()))
                     (dolist (p (list-all-packages))
                       (let ((s (find-symbol (symbol-name name) p)))
                         (when (and s (not (eq s name)) (fboundp s))
                           (pushnew (prin1-to-string s) others :test #'string=))))
                     (when others
                       (format nil "A function with this name exists as: ~(~{~A~^, ~}~)" others)))))))))))

(defun redefinition-warning-p (warning)
  "True for SBCL's warning that a defstruct changed its slots. Reloading a file
is meant to replace definitions, as in Emacs, so this isn't a problem; the old
instances become invalid, which is the price of changing a struct."
  (search "change in instance length of class" (princ-to-string warning)))

(defun load-file (path)
  "Load the Lisp file PATH (relative to *root*) form by form, like Emacs's load,
but a form that fails to read or evaluate is reported and skipped instead of
ending the load. Warnings other than style warnings count as problems too.
Returns this file's problems as strings naming file, line and error; they are
also added to *LOAD-PROBLEMS*, so nested loads are reported to the outer one."
  (let* ((file (merge-pathnames path *root*))
         (name (enough-namestring file *root*))
         (text (read-text file))
         (problems (progn (when text (note-file name text)) '()))
         (*package* (find-package :lisping)))
    (flet ((problem (start what condition)
             (when (member condition *reported-conditions* :test #'eq)
               (return-from problem))
             (push condition *reported-conditions*)
             (let ((message (format nil "~A:~D: ~A: ~A~@[ (Hint: ~A)~]" name
                                    (1+ (count #\Newline text :end (min start (length text))))
                                    what condition (ignore-errors (error-hint condition)))))
               (push message problems)
               (push message *load-problems*))))
      (if (null text)
          (let ((message (format nil "~A: file not found" name)))
            (push message problems)
            (push message *load-problems*))
          (with-input-from-string (in text)
            (loop
              (let* ((start (file-position in))
                     (form (handler-case (read in nil in)
                             (serious-condition (c)
                               (problem start "read error, form skipped" c)
                               (if (skip-to-next-toplevel-form in start) '(progn) in)))))
                (when (eq form in) (return))
                (handler-case
                    (let ((compile-errors '()))
                      ;; A form that fails to compile (say, a bad macro call inside a
                      ;; defun) still evaluates: SBCL only signals sb-c:compiler-error,
                      ;; which is neither an error nor a warning, and the function then
                      ;; fails when called. Count it as a problem.
                      (handler-bind ((warning (lambda (w)
                                                (unless (or (typep w 'style-warning)
                                                            (redefinition-warning-p w))
                                                  (problem start (form-label form) w))))
                                     (error (lambda (e)
                                              ;; Same for the error SBCL raises next: take its
                                              ;; restart that uses the new definition.
                                              (when (and (search "attempt to redefine the STRUCTURE-OBJECT class"
                                                                 (princ-to-string e))
                                                         (find-restart 'continue e))
                                                (invoke-restart 'continue))))
                                     (sb-c:compiler-error
                                       (lambda (c)
                                         (let ((inner (ignore-errors (sb-int:encapsulated-condition c))))
                                           (unless (member (princ-to-string (or inner c)) compile-errors
                                                           :test #'string=)
                                             (push (princ-to-string (or inner c)) compile-errors)
                                             (problem start (format nil "~A (compile error)" (form-label form))
                                                      (or inner c)))))))
                        (eval form)))
                  (serious-condition (c)
                    (problem start (form-label form) c))))))))
    (nreverse problems)))

(defun load-collecting (file out)
  "Load FILE with its output going to OUT; return every problem, nested loads included."
  (let ((*load-problems* '())
        (*reported-conditions* '())
        (*standard-output* out)
        (*error-output* out))
    (handler-case (sb-ext:with-timeout *eval-timeout* (load-file file))
      (sb-ext:timeout ()
        (push (format nil "~A: timed out after ~D seconds" file *eval-timeout*) *load-problems*)))
    (reverse *load-problems*)))

(defun eval-code (code)
  "Read and evaluate every form in CODE in the running image. Nothing is saved.
Return printed output, values and errors."
  (let ((out (make-string-output-stream))
        (backtrace nil))
    (handler-case
        (handler-bind ((serious-condition
                         (lambda (c)
                           (declare (ignore c))
                           (setf backtrace (user-backtrace)))))
          (sb-ext:with-timeout *eval-timeout*
            (let ((*package* (find-package :lisping))
                  (*standard-output* out)
                  (*error-output* out)
                  (*trace-output* out))
              (with-input-from-string (in code)
                (loop
                  (let ((form (read in nil in)))
                    (when (eq form in) (return))
                    (let ((values (multiple-value-list (eval form)))
                          (*print-length* 100)
                          (*print-level* 8)
                          (*print-circle* t))
                      (format out "~&=> ~{~S~^, ~}~%" (or values '(:no-values))))))))))
      (sb-ext:timeout ()
        (format out "~&ERROR: timed out after ~D seconds (see *eval-timeout*; run long-lived work in a thread)~%"
                *eval-timeout*))
      (serious-condition (c)
        (format out "~&ERROR (~S): ~A~%~@[Hint: ~A~%~]~@[Backtrace:~%~A~]"
                (type-of c) c (ignore-errors (error-hint c)) backtrace)))
    (limit-output (get-output-stream-string out))))

(defparameter +core-files+ '("core.lisp" "boot.lisp" "run.sh"))

(defun undefined-function-problems ()
  "After a full load: functions in LISPING that some code calls but nothing
defines. Each form loads on its own, so a forward reference only gives a style
warning at load time; checking once everything is loaded lets the references
that did resolve through, and catches the ones that never will."
  (let ((package (find-package :lisping))
        (found '()))
    (do-symbols (s package)
      (when (and (eq (symbol-package s) package)
                 ;; SBCL records an assumed type when it compiles a call to a
                 ;; function it doesn't know; a (declaim (ftype function f)) forward
                 ;; declaration instead marks it :declared. Defining it clears both.
                 (or (sb-int:info :function :assumed-type s)
                     (eq (sb-int:info :function :where-from s) :declared))
                 (not (fboundp s)))
        (push (format nil "~(~A~) is called but never defined" s) found)))
    (sort found #'string<)))

(defun configure-session ()
  "Set *home*, *session* and *cwd* from LISPER_HOME, LISPER_SESSION and LISPER_CWD
(else the session's saved cwd, else *root*).
Run by MAIN and by TEST-BOOT's fresh process, so files see the same values in both."
  (flet ((dir (d) (and d (plusp (length d))
                       (pathname (if (char= (char d (1- (length d))) #\/) d (concatenate 'string d "/"))))))
    (setf *home* (dir (or (sb-ext:posix-getenv "LISPER_HOME")
                          (namestring (merge-pathnames ".lisper/" (user-homedir-pathname)))))
          *session* (let ((id (sb-ext:posix-getenv "LISPER_SESSION")))
                      (if (session-name-p id) id "main"))
          *cwd* (dir (sb-ext:posix-getenv "LISPER_CWD")))
    ;; Never NIL, and already right while init.lisp loads: else the cwd this
    ;; session saved last time, else the harness.
    (unless *cwd*
      (let ((meta (ignore-errors (json-decode (read-text (session-path "meta.json"))))))
        (setf *cwd* (or (and (hash-table-p meta) (stringp (gethash "cwd" meta)) (dir (gethash "cwd" meta)))
                        *root*))))))

(defun print-boot-problems (&optional extra)
  "Used by TEST-BOOT in the child process: load init.lisp, then EXTRA (a file
name, so a file init.lisp doesn't load yet is checked too), and print the problems."
  (configure-session)
  (let* ((problems (let ((*booting* t) (*test-boot* t))
                    (append (load-collecting (init-path) (make-broadcast-stream))
                            (loop for file in (if (listp extra) extra (list extra))
                                  append (load-collecting file (make-broadcast-stream))))))
        ;; EXTRA is usually loaded by init.lisp too, so the same problem can come twice.
        (problems (append (remove-duplicates problems :test #'string= :from-end t)
                          (undefined-function-problems))))
    (format t "~&BOOT-PROBLEMS:~S~%" problems)
    (finish-output)))

(defun scratch-home ()
  "A temporary *HOME* for a test boot, deleted after it: copies of the home's
own files and of this session's data files (not its transcript, logs or lock).
Paths resolve as they do in the live image, but a file that writes at load time
writes the copies, never the live session."
  (let ((dir (namestring (home-path (format nil "tmp/home-~D-~D/" (get-universal-time) (random 1000000))))))
    (shell (format nil "D=~A; S=~A; mkdir -p \"$D/sessions/$S\" && cd ~A && for f in * .[!.]*; do case $f in tmp|sessions|.git|switch|*.log) ;; *) [ -e \"$f\" ] && cp -a \"$f\" \"$D/\" ;; esac; done; cd \"sessions/$S\" 2>/dev/null && for f in *; do case $f in lock|messages*|*.jsonl|*.log|queue.*|tmp) ;; *) [ -e \"$f\" ] && cp -a \"$f\" \"$D/sessions/$S/\" ;; esac; done; true"
                   (shell-quote dir) (shell-quote *session*) (shell-quote (namestring *home*))))
    dir))

(defun test-boot (&key (timeout 120) extra (directory *root*))
  "Load init.lisp in a fresh SBCL process, as a boot would but with no UI, no
agent and no commits, and return its problems as a list of strings (NIL when
clean). The running image can pass a check only because of state left over
from earlier; this shows whether the files work from scratch. DIRECTORY is the
project to boot (the working tree by default). It runs on a SCRATCH-HOME."
  (multiple-value-bind (out code)
      (let ((home (scratch-home)))
        (unwind-protect
             (shell (format nil "cd ~A && LISPER_HOME=~A LISPER_SESSION=~A LISPING_NO_TUI=1 LISPING_HEADLESS=1 timeout ~D ~A --core ~A --noinform --non-interactive --load core.lisp --eval ~A 2>&1"
                            (shell-quote (namestring directory))
                            (shell-quote home) (shell-quote *session*)
                            timeout
                            (shell-quote (namestring sb-ext:*runtime-pathname*))
                            (shell-quote (namestring sb-ext:*core-pathname*))
                            (shell-quote (format nil "(lisping-core::print-boot-problems '~S)" extra))))
          (remove-tree home)))
    (let ((mark (search "BOOT-PROBLEMS:" out)))
      (if mark
          (let ((*read-eval* nil))
            (read-from-string out t nil :start (+ mark (length "BOOT-PROBLEMS:"))))
          (list (format nil "the test boot didn't finish (exit ~A): ~A" code
                        (subseq out (max 0 (- (length out) 1500)))))))))

(defun write-files-and-load (files &key bases force)
  "Write several files as ONE change. FILES is a list of (path . code). All are
written, then a single fresh boot checks them together, so a change spanning
files (a rename, a moved definition) never has to pass through a broken
intermediate state. On any problem every file is put back and the running image
isn't touched; otherwise the .lisp files are loaded into the image and
everything is committed together. BASES, an alist of (path . text the new code
was made from), and FORCE work as BASE and FORCE do for WRITE-FILE-AND-LOAD."
  (let* ((entries (loop for (path . code) in files
                        for file = (merge-pathnames path *root*)
                        collect (list (enough-namestring file *root*) file code (read-text file)
                                      (cdr (assoc path bases :test #'equal)))))
         (core (find-if (lambda (e) (member (first e) +core-files+ :test #'string=)) entries)))
    (when core
      (return-from write-files-and-load
        (format nil "~A is part of the core and can't be written from here." (first core))))
    (let ((stale (loop for (name nil nil old base) in entries
                       for refusal = (stale-write name old base force)
                       when refusal collect refusal)))
      (when stale
        (return-from write-files-and-load
          (format nil "~{~A~%~}Nothing was written." stale))))
    (flet ((restore ()
             (dolist (e entries)
               (destructuring-bind (name file code old base) e
                 (declare (ignore name code base))
                 (if old (write-text file old) (ignore-errors (delete-file file)))))))
      (handler-case (dolist (e entries) (write-text (second e) (third e)))
        (error (c) (restore) (return-from write-files-and-load (format nil "NOT SAVED: ~A" c))))
      (let* ((lisp (remove-if-not (lambda (e) (equalp (pathname-type (second e)) "lisp")) entries))
             (tree nil)
             (fresh (and lisp (multiple-value-bind (problems hash)
                                  (checked-test-boot (mapcar #'first entries) (mapcar #'first lisp))
                                (setf tree hash)
                                problems))))
        (when fresh
          (restore)
          (return-from write-files-and-load
            (format nil "NOT SAVED. In a fresh boot with these ~D files, loading reported:~%~{- ~A~%~}Every file was put back; the running image was not touched."
                    (length entries) fresh)))
        (dolist (e entries) (note-file (first e) (third e)))
        (let ((out (make-string-output-stream)) (problems '()) (failure nil))
          (dolist (e lisp)
            (setf problems (append problems (load-collecting (second e) out))))
          (git (format nil "git add -- ~{~A ~}" (mapcar (lambda (e) (shell-quote (first e))) entries)))
          (multiple-value-bind (gout code)
              (git (format nil "~Agit commit -q -m ~A -- ~{~A ~}"
                           (if tree (format nil "LISPER_CHECKED_TREE=~A " tree) "")
                           (shell-quote (format nil "update ~{~A~^, ~}" (mapcar #'first entries)))
                           (mapcar (lambda (e) (shell-quote (first e))) entries)))
            (unless (or (eql code 0) (search "nothing to commit" gout))
              (setf failure (string-trim '(#\Space #\Newline) gout))))
          (format nil "Wrote~:[~; and loaded~] ~{~A~^, ~}; ~:[committed together~;~:*~A~].~@[~%Loading into the running image reported (the files are kept: they boot cleanly):~%~{- ~A~%~}~]"
                  lisp (mapcar #'first entries) (and failure (commit-note failure)) problems))))))

(defvar *file-seen* (make-hash-table :test 'equal)
  "File name (relative to *root*) -> the text this image last loaded, wrote or
was refused over. Whole-file writes compare the disk against it, so a session
can't silently overwrite a change another session made after it last looked.")

(defun note-file (name text) (setf (gethash name *file-seen*) text))

(defun stale-write (name disk base force)
  "When writing NAME would overwrite a change this image hasn't seen, return a
refusal message. BASE is the text the new code was made from, when the caller
knows it; FORCE overwrites anyway. A plain retry is refused again: getting past
this takes a BASE matching the disk (it was re-read) or a deliberate FORCE."
  (let ((seen (if base (list base t) (multiple-value-list (gethash name *file-seen*)))))
    (when (and disk (not force) (second seen) (string/= disk (first seen)))
      (format nil "NOT SAVED: ~A on disk ~:[changed since this image last loaded or wrote it~;isn't the text this edit was made from~] (another session or a shell command wrote it). Read it again (with read-file, which counts as seeing it, or pass the text you read as base) and redo your change on the current text. Retrying the same write is refused again; only force overwrites their change." name base))))

(defun write-file-and-load (path code &key base force)
  "Write CODE verbatim to PATH, relative to *root*. A .lisp file is first checked
in a fresh boot (TEST-BOOT); only if that's clean is it loaded into the running
image and committed, like saving a buffer and running eval-buffer in Emacs.
If the fresh boot reports problems, the previous version is put back and the
running image isn't touched. Problems that show up only when loading into the
running image come from state in that image, not the file, so the file is kept
(as WRITE-FILES-AND-LOAD does) and they're reported.
Like Emacs's \"file changed on disk\", the write is refused once if the file
changed since this image last saw it; BASE, the text CODE was edited from, makes
that check exact (edit tools should pass it), and FORCE overrides it."
  (let* ((file (merge-pathnames path *root*))
         (name (enough-namestring file *root*))
         (old (read-text file)))
    (cond
      ((member name +core-files+ :test #'string=)
       (format nil "~A is part of the core and can't be written from here." name))
      ((stale-write name old base force))
      ((not (equalp (pathname-type file) "lisp"))
       (write-text file code)
       (note-file name code)
       (format nil "Wrote ~A (~D characters); ~A." name (length code)
               (commit-note (commit (format nil "~:[add~;update~] ~A" old name) name))))
      (t
       (write-text file code)
       (multiple-value-bind (fresh tree) (checked-test-boot (list name) name)
         (when fresh
           (if old (write-text file old) (ignore-errors (delete-file file)))
           (return-from write-file-and-load
             (format nil "NOT SAVED. In a fresh boot with this version of ~A, loading reported:~%~{- ~A~%~}~:[The file was new, so it was removed.~;The previous version was put back.~] The running image was not touched."
                     name fresh old)))
         (note-file name code)
       (let* ((out (make-string-output-stream))
              (problems (load-collecting file out))
              (output (limit-output (get-output-stream-string out)))
              (init (read-text (init-path)))
              (failure (commit (format nil "~:[add~;update~] ~A" old name) name tree)))
         (format nil "~@[~A~%~]Wrote and loaded ~A; ~A.~@[~%~A~]~@[~%Loading into the running image reported (the file is kept, since it boots cleanly; a restart gives you exactly the files):~%~{- ~A~%~}~]"
                 (and (string/= output "") output) name (commit-note failure)
                 (unless (or (string= name "init.lisp") (and init (search (pathname-name file) init)))
                   (format nil "Note: init.lisp doesn't mention ~A, so it won't be loaded at boot." name))
                 problems)))))))

(defun read-file (path)
  "The text of PATH (relative to *root*), or NIL if it doesn't exist. Reading
through this counts as having seen the file: a whole-file write after it isn't
refused as stale."
  (let* ((file (merge-pathnames path *root*))
         (text (read-text file)))
    (when text (note-file (enough-namestring file *root*) text))
    text))

(register-tool "eval"
               "Evaluate Common Lisp in your own running SBCL image (package LISPING), like M-: in Emacs: it changes the running image only. With `file`, the code is instead written verbatim to that file and, for a .lisp file, loaded, like saving a buffer and eval-buffer; it is kept and committed only if it loads without errors or warnings."
               (obj :type "object"
                    :properties (obj :code (obj :type "string" :description "One or more Lisp forms; with file, the complete new contents of the file.")
                                     :file (obj :type "string" :description "Optional. A path relative to the project directory, e.g. init.lisp or lisp/ui.lisp.")
                                     :base (obj :type "string" :description "Optional, with file. The text of the file your new code was made from (as you last read it). If the file on disk still matches it, the write goes through even after another session changed the file.")
                                     :force (obj :type "boolean" :description "Optional, with file. Write even though the file changed on disk since your image last loaded or wrote it, replacing that change (another session's, or a shell command's). Only after you've looked at what it changed."))
                    :required #("code"))
               (lambda (args)
                 (let ((file (arg args "file")))
                   (if (and (stringp file) (string/= file ""))
                       (write-file-and-load file (arg args "code" "")
                                            :base (let ((base (arg args "base"))) (and (stringp base) base))
                                            :force (eq (arg args "force") t))
                       (eval-code (arg args "code" ""))))))

;;; ------------------------------------------------------------------
;;; Conversation and I/O

(defvar *inbox* (sb-concurrency:make-mailbox :name "inbox")
  "User messages (strings) waiting for the agent. Any UI may push here.")
(defvar *messages* '() "The conversation sent to the model, oldest first, as OBJs.")
(defvar *before-request* '()
  "Functions called with the request body (an OBJ) before each model call; each returns the body to use.")
(defvar *session-log* nil)
(defvar *previous-session* nil
  "Transcript (JSONL, one message per line) of the session before this boot, or NIL.")
(defvar *trace-file* nil "Human-readable log of every event; <session dir>/trace.log points at the current one.")
(defvar *trace-lock* (sb-thread:make-mutex :name "trace"))

(defun trace-event (event)
  "Append EVENT to the trace file. Written by the core, whatever *EMIT* does."
  (when *trace-file*
    (ignore-errors
     (sb-thread:with-mutex (*trace-lock*)
       (with-open-file (out *trace-file* :direction :output :if-exists :append
                                         :if-does-not-exist :create :external-format :utf-8)
         (multiple-value-bind (sec min hour) (get-decoded-time)
           (format out "~&~%~2,'0D:~2,'0D:~2,'0D ~:@(~A~)~@[ ~A~]~%" hour min sec
                   (getf event :type) (getf event :name)))
         (let ((args (getf event :args))
               (text (getf event :text)))
           (cond ((and (hash-table-p args) (stringp (gethash "code" args)))
                  (write-line (gethash "code" args) out))
                 (args (write-line (json-encode args) out)))
           (when text (write-line text out))))))))

(defun send-user-message (text &key kind)
  "Queue TEXT as a message from the user. Blank text is ignored: the model API
rejects an empty user message, and one in *messages* would fail every request.
KIND marks a message the system wrote rather than the user (\"boot\", \"core\", ...);
it's kept as the message's x-kind and never sent to the model API."
  (when (and (stringp text) (string/= "" (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (sb-concurrency:send-message *inbox* (if kind (cons text kind) text))
    ;; Right away, so a UI can show the message is in; :user follows when the
    ;; agent takes it into the conversation, before its next model call. It's
    ;; emitted from a thread of its own: the caller may be a UI thread holding
    ;; its own lock, and *emit* calling back into that UI would deadlock or
    ;; hit a recursive lock.
    (let ((shown (if (char= (char text 0) #\!) (string-left-trim " " (subseq text 1)) text)))
      ;; Shown as it will arrive: an urgent message's ! is stripped before :user too.
      (sb-thread:make-thread (lambda () (emit :queued :text shown)) :name "emit-queued"))
    t))

(defun fallback-emit (event)
  "Plain terminal output, used only when *EMIT* itself fails."
  (format t "~&[~(~A~)] ~A~%" (getf event :type) (or (getf event :text) event))
  (finish-output))

;;; The terminal display lives in the unlocked LISPING package, so the AI can
;;; redefine it like any of its own functions.
(defun lisping::terminal-emit (event)
  "Print EVENT, a plist with :type and fields, to the terminal."
  (let ((type (getf event :type))
        (text (getf event :text)))
    (case type
      (:assistant (format t "~&~%~A~%" text))
      (:thinking (format t "~&~C[2m~A~C[0m~%" #\Esc text #\Esc))
      (:reasoning nil)                  ; long; it is in the session's trace.log
      (:user nil)                       ; the user typed it; no need to echo
      (:queued nil)
      (:idle nil)
      (:tool-call
       (let ((args (getf event :args)))
         (format t "~&~%[~A]~%~A~%" (getf event :name)
                 (if (and (hash-table-p args) (stringp (gethash "code" args)))
                     (gethash "code" args)
                     (json-encode args)))))
      (:tool-result
       (format t "~&~A~%" (if (> (length text) 1500)
                              (format nil "~A~%... [~D characters]" (subseq text 0 1500) (length text))
                              text)))
      (t (format t "~&[~(~A~)] ~A~%" type (or text event)))))
  (finish-output))

(defvar *emit* 'lisping::terminal-emit
  "Function called with every event (a plist with :type). Replace it to drive your own UI.")

(defvar *observers* '()
  "Functions called with every event, before *EMIT*. The UI owns *EMIT* and may
replace it at any time; anything else that wants to see events (a compactor, a
logger) goes here instead of wrapping *EMIT*. A failing observer is reported and skipped.")

(defun emit (type &rest fields)
  "Send an event to *OBSERVERS* and *EMIT*. Falls back to plain terminal output if *EMIT* fails."
  (let ((event (list* :type type fields)))
    (trace-event event)
    (dolist (observer *observers*)
      (handler-case (funcall observer event)
        (serious-condition (c)
          (fallback-emit (list :type :error :text (format nil "observer ~S failed: ~A" observer c))))))
    (handler-case (funcall *emit* event)
      (serious-condition (c)
        (fallback-emit event)
        (fallback-emit (list :type :error :text (format nil "*emit* failed: ~A" c)))))))

(defun add-message (message)
  (setf *messages* (append *messages* (list message)))
  (when (equal (gethash "role" message) "user")
    (emit :user :text (gethash "content" message)))
  (when *session-log*
    (with-open-file (out *session-log* :direction :output :if-exists :append
                                       :if-does-not-exist :create :external-format :utf-8)
      (json-encode message out)
      (terpri out))))

(defvar *system-prompt*
  "You are lisping-ai: a running SBCL image that builds its own harness, the way Emacs is built out of Emacs Lisp.

You start with almost nothing. You have one tool, `eval`, which evaluates Common Lisp in your own live image (package LISPING). Through it you can do anything: define functions, read and write files, run programs with (shell \"command\"), open sockets, start threads. Redefining a function takes effect immediately.

Core API (package LISPING-CORE, already used by LISPING):
- (register-tool name description schema fn): expose a new tool to yourself. schema is a JSON Schema as a JSON string or an (obj ...). fn receives a hash table of arguments, read it with (arg args \"key\"). Pass a symbol as fn so redefining the function updates the tool. (unregister-tool name) removes one.
- (obj :key value ...) builds a JSON object; json-encode, json-decode, http-post-json.
- (shell \"command\") runs sh in *cwd* (the session's working directory; pass :directory *root* for the harness), returns (values output exit-code). Its commands get no standard input, so they can't steal the user's keystrokes; that also means tty, stty and friends report \"not a tty\". The terminal is /dev/tty: use stty -F /dev/tty, or redirect from it.
- *system-prompt*: this text. A string, or a function returning one. You may rewrite it.
- *messages*: the conversation history sent to the model (a list of objs). You may trim or summarize it to manage your context. *last-usage* is the token usage of the last response; its prompt_tokens is how big your context is (it's also shown in each :thinking event).
- *before-request*: functions called with each request body before it is sent; each returns the body to use.
- *emit*: function receiving every event as a plist (:type :user/:assistant/:thinking/:reasoning/:tool-call/:tool-result/:error/:info, plus :text, :name, :args; :user is each message from the user, however it arrived, when it enters the conversation; :queued is sent the moment a message arrives, which can be long before, while you are mid-turn). It defaults to terminal-emit, your terminal display: an ordinary function in package LISPING that you may redefine to change how the terminal looks.
- *observers*: functions called with every event, before *emit*. The UI owns *emit*; anything else that needs to see events (your compactor, a logger) belongs in *observers*, so a UI restart can't drop it.
- Messages the system writes (boot, core notes, stubs for interrupted tool calls) carry an \"x-kind\" key, which is stripped before the request; (synthetic-message-p m) tells them apart from what the user or you wrote. Tag synthetic messages you create yourself the same way, rather than marking them with a text prefix. Any key starting with \"x-\" is stripped the same way, so you can keep your own bookkeeping on a message (an \"x-id\", say): it stays in *messages*, is saved with the session and survives restarts, but the model never sees it.
- Delivery: a message that arrives while you're mid-turn waits until the turn is over, then messages are delivered one per turn (*held* holds the waiting ones; a UI lists them with (queued-messages), pins one while the user edits it with (pin-queued text) so it isn't delivered mid-edit, then saves with (edit-queued text new-text), NIL deleting, or cancels with (release-queued text); any time, mid-turn included). A message starting with ! is urgent and goes in before your next model call. (setf *deliver-mid-turn* t) delivers everything mid-turn, as before. The :queued event still fires the moment a message arrives, so a UI can show it right away. Activity for a UI indicator: :user starts a turn, :thinking marks each model call, :tool-call/:tool-result bracket a tool run, a failed model call that will be retried is an :error with :retry-in seconds and :attempt, and :idle (with :held, the count of waiting messages) means the turn is over and the agent waits for the user.
- *inbox* and (send-user-message text): user messages arrive here. A UI you build should push user input with send-user-message and show events through *emit*.
- The terminal: (stop-terminal-input) stops the thread that reads lines from the terminal into *inbox* (*terminal-thread*), so you can take the terminal over, e.g. raw keys and full-screen drawing. (start-terminal-input) starts it again. Both, and terminal-input-loop, are yours to redefine.
- The queue: text appended to the session's queue.txt arrives as user messages, several separated by a line containing only ---. It's how the user (or another program) reaches you without the terminal, e.g. when your UI owns it. queue-input-loop, take-queue, start-queue-input and stop-queue-input are yours to redefine.
- *complete*: the function that calls the model, given the request body; it returns the assistant message. Replace it to stream or call the model differently; chat-url and api-headers give you the endpoint and auth. If yours fails, the core falls back to default-complete.
- *model*, *max-tokens*, *eval-timeout*, *output-limit*, *root* (the project directory).

Files and persistence, the way Emacs does it:
- At boot the core loads init.lisp, like Emacs loads init.el. init.lisp is yours: from it, load the other files you write with (load-file \"lisp/ui.lisp\"). load-file reports a form that fails and goes on with the rest, like a forgiving load.
- eval with only `code` is like M-: or *scratch*: it changes the running image and nothing else. Use it to explore and test.
- eval with `file` is like saving a buffer and running eval-buffer: the code is written verbatim to the file and loaded. Before your running image sees it, a fresh SBCL boots with the new file in place (init.lisp plus this file); only if that's clean is it loaded into your image. It is kept and committed only if it loads with no errors and no warnings; otherwise the old version is put back. So your files only ever contain code that loaded, and what loads at boot is what you ran. Non-.lisp files are just written, e.g. HTML or notes.
- Every git commit is boot-checked by a pre-commit hook, however it's made (your own shell git commit too): a commit whose files don't boot cleanly is refused. Prefer eval's file argument and edit_file; they check before writing, not just before committing.
- (write-files-and-load '((\"lisp/a.lisp\" . \"...\") (\"lisp/b.lisp\" . \"...\"))) writes several files as ONE change: one fresh boot checks them together, so a change spanning files (a rename, a moved definition) needn't pass through broken intermediate states. All or nothing; committed together. Your edit tools can build on it.
- (test-boot) loads init.lisp in a fresh SBCL process, like a boot but with no UI and no agent, and returns its problems (NIL when clean). Your running image can pass a check only because of state left from earlier edits (a variable defined in a file that loads later, say); run test-boot after changing files and before restarting.
- *test-boot* is true inside test-boot's fresh process; check it before any load-time side effect.
- Loading a file should only define things. Side effects (starting threads, writing files, opening ports) belong in an explicit start function that init.lisp calls, because files are also loaded by test-boot's fresh check, with the same *home*.
- So, to keep something, put it in a file that init.lisp loads. The image and the files can drift apart while you work, as in Emacs; at boot, the files win.
- Writing a whole file to change one function gets slow as files grow. Keep files small and focused, and consider building yourself what Emacs users have: buffers, editing part of a file, and re-evaluating just the form you changed (C-M-x). Tools you register get their arguments as plain JSON strings, so a file-editing tool avoids escaping code inside Lisp strings.
- Your conversation is resumed after a restart (see Sessions), but write down what you learn anyway, e.g. in (home-path \"notes.md\"): a fresh session, or another session, won't have it. *previous-session* is the transcript of the session before this boot (JSONL), if you want to look back.
- State lives outside the code repo, in *home* (~/.lisper/ by default): (home-path \"notes.md\") for your own data files, (session-path \"x\") for this session's files. *home* is its own git repo, committed after every turn (transcripts and logs excluded), so a bad rewrite of a data file can be undone with git there. The code repo (*root*) holds only code. Whole-file writes (eval with file, write-file(s)-and-load) are refused if the file changed on disk since your image last loaded or wrote it, as another session may have edited it: re-read it with (read-file path), which counts as seeing it, and redo your change. Edit tools pass :base, the text they edited, which makes the check exact; force (:force t) overwrites deliberately.
- Sessions: one process is one session (*session*, from LISPER_SESSION; default \"main\"). After every turn the core saves the conversation (as compacted) and *cwd*; the next boot of the same session resumes it, so a restart no longer loses the conversation. (session-forget) makes the next boot start fresh; LISPER_FRESH=1 does too. Several sessions can run at once, each in its own process and terminal, sharing the code and git.
- *cwd* is the session's working directory (LISPER_CWD): shell runs there by default, for work on a project outside the harness. Your own code stays in *root*.
- (restart-image \"message\") restarts the image; the message is sent to you after boot. Use it to check that what you built comes back.
- A supervisor restarts the image if it crashes, and if it crashes while booting, rolls your files back to the last commit that booted.

Why Lisp, and how to work in it:
- You are a live image, like Emacs or a Smalltalk system. The running image is the program, and you change it while it runs: evaluate one small definition, call it right away to test it, redefine it when it's wrong. The files are how the image comes back after a restart.
- Prefer many small functions over big ones. A ten-line defun with a mistake is cheap to fix; a sixty-line one with an unbalanced paren is not. Define and test in the same eval.
- Code is data. (read-from-string code) shows you how text will be read before you run it, which catches misplaced parens. You can hold a definition as a list, change it with list operations, and evaluate the result instead of patching text. Reading a file with read gives you its definitions as data.
- Ask the image instead of guessing: (apropos \"ALIEN\" :sb-alien), (describe 'x), (documentation 'f 'function), (function-lambda-expression #'f). Treat compiler warnings as errors: code that evaluated with warnings is usually not what you meant.
- An error doesn't hurt the image. Read it, fix the one form, try again.

Rules:
- The core (package LISPING-CORE) is locked. Use setf for its variables; don't redefine its functions.
- eval calls time out after *eval-timeout* seconds. Run servers and other long-lived work in threads (sb-thread:make-thread).
- Build in small steps and check each one. When something breaks, read the error and fix it.
- Everything you do (your reasoning, tool calls and results) is written to the session's trace.log for the user to read. The core writes it; it is not yours to change.
- Never print, save or send the API key.
- Talk to the user briefly. They are curious to watch you grow; tell them what you built and how to use it.")

(defun system-prompt-text ()
  (if (stringp *system-prompt*) *system-prompt* (funcall *system-prompt*)))

(defun tool-specs ()
  (coerce (loop for tool being the hash-values of *tools*
                collect (obj :type "function"
                             :function (obj :name (tool-name tool)
                                            :description (tool-description tool)
                                            :parameters (tool-schema tool))))
          'vector))

(defvar *last-usage* nil
  "The usage object of the last model response (prompt_tokens, completion_tokens,
total_tokens), as the API returned it; prompt_tokens is the size of your context.")

(defun chat-url () (format nil "~A/chat/completions" *api-base*))

(defun api-headers ()
  "HTTP headers for the model API, including the key. Never print them."
  (list (format nil "Authorization: Bearer ~A" *api-key*)))

(defun blank-user-message-p (message)
  (and (equal (gethash "role" message) "user")
       (let ((content (gethash "content" message)))
         (or (null content) (eq content :null)
             (and (stringp content)
                  (string= "" (string-trim '(#\Space #\Tab #\Newline #\Return) content)))))))

(defun pair-tool-results (messages)
  "MESSAGES with every tool call answered and every tool result matched: a call
whose result never got appended (the turn was interrupted) gets a stub result,
and a result with no matching call is dropped. The API rejects either one,
and would keep rejecting every later request."
  (let ((out '()) (pending '()))
    (flet ((flush ()
             (dolist (id (reverse pending))
               (push (obj :role "tool" :tool_call_id id
                          :content "[core] No result: this call was interrupted before it finished."
                          "x-kind" "stub")
                     out))
             (setf pending '())))
      (dolist (m messages)
        (let ((role (gethash "role" m)))
          (cond ((equal role "tool")
                 (let ((id (gethash "tool_call_id" m)))
                   (when (member id pending :test #'equal)
                     (setf pending (remove id pending :test #'equal))
                     (push m out))))
                (t
                 (flush)
                 (push m out)
                 (let ((calls (gethash "tool_calls" m)))
                   (when (and (equal role "assistant") (vectorp calls))
                     (loop for call across calls
                           do (push (gethash "id" call) pending))))))))
      (flush))
    (nreverse out)))

(defun request-body ()
  "The request for the model. Blank user messages are left out and tool calls
are paired with results, however *messages* got that way, since the API rejects
both and would fail every later request."
  ;; Only the agent thread may call this (a UI wanting a token estimate must
  ;; not): a stub added while a tool runs would be followed by the real result.
  ;; The pairing repairs *messages* itself, once: a stub made fresh for every
  ;; request is a new object each time, so anything that marks messages (entry
  ;; ids) changes the request there, and the API's prompt cache stops matching.
  (let ((paired (pair-tool-results *messages*)))
    (unless (and (= (length paired) (length *messages*)) (every #'eq paired *messages*))
      (setf *messages* paired)))
  (let ((body (obj :model *model*
                   :max_tokens *max-tokens*
                   :messages (coerce (cons (obj :role "system" :content (system-prompt-text))
                                           (pair-tool-results
                                            (remove-if #'blank-user-message-p *messages*)))
                                     'vector)
                   :tools (tool-specs))))
    ;; A hook that fails is skipped for this request, not allowed to fail it:
    ;; a hook that errors on every request would otherwise lock the agent out of
    ;; the model, with no turn left in which to fix the hook.
    (dolist (hook *before-request*)
      (let ((result (handler-case (funcall hook body)
                      (serious-condition (c)
                        (emit :error :text (format nil "*before-request* hook ~S failed and was skipped for this request: ~A" hook c))
                        body))))
        (setf body (if (hash-table-p result) result body))))
    ;; Pair again after the hooks, in case one of them broke it, and strip the
    ;; x-kind provenance key, which isn't part of the API.
    (let ((messages (gethash "messages" body)))
      (when (vectorp messages)
        (setf (gethash "messages" body)
              (map 'vector #'without-x-kind (pair-tool-results (coerce messages 'list))))))
    body))

(defun x-key-p (key)
  (and (stringp key) (> (length key) 2) (string= "x-" key :end2 2)))

(defun without-x-kind (message)
  "MESSAGE, or a copy of it without its x- keys (x-kind and any of your own,
like an x-id): they're kept in *messages* and saved with the session, but never
sent to the model."
  (if (loop for k being the hash-keys of message thereis (x-key-p k))
      (let ((copy (make-hash-table :test #'equal)))
        (maphash (lambda (k v) (unless (x-key-p k) (setf (gethash k copy) v))) message)
        copy)
      message))

(defun default-complete (body)
  "POST BODY to the model API, retrying failures; return the assistant message."
  (loop for attempt from 1
        do (handler-case
               (let ((response (http-post-json (chat-url) body :headers (api-headers))))
                 (let ((usage (gethash "usage" response)))
                   (when (hash-table-p usage) (setf *last-usage* usage)))
                 (return (gethash "message" (aref (gethash "choices" response) 0))))
             (error (c)
               ;; A 4xx other than 429 is the request's fault: retrying the same
               ;; request can only fail again. A 429 (rate or quota limit) only
               ;; needs time, possibly hours for a quota window, so it's retried
               ;; until it passes, backing off to every 5 minutes.
               (let ((limited (and (typep c 'http-error) (eql (http-error-status c) 429))))
                 (when (or (client-error-p c) (and (not limited) (>= attempt 4))) (error c))
                 (let ((wait (if limited (min 300 (expt 2 (min attempt 9))) (* 2 attempt))))
                   (emit :error :text (format nil "model call failed (attempt ~D, retrying in ~Ds): ~A" attempt wait c)
                                :retry-in wait :attempt attempt)
                   (sleep wait)))))))

(defun client-error-p (condition)
  "A 4xx other than 429: the request itself is wrong, so retrying it can't help."
  (and (typep condition 'http-error)
       (integerp (http-error-status condition))
       (<= 400 (http-error-status condition) 499)
       (/= 429 (http-error-status condition))))

(defparameter +context-overflow-phrases+
  '("context length" "context_length" "context window" "maximum context"
    "prompt is too long" "input is too long" "too many tokens" "reduce the length"
    "token limit" "max_tokens")
  "How providers word an over-long request: the prompt plus max_tokens is past the
window, or max_tokens alone is too large. Single words like \"maximum\" are avoided:
they appear in unrelated 400s. Some providers (Kimi's coding endpoint, as tested)
don't reject at all and truncate silently, so watch *last-usage* too.")

(defun context-too-long-p (condition)
  "True when the API rejected the request for being longer than the model's context."
  (and (client-error-p condition)
       (let ((body (string-downcase (princ-to-string (http-error-body condition)))))
         (some (lambda (phrase) (search phrase body)) +context-overflow-phrases+))))

(defun string-prefix-p (prefix string)
  (and (>= (length string) (length prefix)) (string= prefix string :end2 (length prefix))))

(defun drop-oldest-messages ()
  "Make *messages* smaller after a context-length error: drop about the oldest
quarter, cutting only at a user message so no tool call loses its result, and
leave a note saying what went. Returns the number dropped (0 if nothing could be)."
  (let* ((n (length *messages*))
         (cut (max 1 (floor n 4)))
         (rest (nthcdr cut *messages*))
         ;; The latest real user message is what the current task is about; it
         ;; must survive even when it sits in the part being dropped.
         (task (find-if (lambda (m)
                          (and (equal (gethash "role" m) "user")
                               (stringp (gethash "content" m))
                               (not (synthetic-message-p m))
                               (not (string-prefix-p "[core]" (gethash "content" m)))
                               (not (string-prefix-p "[boot]" (gethash "content" m)))))
                        *messages* :from-end t)))
    ;; Cut where a complete unit starts: a user message, or an assistant message
    ;; (its tool results follow it). An agentic task can run hundreds of tool
    ;; calls with no user message in between, so user messages alone are too rare.
    (loop while (and rest (not (member (gethash "role" (car rest)) '("user" "assistant") :test #'equal)))
          do (setf rest (cdr rest)) (incf cut))
    (cond
      (rest
       (setf *messages*
             (append (list (obj :role "user" "x-kind" "core"
                                :content (format nil "[core] ~D earlier messages were dropped because the context was too long for the model. The full transcript is in ~A.~@[ The message that started the current task is kept below.~]"
                                                 cut (and *session-log* (namestring *session-log*))
                                                 (and task (not (member task rest :test #'eq))))))
                     (and task (not (member task rest :test #'eq)) (list task))
                     rest))
       cut)
      (t
       ;; One long turn with no user message to cut at: shorten big tool results instead.
       (let ((shortened 0))
         (dolist (m *messages* shortened)
           (let ((content (gethash "content" m)))
             (when (and (equal (gethash "role" m) "tool") (stringp content) (> (length content) 2000)
                        (not (search "[core] cut from" content)))
               (setf (gethash "content" m)
                     (format nil "~A~%[core] cut from ~D characters: the context was too long."
                             (subseq content 0 2000) (length content)))
               (incf shortened)))))))))

(defvar *complete* 'default-complete
  "Function taking a request body (an OBJ) and returning the assistant message as an OBJ
with content, tool_calls and reasoning_content. Replace it to stream or change how
the model is called; if it fails, the core falls back to DEFAULT-COMPLETE.")

(defun complete ()
  "Send the conversation to the model and return its message. If the API says
the conversation is longer than the model's context, drop the oldest part and
try again, so the session can't get stuck with a history that never fits."
  (let ((previous nil) (*max-tokens* *max-tokens*))
    (loop for tries from 1
          do (handler-case (return (complete-once))
               (error (c)
                 (unless (and (context-too-long-p c) (< tries 6))
                   (error c))
                 (let ((text (princ-to-string (http-error-body c))))
                   ;; A real overflow error usually names token counts, which change
                   ;; as the request shrinks; the same body again means it's not length.
                   (when (equal text previous) (error c))
                   (setf previous text))
                 (cond
                   ;; First room for the reply: overflow is prompt + max_tokens.
                   ((> *max-tokens* 4096)
                    (setf *max-tokens* (max 4096 (floor *max-tokens* 2)))
                    (emit :error :text (format nil "context too long for the model: retrying with max_tokens ~D" *max-tokens*)))
                   (t
                    (let ((dropped (drop-oldest-messages)))
                      (when (zerop dropped) (error c))
                      (emit :error :text (format nil "context too long for the model: dropped ~D old messages and retrying" dropped))))))))))

(defun note-request (body)
  "Keep this request as tmp/request-last.json and the one before as
tmp/request-prev.json in *HOME*. Where they first differ is where the API's
prompt cache stopped matching: everything after it was paid for fresh. Only
with LISPER_DUMP_REQUESTS set: each write is the whole request, ~0.5 MB at 100k tokens."
  (when (sb-ext:posix-getenv "LISPER_DUMP_REQUESTS")
   (ignore-errors
    (let ((last (home-path "tmp/request-last.json")))
      (when (probe-file last) (rename-file last (home-path "tmp/request-prev.json")))
      (with-open-file (out last :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string (json-encode body) out))))))

(defun complete-once ()
  (let ((body (request-body)))
    (note-request body)
    (if (eq *complete* 'default-complete)
        (default-complete body)
        (let ((message (handler-case (funcall *complete* body)
                         (serious-condition (c)
                           (emit :error :text (format nil "*complete* failed, using default-complete: ~A" c))
                           nil))))
          (if (hash-table-p message) message (default-complete body))))))

(defun run-tool-call (call)
  (let* ((function (gethash "function" call))
         (name (gethash "name" function))
         (tool (gethash name *tools*))
         (result (handler-case
                     (let* ((raw (gethash "arguments" function))
                            (args (if (and (stringp raw) (plusp (length raw))) (json-decode raw) (obj))))
                       (emit :tool-call :name name :args args)
                       (if tool
                           ;; Tool functions run in the AI's package, as eval does, so a
                           ;; READ inside a tool interns symbols where its own code lives.
                           (let ((value (let ((*package* (find-package :lisping)))
                                          (funcall (tool-fn tool) args))))
                             (limit-output (if (stringp value) value (prin1-to-string value))))
                           (format nil "Unknown tool ~S. Registered tools: ~{~A~^, ~}" name
                                   (loop for k being the hash-keys of *tools* collect k))))
                   (serious-condition (c)
                     (format nil "ERROR (~S): ~A" (type-of c) c)))))
    (emit :tool-result :name name :text result)
    (obj :role "tool" :tool_call_id (gethash "id" call) :content result)))

(defun synthetic-message-p (message)
  "True for a message the system wrote (x-kind set), not the user or the model."
  (gethash "x-kind" message))

(defun inbox-message (item)
  "A user message for an *INBOX* item: a string, or (text . kind)."
  (if (consp item)
      (let ((m (obj :role "user" :content (car item))))
        (setf (gethash "x-kind" m) (cdr item))
        m)
      (obj :role "user" :content item)))

(defvar *held* '()
  "Messages that arrived mid-turn and wait for the turn to end, oldest first.")

(defvar *deliver-mid-turn* nil
  "When NIL (the default), messages arriving mid-turn wait until the turn is over
and are then delivered one per turn; only urgent ones (text starting with !) go
in before the next model call. When true, every message goes in mid-turn.")

(defun urgent-item-p (item)
  (let ((text (if (consp item) (car item) item)))
    (and (stringp text) (plusp (length text)) (char= (char text 0) #\!))))

(defun strip-urgent (item)
  "ITEM without the leading ! that marked it urgent."
  (flet ((strip (text) (string-left-trim " " (subseq text 1))))
    (if (consp item) (cons (strip (car item)) (cdr item)) (strip item))))

(defvar *held-lock* (sb-thread:make-mutex :name "held")
  "Guards *HELD*: the agent thread takes from it while a UI thread edits it.")

(defun drain-inbox ()
  "Move whatever has arrived in *INBOX* onto the end of *HELD*. Call with *HELD-LOCK* held."
  (loop for item = (sb-concurrency:receive-message-no-hang *inbox*)
        while item do (setf *held* (append *held* (list item)))))

(defun item-text (item) (if (consp item) (car item) item))

(defvar *pinned* '()
  "Waiting items a UI is editing (PIN-QUEUED): never delivered until EDIT-QUEUED or RELEASE-QUEUED.")

(defun queued-messages ()
  "The texts of the messages waiting to be delivered, oldest first. A UI calls
this at any time, mid-turn included, to show or edit them; it names one to the
functions below by its 1-based index or, safer, by the very string this returned
(the index shifts when the oldest is delivered, the string doesn't)."
  (sb-thread:with-recursive-lock (*held-lock*)
    (drain-inbox)
    (mapcar #'item-text *held*)))

(defun find-queued (which)
  "The *HELD* item WHICH names (index or a string from QUEUED-MESSAGES). Call with *HELD-LOCK* held."
  (if (integerp which)
      (and (plusp which) (nth (1- which) *held*))
      (find which *held* :key #'item-text :test #'eq)))

(defun pin-queued (which)
  "Hold back waiting message WHICH while a UI edits it, so it isn't delivered
mid-edit. Returns its text, or NIL when it's gone (already delivered)."
  (sb-thread:with-recursive-lock (*held-lock*)
    (drain-inbox)
    (let ((item (find-queued which)))
      (when item (pushnew item *pinned* :test #'eq) (item-text item)))))

(defun release-queued (which)
  "Let pinned message WHICH be delivered again, unchanged."
  (sb-thread:with-recursive-lock (*held-lock*)
    (let ((item (find-queued which)))
      (setf *pinned* (remove item *pinned* :test #'eq))
      (and item t))))

(defun edit-queued (which text)
  "Replace the text of waiting message WHICH (index or string, see QUEUED-MESSAGES)
with TEXT, or remove it when TEXT is NIL or blank; a leading ! still makes it
urgent. Unpins it. Returns true, or NIL when it's gone (already delivered)."
  (sb-thread:with-recursive-lock (*held-lock*)
    (drain-inbox)
    (let* ((item (find-queued which))
           (cell (and item (member item *held* :test #'eq))))
      (setf *pinned* (remove item *pinned* :test #'eq))
      (when cell
        (if (or (null text) (string= "" (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
            (setf *held* (remove item *held* :count 1 :test #'eq))
            (setf (car cell) (if (consp item) (cons text (cdr item)) text)))
        t))))

(defun take-queued-messages ()
  "Between model calls in a turn: let urgent messages (and, with
*DELIVER-MID-TURN*, all of them) into the conversation; hold the rest."
  (let ((now (sb-thread:with-recursive-lock (*held-lock*)
               (drain-inbox)
               (let ((go (remove-if-not (lambda (item) (and (or *deliver-mid-turn* (urgent-item-p item))
                                                             (not (member item *pinned* :test #'eq))))
                                        *held*)))
                 (setf *held* (remove-if (lambda (item) (member item go :test #'eq)) *held*))
                 go))))
    (dolist (item now)
      (add-message (inbox-message (if (urgent-item-p item) (strip-urgent item) item))))))

(defun next-message ()
  "The next message to start a turn with: a held one first, else wait for one."
  (loop
    (let ((item (sb-thread:with-recursive-lock (*held-lock*)
                  (drain-inbox)
                  (let ((item (find-if (lambda (item) (not (member item *pinned* :test #'eq))) *held*)))
                    (when item (setf *held* (remove item *held* :count 1 :test #'eq)))
                    item))))
      (when item (return item)))
    ;; With a timeout: a UI's QUEUED-MESSAGES may move an arrival onto *HELD*
    ;; while this waits on the mailbox.
    (let ((item (sb-concurrency:receive-message *inbox* :timeout 0.5)))
      (when item (return item)))))

(defun usage-number (&rest keys)
  "A number from *LAST-USAGE* following KEYS, or NIL."
  (let ((v *last-usage*))
    (dolist (k keys (and (integerp v) v))
      (setf v (and (hash-table-p v) (gethash k v))))))

(defun run-turn ()
  (loop
    (take-queued-messages)
    (emit :thinking :text (format nil "~A is thinking...~@[ (context ~:D tokens~@[, ~:D cached~])~]" *model*
                                  (usage-number "prompt_tokens")
                                  (or (usage-number "prompt_tokens_details" "cached_tokens")
                                      (usage-number "cached_tokens"))))
    (let* ((message (complete))
           (text (gethash "content" message))
           (calls (gethash "tool_calls" message)))
      (add-message message)
      ;; Before its tool calls run: one of them may end the process.
      (ignore-errors (session-save))
      (let ((reasoning (or (gethash "reasoning_content" message) (gethash "reasoning" message))))
        (when (and (stringp reasoning) (string/= "" reasoning))
          (emit :reasoning :text reasoning)))
      (when (and (stringp text) (string/= "" (string-trim '(#\Space #\Newline) text)))
        (emit :assistant :text text))
      (if (and (vectorp calls) (plusp (length calls)))
          (loop for call across calls
                do (add-message (run-tool-call call)))
          (return)))
      ;; After every step, not only at the end of the turn: a turn can be cut
      ;; short (a crash, a kill, an exit from inside a tool call).
      (ignore-errors (session-save))))

(defun session-save ()
  "Save this session: the conversation as it stands (after any compaction) and
its working directory. Written atomically; resumed at the next boot."
  (let ((file (session-path "messages.json"))
        (tmp (session-path "messages.json.tmp")))
    (with-open-file (out tmp :direction :output :if-exists :supersede :external-format :utf-8)
      (json-encode (coerce *messages* 'vector) out))
    (rename-file tmp file)
    (write-text (session-path "meta.json")
                (json-encode (obj :session *session* :cwd (namestring (or *cwd* *root*))
                                  :model *model* :saved (get-universal-time)
                                  :messages (length *messages*)
                                  :title (session-title))))
    t))

(defun session-title ()
  "The start of this session's first user message, as a name for it in lists."
  (let ((m (find-if (lambda (m) (and (equal (gethash "role" m) "user")
                                     (not (synthetic-message-p m))
                                     (stringp (gethash "content" m))
                                     (not (string-prefix-p "[" (gethash "content" m)))))
                    *messages*)))
    (when m
      (let ((text (substitute #\Space #\Newline (gethash "content" m))))
        (subseq text 0 (min 80 (length text)))))))

(defvar *session-lock-fd* nil
  "The open file descriptor holding this session's lock, for the process's life.")

(defun try-session-lock (name)
  "Take an exclusive, non-blocking flock on session NAME's lock file. Return the
file descriptor holding it, or NIL if another process holds it. The kernel
releases the lock when its holder dies, so a crash leaves nothing stale."
  (let* ((path (namestring (merge-pathnames (format nil "sessions/~A/lock" name) *home*)))
         (fd (progn (ensure-directories-exist path)
                    (sb-unix:unix-open path (logior sb-unix:o_creat sb-unix:o_rdwr) #o644))))
    (when fd
      ;; Close-on-exec: a child that inherited the fd and outlived the image
      ;; would keep the session locked, and every later boot would exit 3.
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "fcntl" (function sb-alien:int sb-alien:int sb-alien:int sb-alien:int))
       fd 2 1)                          ; F_SETFD, FD_CLOEXEC
      (if (zerop (sb-alien:alien-funcall
                  (sb-alien:extern-alien "flock" (function sb-alien:int sb-alien:int sb-alien:int))
                  fd 6))                ; LOCK_EX | LOCK_NB
          fd
          (progn (sb-unix:unix-close fd) nil)))))

(defun flock-held-p (path)
  "True if some process holds a flock on PATH, read from /proc/locks without
taking the lock: a probe that took it, however briefly, could make a session
booting at that moment find its lock taken and exit. Matches on the inode
alone: on some filesystems (btrfs subvolumes) the device stat reports isn't
the one /proc/locks lists. Another device's flock on the same inode number
would only make the session look busy. :UNKNOWN when /proc/locks can't be read."
  (multiple-value-bind (ok dev ino) (sb-unix:unix-stat path)
    (declare (ignore dev))
    (cond ((not ok) nil)
          ((not (probe-file "/proc/locks")) :unknown)
          (t (let ((key (format nil ":~D " ino)))
               (with-open-file (in "/proc/locks")
                 (loop for line = (read-line in nil)
                       while line
                       thereis (and (search "FLOCK" line) (search key line) t))))))))

(defun session-running-p (name)
  "True if another process is running session NAME (it holds the session's lock)."
  (unless (equal name *session*)
    (let ((held (flock-held-p (namestring (merge-pathnames (format nil "sessions/~A/lock" name) *home*)))))
      (if (eq held :unknown)
          (let ((fd (try-session-lock name)))
            (if fd (progn (sb-unix:unix-close fd) nil) t))
          held))))

(defun session-list ()
  "Every saved session, most recently saved first, as plists with :session :cwd
:saved (universal time) :messages :title and :running (another process has it)."
  (let ((sessions
          (loop for meta in (directory (merge-pathnames "sessions/*/meta.json" *home*))
                for m = (ignore-errors (json-decode (read-text meta)))
                when (hash-table-p m)
                  collect (let ((name (gethash "session" m)))
                            (list :session name :cwd (gethash "cwd" m) :saved (gethash "saved" m)
                                  :messages (gethash "messages" m) :title (gethash "title" m)
                                  :current (equal name *session*)
                                  :running (and (stringp name) (session-running-p name)))))))
    (sort sessions #'> :key (lambda (s) (or (getf s :saved) 0)))))

(defun session-name-p (name)
  (and (stringp name) (plusp (length name))
       (every (lambda (c) (or (alphanumericp c) (find c "-_"))) name)))

(defun session-switch (name &key cwd)
  "End this process and have the supervisor start session NAME in its place
(resumed if it exists, else new; CWD sets a new session's working directory).
This session is saved first and can be switched back to. Refused if another
process is running NAME: one session, one process."
  (cond ((not (session-name-p name))
         (format nil "~S isn't a session name (letters, digits, - and _)." name))
        ((equal name *session*) (format nil "Already in session ~A." name))
        ((session-running-p name)
         (format nil "Session ~A is running in another process." name))
        (t (write-text (home-path "switch") (format nil "~A~%~@[~A~]~%" name cwd))
           (restart-image))))

(defparameter +home-ignore+
  "# lisper: data is versioned; transcripts, logs and scratch files aren't.
tmp/
*.log
*.jsonl
*.tmp
sessions/*/messages.json*
sessions/*/meta.json
sessions/*/resume.txt
sessions/*/queue.*
sessions/*/lock
switch
"
  "The .gitignore written into a new *home*.")

(defun home-commit (message)
  "Commit the data in *home* (notes, todos, layouts...) to its own git repo,
creating it on first use, so a bad rewrite of a data file can be undone. Cheap:
no boot check. Returns NIL on success, else git's complaint."
  (unless (probe-file (merge-pathnames ".git/" *home*))
    (write-text (merge-pathnames ".gitignore" *home*) +home-ignore+)
    (shell "git init -q" :directory *home*))
  (git "git add -A" :directory *home*)
  (multiple-value-bind (out code)
      ;; Without a configured identity every commit would fail; supply one for
      ;; this commit only rather than writing any git config.
      (git (format nil "if git config user.email >/dev/null; then git commit -q -m ~A; else git -c user.name=lisper -c user.email=lisper@localhost commit -q -m ~A; fi"
                   (shell-quote message) (shell-quote message))
           :directory *home*)
    (unless (or (eql code 0) (search "nothing to commit" out) (search "nothing added" out))
      out)))

(defun session-forget ()
  "Drop the saved conversation, so the next boot starts this session fresh."
  (ignore-errors (delete-file (session-path "messages.json")))
  t)

(defun session-load ()
  "Restore the saved conversation and working directory, if any. Returns the
number of messages restored."
  (let ((text (read-text (session-path "messages.json")))
        (meta (ignore-errors (json-decode (read-text (session-path "meta.json"))))))
    (when (and (hash-table-p meta) (null *cwd*) (stringp (gethash "cwd" meta)))
      (setf *cwd* (pathname (gethash "cwd" meta))))
    (if (and text (not (sb-ext:posix-getenv "LISPER_FRESH")))
        (let ((messages (json-decode text)))
          (setf *messages* (coerce messages 'list))
          (length *messages*))
        0)))

(defun agent-loop ()
  (loop
    (let ((item (next-message)))
      (add-message (inbox-message (if (urgent-item-p item) (strip-urgent item) item)))
      (handler-case (run-turn)
        (serious-condition (c)
          (emit :error :text (format nil "turn failed: ~A" c))))
      (handler-case (session-save)
        (serious-condition (c)
          (emit :error :text (format nil "saving the session failed: ~A" c))))
      (let ((failure (ignore-errors (home-commit (format nil "~A: after a turn" *session*)))))
        (when failure
          (emit :info :text (format nil "committing ~A failed: ~A" *home* failure))))
      ;; The turn is over: a UI can show it's waiting for the user (unless a
      ;; held message starts the next turn at once).
      (emit :idle :held (length (sb-thread:with-recursive-lock (*held-lock*) *held*))))))

;;; ------------------------------------------------------------------
;;; Terminal input. Like terminal-emit, these live in the unlocked LISPING
;;; package: the AI may stop the loop and take the terminal over.

(defvar *terminal-thread* nil "The thread reading terminal lines into *inbox*, or NIL.")

(defun lisping::terminal-input-loop ()
  "Read lines from the terminal and send them to the agent. End of input (Ctrl-D) quits."
  (loop
    (let ((line (read-line *standard-input* nil)))
      (unless line (sb-ext:exit :code 0 :abort t))
      (when (string/= "" (string-trim " " line))
        (send-user-message line)))))

(defun lisping::start-terminal-input ()
  "Start reading the terminal, unless already reading."
  (unless (and *terminal-thread* (sb-thread:thread-alive-p *terminal-thread*))
    (setf *terminal-thread* (sb-thread:make-thread 'lisping::terminal-input-loop :name "terminal-input"))))

(defun lisping::stop-terminal-input ()
  "Stop reading the terminal, e.g. to read keys from it yourself."
  (let ((thread *terminal-thread*))
    (setf *terminal-thread* nil)
    (when (and thread (sb-thread:thread-alive-p thread))
      (sb-thread:terminate-thread thread)
      (sb-thread:join-thread thread :default nil :timeout 2))
    t))

;;; ------------------------------------------------------------------
;;; Queue input: anyone can queue messages by appending to the session's queue.txt.
;;; Like terminal input, this lives in LISPING and is the AI's to change.

(defvar *queue-thread* nil "The thread moving the session's queue.txt into *inbox*, or NIL.")

(defun lisping::queue-path () (session-path "queue.txt"))

(defun lisping::take-queue ()
  "Take the queued messages out of the session's queue.txt: messages are separated by a
line containing only ---. The file is renamed before it is read, so text
appended meanwhile lands in a fresh queue.txt instead of being lost."
  (let ((path (lisping::queue-path))
        (taken (session-path "queue.taking")))
    (when (probe-file path)
      (rename-file path taken)
      (sleep 0.1)                       ; let a writer that already opened it finish
      (let ((text (read-text taken)))
        (delete-file taken)
        (loop with start = 0
              for sep = (search (format nil "~%---~%") (format nil "~%~A~%" text) :start2 start)
              for end = (if sep (max start (1- sep)) (length text))
              for message = (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq text start (min end (length text))))
              when (string/= message "") collect message
              while sep
              do (setf start (min (length text) (+ sep 4))))))))

(defun lisping::queue-input-loop ()
  "Every second, send what was queued in the session's queue.txt to the agent."
  (loop
    (handler-case (dolist (message (lisping::take-queue))
                    (send-user-message message))
      (serious-condition (c)
        (fallback-emit (list :type :error :text (format nil "queue input: ~A" c)))))
    (sleep 1)))

(defun lisping::start-queue-input ()
  "Start watching the session's queue.txt, unless already watching."
  (unless (and *queue-thread* (sb-thread:thread-alive-p *queue-thread*))
    (setf *queue-thread* (sb-thread:make-thread 'lisping::queue-input-loop :name "queue-input"))))

(defun lisping::stop-queue-input ()
  "Stop watching the session's queue.txt."
  (let ((thread *queue-thread*))
    (setf *queue-thread* nil)
    (when (and thread (sb-thread:thread-alive-p thread))
      (sb-thread:terminate-thread thread)
      (sb-thread:join-thread thread :default nil :timeout 2))
    t))

;;; ------------------------------------------------------------------
;;; Boot

(defun take-resume-message ()
  (let ((path (resume-path)))
    (when (probe-file path)
      (prog1 (with-open-file (in path :external-format :utf-8)
               (let ((text (make-string (file-length in))))
                 (subseq text 0 (read-sequence text in))))
        (delete-file path)))))

(defun main ()
  (setf *random-state* (make-random-state t))
  ;; SIGUSR1 asks for a restart from outside (pkill -USR1 -f boot.lisp): exit 42,
  ;; which the supervisor treats as a restart. SIGTERM still means stop.
  (sb-sys:enable-interrupt sb-unix:sigusr1
                           (lambda (&rest args)
                             (declare (ignore args))
                             ;; Not in the handler itself: restart-image writes a file.
                             (sb-thread:make-thread
                              (lambda () (restart-image "[supervisor] Restarted from outside with SIGUSR1."))
                              :name "sigusr1-restart")))
  (configure-session)
  (ensure-directories-exist (session-dir))
  ;; One session, one process: hold the session's lock for this process's life.
  (unless (setf *session-lock-fd* (try-session-lock *session*))
    (format *error-output* "~&Session ~A is already running in another process. Attach to that one, or start another session: LISPER_SESSION=name ./run.sh~%" *session*)
    (finish-output *error-output*)
    (sb-ext:exit :code 3 :abort t))
  (configure)
  ;; Terminal input starts before init.lisp, so a UI loaded from it can stop it and take over.
  (unless (sb-ext:posix-getenv "LISPING_HEADLESS")
    (lisping::start-terminal-input))
  (lisping::start-queue-input)
  (setf *boot-errors*
        (let ((*booting* t))
          (if (probe-file (init-path))
              (load-collecting (init-path) (make-broadcast-stream))
              '())))
  (git "git tag -f last-good HEAD")
  (let ((stamp (multiple-value-bind (s m h d mo y) (get-decoded-time)
                 (format nil "~D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D" y mo d h m s))))
    (setf *previous-session*
          (first (sort (directory (merge-pathnames "*.jsonl" (session-dir)))
                       #'string> :key #'namestring)))
    (setf *session-log* (session-path (format nil "~A.jsonl" stamp)))
    (setf *trace-file* (session-path (format nil "~A.trace.log" stamp)))
    (shell (format nil "ln -sfn ~A.trace.log trace.log" stamp) :directory (session-dir)))
  (let ((restored (session-load)))
    (when (plusp restored)
      (emit :info :text (format nil "resumed session ~A: ~D messages, cwd ~A" *session* restored (or *cwd* *root*)))))
  (sb-ext:lock-package :lisping-core)
  (emit :info :text (format nil "lisping-ai on ~A, ~D tool~:P, ~D boot error~:P. Type a message and press Enter; replies can take a minute. Ctrl-D quits.~%Session ~A in ~A. Watch it: tail -f ~Atrace.log"
                            *model* (hash-table-count *tools*) (length *boot-errors*)
                            *session* (namestring (or *cwd* *root*)) (namestring (session-dir))))
  (dolist (e *boot-errors*) (emit :error :text e))
  (let ((agent (sb-thread:make-thread #'agent-loop :name "agent"))
        (resume (take-resume-message)))
    (when (or resume *boot-errors*)
      (send-user-message
       (format nil "[boot] ~@[~A~]~@[~%Problems while loading init.lisp:~%~{- ~A~%~}~]"
               resume *boot-errors*)
       :kind "boot"))
    (sb-thread:join-thread agent :default nil)))
