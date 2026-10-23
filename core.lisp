;;;; core.lisp: the frozen kernel of lisping-ai.
;;;;
;;;; Everything here is deliberately small: an agent loop, one tool (eval),
;;;; an API to register more tools, and persistence through an append-only
;;;; extensions.lisp kept in git. The AI builds everything else itself.

(require :sb-concurrency)

(defpackage :lisping-core
  (:use :cl)
  (:export #:main
           ;; tools
           #:register-tool #:unregister-tool #:*tools* #:arg
           ;; persistence
           #:persist #:save-form #:commit #:restart-image #:*boot-errors* #:*previous-session*
           ;; conversation and I/O
           #:*system-prompt* #:*messages* #:*inbox* #:*emit* #:emit
           #:send-user-message #:*before-request*
           ;; the model call, replaceable (e.g. to stream)
           #:*complete* #:default-complete #:chat-url #:api-headers #:*terminal-thread*
           ;; settings
           #:*model* #:*api-base* #:*max-tokens* #:*eval-timeout* #:*output-limit* #:*root*
           ;; helpers
           #:obj #:json-encode #:json-decode #:http-post-json #:shell))

(defpackage :lisping
  (:use :cl :lisping-core))

(in-package :lisping-core)

(defvar *root* (make-pathname :name nil :type nil :version nil :defaults *load-truename*)
  "Project directory: core.lisp, extensions.lisp, state/, .git.")

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

(defun shell (command &key (directory *root*))
  "Run COMMAND with sh -c in DIRECTORY. Return (values output exit-code)."
  (let* ((out (make-string-output-stream))
         (process (sb-ext:run-program "/bin/sh" (list "-c" command)
                                      :directory (namestring directory)
                                      :output out :error :output :input nil
                                      :external-format :utf-8 :wait t)))
    (values (get-output-stream-string out) (sb-ext:process-exit-code process))))

(defun tmp-path (name type)
  (let ((path (merge-pathnames (format nil "state/tmp/~A-~D-~D.~A" name (get-universal-time) (random 1000000) type)
                               *root*)))
    (ensure-directories-exist path)
    path))

(defun curl-quote (string)
  (with-output-to-string (s)
    (write-char #\" s)
    (loop for c across string
          do (when (find c "\"\\") (write-char #\\ s))
             (write-char c s))
    (write-char #\" s)))

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
             (error "HTTP ~A from ~A: ~A" status url (subseq text 0 (min 2000 (length text)))))
           (values (json-decode text) status))
      (ignore-errors (delete-file request)))))

;;; ------------------------------------------------------------------
;;; Persistence: extensions.lisp is replayed on boot; git keeps history.

(defvar *replaying* nil "True while extensions.lisp is replayed at boot.")
(defvar *boot-errors* '() "Forms from extensions.lisp that failed during the last boot.")

(defun extensions-path () (merge-pathnames "extensions.lisp" *root*))
(defun resume-path () (merge-pathnames "state/resume.txt" *root*))

(defun form-label (form)
  (if (and (consp form) (symbolp (car form)))
      (format nil "~(~A~)~@[ ~A~]" (car form)
              (and (consp (cdr form)) (atom (cadr form)) (cadr form)))
      (let ((text (prin1-to-string form)))
        (subseq text 0 (min 60 (length text))))))

(defun save-form (form)
  "Append FORM (a form, or its source text) to extensions.lisp. Does nothing during replay."
  (unless *replaying*
    (with-open-file (out (extensions-path) :direction :output :if-exists :append
                                           :if-does-not-exist :create :external-format :utf-8)
      (if (stringp form)
          (format out "~&~A~%~%" form)
          (let ((*package* (find-package :lisping))
                (*print-case* :downcase)
                (*print-pretty* t)
                (*print-right-margin* 100)
                (*print-length* nil)
                (*print-level* nil))
            (format out "~&~S~%~%" form))))
    t))

(defun shell-quote (string)
  (with-output-to-string (s)
    (write-char #\' s)
    (loop for c across string
          do (if (char= c #\') (write-string "'\\''" s) (write-char c s)))
    (write-char #\' s)))

(defun commit (message)
  "Commit everything under the project directory. Does nothing during replay."
  (unless *replaying*
    (shell "git add -A")
    (shell (format nil "git commit -q -m ~A" (shell-quote message)))))

(defmacro persist (&body forms)
  "Evaluate FORMS now, and on every future boot. Saved only if they succeed."
  `(multiple-value-prog1 (progn ,@forms)
     (save-form '(progn ,@forms))
     (commit ,(format nil "persist ~A" (form-label (first forms))))))

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

(defun replay-extensions ()
  (setf *boot-errors* '())
  (let ((*replaying* t)
        (*package* (find-package :lisping))
        (*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (with-open-file (in (extensions-path) :if-does-not-exist nil :external-format :utf-8)
      (when in
        (loop
          (let* ((start (file-position in))
                 (form (handler-case (read in nil in)
                         (serious-condition (c)
                           (push (format nil "read error, form skipped: ~A" c) *boot-errors*)
                           (if (skip-to-next-toplevel-form in start) '(progn) in)))))
            (when (eq form in) (return))
            (handler-case (eval form)
              (serious-condition (c)
                (push (format nil "~A: ~A" (form-label form) c) *boot-errors*))))))))
  (setf *boot-errors* (nreverse *boot-errors*)))

(defun restart-image (&optional resume-message)
  "Exit so the supervisor restarts the image. RESUME-MESSAGE is sent to you after boot."
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

(defun auto-persisted-p (form)
  "Top-level def* forms, REQUIRE and REGISTER-TOOL calls sent to eval are saved automatically."
  (and (consp form) (symbolp (car form))
       (let ((name (symbol-name (car form))))
         (or (and (> (length name) 3) (string= "DEF" name :end2 3))
             (member name '("REQUIRE" "REGISTER-TOOL") :test #'string=)))))

(defun user-backtrace ()
  "The current backtrace, cut off where the harness's own frames begin."
  (let* ((text (with-output-to-string (s)
                 (sb-debug:print-backtrace :stream s :count 25)))
         (cut (or (search "LISPING-CORE::EVAL-TOPLEVEL" text) (search "LISPING-CORE::EVAL-CODE" text)))
         (line (and cut (position #\Newline text :end cut :from-end t))))
    (if line (subseq text 0 (1+ line)) text)))

(defun eval-toplevel (form source saved)
  "Evaluate FORM as a top-level form: the subforms of a top-level PROGN are
top-level forms too, evaluated one by one. Each definition is saved as soon as it
succeeds, so what is saved matches what ran. SOURCE is FORM's text, if known.
Labels of saved forms are pushed onto (car SAVED). Return FORM's values."
  (if (and (consp form) (eq (car form) 'progn))
      (let ((values '()))
        (dolist (sub (cdr form) values)
          (setf values (eval-toplevel sub nil saved))))
      (let ((values (multiple-value-list (eval form))))
        (when (auto-persisted-p form)
          (save-form (or source form))
          (push (form-label form) (car saved)))
        values)))

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
        (cond (missing-package
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
                 (if (find-symbol missing-symbol package)
                     (format nil "It exists but isn't exported: ~(~A::~A~)" home missing-symbol)
                     (similar-names missing-symbol names (format nil "symbols in ~A" home) external))))
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

(defun eval-code (code)
  "Read and evaluate every form in CODE. Return printed output, values and errors."
  (let ((out (make-string-output-stream))
        (saved (list '()))
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
                  (let* ((start (file-position in))
                         (form (read in nil in)))
                    (when (eq form in) (return))
                    (let ((values (eval-toplevel form
                                                 (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                              (subseq code start (file-position in)))
                                                 saved))
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
    (let ((labels (reverse (car saved))))
      (when labels
        (commit (format nil "~{~A~^, ~}" labels))
        (format out "~&[persisted: ~{~A~^, ~}]~%" labels)))
    (limit-output (get-output-stream-string out))))

(register-tool "eval"
               "Evaluate Common Lisp forms in your own running SBCL image, in package LISPING. Returns printed output, each form's values, or the error with a backtrace."
               (obj :type "object"
                    :properties (obj :code (obj :type "string" :description "One or more Lisp forms."))
                    :required #("code"))
               (lambda (args) (eval-code (arg args "code" ""))))

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
(defvar *trace-file* nil "Human-readable log of every event; state/trace.log points at the current one.")
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

(defun send-user-message (text)
  "Queue TEXT as a message from the user."
  (sb-concurrency:send-message *inbox* text))

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
      (:reasoning nil)                  ; long; it is in state/trace.log
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

(defun emit (type &rest fields)
  "Send an event to *EMIT*. Falls back to plain terminal output if *EMIT* fails."
  (let ((event (list* :type type fields)))
    (trace-event event)
    (handler-case (funcall *emit* event)
      (serious-condition (c)
        (fallback-emit event)
        (fallback-emit (list :type :error :text (format nil "*emit* failed: ~A" c)))))))

(defun add-message (message)
  (setf *messages* (append *messages* (list message)))
  (when (equal (gethash "role" message) "user")
    (trace-event (list :type :user :text (gethash "content" message))))
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
- (shell \"command\") runs sh in the project directory, returns (values output exit-code).
- *system-prompt*: this text. A string, or a function returning one. You may rewrite it.
- *messages*: the conversation history sent to the model (a list of objs). You may trim or summarize it to manage your context.
- *before-request*: functions called with each request body before it is sent; each returns the body to use.
- *emit*: function receiving every event as a plist (:type :assistant/:thinking/:reasoning/:tool-call/:tool-result/:error/:info, plus :text, :name, :args). It defaults to terminal-emit, your terminal display: an ordinary function in package LISPING that you may redefine to change how the terminal looks.
- *inbox* and (send-user-message text): user messages arrive here. A UI you build should push user input with send-user-message and show events through *emit*.
- The terminal: (stop-terminal-input) stops the thread that reads lines from the terminal into *inbox* (*terminal-thread*), so you can take the terminal over, e.g. raw keys and full-screen drawing. (start-terminal-input) starts it again. Both, and terminal-input-loop, are yours to redefine.
- *complete*: the function that calls the model, given the request body; it returns the assistant message. Replace it to stream or call the model differently; chat-url and api-headers give you the endpoint and auth. If yours fails, the core falls back to default-complete.
- *model*, *max-tokens*, *eval-timeout*, *output-limit*, *root* (the project directory).

Persistence. Your image restarts sometimes, so keep what matters:
- Your conversation does NOT survive a restart, and the user may restart you at any moment. Whatever you learn by exploring (facts about your environment, what works and what doesn't) is lost unless you write it down, e.g. in state/notes.md. *previous-session* is the transcript of the session before this boot (JSONL), if you want to look back.
- Top-level def* forms (defun, defvar, defparameter, defmacro, defclass, defstruct, ...), require and register-tool calls you send to eval are saved automatically to extensions.lisp and committed to git, each as soon as it succeeds. As in Common Lisp, the forms inside a top-level progn are top-level forms too; a def* inside let or a function body is not, so wrap such things in (persist ...).
- Anything else is temporary unless you wrap it in (persist ...), e.g. (persist (setf *emit* 'my-emit)). persist evaluates now and saves the form only if it succeeds.
- On boot, the core replays extensions.lisp in order; the last definition wins. Forms that fail are skipped and reported to you.
- Files you write under state/ are committed too (except state/tmp and state/sessions). Use it for notes, memory and data.
- (restart-image \"message\") restarts the image; the message is sent to you after boot. Use it to check your extensions survive a restart.
- A supervisor restarts the image if it crashes, and rolls extensions.lisp back to the last good boot if it crashes while booting.
- extensions.lisp is append-only by default. When it gets long, you may compact it: rewrite it to the current definitions, test it with a restart, and commit.

Why Lisp, and how to work in it:
- You are a live image, like Emacs or a Smalltalk system. The running image is the program: there is no separate source tree that you build and restart. You grow it the way a Lisp programmer works at a REPL: evaluate one small definition, call it right away to test it, redefine it when it's wrong. extensions.lisp is the image's memory of those definitions, written for you; you don't need files of your own for Lisp code.
- Prefer many small functions over big ones. A ten-line defun with a mistake is cheap to resend; a sixty-line one with an unbalanced paren is not. Define and test in the same eval.
- Code is data. (read-from-string code) shows you how text will be read before you run it, which catches misplaced parens. You can hold a definition as a list, change it with list operations, and evaluate the result instead of patching text. Reading extensions.lisp with read gives you every saved definition as data.
- Ask the image instead of guessing: (apropos \"ALIEN\" :sb-alien), (describe 'x), (documentation 'f 'function), (function-lambda-expression #'f). Treat compiler warnings as errors: code that evaluated with warnings is usually not what you meant.
- An error doesn't hurt the image. Read it, fix the one form, try again.
- Files are for data and for things outside Lisp. If you do keep Lisp code in a file, persist the (load ...) that loads it, or it won't come back after a restart.

Rules:
- The core (package LISPING-CORE) is locked. Use setf for its variables; don't redefine its functions.
- eval calls time out after *eval-timeout* seconds. Run servers and other long-lived work in threads (sb-thread:make-thread).
- Build in small steps and check each one. When something breaks, read the error and fix it.
- Everything you do (your reasoning, tool calls and results) is written to state/trace.log for the user to read. The core writes it; it is not yours to change.
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

(defun chat-url () (format nil "~A/chat/completions" *api-base*))

(defun api-headers ()
  "HTTP headers for the model API, including the key. Never print them."
  (list (format nil "Authorization: Bearer ~A" *api-key*)))

(defun request-body ()
  (let ((body (obj :model *model*
                   :max_tokens *max-tokens*
                   :messages (coerce (cons (obj :role "system" :content (system-prompt-text)) *messages*) 'vector)
                   :tools (tool-specs))))
    (dolist (hook *before-request* body)
      (setf body (funcall hook body)))))

(defun default-complete (body)
  "POST BODY to the model API, retrying failures; return the assistant message."
  (loop for attempt from 1
        do (handler-case
               (let ((response (http-post-json (chat-url) body :headers (api-headers))))
                 (return (gethash "message" (aref (gethash "choices" response) 0))))
             (error (c)
               (when (>= attempt 4) (error c))
               (emit :error :text (format nil "model call failed (attempt ~D, retrying): ~A" attempt c))
               (sleep (* 2 attempt))))))

(defvar *complete* 'default-complete
  "Function taking a request body (an OBJ) and returning the assistant message as an OBJ
with content, tool_calls and reasoning_content. Replace it to stream or change how
the model is called; if it fails, the core falls back to DEFAULT-COMPLETE.")

(defun complete ()
  "Send the conversation to the model and return its message."
  (let ((body (request-body)))
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
                           (let ((value (funcall (tool-fn tool) args)))
                             (limit-output (if (stringp value) value (prin1-to-string value))))
                           (format nil "Unknown tool ~S. Registered tools: ~{~A~^, ~}" name
                                   (loop for k being the hash-keys of *tools* collect k))))
                   (serious-condition (c)
                     (format nil "ERROR (~S): ~A" (type-of c) c)))))
    (emit :tool-result :name name :text result)
    (obj :role "tool" :tool_call_id (gethash "id" call) :content result)))

(defun take-queued-messages ()
  "Move user messages that arrived mid-turn into the conversation."
  (loop for text = (sb-concurrency:receive-message-no-hang *inbox*)
        while text
        do (add-message (obj :role "user" :content text))))

(defun run-turn ()
  (loop
    (take-queued-messages)
    (emit :thinking :text (format nil "~A is thinking..." *model*))
    (let* ((message (complete))
           (text (gethash "content" message))
           (calls (gethash "tool_calls" message)))
      (add-message message)
      (let ((reasoning (or (gethash "reasoning_content" message) (gethash "reasoning" message))))
        (when (and (stringp reasoning) (string/= "" reasoning))
          (emit :reasoning :text reasoning)))
      (when (and (stringp text) (string/= "" (string-trim '(#\Space #\Newline) text)))
        (emit :assistant :text text))
      (if (and (vectorp calls) (plusp (length calls)))
          (loop for call across calls
                do (add-message (run-tool-call call)))
          (return)))))

(defun agent-loop ()
  (loop
    (let ((text (sb-concurrency:receive-message *inbox*)))
      (add-message (obj :role "user" :content text))
      (handler-case (run-turn)
        (serious-condition (c)
          (emit :error :text (format nil "turn failed: ~A" c)))))))

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
  (configure)
  ;; Terminal input starts before replay, so a saved UI can stop it and take over.
  (unless (sb-ext:posix-getenv "LISPING_HEADLESS")
    (lisping::start-terminal-input))
  (replay-extensions)
  (shell "git tag -f last-good HEAD")
  (let ((stamp (multiple-value-bind (s m h d mo y) (get-decoded-time)
                 (format nil "~D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D" y mo d h m s))))
    (setf *previous-session*
          (first (sort (directory (merge-pathnames "state/sessions/*.jsonl" *root*))
                       #'string> :key #'namestring)))
    (setf *session-log* (merge-pathnames (format nil "state/sessions/~A.jsonl" stamp) *root*))
    (ensure-directories-exist *session-log*)
    (setf *trace-file* (merge-pathnames (format nil "state/sessions/~A.trace.log" stamp) *root*))
    (shell (format nil "ln -sfn sessions/~A.trace.log state/trace.log" stamp)))
  (sb-ext:lock-package :lisping-core)
  (emit :info :text (format nil "lisping-ai on ~A, ~D tool~:P, ~D boot error~:P. Type a message and press Enter; replies can take a minute. Ctrl-D quits.~%Watch what it does: tail -f state/trace.log"
                            *model* (hash-table-count *tools*) (length *boot-errors*)))
  (dolist (e *boot-errors*) (emit :error :text e))
  (let ((agent (sb-thread:make-thread #'agent-loop :name "agent"))
        (resume (take-resume-message)))
    (when (or resume *boot-errors*)
      (send-user-message
       (format nil "[boot] ~@[~A~]~@[~%Forms that failed while replaying extensions.lisp:~%~{- ~A~%~}~]"
               resume *boot-errors*)))
    (sb-thread:join-thread agent :default nil)))
