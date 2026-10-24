;;;; lisp/models.lisp -- several AI providers and a model picker (todo 30).
;;;; /model        popup of provider/model pairs, Enter switches
;;;; /login N URL  asks for the API key in a masked line, then add-provider
;;;; /logout N     remove-provider
;;;; The keys never pass through the model: the masked line keeps them in a
;;;; private buffer (not the input line, not the chat, not keys.log, not an
;;;; event) from the keypress to ADD-PROVIDER.

(in-package :lisping)

(declaim (special *completion-seen* *tui-key-interceptors*))

;;; ---------------------------------------------------------------
;;; what is current

(defun models-provider-list ()
  (ignore-errors (lisping-core:provider-names)))

(defun models-current-provider ()
  "Name of the provider in use, or NIL."
  (let ((c (find-if (lambda (p) (getf p :current)) (models-provider-list))))
    (if c (getf c :name)
        (let ((p lisping-core:*provider*)) (and (stringp p) p)))))

(defun models-label ()
  "provider/model for the status bar."
  (let ((p (models-current-provider)))
    (if p (format nil "~A/~A" p lisping-core:*model*) (format nil "~A" lisping-core:*model*))))

;;; ---------------------------------------------------------------
;;; per-session cache of each provider's /models, fetched off-thread

(defvar *models-cache* (make-hash-table :test 'equal)
  "provider name -> (status models time); status :loading :ok :failed.")

(defparameter *models-retry-after* 60
  "Seconds before a provider whose /models failed is asked again.")

(defun models-fetch (name)
  "Ask provider NAME for its models in a thread; the popup refreshes
when the answer arrives."
  (setf (gethash name *models-cache*) (list :loading nil (get-universal-time)))
  (sb-thread:make-thread
   (lambda ()
     (let ((r (handler-case (lisping-core:list-models name)
                (serious-condition () nil))))
       (setf (gethash name *models-cache*)
             (if (and (consp r) (every #'stringp r))
                 (list :ok r (get-universal-time))
                 (list :failed nil (get-universal-time))))
       (setf *completion-seen* nil)
       (when *tui* (tui-dirty))))
   :name "models-fetch"))

(defun models-status (name)
  "(values status models) for provider NAME, starting a fetch when
nothing is cached (or a failure is old enough to retry)."
  (let ((e (gethash name *models-cache*)))
    (when (or (null e)
              (and (eq (first e) :failed)
                   (> (- (get-universal-time) (third e)) *models-retry-after*)))
      (models-fetch name)
      (setf e (gethash name *models-cache*)))
    (values (first e) (second e))))

;;; ---------------------------------------------------------------
;;; /model

(defun models-items (partial)
  "Popup items (insert . display) for provider/model pairs matching
PARTIAL (substring, any case): the current one marked, a provider
without a model list offering its current model or just PROVIDER/."
  (let* ((cur-p (models-current-provider))
         (cur-m lisping-core:*model*)
         (items nil))
    (dolist (pr (models-provider-list))
      (let ((name (getf pr :name)))
        (multiple-value-bind (status models) (models-status name)
          (let ((ms (cond ((eq status :ok)
                           (if (and (equal name cur-p) (not (member cur-m models :test #'equal)))
                               (cons cur-m models)
                               models))
                          ((equal name cur-p) (list cur-m))
                          (t nil))))
            (if ms
                (dolist (m ms)
                  (let ((pair (format nil "~A/~A" name m)))
                    (push (cons pair
                                (format nil "~A ~A~A"
                                        (if (and (equal name cur-p) (equal m cur-m)) "●" " ")
                                        pair
                                        (if (and (equal name cur-p) (not (eq status :ok)))
                                            "  (model list unavailable)" "")))
                          items)))
                (push (cons (format nil "~A/" name)
                            (format nil "  ~A/  (~A)" name
                                    (if (eq status :loading) "loading models…" "models unavailable — type a model id")))
                      items))))))
    (setf items (nreverse items))
    (remove-if-not (lambda (it) (search partial (car it) :test #'char-equal)) items)))

(defun slash-model-args (ui partial)
  (declare (ignore ui))
  (models-items (string-trim " " partial)))

(defun models-refresh-limits ()
  "The context window follows the model (lisp/context.lisp)."
  (when (and (boundp '*ctx-window*) (fboundp 'ctx-model-window))
    (setf (symbol-value '*ctx-window*) (funcall 'ctx-model-window))
    (setf (symbol-value '*ctx-budget*)
          (min 200000 (floor (* 3 (symbol-value '*ctx-window*)) 4)))))

(defun slash-model (ui args)
  "/model [provider/model]: with no argument open the popup (the input
becomes \"/model \"); with one, switch to it."
  (let ((arg (string-trim " " args)))
    (cond
      ((string= arg "")
       (tui-clear-input ui)
       (dolist (c (coerce "/model " 'list)) (tui-input-insert ui c))
       :keep-input)
      (t
       (let* ((slash (position #\/ arg))
              (known (mapcar (lambda (p) (getf p :name)) (models-provider-list)))
              (prov (if slash (subseq arg 0 slash) (models-current-provider)))
              (model (if slash (subseq arg (1+ slash)) arg)))
         (cond ((not (member prov known :test #'equal))
                (slash-say (format nil "No provider ~A (known: ~{~A~^, ~}). /login NAME URL adds one." prov known)))
               ((string= model "")
                (slash-say (format nil "Give a model: /model ~A/MODEL" prov)))
               (t
                (lisping-core:use-model prov model)
                (models-refresh-limits)
                (slash-say (format nil "Now using ~A/~A from the next request." prov model))
                (tui-dirty))))))))

;;; ---------------------------------------------------------------
;;; /logout

(defun slash-logout-args (ui partial)
  (declare (ignore ui))
  (loop for p in (models-provider-list)
        for n = (getf p :name)
        when (search (string-trim " " partial) n :test #'char-equal) collect n))

(defun slash-logout (ui args)
  (declare (ignore ui))
  (let ((name (string-trim " " args)))
    (cond ((string= name "") (slash-say "Usage: /logout NAME"))
          ((not (member name (models-provider-list) :key (lambda (p) (getf p :name)) :test #'equal))
           (slash-say (format nil "No provider ~A." name)))
          (t (lisping-core:remove-provider name)
             (remhash name *models-cache*)
             (slash-say (format nil "Removed provider ~A." name))
             (tui-dirty)))))

;;; ---------------------------------------------------------------
;;; /login and the masked key line

(defvar *login* nil
  "While the key is being typed: (name url buffer). BUFFER is a private
adjustable string; nothing else ever sees it.")

(defun login-active-p () (and *login* t))

(defun login-wipe ()
  (when *login*
    (let ((b (third *login*))) (when (stringp b) (fill b #\Nul)))
    (setf *login* nil)))

(defun login-finish ()
  (let* ((name (first *login*)) (url (second *login*))
         (key (coerce (third *login*) 'simple-string)))
    (login-wipe)
    (cond ((zerop (length key)) (slash-say "Login cancelled: empty key."))
          (t (handler-case
                 (progn (lisping-core:add-provider name url key)
                        (remhash name *models-cache*)
                        (slash-say (format nil "Saved provider ~A (~A). /model to pick a model." name url)))
               (error () (slash-say (format nil "Could not save provider ~A." name))))))
    (fill key #\Nul)
    (tui-dirty)))

(defun login-cancel ()
  (login-wipe)
  (slash-say "Login cancelled.")
  (tui-dirty))

(defun login-append (buf string)
  (loop for ch across string
        when (and (graphic-char-p ch) (char/= ch #\Space)) do (vector-push-extend ch buf)))

(defun login-key (ui key)
  "Key interceptor (first in line): while the key is asked for, every
key is ours and nothing is echoed."
  (declare (ignore ui))
  (when *login*
    (handler-case
        (let ((buf (third *login*)))
          (cond
            ((or (eq key :newline) (and (characterp key) (member key '(#\Return #\Newline))))
             (login-finish))
            ((or (eq key :escape) (and (characterp key) (member key (list #\Esc (code-char 3) (code-char 4)))))
             (login-cancel))
            ((and (characterp key) (member key '(#\Rubout #\Backspace)))
             (when (plusp (length buf)) (setf (aref buf (1- (length buf))) #\Nul) (decf (fill-pointer buf))))
            ((and (characterp key) (char= key (code-char 21)))
             (fill buf #\Nul) (setf (fill-pointer buf) 0))
            ((and (characterp key) (graphic-char-p key) (char/= key #\Space))
             (vector-push-extend key buf))
            ((and (consp key) (eq (first key) :paste) (stringp (second key)))
             (login-append buf (second key))))
          (tui-dirty))
      (serious-condition () (login-wipe) (tui-dirty)))
    t))

(defun login-draw (g y w)
  "The masked prompt, drawn instead of the input line: dots only."
  (let* ((prompt (tui-seg-chunks (format nil "key ~A" (first *login*)) 0 3))
         (plen (1+ (reduce #'+ prompt :key (lambda (c) (length (car c))))))
         (n (length (third *login*)))
         (shown (min n (max 0 (- w plen 1)))))
    (ui-grid-write-chunks g 0 y prompt w)
    (if (zerop n)
        (ui-grid-write g plen y "paste or type the API key — Enter saves, Esc cancels" 8 w)
        (ui-grid-write g plen y (make-string shown :initial-element #\•) 7 w))
    (values (+ plen shown 1) (1+ y))))

(defun slash-login (ui args)
  (declare (ignore ui))
  (let* ((parts (remove "" (loop with s = 0
                                 for p = (position #\Space args :start s)
                                 collect (subseq args s p)
                                 while p do (setf s (1+ p)))
                        :test #'string=))
         (name (first parts))
         (url (or (second parts)
                  (and name (getf (find name (models-provider-list)
                                        :key (lambda (p) (getf p :name)) :test #'equal)
                                  :base-url)))))
    (cond ((or (null name) (null url))
           (slash-say "Usage: /login NAME URL   (URL like https://api.example.com/v1; the key is asked next, masked)"))
          (t (login-wipe)
             (setf *login* (list name url (make-array 0 :element-type 'character
                                                        :adjustable t :fill-pointer 0)))
             (tui-dirty)))))

;;; ---------------------------------------------------------------

(defun models-install ()
  (define-slash "model" 'slash-model "pick provider/model, or /model P/M" 'slash-model-args)
  (define-slash "login" 'slash-login "/login NAME URL — add a provider (key asked masked)")
  (define-slash "logout" 'slash-logout "/logout NAME — remove a provider" 'slash-logout-args)
  (setf *tui-key-interceptors*
        (cons 'login-key (remove 'login-key *tui-key-interceptors*))))

(defun models-start ()
  (unless lisping-core:*test-boot* (models-install)))
