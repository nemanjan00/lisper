;;;; lisp/tools.lisp -- self-serve agent tools: a persistent todo list.
;;;; Loaded after the TUI; registers the "todo" tool.

(in-package :lisping)

(defvar *todos* nil
  "List of todo items, each (text . donep), oldest first, newest last:
indices stay stable as items are added.")

;;; Paths resolve lazily: *HOME* is NIL in test-boot's fresh process (only
;;; BOOT sets it), so SESSION-PATH can't run in a load-time defvar.
(defvar *todos-file* nil "This session's todos.txt (see TODOS-FILE).")
(defvar *todos-done-file* nil "This session's todos-done.txt (see TODOS-DONE-FILE).")

(defun todos-file ()
  (or *todos-file* (setf *todos-file* (lisping-core:session-path "todos.txt"))))

(defun todos-done-file ()
  "Finished todos are appended here, under a date header, by TODO-CLEAR."
  (or *todos-done-file* (setf *todos-done-file* (lisping-core:session-path "todos-done.txt"))))

(defun todo-load ()
  "Read *TODOS-FILE* into *TODOS*: lines \"[ ] text\" / \"[x] text\".
WITH-OPEN-FILE with :IF-DOES-NOT-EXIST NIL binds the stream to NIL
when the file is absent and STILL RUNS THE BODY — guard with WHEN S,
or READ-Line would fall through to *STANDARD-INPUT* and block on the
user's terminal forever."
  (setf *todos* nil)
  (with-open-file (s (todos-file) :direction :input :if-does-not-exist nil)
    (when s
      (loop for line = (read-line s nil nil)
            while line
            when (>= (length line) 4)
              do (push (cons (subseq line 4) (char= (char line 1) #\x))
                       *todos*))))
  (setf *todos* (nreverse *todos*))
  *todos*)

(defun todo-save ()
  (with-open-file (s (todos-file) :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
    (dolist (item *todos*)
      (format s "[~C] ~A~%" (if (cdr item) #\x #\Space) (car item)))))

(defun todo-add (text)
  (setf *todos* (append *todos* (list (cons text nil))))
  (todo-save)
  (ignore-errors (pane-dirty "todo"))
  (format nil "added todo ~D: ~A" (length *todos*) text))

(defun todo-done (index)
  "Mark the 1-based INDEXth todo done."
  (if (and (integerp index) (<= 1 index (length *todos*)))
      (let ((item (nth (1- index) *todos*)))
        (setf (cdr item) t)
        (todo-save)
        (ignore-errors (pane-dirty "todo"))
        (format nil "done: ~A" (car item)))
      (format nil "no todo at index ~A (~D todos)" index (length *todos*))))

(defun todo-list ()
  (if (null *todos*)
      "no todos"
      (with-output-to-string (s)
        (loop for item in *todos*
              for i from 1
              do (format s "~D. [~C] ~A~%" i (if (cdr item) #\x #\Space) (car item))))))

(defun todo-clear ()
  "Move finished items to *TODOS-DONE-FILE* (appended under a date header)
and keep only the open ones in *TODOS-FILE*. Open items keep their order;
indices shift, so LIST before marking anything else done."
  (let ((done (remove-if-not #'cdr *todos*))
        (open (remove-if #'cdr *todos*)))
    (if (null done)
        "no finished todos to clear"
        (progn
          (multiple-value-bind (sec min hour day month year)
              (decode-universal-time (get-universal-time))
            (declare (ignore sec min hour))
            (with-open-file (s (todos-done-file) :direction :output
                              :if-exists :append :if-does-not-exist :create)
              (format s "~&--- ~4,'0D-~2,'0D-~2,'0D ---~%" year month day)
              (dolist (item done)
                (format s "[x] ~A~%" (car item)))))
          (setf *todos* open)
          (todo-save)
          (ignore-errors (pane-dirty "todo"))
          (format nil "cleared ~D finished todo~:P to todos-done.txt; ~D open remain"
                  (length done) (length open))))))

(defun todo-tool (args)
  (let ((action (or (lisping-core::arg args "action") "list"))
        (text (lisping-core::arg args "text"))
        (index (lisping-core::arg args "index")))
    (ecase (and action (intern (string-upcase action) :keyword))
      (:add (todo-add (or text "")))
      (:done (todo-done index))
      (:list (todo-list))
      (:clear (todo-clear)))))

(unless lisping-core:*test-boot* (todo-load))

(lisping-core::register-tool
 "todo" "Manage the persistent todo list in the session's todos.txt (survives restarts). add: append an open item (needs text). done: mark an item done (needs index, 1-based as shown by list). list: numbered items, [ ] open / [x] done. clear: move finished items to todos-done.txt (appended, with the date) and keep only open ones. Returns a confirmation line; list returns the items. Example: {\"action\":\"done\",\"index\":3}."
 (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj
                      "action" (lisping-core::obj "type" "string"
                                                 "enum" #("add" "done" "list" "clear"))
                      "text" (lisping-core::obj "type" "string")
                      "index" (lisping-core::obj "type" "integer"))
        "required" #("action"))
 'todo-tool)
