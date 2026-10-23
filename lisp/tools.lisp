;;;; lisp/tools.lisp -- self-serve agent tools: a persistent todo list.
;;;; Loaded after the TUI; registers the "todo" tool.

(in-package :lisping)

(defvar *todos* nil
  "List of todo items, each (text . donep), oldest first, newest last:
indices stay stable as items are added.")

(defvar *todos-file* (merge-pathnames "state/todos.txt" lisping-core:*root*))

(defun todo-load ()
  "Read *TODOS-FILE* into *TODOS*: lines \"[ ] text\" / \"[x] text\".
WITH-OPEN-FILE with :IF-DOES-NOT-EXIST NIL binds the stream to NIL
when the file is absent and STILL RUNS THE BODY — guard with WHEN S,
or READ-Line would fall through to *STANDARD-INPUT* and block on the
user's terminal forever."
  (setf *todos* nil)
  (with-open-file (s *todos-file* :direction :input :if-does-not-exist nil)
    (when s
      (loop for line = (read-line s nil nil)
            while line
            when (>= (length line) 4)
              do (push (cons (subseq line 4) (char= (char line 1) #\x))
                       *todos*))))
  (setf *todos* (nreverse *todos*))
  *todos*)

(defun todo-save ()
  (with-open-file (s *todos-file* :direction :output :if-exists :supersede
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
  (setf *todos* nil)
  (todo-save)
  (ignore-errors (pane-dirty "todo"))
  "cleared")

(defun todo-tool (args)
  (let ((action (or (lisping-core::arg args "action") "list"))
        (text (lisping-core::arg args "text"))
        (index (lisping-core::arg args "index")))
    (ecase (and action (intern (string-upcase action) :keyword))
      (:add (todo-add (or text "")))
      (:done (todo-done index))
      (:list (todo-list))
      (:clear (todo-clear)))))

(todo-load)

(lisping-core::register-tool
 "todo" "Manage the persistent todo list in state/todos.txt (survives restarts). add: append an open item (needs text). done: mark an item done (needs index, 1-based as shown by list). list: numbered items, [ ] open / [x] done. clear: remove all items. Returns a confirmation line; list returns the items."
 (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj
                      "action" (lisping-core::obj "type" "string"
                                                 "enum" #("add" "done" "list" "clear"))
                      "text" (lisping-core::obj "type" "string")
                      "index" (lisping-core::obj "type" "integer"))
        "required" #("action"))
 'todo-tool)
