;;;; lisp/edit.lisp -- surgical file edits, Emacs-style: change only the
;;;; lines that differ instead of retyping whole files. Registered as the
;;;; edit_file tool; every edit goes through the core's load check, so a
;;;; file only ever holds code that loaded cleanly.

(in-package :lisping)

(defun edit-file-replace (path old new)
  "Replace the single occurrence of OLD with NEW in the file at PATH.
Writes and loads the result via the core; returns a status string."
  (let ((text (with-open-file (s path :direction :input :external-format :utf-8)
                (with-output-to-string (out)
                  (loop for line = (read-line s nil nil)
                        while line do (write-line line out))))))
    (let ((pos (search old text)))
      (unless pos (error "edit_file: text not found in ~A: ~S" path (subseq old 0 (min 40 (length old)))))
      (when (search old text :start2 (1+ pos))
        (error "edit_file: text occurs more than once in ~A: ~S" path (subseq old 0 (min 40 (length old)))))
      (let ((result (lisping-core::write-file-and-load
                     path (concatenate 'string (subseq text 0 pos) new
                                       (subseq text (+ pos (length old)))))))
        (if (and (stringp result) (search "NOT SAVED" result))
            result
            (let ((stuck (with-open-file (in path :direction :input
                                                :external-format :utf-8)
                           (search new (with-output-to-string (o)
                                         (loop for l = (read-line in nil nil)
                                               while l do (write-line l o)))))))
              (if stuck
                  (format nil "edited ~A (~A)" path (or result "loaded and committed"))
                  (format nil "NOT SAVED ~A: change did not stick (~A)" path result))))))))

(defun edit-file-tool (args)
  (edit-file-replace (lisping-core::arg args "path")
                     (lisping-core::arg args "old")
                     (lisping-core::arg args "new")))

(lisping-core::register-tool
 "edit_file" "Surgically edit a file: replace the single occurrence of `old` with `new` in `path` (relative to project root), then write and load the file. Fails if `old` is absent or ambiguous, or if the result does not load cleanly (old version is restored)."
 (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj
                      "path" (lisping-core::obj "type" "string")
                      "old" (lisping-core::obj "type" "string")
                      "new" (lisping-core::obj "type" "string"))
        "required" #("path" "old" "new"))
 'edit-file-tool)

;;; ---------------------------------------------------------------
;;; replace_form: swap a whole top-level form by NAME. The old form's
;;; character range comes from reading the file with the Lisp reader;
;;; the replacement must read as exactly one form. No text matching,
;;; no paren-count slips.

(defun rf-read-file-string (path)
  "Whole file as a string (LF-normalized)."
  (with-open-file (s path :direction :input :external-format :utf-8)
    (with-output-to-string (out)
      (loop for line = (read-line s nil nil)
            while line do (write-line line out)))))

(defun rf-form-start (text pos)
  "Advance POS past whitespace, ;-to-EOL comments and (nested) #|...|#
blocks: the position of the next form's opening character. READ skips
the same trivia, so the char at the result is what it consumed first —
a quoting prefix (', #', #,) included."
  (let ((n (length text)))
    (loop
      (when (>= pos n) (return pos))
      (let ((c (char text pos)))
        (cond ((member c '(#\Space #\Tab #\Newline #\Return) :test #'char=)
               (incf pos))
              ((char= c #\;)
               (let ((nl (position #\Newline text :start pos)))
                 (setf pos (if nl (1+ nl) n))))
              ((and (char= c #\#) (< (1+ pos) n)
                    (char= (char text (1+ pos)) #\|))
               (let ((depth 1) (i (+ pos 2)))
                 (loop while (and (< i n) (plusp depth))
                       do (cond ((and (char= (char text i) #\#)
                                      (< (1+ i) n)
                                      (char= (char text (1+ i)) #\|))
                                 (incf depth) (incf i 2))
                                ((and (char= (char text i) #\|)
                                      (< (1+ i) n)
                                      (char= (char text (1+ i)) #\#))
                                 (decf depth) (incf i 2))
                                (t (incf i))))
                 (setf pos i)))
              (t (return pos)))))))

(defun rf-form-ranges (text)
  "Read TEXT with the Lisp reader; return (form start end) per top-level
form, START/END being character positions into TEXT. START is advanced
past inter-form whitespace and comments (RF-FORM-START): the raw
position before READ is the previous form's end, and replacing from
there would silently delete blank lines and comment headers."
  (let ((*read-eval* nil))
    (with-input-from-string (s text)
      (loop for raw = (file-position s)
            for form = (read s nil :eof)
            until (eq form :eof)
            collect (list form (rf-form-start text raw) (file-position s))))))

(defun rf-defines-p (form name)
  (and (consp form)
       (symbolp (car form))
       (member (car form)
               '(defun defvar defparameter defconstant defmacro defclass
                 defgeneric defmethod defstruct deftype)
               :test #'eq)
       (symbolp (cadr form))
       (string-equal (symbol-name (cadr form)) name)))

(defun rf-check-one-form (new-source)
  "NEW-SOURCE must read as exactly one complete form."
  (let ((*read-eval* nil))
    (with-input-from-string (s new-source)
      (let ((first (read s nil :eof)))
        (when (eq first :eof) (error "replace_form: new_source is empty"))
        (unless (eq (read s nil :eof) :eof)
          (error "replace_form: new_source holds more than one form"))))))

(defun replace-form (path name new-source)
  "Replace the whole top-level form defining NAME in PATH with
NEW-SOURCE, by character range found with the reader. NEW-SOURCE must
read as exactly one complete form; the result goes through the core's
write-and-load check."
  (let* ((text (rf-read-file-string path))
         (hits (remove-if-not (lambda (r) (rf-defines-p (first r) name))
                              (rf-form-ranges text))))
    (unless hits (error "replace_form: no top-level form defines ~A in ~A" name path))
    (when (cdr hits) (error "replace_form: ~A is defined ~D times in ~A" name (length hits) path))
    (rf-check-one-form new-source)
    (destructuring-bind (form start end) (first hits)
      (declare (ignore form))
      (let ((result (lisping-core::write-file-and-load
                     path (concatenate 'string (subseq text 0 start)
                                       new-source
                                       (subseq text end)))))
        (if (and (stringp result) (search "NOT SAVED" result))
            result
            (format nil "replaced ~A in ~A (~A)" name path (or result "loaded and committed")))))))

(defun replace-form-tool (args)
  (replace-form (lisping-core::arg args "path")
                (lisping-core::arg args "name")
                (lisping-core::arg args "new_source")))

(lisping-core::register-tool
 "replace_form"
 "Replace the whole top-level form (defun, defvar, defmacro, ...) defining `name` in `path` with `new_source`. The old form's character range is found by reading the file with the Lisp reader, and `new_source` must read as exactly one complete form — both checked before writing. The result then goes through the core's boot-checked write-and-load, like edit_file. Use this instead of edit_file when replacing an entire definition: no text matching, no paren-count slips."
 (lisping-core::obj "type" "object"
      "properties" (lisping-core::obj
                    "path" (lisping-core::obj "type" "string")
                    "name" (lisping-core::obj "type" "string")
                    "new_source" (lisping-core::obj "type" "string"))
      "required" #("path" "name" "new_source"))
 'replace-form-tool)
