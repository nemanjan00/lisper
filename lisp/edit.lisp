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
