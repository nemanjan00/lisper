;;; pathcomp.lisp — Tab path completion (todo 10), a source for the
;;; completion popup in slash.lisp.
;;;
;;; The word under the cursor, when it starts with / ./ ../ or ~, is
;;; completed against the directory it names: relative paths are
;;; relative to *CWD*, ~ is the home directory.  Directories complete
;;; with a trailing / so Tab can be pressed again to go deeper.

(in-package :lisping)

(defun path-word-p (word)
  "True when WORD looks like a path to complete."
  (and (plusp (length word))
       (or (char= (char word 0) #\/)
           (char= (char word 0) #\~)
           (and (>= (length word) 2) (string= word "./" :end1 2))
           (and (>= (length word) 3) (string= word "../" :end1 3))
           (string= word ".") (string= word ".."))))

(defun path-expand-dir (dir)
  "DIR (the part of the word up to its last /, with the slash) as an
absolute directory string, or NIL."
  (let* ((home (string-right-trim "/" (namestring (user-homedir-pathname))))
         (cwd (string-right-trim "/" (namestring lisping-core:*cwd*)))
         (d (cond ((string= dir "") (concatenate 'string cwd "/"))
                  ((char= (char dir 0) #\~)
                   (concatenate 'string home (subseq dir 1)))
                  ((char= (char dir 0) #\/) dir)
                  (t (concatenate 'string cwd "/" dir)))))
    (if (and (plusp (length d)) (char= (char d (1- (length d))) #\/))
        d
        (concatenate 'string d "/"))))

(defun path-list-dir (abs-dir)
  "Entries of ABS-DIR as (name . directory-p), via readdir (no wildcard
parsing, so names with [ * ? are fine). NIL when unreadable."
  (let ((dir (ignore-errors (sb-posix:opendir abs-dir))) (out nil))
    (when dir
      (unwind-protect
           (loop for ent = (sb-posix:readdir dir)
                 until (sb-alien:null-alien ent)
                 do (let ((name (sb-posix:dirent-name ent)))
                      (unless (member name '("." "..") :test #'string=)
                        (push (cons name
                                    (ignore-errors
                                     (eq (sb-impl::native-file-kind
                                          (concatenate 'string abs-dir name))
                                         :directory)))
                              out))))
        (sb-posix:closedir dir)))
    out))

(defun path-source (ui text start end)
  "Completion source: candidates for the path under the cursor."
  (declare (ignore ui))
  (let ((word (subseq text start (min end (length text)))))
    (when (path-word-p word)
      (let* ((slash (position #\/ word :from-end t))
             ;; "~" and "." / ".." alone complete to themselves + /
             (dir (cond (slash (subseq word 0 (1+ slash)))
                        ((string= word "~") "~/")
                        (t "")))
             (prefix (if slash (subseq word (1+ slash)) ""))
             (entries (and (not (and (null slash) (string/= word "~")
                                     (not (member word '("." "..") :test #'string=))))
                           (path-list-dir (path-expand-dir dir)))))
        (cond
          ((and (null slash) (member word '("." "..") :test #'string=))
           (list (cons (concatenate 'string word "/") (concatenate 'string word "/"))))
          ((and (null slash) (string= word "~"))
           (list (cons "~/" "~/")))
          (t
           (let ((shown (sort
                         (loop for (name . dirp) in entries
                               when (and (<= (length prefix) (length name))
                                         (string= prefix name :end2 (length prefix))
                                         (not (find #\Space name))
                                         ;; dotfiles only when asked for
                                         (or (plusp (length prefix))
                                             (char/= (char name 0) #\.)
                                             nil)
                                         (or (zerop (length prefix))
                                             t))
                                 collect (cons name dirp))
                         #'string< :key #'car)))
             ;; With a "." prefix dotfiles are wanted.
             (when (and (plusp (length prefix)) (char= (char prefix 0) #\.))
               nil)
             (loop for (name . dirp) in (subseq shown 0 (min 200 (length shown)))
                   collect (let ((full (concatenate 'string dir name (if dirp "/" ""))))
                             (cons full (concatenate 'string name (if dirp "/" "")))))))))))) 

(defun pathcomp-install ()
  (pushnew 'path-source *completion-sources*)
  t)

(defun pathcomp-start ()
  (unless lisping-core:*test-boot* (pathcomp-install)))
