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
         ;; *CWD* is NIL in a fresh session: fall back to the process
         ;; directory, then home.
         (cwd (string-right-trim
               "/" (namestring (or lisping-core:*cwd*
                                   (ignore-errors (sb-posix:getcwd))
                                   home))))
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
                                     ;; T: follow symlinks, so a link to a
                                     ;; directory completes with a /.
                                     (eq (sb-impl::native-file-kind
                                          (concatenate 'string abs-dir name) t)
                                         :directory)))
                              out))))
        (sb-posix:closedir dir)))
    out))

(defun path-source (ui text start end)
  "Completion source: candidates for the path under the cursor, as
(insert . display) pairs.  Dotfiles are listed only once the typed
prefix starts with a dot; directories get a trailing /."
  (declare (ignore ui))
  (let ((word (subseq text start (min end (length text)))))
    (when (path-word-p word)
      (let ((slash (position #\/ word :from-end t)))
        (cond
          ;; bare "." / ".." / "~" complete to themselves plus a slash
          ((and (null slash) (member word '("." ".." "~") :test #'string=))
           (let ((s (concatenate 'string word "/")))
             (list (cons s s))))
          ((null slash) nil)
          (t
           (let* ((dir (subseq word 0 (1+ slash)))
                  (prefix (subseq word (1+ slash)))
                  (hits (sort (loop for (name . dirp) in (path-list-dir (path-expand-dir dir))
                                    when (and (<= (length prefix) (length name))
                                              (string= prefix name :end2 (length prefix))
                                              (not (find #\Space name))
                                              (or (plusp (length prefix))
                                                  (char/= (char name 0) #\.)))
                                      collect (cons name dirp))
                              #'string< :key #'car)))
             (loop for (name . dirp) in (subseq hits 0 (min 200 (length hits)))
                   collect (cons (concatenate 'string dir name (if dirp "/" ""))
                                 (concatenate 'string name (if dirp "/" ""))))))))))) 

(defun pathcomp-install ()
  (pushnew 'path-source *completion-sources*)
  t)

(defun pathcomp-start ()
  (unless lisping-core:*test-boot* (pathcomp-install)))
