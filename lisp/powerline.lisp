;;; powerline.lisp — the separator style of the status bar (/powerline).
;;;
;;;   /powerline            show the current style and the choices
;;;   /powerline nerd       Nerd/Powerline font arrows (U+E0B0 / U+E0B2)
;;;   /powerline triangles  plain Unicode triangles, no patched font needed
;;;   /powerline flat       no glyphs, only the colour change between segments
;;; The choice is kept in (home-path "powerline") like /theme's.  The glyphs
;;; are the pair in *TUI-STATUS-ARROWS* (right-pointing . left-pointing); a
;;; flat style uses spaces, whose cell takes the next segment's background.

(defvar *powerline-styles*
  (list (list "nerd" (code-char #xE0B0) (code-char #xE0B2))
        (list "triangles" (code-char #x25BA) (code-char #x25C4))
        (list "flat" #\Space #\Space))
  "(NAME RIGHT-GLYPH LEFT-GLYPH)")

(defun powerline-file () (lisping-core:home-path "powerline"))

(defun powerline-names () (mapcar #'first *powerline-styles*))

(defun powerline-saved-name ()
  "The persisted style name, \"nerd\" when nothing is saved."
  (or (ignore-errors
       (let ((p (powerline-file)))
         (and (probe-file p)
              (let ((s (string-trim '(#\Space #\Newline #\Return) (lisping-core:read-file p))))
                (and (plusp (length s)) s)))))
      "nerd"))

(defun powerline-current-name ()
  (or (first (find-if (lambda (s) (and (eql (second s) (car *tui-status-arrows*))
                                       (eql (third s) (cdr *tui-status-arrows*))))
                      *powerline-styles*))
      "custom"))

(defun powerline-apply (name)
  "Make style NAME active; NIL when unknown."
  (let ((s (find name *powerline-styles* :key #'first :test #'string-equal)))
    (when s
      (setf *tui-status-arrows* (cons (second s) (third s)))
      t)))

(defun powerline-save (name)
  (ignore-errors
   (with-open-file (o (powerline-file) :direction :output :if-exists :supersede
                                       :if-does-not-exist :create)
     (write-line name o))))

(defun powerline-set (name)
  "Switch to NAME, remember it and repaint.  Returns a status string."
  (if (powerline-apply name)
      (progn (powerline-save (string-downcase name))
             (when (fboundp 'theme-redraw) (funcall 'theme-redraw))
             (format nil "powerline: ~A" (string-downcase name)))
      (format nil "unknown powerline style ~S (available: ~{~A~^, ~})" name (powerline-names))))

(defun slash-powerline (ui args)
  (declare (ignore ui))
  (let ((name (string-trim " " (or args ""))))
    (slash-say (if (string= name "")
                   (format nil "powerline: ~A (available: ~{~A~^, ~})"
                           (powerline-current-name) (powerline-names))
                   (powerline-set name)))))

(defun powerline-start ()
  "Called from init.lisp, before the UI starts: load the saved style and register /powerline."
  (powerline-apply (powerline-saved-name))
  (define-slash "powerline" 'slash-powerline
    "status bar separators: /powerline nerd | triangles | flat"
    (lambda (ui partial) (declare (ignore ui))
      (remove-if-not (lambda (n) (and (<= (length partial) (length n))
                                      (string-equal partial n :end2 (length partial))))
                     (powerline-names)))))
