;;; theme.lisp — choosing the TUI colour theme (todo 11).
;;;
;;; The palettes live in ansi.lisp (define-tui-theme, *tui-themes*); all
;;; drawing code uses the 8-bit attr slots, so switching a theme only
;;; changes how ui-attr-codes turns a slot into SGR.  The choice is kept
;;; in (home-path "theme") — "dracula" (the default) or "off" for the
;;; terminal's own 16 colours — and changed with /theme NAME.

(defun theme-file () (lisping-core:home-path "theme"))

(defun theme-saved-name ()
  "The persisted theme name, \"dracula\" when nothing is saved."
  (or (ignore-errors
        (let ((p (theme-file)))
          (and (probe-file p)
               (let ((s (string-trim '(#\Space #\Newline #\Return)
                                     (lisping-core:read-file p))))
                 (and (plusp (length s)) s)))))
      "dracula"))

(defun theme-names ()
  (append (mapcar #'car *tui-themes*) (list "off")))

(defun theme-apply (name)
  "Make theme NAME active (\"off\" = plain ANSI); NIL when unknown."
  (cond ((string-equal name "off") (setf *tui-theme* nil) t)
        (t (let ((th (cdr (assoc name *tui-themes* :test #'string-equal))))
             (when th (setf *tui-theme* th) t)))))

(defun theme-redraw ()
  "Repaint everything in the new colours."
  (when (and *tui* (fboundp 'tui-drop-frame))
    (ignore-errors (funcall 'tui-drop-frame))
    (ignore-errors (funcall 'tui-force-redraw))))

(defun theme-save (name)
  (ignore-errors
    (with-open-file (o (theme-file) :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
      (write-line name o))))

(defun theme-set (name)
  "Switch to NAME, remember it and repaint.  Returns a status string."
  (if (theme-apply name)
      (progn (theme-save (string-downcase name)) (theme-redraw)
             (format nil "theme: ~A" (string-downcase name)))
      (format nil "unknown theme ~S (available: ~{~A~^, ~})" name (theme-names))))

(defun slash-theme (ui args)
  (declare (ignore ui))
  (let ((name (string-trim " " args)))
    (slash-say (if (string= name "")
                   (format nil "theme: ~A (available: ~{~A~^, ~})"
                           (if *tui-theme* (theme-name *tui-theme*) "off") (theme-names))
                   (theme-set name)))))

(defun slash-theme-args (ui partial)
  (declare (ignore ui))
  (loop for n in (theme-names)
        when (and (<= (length partial) (length n))
                  (string-equal partial n :end2 (length partial)))
          collect n))

(defun theme-start ()
  "Called from init.lisp, before the UI starts: load the saved theme and
register /theme."
  (theme-apply (theme-saved-name))
  (unless lisping-core:*test-boot* (theme-redraw))
  (define-slash "theme" 'slash-theme "colour theme: /theme dracula | off" 'slash-theme-args))
