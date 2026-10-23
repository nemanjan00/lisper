;;;; lisp/tui-input.lisp -- keyboard handling and the weather pane's
;;;; refresh loop. Runs in its own threads; state changes go through
;;;; *tui-lock* and end with TUI-DIRTY.

(in-package :lisping)

(declaim (ftype function tui-read-key tui-read-escape tui-read-csi tui-csi-key
                tui-prefix-key tui-normal-key tui-submit-input tui-input-backspace
                tui-clear-input tui-ctrl-d tui-scroll-chat tui-cycle-pane
                tui-weather-refresh))

(defvar *tui-quit* nil "Set by prefix q / C-d: the key loop then stops the UI.")

;;; ---------------------------------------------------------------
;;; Key reading. Returns a character or a keyword (:up :down :left
;;; :right :home :end :pgup :pgdn :delete :escape).

(defun tui-log-key (key)
  (ignore-errors
    (with-open-file (s (merge-pathnames "state/tmp/keys.log" *root*)
                       :direction :output :if-exists :append
                       :if-does-not-exist :create)
      (format s "~S~%" key))))

(defun tui-key-loop (ui)
  (setf *tui-quit* nil)
  (loop while (and *tui* (eq *tui* ui))
        do (handler-case
               (let ((key (tui-read-key ui)))
                 (tui-log-key key)
                 (sb-thread:with-mutex (*tui-lock*)
                   (when (and *tui* (eq *tui* ui))
                     (if (tui-state-prefix ui)
                         (tui-prefix-key ui key)
                         (tui-normal-key ui key))))
                 (tui-dirty)
                 (when *tui-quit* (return)))
             (serious-condition (c)
               (when (eq *tui* ui) (tui-crash c)))))
  (when (and *tui* (eq *tui* ui) *tui-quit*)
    (setf *tui-quit* nil)
    (tui-stop)))

(defun tui-read-key (ui)
  (let ((tty (tui-state-tty ui)))
    (let ((c (read-char tty)))
      (if (char= c #\Esc) (tui-read-escape tty) c))))

(defun tui-fd-ready-p (tty timeout)
  (not (eq (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd tty) :input timeout)
           :timeout)))

(defun tui-read-escape (tty)
  (if (tui-fd-ready-p tty 0.03)
      (let ((c (read-char tty)))
        (cond ((char= c #\[) (tui-read-csi tty))
              (t :escape)))
      :escape))

(defun tui-read-csi (tty)
  (let ((params (make-string-output-stream)))
    (loop for c = (read-char tty)
          until (or (alpha-char-p c) (char= c #\~))
          do (write-char c params)
          finally (return (tui-csi-key (get-output-stream-string params) c)))))

(defun tui-csi-key (params final)
  (cond ((and (string= params "") (char= final #\A)) :up)
        ((and (string= params "") (char= final #\B)) :down)
        ((and (string= params "") (char= final #\C)) :right)
        ((and (string= params "") (char= final #\D)) :left)
        ((and (string= params "") (char= final #\H)) :home)
        ((and (string= params "") (char= final #\F)) :end)
        ((and (string= params "5") (char= final #\~)) :pgup)
        ((and (string= params "6") (char= final #\~)) :pgdn)
        ((and (string= params "3") (char= final #\~)) :delete)
        (t :escape)))

;;; ---------------------------------------------------------------
;;; Key dispatch. C-b (#\Stx) is the prefix, like tmux.

(defun tui-prefix-key (ui key)
  (setf (tui-state-prefix ui) nil)
  (cond ((and (characterp key) (char-equal key #\q)) (setf *tui-quit* t))
        ((and (characterp key) (char= key #\1)) (setf (tui-state-active ui) 0))
        ((and (characterp key) (char= key #\2)) (setf (tui-state-active ui) 1))
        ((member key '(:left :right)) (tui-cycle-pane ui key))
        (t nil)))

(defun tui-normal-key (ui key)
  (cond ((and (characterp key) (char= key #\Stx)) (setf (tui-state-prefix ui) t))
        ((eq key :pgup) (tui-scroll-chat ui :pgup))
        ((eq key :pgdn) (tui-scroll-chat ui :pgdn))
        ((member key '(:left :right)) (tui-cycle-pane ui key))
        ((and (characterp key) (char= key #\Return)) (tui-submit-input ui))
        ((and (characterp key) (member key '(#\Rubout #\Backspace)))
         (tui-input-backspace ui))
        ((and (characterp key) (char= key #\Nak)) (tui-clear-input ui)) ; C-u
        ((and (characterp key) (char= key #\Etx)) (tui-clear-input ui)) ; C-c
        ((and (characterp key) (char= key #\Eot)) (tui-ctrl-d ui))       ; C-d
        ((and (characterp key) (graphic-char-p key))
         (vector-push-extend key (tui-state-input ui)))
        (t nil)))

(defun tui-submit-input (ui)
  (let* ((in (tui-state-input ui))
         (text (string-trim " " (coerce in 'string))))
    (when (plusp (length text))
      (setf (tui-state-pending-user ui) text
            (fill-pointer in) 0)
      (send-user-message text))))

(defun tui-input-backspace (ui)
  (let ((in (tui-state-input ui)))
    (when (plusp (length in)) (decf (fill-pointer in)))))

(defun tui-clear-input (ui)
  (setf (fill-pointer (tui-state-input ui)) 0))

(defun tui-ctrl-d (ui)
  (if (zerop (length (tui-state-input ui)))
      (setf *tui-quit* t)
      (tui-input-backspace ui)))

(defun tui-scroll-chat (ui key)
  (let* ((h (max 1 (- (tui-state-height ui) 4)))
         (delta (max 1 (floor h 2)))
         (s (tui-state-scroll ui)))
    (setf (tui-state-scroll ui)
          (max 0 (if (eq key :pgup) (+ s delta) (- s delta))))))

(defun tui-cycle-pane (ui key)
  (setf (tui-state-active ui)
        (mod (+ (tui-state-active ui) (if (eq key :left) -1 1)) 2)))

;;; ---------------------------------------------------------------
;;; Weather pane: wttr.in/Belgrade, refreshed every few minutes.

(defun tui-weather-loop (ui)
  (loop while (and *tui* (eq *tui* ui))
        do (handler-case
               (progn
                 (tui-weather-refresh ui)
                 (dotimes (k 300)
                   (unless (and *tui* (eq *tui* ui)) (return))
                   (sleep 1)))
             (serious-condition (c)
               (when (eq *tui* ui) (tui-crash c))))))

(defun tui-narrow-line (line)
  "Replace characters that are wide or ambiguous on a terminal with a
space: anything above Latin-1 except the box-drawing range."
  (map 'string (lambda (ch)
                 (let ((code (char-code ch)))
                   (if (or (< code 256)
                           (<= #x2500 code #x257f))
                       ch #\Space)))
       line))

(defun tui-weather-refresh (ui)
  (multiple-value-bind (out code) (shell "curl -s --max-time 20 'wttr.in/Belgrade?n'")
    (declare (ignore code))
    (when (and (stringp out) (plusp (length out)))
      (let ((buf (make-uibuf :align :top)))
        (dolist (line (ui-split-lines out))
          (unless (or (search "Follow" line) (search "igor_chubin" line))
            (uibuf-append buf (ui-parse-ansi (tui-narrow-line line)) :fresh t)))
        (sb-thread:with-mutex (*tui-lock*)
          (when (eq *tui* ui)
            (setf (tui-state-weather ui) buf
                  (tui-state-weather-at ui) (get-universal-time))))
        (tui-dirty)))))
