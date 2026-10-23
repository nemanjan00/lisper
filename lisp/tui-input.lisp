;;;; lisp/tui-input.lisp -- keyboard handling. Runs in its own thread;
;;;; state changes go through *tui-lock* and end with TUI-DIRTY.

(in-package :lisping)

(declaim (ftype function tui-read-key tui-read-escape tui-read-csi tui-csi-key
                tui-read-ss3 tui-parse-mouse tui-mouse tui-scroll-chat-by
                tui-prefix-key tui-normal-key tui-submit-input tui-input-backspace
                tui-clear-input tui-ctrl-d tui-scroll-chat))

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
        do (handler-bind ((serious-condition #'tui-log-crash-at-signal))
             (handler-case
                 (let ((key (tui-read-key ui)))
                   (tui-log-key key)
                   (sb-thread:with-recursive-lock (*tui-lock*)
                     (when (and *tui* (eq *tui* ui))
                       (if (tui-ui-prefix ui)
                           (tui-prefix-key ui key)
                           (tui-normal-key ui key))))
                   (tui-dirty)
                   (when *tui-quit* (return)))
               (serious-condition (c)
                 (when (eq *tui* ui) (tui-crash c))))))
  (when (and *tui* (eq *tui* ui) *tui-quit*)
    (setf *tui-quit* nil)
    (tui-stop)))

(defun tui-read-key (ui)
  (let ((tty (tui-ui-tty ui)))
    (let ((c (read-char tty)))
      (if (char= c #\Esc) (tui-read-escape tty) c))))

(defun tui-fd-ready-p (tty timeout)
  (not (eq (sb-sys:wait-until-fd-usable (sb-sys:fd-stream-fd tty) :input timeout)
           :timeout)))

(defun tui-read-escape (tty)
  "ESC arrived. The next byte may already sit in the stream's input
buffer — one read pulls several sequences off the fd at once, so an
fd-level wait would time out while LISTEN sees data. Read the buffered
char first; only when the buffer is empty wait on the fd (~0.15 s) for
bytes still in flight. A char that turns out not to start a sequence is
put back: it is real input, not part of the escape."
  (let ((c (or (read-char-no-hang tty)
               (when (tui-fd-ready-p tty 0.15) (read-char tty)))))
    (cond ((null c) :escape)
          ((char= c #\[) (tui-read-csi tty))
          ((char= c #\O) (tui-read-ss3 tty))
          (t (unread-char c tty) :escape))))

(defun tui-read-ss3 (tty)
  "ESC O <final>: application-mode cursor keys (and F-keys we ignore)."
  (case (read-char tty)
    (#\A :up)
    (#\B :down)
    (#\C :right)
    (#\D :left)
    (#\H :home)
    (#\F :end)
    (t :escape)))

(defun tui-read-csi (tty)
  (let ((params (make-string-output-stream)))
    (loop for c = (read-char tty)
          until (or (alpha-char-p c) (char= c #\~))
          do (write-char c params)
          finally (return (let ((p (get-output-stream-string params)))
                            (if (and (plusp (length p)) (char= (char p 0) #\<))
                                (tui-parse-mouse p c)
                                (tui-csi-key p c)))))))

(defun tui-parse-mouse (params final)
  "Parse the body of an SGR mouse sequence (\"<b;x;y\") with FINAL #\\M
(press) or #\\m (release) into (:mouse b x y kind), or NIL if unparseable.
B is the button code: 0-2 buttons, 64 wheel up, 65 wheel down, plus
modifier/motion bits we ignore."
  (ignore-errors
    (let* ((s1 (position #\; params))
           (s2 (and s1 (position #\; params :start (1+ s1)))))
      (when (and s1 s2)
        (list :mouse
              (parse-integer params :start 1 :end s1)
              (parse-integer params :start (1+ s1) :end s2)
              (parse-integer params :start (1+ s2))
              (if (char= final #\m) :release :press))))))

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
  (setf (tui-ui-prefix ui) nil)
  (cond ((and (characterp key) (char-equal key #\q)) (setf *tui-quit* t))
        ((and (characterp key) (char-equal key #\r)) (tui-force-redraw))
        (t nil)))

(defun tui-normal-key (ui key)
  (cond ((and (characterp key) (char= key #\Stx)) (setf (tui-ui-prefix ui) t))
        ;; The terminal wheel becomes Up/Down arrows when the host tmux
        ;; has mouse mode off — scroll a few lines per notch.
        ((eq key :up) (tui-scroll-chat-by ui 3))
        ((eq key :down) (tui-scroll-chat-by ui -3))
        ((and (consp key) (eq (first key) :mouse)) (tui-mouse ui key))
        ((eq key :pgup) (tui-scroll-chat ui :pgup))
        ((eq key :pgdn) (tui-scroll-chat ui :pgdn))
        ((and (characterp key)
              (member key '(#\Return #\Newline)))
         (tui-submit-input ui))
        ((and (characterp key) (member key '(#\Rubout #\Backspace)))
         (tui-input-backspace ui))
        ((and (characterp key) (char= key #\Nak)) (tui-clear-input ui)) ; C-u
        ((and (characterp key) (char= key #\Etx)) (tui-clear-input ui)) ; C-c
        ((and (characterp key) (char= key #\Eot)) (tui-ctrl-d ui))       ; C-d
        ((and (characterp key) (graphic-char-p key))
         (vector-push-extend key (tui-ui-input ui)))
        (t nil)))

(defun tui-submit-input (ui)
  (let* ((in (tui-ui-input ui))
         ;; COPY-SEQ: IN is already a string, so COERSE returns the same
         ;; object and STRING-TRIM may too — TEXT must not alias IN, or
         ;; resetting IN's fill-pointer below would blank the message
         ;; before SEND-USER-MESSAGE reads it.
         (text (string-trim " " (copy-seq in))))
    (when (plusp (length text))
      (setf (fill-pointer in) 0)
      ;; Echo it dimmed right away; the core's :queued/:user events and
      ;; TUI-SHOW-USER dedupe and upgrade this line.
      (tui-show-queued ui text)
      (send-user-message text))))

(defun tui-input-backspace (ui)
  (let ((in (tui-ui-input ui)))
    (when (plusp (length in)) (decf (fill-pointer in)))))

(defun tui-clear-input (ui)
  (setf (fill-pointer (tui-ui-input ui)) 0))

(defun tui-ctrl-d (ui)
  (if (zerop (length (tui-ui-input ui)))
      (setf *tui-quit* t)
      (tui-input-backspace ui)))

(defun tui-scroll-chat-by (ui delta)
  (declare (ignore ui))
  "Scroll the FOCUSED pane (the chat by default)."
  (pane-scroll-by *pane-focus* delta))

(defun tui-mouse (ui key)
  "Handle an SGR mouse event (:mouse b x y kind). The wheel scrolls the
pane under the pointer; a click focuses that pane. The input and status
rows belong to no pane and are ignored."
  (declare (ignore ui))
  (destructuring-bind (_ b x y kind) key
    (declare (ignore _))
    (let ((target (pane-at (1- x) (1- y))))  ; 0-based
      (cond ((and (eq kind :press) target (<= 64 b 67))
             (pane-scroll-by target (if (evenp b) 3 -3)))
            ((and (eq kind :press) target (<= b 2))
             (pane-focus-pane target))))))

(defun tui-scroll-chat (ui key)
  (declare (ignore ui))
  "Page the FOCUSED pane up or down."
  (pane-scroll-by *pane-focus* (if (eq key :pgup) 15 -15)))
