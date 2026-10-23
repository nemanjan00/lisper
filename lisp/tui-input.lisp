;;;; lisp/tui-input.lisp -- keyboard handling. Runs in its own thread;
;;;; state changes go through *tui-lock* and end with TUI-DIRTY.

(in-package :lisping)

(declaim (ftype function tui-read-key tui-read-escape tui-read-csi tui-csi-key
                tui-read-ss3 tui-parse-mouse tui-mouse tui-scroll-chat-by
                tui-prefix-key tui-normal-key tui-submit-input tui-input-backspace
                tui-clear-input tui-ctrl-d tui-scroll-chat
                tui-cursor-move tui-input-insert tui-input-delete
                pane-scroll-by pane-at pane-focus-pane
                pane-focus-nth pane-focus-step))

(declaim (special *pane-focus*))

(defvar *tui-quit* nil "Set by prefix q / C-d: the key loop then stops the UI.")

(defvar *tui-key-interceptor* nil
  "NIL or a funcallable (ui key) -> consumed-p, called first by
TUI-NORMAL-KEY. A SYMBOL (ask.lisp sets 'ASK-KEY) so redefining the
function takes effect without re-setting the variable.")

(defvar *tui-mouse-interceptor* nil
  "NIL or a funcallable (ui key) -> consumed-p, called first by TUI-MOUSE.")

;;; The input cursor and hscroll are slots of TUI-UI (tui.lisp) —
;;; tui-render.lisp loads before this file and already draws the
;;; terminal cursor from them.

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

(defun tui-read-seq-char (tty timeout)
  "Next sequence byte: buffered char first, else wait on the fd for
bytes still in flight. Never blocks past TIMEOUT."
  (or (read-char-no-hang tty)
      (when (tui-fd-ready-p tty timeout) (read-char tty))))

(defun tui-read-escape (tty)
  "ESC arrived. The next byte may already sit in the stream's input
buffer — one read pulls several sequences off the fd at once, so an
fd-level wait would time out while LISTEN sees data. Read the buffered
char first; only when the buffer is empty wait on the fd for bytes
still in flight. A char that turns out not to start a sequence is put
back: it is real input, not part of the escape. Home/End also arrive
bare (ESC H / ESC F / ESC E) when tmux passes them through without the
CSI or SS3 introducer."
  (let ((c (tui-read-seq-char tty 0.1)))
    (cond ((null c) :escape)
          ((char= c #\[) (tui-read-csi tty))
          ((char= c #\O) (tui-read-ss3 tty))
          ((char= c #\H) :home)
          ((member c '(#\F #\E)) :end)
          (t (unread-char c tty) :escape))))

(defun tui-read-ss3 (tty)
  "ESC O <final>: application-mode cursor keys (and F-keys we ignore).
Per-byte timeout: a split sequence bails out instead of swallowing the
bytes of the next key."
  (case (tui-read-seq-char tty 0.05)
    (#\A :up)
    (#\B :down)
    (#\C :right)
    (#\D :left)
    (#\H :home)
    (#\F :end)
    (t :escape)))

(defun tui-read-csi (tty)
  "ESC [ <params> <final>. Per-byte timeout so a sequence whose bytes
arrive split (or a bare ESC followed by typed text) can never block on
a final byte that belongs to the next key."
  (let ((params (make-string-output-stream)))
    (loop for c = (tui-read-seq-char tty 0.05)
          until (or (null c) (alpha-char-p c) (char= c #\~))
          do (write-char c params)
          finally (return (if (null c)
                              :escape
                              (let ((p (get-output-stream-string params)))
                                (if (and (plusp (length p)) (char= (char p 0) #\<))
                                    (tui-parse-mouse p c)
                                    (tui-csi-key p c))))))))

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
        ;; Home/End as CSI ~-forms: 1~/7~ and 4~/8~ (screen-256color
        ;; sends 1~ and 4~).
        ((and (member params '("1" "7") :test #'string=) (char= final #\~))
         :home)
        ((and (member params '("4" "8") :test #'string=) (char= final #\~))
         :end)
        (t (tui-log-key (list :unknown-csi params final))
           :escape)))

;;; ---------------------------------------------------------------
;;; Key dispatch. C-b (#\Stx) is the prefix, like tmux.

(defun tui-prefix-key (ui key)
  (setf (tui-ui-prefix ui) nil)
  (cond ((and (characterp key) (char-equal key #\q)) (setf *tui-quit* t))
        ((and (characterp key) (char-equal key #\r)) (tui-force-redraw))
        ;; Pane switching: arrows step through panes in layout order,
        ;; digits focus the Nth pane directly.
        ((eq key :left) (pane-focus-step -1))
        ((eq key :right) (pane-focus-step 1))
        ((and (characterp key) (digit-char-p key))
         (pane-focus-nth (digit-char-p key)))
        (t nil)))

(defun tui-normal-key (ui key)
  (when (and *tui-key-interceptor* (funcall *tui-key-interceptor* ui key))
    (return-from tui-normal-key t))
  (cond ((and (characterp key) (char= key #\Stx)) (setf (tui-ui-prefix ui) t))
        ;; The terminal wheel becomes Up/Down arrows when the host tmux
        ;; has mouse mode off — scroll a few lines per notch.
        ((eq key :up) (tui-scroll-chat-by ui 3))
        ((eq key :down) (tui-scroll-chat-by ui -3))
        ((and (consp key) (eq (first key) :mouse)) (tui-mouse ui key))
        ((eq key :pgup) (tui-scroll-chat ui :pgup))
        ((eq key :pgdn) (tui-scroll-chat ui :pgdn))
        ;; Input-line editing: arrows/Home/End move the cursor,
        ;; typing and Backspace/Delete act at the cursor.
        ((eq key :left) (tui-cursor-move ui -1))
        ((eq key :right) (tui-cursor-move ui 1))
        ((eq key :home) (setf (tui-ui-input-cursor ui) 0))
        ((eq key :end) (setf (tui-ui-input-cursor ui)
                             (length (tui-ui-input ui))))
        ((eq key :delete) (tui-input-delete ui))
        ((and (characterp key)
              (member key '(#\Return #\Newline)))
         (tui-submit-input ui))
        ((and (characterp key) (member key '(#\Rubout #\Backspace)))
         (tui-input-backspace ui))
        ((and (characterp key) (char= key #\Nak)) (tui-clear-input ui)) ; C-u
        ((and (characterp key) (char= key #\Etx)) (tui-clear-input ui)) ; C-c
        ((and (characterp key) (char= key #\Eot)) (tui-ctrl-d ui))       ; C-d
        ((and (characterp key) (graphic-char-p key))
         (tui-input-insert ui key))
        (t nil)))

(defun tui-submit-input (ui)
  (let* ((in (tui-ui-input ui))
         ;; COPY-SEQ: IN is already a string, so COERCE returns the same
         ;; object and STRING-TRIM may too — TEXT must not alias IN, or
         ;; resetting IN's fill-pointer below would blank the message
         ;; before SEND-USER-MESSAGE reads it.
         (text (string-trim " " (copy-seq in))))
    (when (plusp (length text))
      (setf (fill-pointer in) 0
            (tui-ui-input-cursor ui) 0)
      ;; Enter always jumps the chat to the bottom and clears the
      ;; "N new" marker.
      (let ((p (pane-of-kind :chat)))
        (when p
          (setf (pane-scroll p) 0
                *tui-chat-new* 0
                (pane-needs-redraw p) 1)))
      ;; Echo it dimmed right away; the core's :queued/:user events and
      ;; TUI-SHOW-USER dedupe and upgrade this line.
      (tui-show-queued ui text)
      (send-user-message text))))

(defun tui-cursor-move (ui delta)
  "Move the input cursor DELTA characters, clamped to the buffer."
  (let ((len (length (tui-ui-input ui))))
    (setf (tui-ui-input-cursor ui)
          (max 0 (min len (+ (tui-ui-input-cursor ui) delta))))))

(defun tui-input-insert (ui c)
  "Insert C at the cursor, shifting the tail right. REPLACE copies as if
via an intermediate, so the overlapping shift is safe."
  (let* ((in (tui-ui-input ui))
         (len (length in))
         (cur (tui-ui-input-cursor ui)))
    (vector-push-extend #\Nul in)     ; grow one slot at the end
    (replace in in :start1 (1+ cur) :start2 cur :end2 len)
    (setf (char in cur) c)
    (setf (tui-ui-input-cursor ui) (1+ cur))))

(defun tui-input-delete (ui)
  "Delete the character under the cursor (the Delete key)."
  (let ((in (tui-ui-input ui))
        (cur (tui-ui-input-cursor ui)))
    (when (< cur (length in))
      (replace in in :start1 cur :start2 (1+ cur))
      (decf (fill-pointer in)))))

(defun tui-input-backspace (ui)
  "Delete the character before the cursor."
  (let ((in (tui-ui-input ui))
        (cur (tui-ui-input-cursor ui)))
    (when (plusp cur)
      (replace in in :start1 (1- cur) :start2 cur)
      (decf (fill-pointer in))
      (setf (tui-ui-input-cursor ui) (1- cur)))))

(defun tui-clear-input (ui)
  (setf (fill-pointer (tui-ui-input ui)) 0
        (tui-ui-input-cursor ui) 0))

(defun tui-ctrl-d (ui)
  (if (zerop (length (tui-ui-input ui)))
      (setf *tui-quit* t)
      (tui-input-backspace ui)))

(defun tui-scroll-chat-by (ui delta)
  "Scroll the FOCUSED pane (the chat by default)."
  (declare (ignore ui))
  (pane-scroll-by *pane-focus* delta))

(defun tui-mouse (ui key)
  "Handle an SGR mouse event (:mouse b x y kind). The wheel scrolls the
pane under the pointer; a click focuses that pane. A press on an hsplit
separator column starts a drag: motion events (bit 5 of B) move the
split's ratio, repainting at most ~10 times a second (the SSH budget),
and the release does one full redraw and saves the layout. The input
and status rows belong to no pane and are ignored."
  (declare (ignore ui))
  (when (and *tui-mouse-interceptor* (funcall *tui-mouse-interceptor* ui key))
    (return-from tui-mouse t))
  (destructuring-bind (_ b x y kind) key
    (declare (ignore _))
    (let ((x0 (1- x)) (y0 (1- y)))            ; SGR coords are 1-based
      (cond
        ;; Release while dragging: finish, repaint from scratch, persist.
        ((and (eq kind :release) *pane-drag*)
         (setf *pane-drag* nil)
         (tui-force-redraw)
         (panes-save))
        ;; Motion with a button held (bit 5): resize the dragged split,
        ;; throttled — skipped events leave nothing dirty, so the
        ;; redraw loop's pass is a cheap no-op.
        ((and (eq kind :press) (logbitp 5 b) *pane-drag*)
         (when (>= (- (get-internal-real-time) *pane-drag-last-draw*)
                   (truncate internal-time-units-per-second 10))
           (setf *pane-drag-last-draw* (get-internal-real-time))
           (pane-drag-to x0 y0)
           (panes-mark-all-dirty)))
        ;; Press: on a separator start a drag; wheel scrolls; else focus.
        ((eq kind :press)
         (let ((target (pane-at x0 y0))
               (gap (split-gap-at x0 y0)))
           (cond ((and gap (<= (logand b 3) 2) (zerop (logand b 64)))
                  (setf *pane-drag* gap
                        *pane-drag-last-draw* 0))
                 ((and target (<= 64 b 67))
                  (pane-scroll-by target (if (evenp b) 3 -3)))
                 ((and target (<= (logand b 3) 2))
                  (pane-focus-pane target)))))
        (t nil)))))

(defun tui-scroll-chat (ui key)
  "Page the FOCUSED pane up or down."
  (declare (ignore ui))
  (pane-scroll-by *pane-focus* (if (eq key :pgup) 15 -15)))
