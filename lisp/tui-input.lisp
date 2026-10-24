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
  "DEPRECATED, singular. Use *TUI-MOUSE-INTERCEPTORS*.")

(defvar *tui-key-interceptors* nil
  "List of funcallables (ui key) -> consumed-p, walked in order by
TUI-NORMAL-KEY until one returns T. List elements are SYMBOLS (ask.lisp
uses 'ASK-KEY) so redefining the function takes effect without
re-setting the list.")

(defvar *tui-mouse-interceptors* nil
  "List of funcallables (ui key) -> consumed-p, walked in order by
TUI-MOUSE until one returns T.")

;;; The input cursor and hscroll are slots of TUI-UI (tui.lisp) —
;;; tui-render.lisp loads before this file and already draws the
;;; terminal cursor from them.

;;; ---------------------------------------------------------------
;;; Key reading. Returns a character or a keyword (:up :down :left
;;; :right :home :end :pgup :pgdn :delete :escape).

(defun tui-log-key (key)
  (ignore-errors
    (with-open-file (s (lisping-core:session-path "tmp/keys.log")
                       :direction :output :if-exists :append
                       :if-does-not-exist :create)
      (format s "~S~%" key))))

(defun tui-key-loop (ui)
  (setf *tui-quit* nil)
  (loop while (and *tui* (eq *tui* ui))
        do (handler-bind ((serious-condition #'tui-log-crash-at-signal))
             (handler-case
                 (let ((key (tui-read-key ui)))
                   (unless (and (boundp '*login*) (symbol-value '*login*)) ; a key being typed
                     (tui-log-key key))
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
          ;; ESC Return = Alt+Return: a newline in the input.
          ((member c '(#\Return #\Newline)) :newline)
          (t (unread-char c tty) :escape))))

(defun tui-read-paste (tty)
  "Bracketed paste: the text up to ESC [ 2 0 1 ~ as (:paste STRING), line
ends normalised to newlines. Waits up to a second per byte; a paste whose
end marker never comes ends at the timeout."
  (let ((end (format nil "~C[201~~" #\Esc))
        (buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for c = (tui-read-seq-char tty 1.0)
          while c
          do (vector-push-extend c buf)
             (let ((n (length buf)) (m (length end)))
               (when (and (>= n m) (string= end buf :start2 (- n m)))
                 (decf (fill-pointer buf) m)
                 (return))))
    (let ((s (coerce buf 'string)))
      (list :paste
            (with-output-to-string (o)
              (loop with i = 0 while (< i (length s))
                    do (let ((c (char s i)))
                         (cond ((and (char= c #\Return) (< (1+ i) (length s))
                                     (char= (char s (1+ i)) #\Newline))
                                nil)
                               ((char= c #\Return) (write-char #\Newline o))
                               (t (write-char c o))))
                       (incf i)))))))

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
                                (cond ((and (plusp (length p)) (char= (char p 0) #\<))
                                       (tui-parse-mouse p c))
                                      ((and (string= p "200") (char= c #\~))
                                       (tui-read-paste tty))
                                      (t (tui-csi-key p c)))))))))

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
        ;; Shift/Ctrl/Alt+Return where the terminal reports it: CSI u
        ;; (13;MOD u) or xterm modifyOtherKeys (27;MOD;13 ~).
        ((and (char= final #\u) (> (length params) 3) (string= "13;" params :end2 3))
         :newline)
        ((and (char= final #\~) (> (length params) 6) (string= "27;" params :end2 3)
              (string= ";13" params :start2 (- (length params) 3)))
         :newline)
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

(defun tui-queue-mode (ui)
  "Queue interaction state: :edit when a queued message is loaded in
the input line, :sel when one is selected in the queue list, else NIL."
  (cond ((tui-ui-queue-edit ui) :edit)
        ((tui-ui-queue-sel ui) :sel)))

(defun tui-queue-key (ui key)
  "Handle KEY while queue-selecting/editing. T when the key was
consumed. Selection: Up/Down move, Enter/e loads into the input line,
Esc cancels. Editing: Enter saves (empty deletes), Esc cancels, other
keys fall through to normal input editing."
  (let ((mode (tui-queue-mode ui)))
    (case mode
      (:sel
       (let* ((n (length (lisping-core:queued-messages)))
              (sel (tui-ui-queue-sel ui)))
         (cond ((zerop n)
                (setf (tui-ui-queue-sel ui) nil) t)   ; queue drained
               ((eq key :up)
                (setf (tui-ui-queue-sel ui) (max 0 (1- sel))) t)
               ((eq key :down)
                (if (< sel (1- n))
                    (setf (tui-ui-queue-sel ui) (1+ sel))
                    (setf (tui-ui-queue-sel ui) nil)) ; past newest: out
                t)
               ((or (eq key :escape)
                    (and (characterp key) (char= key #\Esc)))
                (setf (tui-ui-queue-sel ui) nil) t)
               ((or (and (characterp key) (member key '(#\Return #\Newline)))
                    (and (characterp key) (char-equal key #\e)))
                (let ((text (nth sel (lisping-core:queued-messages))))
                  (when text
                    ;; PIN-QUEUED with the very string QUEUED-MESSAGES gave:
                    ;; it can't be delivered while edited, and we keep the
                    ;; string (not an index, which shifts) as the handle.
                    (if (lisping-core:pin-queued text)
                        (let ((in (tui-ui-input ui)))
                          (setf (fill-pointer in) 0)
                          (loop for c across text do (vector-push-extend c in))
                          (setf (tui-ui-input-cursor ui) (length in)
                                (tui-ui-input-hscroll ui) 0
                                (tui-ui-queue-edit ui) text
                                (tui-ui-queue-sel ui) nil))
                        (progn
                          (setf (tui-ui-queue-sel ui) nil)
                          (terminal-emit
                           (list :type :info
                                 :text "That message was already delivered."))))))
                t)
               (t nil))))
      (:edit
       (cond ((or (eq key :escape)
                  (and (characterp key) (char= key #\Esc)))
              (lisping-core:release-queued (tui-ui-queue-edit ui))
              (setf (tui-ui-queue-edit ui) nil)
              (tui-clear-input ui)
              t)
             (t nil)))
      (t nil))))

(defvar *tui-submit-hooks* nil
  "Symbols/functions (ui text) -> handled-p, walked by TUI-SUBMIT-INPUT
with the trimmed input before it is sent to the model; the first to
return T consumes it (and must clear the input itself if it wants to).
Slash commands live here.")

(defun tui-line-bounds (ui &optional (pos (tui-ui-input-cursor ui)))
  "Start and end of the input line holding position POS."
  (let* ((in (tui-ui-input ui))
         (s (let ((p (position #\Newline in :end pos :from-end t))) (if p (1+ p) 0)))
         (e (or (position #\Newline in :start pos) (length in))))
    (values s e)))

(defun tui-cursor-line-move (ui delta)
  "Move the cursor one input line up (DELTA -1) or down (1), keeping the
column. True when it moved; NIL on the first/last line."
  (let ((in (tui-ui-input ui)) (cur (tui-ui-input-cursor ui)))
    (multiple-value-bind (s e) (tui-line-bounds ui cur)
      (let ((col (- cur s)))
        (cond ((and (minusp delta) (plusp s))
               (multiple-value-bind (ps pe) (tui-line-bounds ui (1- s))
                 (setf (tui-ui-input-cursor ui) (min pe (+ ps col)))
                 t))
              ((and (plusp delta) (< e (length in)))
               (multiple-value-bind (ns ne) (tui-line-bounds ui (1+ e))
                 (setf (tui-ui-input-cursor ui) (min ne (+ ns col)))
                 t)))))))

(defun tui-input-insert-string (ui text)
  "Insert TEXT (a paste) at the cursor: newlines kept, tabs to spaces,
other control characters dropped."
  (loop for c across text
        do (cond ((char= c #\Newline) (tui-input-insert ui c))
                 ((char= c #\Tab) (tui-input-insert ui #\Space))
                 ((graphic-char-p c) (tui-input-insert ui c)))))

(defun tui-normal-key (ui key)
  (when (or (and *tui-key-interceptor* (funcall *tui-key-interceptor* ui key))
            (some (lambda (f) (funcall f ui key)) *tui-key-interceptors*))
    (return-from tui-normal-key t))
  (when (tui-queue-key ui key)
    (return-from tui-normal-key t))
  (cond ((and (characterp key) (char= key #\Stx)) (setf (tui-ui-prefix ui) t))
        ;; The terminal wheel becomes Up/Down arrows when the host tmux
        ;; has mouse mode off — scroll a few lines per notch.
        ((eq key :up)
         (cond ((tui-cursor-line-move ui -1))   ; inside a multi-line input
               ((and (zerop (length (tui-ui-input ui)))
                     (lisping-core:queued-messages))
                ;; Empty input + Up: select the newest queued message.
                (setf (tui-ui-queue-sel ui)
                      (1- (length (lisping-core:queued-messages)))))
               (t (tui-scroll-chat-by ui 3))))
        ((eq key :down)
         (unless (tui-cursor-line-move ui 1)
           (tui-scroll-chat-by ui -3)))
        ((or (eq key :newline) (and (characterp key) (char= key #\Newline)))
         (tui-input-insert ui #\Newline))     ; Alt+Return, Ctrl-J
        ((and (consp key) (eq (first key) :paste))
         (tui-paste ui (second key)))
        ((and (consp key) (eq (first key) :mouse)) (tui-mouse ui key))
        ((eq key :pgup) (tui-scroll-chat ui :pgup))
        ((eq key :pgdn) (tui-scroll-chat ui :pgdn))
        ;; Input-line editing: arrows/Home/End move the cursor,
        ;; typing and Backspace/Delete act at the cursor.
        ((eq key :left) (tui-cursor-move ui -1))
        ((eq key :right) (tui-cursor-move ui 1))
        ((or (eq key :home) (and (characterp key) (char= key #\Soh))) ; C-a
         (setf (tui-ui-input-cursor ui) (tui-line-bounds ui)))
        ((or (eq key :end) (and (characterp key) (char= key #\Enq))) ; C-e
         (setf (tui-ui-input-cursor ui) (nth-value 1 (tui-line-bounds ui))))
        ((eq key :delete) (tui-input-delete ui))
        ((and (characterp key) (char= key #\Return))
         (tui-submit-input ui))
        ((and (characterp key) (member key '(#\Rubout #\Backspace)))
         (tui-input-backspace ui))
        ((and (characterp key) (char= key #\Nak)) (tui-clear-input ui)) ; C-u
        ;; C-c: stops a running turn when the input is empty, else clears it.
        ((and (characterp key) (char= key #\Etx))
         (unless (and (zerop (length (tui-ui-input ui)))
                      (fboundp 'interrupt-turn)
                      (funcall 'interrupt-turn))
           (tui-clear-input ui)))
        ;; Esc twice while a turn runs: interrupt it.
        ((or (eq key :escape) (and (characterp key) (char= key #\Esc)))
         (when (fboundp 'interrupt-esc) (funcall 'interrupt-esc)))
        ((and (characterp key) (char= key #\Eot)) (tui-ctrl-d ui))       ; C-d
        ((and (characterp key) (graphic-char-p key))
         (tui-input-insert ui key))
        (t nil)))

(defvar *tui-pastes* nil
  "Alist (PLACEHOLDER . FULL-TEXT) of big pastes shown in the input line
as a short placeholder such as [pasted 29 lines].")

(defun tui-submit-input (ui)
  (let* ((in (tui-ui-input ui))
         ;; COPY-SEQ: IN is already a string, so COERCE returns the same
         ;; object and STRING-TRIM may too — TEXT must not alias IN, or
         ;; resetting IN's fill-pointer below would blank the message
         ;; before SEND-USER-MESSAGE reads it.
         (text (tui-expand-pastes (string-trim " " (copy-seq in)))))
    ;; Editing a queued message: Enter saves it back (empty deletes),
    ;; it is NOT sent as a new message.  QUEUE-EDIT is the pinned
    ;; string; EDIT-QUEUED returns NIL when it was delivered anyway
    ;; (e.g. a urgent !) — then the text stays in the input line so
    ;; the user can send it again as a new message.
    (when (tui-ui-queue-edit ui)
      (let ((handle (tui-ui-queue-edit ui)))
        (setf (tui-ui-queue-edit ui) nil)
        (cond ((lisping-core:edit-queued handle
                                         (and (plusp (length text)) text))
               (setf (fill-pointer in) 0
                     (tui-ui-input-cursor ui) 0))
              (t (terminal-emit
                  (list :type :info
                        :text "That message was already sent; your edit is still in the input line — Enter sends it again as a new message.")))))
      (return-from tui-submit-input t))
    ;; A hook returns T (consumed), a string (send that instead of the
    ;; typed text), or NIL (not mine).
    (when (plusp (length text))
      (dolist (f *tui-submit-hooks*)
        (let ((r (funcall f ui text)))
          (cond ((stringp r) (setf text r) (return))
                (r (return-from tui-submit-input t))))))
    (when (plusp (length text))
      (setf (fill-pointer in) 0
            (tui-ui-input-cursor ui) 0
            *tui-pastes* nil)
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

(defvar *tui-paste-min-lines* 3 "Pastes of at least this many lines are compacted.")
(defvar *tui-paste-min-chars* 200 "... or at least this many characters.")

(defun tui-paste-label (text)
  (let ((lines (1+ (count #\Newline (string-right-trim '(#\Newline) text)))))
    (if (> lines 1)
        (format nil "pasted ~D lines" lines)
        (format nil "pasted ~D chars" (length text)))))

(defun tui-paste (ui text)
  "A bracketed paste: short text is inserted as typed; a big one becomes
a placeholder in the input line (the full text is kept in *TUI-PASTES*
and put back by TUI-EXPAND-PASTES when the message is sent)."
  (if (or (>= (1+ (count #\Newline (string-right-trim '(#\Newline) text)))
              *tui-paste-min-lines*)
          (>= (length text) *tui-paste-min-chars*))
      (let* ((base (tui-paste-label text))
             (name (format nil "[~A]" base)))
        (when (zerop (length (tui-ui-input ui))) (setf *tui-pastes* nil))
        ;; Two pastes of the same size need different placeholders.
        (loop for n from 2
              while (or (assoc name *tui-pastes* :test #'string=)
                        (search name (coerce (tui-ui-input ui) 'string)))
              do (setf name (format nil "[~A #~D]" base n)))
        (push (cons name text) *tui-pastes*)
        (tui-input-insert-string ui name))
      (tui-input-insert-string ui text)))

(defun tui-expand-pastes (text)
  "TEXT with every paste placeholder replaced by the pasted text."
  (let ((out text))
    (dolist (p *tui-pastes* out)
      (loop for pos = (search (car p) out)
            while pos
            do (setf out (concatenate 'string (subseq out 0 pos) (cdr p)
                                      (subseq out (+ pos (length (car p))))))))))

(defun tui-paste-before (ui)
  "Length of the placeholder that ends at the cursor, or NIL."
  (let ((text (coerce (tui-ui-input ui) 'string))
        (cur (tui-ui-input-cursor ui)))
    (loop for p in *tui-pastes*
          for n = (length (car p))
          when (and (>= cur n) (string= (car p) text :start2 (- cur n) :end2 cur))
            return n)))

(defun tui-paste-after (ui)
  "Length of the placeholder that starts at the cursor, or NIL."
  (let ((text (coerce (tui-ui-input ui) 'string))
        (cur (tui-ui-input-cursor ui)))
    (loop for p in *tui-pastes*
          for n = (length (car p))
          when (and (<= (+ cur n) (length text))
                    (string= (car p) text :start2 cur :end2 (+ cur n)))
            return n)))

(defun tui-cursor-move (ui delta)
  "Move the input cursor DELTA characters, clamped to the buffer; one
step over a paste placeholder skips all of it."
  (let ((len (length (tui-ui-input ui)))
        (step (cond ((= delta -1) (- (or (tui-paste-before ui) 1)))
                    ((= delta 1) (or (tui-paste-after ui) 1))
                    (t delta))))
    (setf (tui-ui-input-cursor ui)
          (max 0 (min len (+ (tui-ui-input-cursor ui) step))))))

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
  (let* ((in (tui-ui-input ui))
         (cur (tui-ui-input-cursor ui))
         (n (or (tui-paste-after ui) 1)))
    (when (< cur (length in))
      (replace in in :start1 cur :start2 (+ cur n))
      (decf (fill-pointer in) n))))

(defun tui-input-backspace (ui)
  "Delete the character before the cursor."
  (let* ((in (tui-ui-input ui))
         (cur (tui-ui-input-cursor ui))
         (n (or (tui-paste-before ui) 1)))
    (when (plusp cur)
      (replace in in :start1 (- cur n) :start2 cur)
      (decf (fill-pointer in) n)
      (setf (tui-ui-input-cursor ui) (- cur n)))))

(defun tui-clear-input (ui)
  (setf (fill-pointer (tui-ui-input ui)) 0
        (tui-ui-input-cursor ui) 0
        *tui-pastes* nil))

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
  (when (or (and *tui-mouse-interceptor* (funcall *tui-mouse-interceptor* ui key))
            (some (lambda (f) (funcall f ui key)) *tui-mouse-interceptors*))
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
