;;;; lisp/tui-render.lisp -- turning agent events into chat lines and
;;;; compositing panes, status bar and input line into the grid.
;;;; Draws only from the redraw thread; takes *tui-lock* while reading.

(in-package :lisping)

;;; ---------------------------------------------------------------
;;; Hooks: events in, chat buffer out.

(defun tui-install-hooks ()
  (setf *emit* 'tui-emit)
  (unless (member 'tui-before-request *before-request*)
    (setf *before-request* (append *before-request* (list 'tui-before-request)))))

(defun tui-emit (event)
  "Receives every agent event. With no UI running, print like the core."
  (if (null *tui*)
      (terminal-emit event)
      (handler-case (tui-log-event event)
        (serious-condition (c) (tui-crash c)))))

(defun tui-before-request (body)
  "Show the newest user message (covers the queue and the pre-restart core
that does not emit :user events)."
  (when *tui*
    (handler-case
        (let* ((msgs (gethash "messages" body))
               (last (and (vectorp msgs) (plusp (length msgs))
                          (aref msgs (1- (length msgs))))))
          (when (and last (string= (gethash "role" last) "user")
                     (stringp (gethash "content" last)))
            (tui-show-user (gethash "content" last))))
      (serious-condition (c) (tui-crash c))))
  body)

(defun tui-show-user (text)
  "Log a user message unless it is already on screen."
  (sb-thread:with-mutex (*tui-lock*)
    (when (and *tui* (not (equal text (tui-state-shown-user *tui*))))
      (setf (tui-state-shown-user *tui*) text
            (tui-state-pending-user *tui*) nil
            (tui-state-scroll *tui*) 0)
      (uibuf-append (tui-state-chat *tui*)
                    (list (cons "you ❯ " (ui-attr 2 :bright t))
                          (cons text (ui-attr 2 :bright t)))
                    :fresh t)))
  (tui-dirty))

(defun tui-log-event (event)
  (let ((type (getf event :type)))
    (case type
      (:assistant (tui-chat-add (ui-parse-ansi (getf event :text))))
      (:thinking (tui-chat-add (list (cons (getf event :text) 8))))
      (:tool-call
       (tui-chat-add
        (list (cons (format nil "⚙ ~A ~A"
                            (getf event :name)
                            (tui-short-args (getf event :args)))
                    (ui-attr 4 :bright t)))))
      (:tool-result (tui-chat-result (getf event :name) (getf event :text)))
      (:error (tui-chat-add (ui-parse-ansi (getf event :text))))
      (:info (tui-chat-add (list (cons (getf event :text) 8))))
      (:user (let ((text (getf event :text)))
               (unless (equal text (tui-state-pending-user *tui*))
                 (tui-show-user text))))
      (t nil))))

(defun tui-chat-add (chunks)
  "Append one logical line of CHUNKS to the chat buffer."
  (when *tui*
    (sb-thread:with-mutex (*tui-lock*)
      (when *tui*
        (uibuf-append (tui-state-chat *tui*) chunks :fresh t)))
    (tui-dirty)))

(defun first-line (s)
  (subseq s 0 (or (position #\Newline s) (length s))))

(defun str-truncate (s n)
  (if (> (length s) n) (subseq s 0 n) s))

(defun tui-short-args (args)
  (let ((s (cond ((and (hash-table-p args) (stringp (gethash "code" args)))
                  (first-line (gethash "code" args)))
                 ((and (hash-table-p args) (stringp (gethash "command" args)))
                  (gethash "command" args))
                 ((hash-table-p args) (json-encode args))
                 ((stringp args) args)
                 (t (princ-to-string args)))))
    (str-truncate s 90)))

(defun tui-chat-result (name text)
  "Short form of a tool result: up to 2 lines, truncated, gray."
  (when (stringp text)
    (tui-chat-flush (ui-parse-ansi (str-truncate text 300)) 2)
    (when (> (length text) 300)
      (tui-chat-add (list (cons (format nil "  … ~A: ~D chars" name (length text))
                                8))))))

(defun tui-chat-flush (chunks max-lines)
  "Write CHUNKS to the chat as at most MAX-LINES gray lines."
  (let ((shown 0))
    (loop for ck in chunks
          while (< shown max-lines)
          do (let* ((s (car ck))
                    (p (position #\Newline s)))
               (if p
                   (progn
                     (tui-chat-add (list (cons (subseq s 0 p) 8)))
                     (incf shown)
                     (when (and (< shown max-lines) (< (1+ p) (length s)))
                       (tui-chat-add (list (cons (subseq s (1+ p)) 8)))
                       (incf shown)))
                   (progn
                     (tui-chat-add (list (cons s 8)))
                     (incf shown)))))))

;;; ---------------------------------------------------------------
;;; Drawing. Only the redraw thread calls TUI-DRAW.

(defun tui-redraw-loop ()
  (loop while *tui*
        do (handler-case
               (progn
                 (sb-thread:with-mutex (*tui-lock*)
                   (loop while (and *tui* (zerop *tui-dirty*))
                         do (sb-thread:condition-wait *tui-cv* *tui-lock*
                                                      :timeout 1.0)))
                 (when *tui* (tui-draw)))
             (serious-condition (c) (tui-crash c)))))

(defun tui-draw ()
  (tui-update-size)
  (sb-thread:with-mutex (*tui-lock*)
    (when *tui*
      (let* ((w (tui-state-width *tui*))
             (h (tui-state-height *tui*))
             (body-h (max 3 (- h 2)))
             (chat-w (max 20 (min (- w 20) (round (* w 0.62)))))
             (g (ui-grid-make w h)))
        (tui-draw-pane g 0 0 chat-w body-h "chat"
                       (tui-state-chat *tui*) (zerop (tui-state-active *tui*))
                       (tui-state-scroll *tui*))
        (tui-draw-pane g chat-w 0 (- w chat-w) body-h "weather"
                       (tui-state-weather *tui*) (= 1 (tui-state-active *tui*))
                       0)
        (tui-draw-input g (- h 2) w)
        (tui-draw-status g (1- h) w)
        (uifix-boxes g)
        (let ((s (ui-grid-render g)))
          (tty-write
           (format nil "~C[?25l~C[H~A~C[H~D;~DH~C[?25h"
                   #\Esc #\Esc
                   (subseq s 0 (1- (length s)))
                   #\Esc
                   (1- h)
                   (min w (1+ (length (tui-state-input *tui*))))
                   #\Esc)))))))

(defun tui-draw-pane (g x y w h title buf activep scroll)
  (ui-draw-box g x y w h
               (if activep (ui-attr 7 :bright t :bg 4) 8)
               (format nil " ~A " title))
  (let ((inner-w (- w 2))
        (inner-h (- h 2)))
    (when (and (plusp inner-w) (plusp inner-h))
      (let ((lines (tui-visible-lines buf inner-w inner-h scroll)))
        (loop for line in lines
              for i from 0
              do (ui-grid-write-chunks g (1+ x) (+ y 1 i) line inner-w))))))

(defun tui-visible-lines (buf width height scroll)
  "Top-first chunk-lists fitting HEIGHT rows, scrolled up SCROLL lines."
  (let* ((src (uibuf-lines buf))
         (recent (last src (min (length src) 400)))
         (all (loop for ln in recent
                    append (or (ui-wrap-chunks (uline-chunks ln) width)
                               (list nil))))
         (n (length all))
         (end (max 0 (- n scroll)))
         (start (max 0 (- end height))))
    (subseq all start end)))

(defun tui-draw-input (g y w)
  (let* ((prefix (tui-state-prefix *tui*))
         (pattr (if prefix (ui-attr 3 :bright t) (ui-attr 2 :bright t)))
         (prompt (if prefix "C-b-» " "❯ ")))
    (ui-grid-write g 0 y prompt pattr w)
    (ui-grid-write g (length prompt) y
                   (coerce (tui-state-input *tui*) 'string) 7 w)))

(defun tui-draw-status (g y w)
  (let ((attr (ui-attr 7 :bright t :bg 4)))
    (dotimes (x w) (ui-grid-put g x y #\Space attr))
    (ui-grid-write g 1 y (format nil " lisping-ai ~A" *model*) attr (1- w))
    (let* ((sel (tui-state-active *tui*))
           (panes (format nil " ~:[ ○~; ●~]chat  ~:[○~;●~]weather "
                          (zerop sel) (= 1 sel))))
      (ui-grid-write g (max 1 (- (floor w 2) (floor (length panes) 2)))
                     y panes attr (1- w)))
    (let* ((now (multiple-value-list (decode-universal-time (get-universal-time))))
           (time (format nil "~2,'0d:~2,'0d " (nth 2 now) (nth 1 now)))
           (hint (if (tui-state-prefix *tui*)
                     "C-b cancel · q quit · 1/2 pane · PgUp/PgDn scroll"
                     "C-b prefix")))
      (ui-grid-write g (max 1 (- w (length hint) (length time) 1)) y
                     (concatenate 'string hint " " time) attr w))))