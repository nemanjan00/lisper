;;;; lisp/tui-render.lisp -- turning agent events into chat lines and
;;;; compositing panes, status bar and input line into the grid.
;;;; Draws only from the redraw thread; takes *tui-lock* while reading.

(in-package :lisping)

;;; ---------------------------------------------------------------
;;; Frame state and perf counters. Only the redraw thread touches these.
;;; The previous frame's grid is kept so frames can be diffed: a full
;;; redraw (~18KB) happens only when *TUI-PREV-GRID* is NIL — on UI
;;; start, on resize, on C-b r — never per keystroke.

(defvar *tui-prev-grid* nil "Grid of the last frame written to the tty.")
(defvar *tui-prev-cursor* nil "Last (row . col) the cursor was placed at.")
(defvar *tui-clock-minute* nil "Minute value drawn in the status bar.")
(defvar *tui-frames-total* 0 "TUI-DRAW calls, including skipped ones.")
(defvar *tui-frames-skipped* 0 "Frames where nothing needed writing.")
(defvar *tui-frames-full* 0 "Frames written as full renders.")
(defvar *tui-bytes-total* 0 "Bytes written to the tty by TUI-DRAW.")

(defun tui-perf ()
  "Plist of frame counters for measuring redraw cost."
  (list :frames *tui-frames-total*
        :skipped *tui-frames-skipped*
        :full *tui-frames-full*
        :bytes *tui-bytes-total*))

(defun tui-force-redraw ()
  "Drop the remembered frame so the next draw is a full render (C-b r)."
  (setf *tui-prev-grid* nil))

;;; ---------------------------------------------------------------
;;; Hooks: events in, chat buffer out.

(defun tui-install-hooks ()
  (setf *emit* 'tui-emit)
  (unless (member 'tui-before-request *before-request*)
    (setf *before-request* (append *before-request* (list 'tui-before-request)))))

(defun tui-emit (event)
  "Receives every agent event. With no UI running, print like the core."
  (let ((ui *tui*))
    (if (null ui)
        (terminal-emit event)
        (handler-case (tui-log-event ui event)
          (serious-condition (c) (when (eq *tui* ui) (tui-crash c)))))))

(defun tui-before-request (body)
  "Show the newest user message (covers the queue and the pre-restart core
that does not emit :user events)."
  (let ((ui *tui*))
    (when ui
      (handler-case
          (let* ((msgs (gethash "messages" body))
                 (last (and (vectorp msgs) (plusp (length msgs))
                            (aref msgs (1- (length msgs))))))
            (when (and last (string= (gethash "role" last) "user")
                       (stringp (gethash "content" last)))
              (tui-show-user ui (gethash "content" last))))
        (serious-condition (c) (when (eq *tui* ui) (tui-crash c))))))
  body)

(defun tui-show-user (ui text)
  "Log a user message unless it is already on screen."
  (sb-thread:with-mutex (*tui-lock*)
    (when (and (eq *tui* ui)
               (not (equal text (tui-state-shown-user ui))))
      (setf (tui-state-shown-user ui) text
            (tui-state-pending-user ui) nil
            (tui-state-scroll ui) 0)
      (uibuf-append (tui-state-chat ui)
                    (list (cons "you> " (ui-attr 2 :bright t))
                          (cons text (ui-attr 2 :bright t)))
                    :fresh t)))
  (tui-dirty))

(defun tui-log-event (ui event)
  (let ((type (getf event :type)))
    (case type
      (:assistant (tui-chat-add ui (ui-parse-ansi (getf event :text))))
      (:thinking (tui-chat-add ui (list (cons (getf event :text) 8))))
      (:tool-call
       (tui-chat-add
        ui
        (list (cons (format nil "* ~A ~A"
                            (getf event :name)
                            (tui-short-args (getf event :args)))
                    (ui-attr 4 :bright t)))))
      (:tool-result (tui-chat-result ui (getf event :name) (getf event :text)))
      (:error (tui-chat-add ui (ui-parse-ansi (getf event :text))))
      (:info (tui-chat-add ui (list (cons (getf event :text) 8))))
      (:user (let ((text (getf event :text)))
               (unless (equal text (tui-state-pending-user ui))
                 (tui-show-user ui text))))
      (t nil))))

(defun tui-chat-add (ui chunks)
  "Append CHUNKS to the chat buffer, one buffer line per text line:
embedded newlines must never reach the grid."
  (when (eq *tui* ui)
    (sb-thread:with-mutex (*tui-lock*)
      (when (eq *tui* ui)
        (dolist (line (tui-chunk-lines chunks))
          (uibuf-append (tui-state-chat ui) line :fresh t))))
    (tui-dirty)))

(defun tui-chunk-lines (chunks)
  "Split (text . attr) CHUNKS into a list of chunk-lists, one per line."
  (let ((out nil)
        (cur nil))
    (dolist (ck chunks)
      (let ((s (car ck))
            (a (cdr ck))
            (start 0))
        (loop
          (let ((p (position #\Newline s :start start)))
            (if p
                (progn
                  (push (cons (subseq s start p) a) cur)
                  (push (nreverse cur) out)
                  (setf cur nil)
                  (setf start (1+ p)))
                (progn
                  (push (cons (subseq s start) a) cur)
                  (return)))))))
    (when cur (push (nreverse cur) out))
    (nreverse out)))

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

(defun tui-chat-result (ui name text)
  "Short form of a tool result: up to 2 lines, truncated, gray."
  (when (stringp text)
    (let* ((lines (tui-chunk-lines (ui-parse-ansi (str-truncate text 2000))))
           (shown (subseq lines 0 (min 2 (length lines)))))
      (dolist (line shown)
        (tui-chat-add ui (list (cons (chunks-text line) 8))))
      (when (> (length text) 2000)
        (tui-chat-add ui (list (cons (format nil "  ... ~A: ~D chars" name (length text))
                                     8)))))))

(defun chunks-text (chunks)
  (with-output-to-string (out)
    (dolist (ck chunks) (write-string (car ck) out))))

;;; ---------------------------------------------------------------
;;; Drawing. Only the redraw thread calls TUI-DRAW.

(defun tui-minute-changed-p ()
  "True when the wall-clock minute differs from the one on screen."
  (/= (or *tui-clock-minute* -1)
      (nth-value 1 (decode-universal-time (get-universal-time)))))

(defun tui-redraw-loop (ui)
  (loop while (and *tui* (eq *tui* ui))
        do (handler-case
               (progn
                 (sb-thread:with-mutex (*tui-lock*)
                   (loop while (and *tui* (eq *tui* ui) (zerop *tui-dirty*))
                         do (sb-thread:condition-wait *tui-cv* *tui-lock*
                                                      :timeout 1.0)
                            ;; Waking is not a reason to draw; only the
                            ;; once-a-minute clock tick dirties an idle UI.
                            (when (and (zerop *tui-dirty*)
                                       (tui-minute-changed-p))
                              (incf *tui-dirty*)))
                   ;; Consume the dirty flag BEFORE drawing: emitters only
                   ;; append state and bump the counter, so this cannot
                   ;; lose an update — the new state is drawn now, and a
                   ;; bump during the draw schedules one more pass.
                   (when (and *tui* (eq *tui* ui))
                     (setf *tui-dirty* 0)))
                 (when (and *tui* (eq *tui* ui)) (tui-draw ui)))
             (serious-condition (c)
               (when (eq *tui* ui) (tui-crash c))))))

(defun tui-draw (ui)
  (tui-maybe-update-size ui)
  (sb-thread:with-mutex (*tui-lock*)
    (when (and *tui* (eq *tui* ui))
      (let* ((w (tui-state-width ui))
             (h (tui-state-height ui))
             (body-h (max 3 (- h 2)))
             (chat-w (max 20 (min (- w 20) (round (* w 0.62)))))
             (g (ui-grid-make w h)))
        (tui-draw-pane g 0 0 chat-w body-h "chat"
                       (tui-state-chat ui) (zerop (tui-state-active ui))
                       (tui-state-scroll ui))
        (tui-draw-pane g chat-w 0 (- w chat-w) body-h "weather"
                       (tui-state-weather ui) (= 1 (tui-state-active ui))
                       0)
        (tui-draw-input g ui (- h 2) w)
        (tui-draw-status g ui (1- h) w)
        (uifix-boxes g)
        (let* ((prompt-len (length (if (tui-state-prefix ui) "*> " "> ")))
               (row (1- h))
               (col (min w (+ prompt-len (length (tui-state-input ui)) 1)))
               (full (or (null *tui-prev-grid*)
                         (/= (uigrid-w *tui-prev-grid*) w)
                         (/= (uigrid-h *tui-prev-grid*) h)))
               (body (ui-grid-diff-render (and (not full) *tui-prev-grid*) g)))
          (incf *tui-frames-total*)
          (cond ((and (zerop (length body))
                      (equal *tui-prev-cursor* (cons row col)))
                 ;; Nothing changed: write nothing at all.
                 (incf *tui-frames-skipped*))
                (t
                 (when full (incf *tui-frames-full*))
                 ;; One synchronized update per frame: the terminal shows
                 ;; it atomically, so the cursor does not flicker. ?25h
                 ;; re-shows the cursor hidden by ?25l at UI start — it
                 ;; must be inside the sync wrap, after positioning.
                 (let ((frame (format nil "~C[?2026h~A~C[~D;~DH~C[?25h~C[?2026l"
                                      #\Esc body #\Esc row col #\Esc #\Esc)))
                   (incf *tui-bytes-total* (length frame))
                   (tty-write frame))))
          (setf *tui-prev-grid* g
                *tui-prev-cursor* (cons row col)
                *tui-clock-minute* (nth-value 1 (decode-universal-time
                                                 (get-universal-time)))))))))

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
              do (ui-grid-write-chunks g (1+ x) (+ y 1 i) line
                                       (+ x 1 inner-w)))))))

(defun tui-visible-lines (buf width height scroll)
  "Top-first chunk-lists fitting HEIGHT rows, scrolled up SCROLL lines."
  (let* ((src (uibuf-lines buf))
         (n0 (length src))
         (recent (loop for i from (max 0 (- n0 400)) below n0
                       collect (aref src i)))
         (all (loop for ln in recent
                    append (or (ui-wrap-chunks (uline-chunks ln) width)
                               (list nil))))
         (n (length all))
         (end (max 0 (- n scroll)))
         (start (max 0 (- end height))))
    (subseq all start end)))

(defun tui-draw-input (g ui y w)
  (let* ((prefix (tui-state-prefix ui))
         (pattr (if prefix (ui-attr 3 :bright t) (ui-attr 2 :bright t)))
         (prompt (if prefix "*> " "> ")))
    (ui-grid-write g 0 y prompt pattr w)
    (ui-grid-write g (length prompt) y
                   (coerce (tui-state-input ui) 'string) 7 w)))

(defun tui-draw-status (g ui y w)
  (let ((attr (ui-attr 7 :bright t :bg 4)))
    (dotimes (x w) (ui-grid-put g x y #\Space attr))
    (ui-grid-write g 1 y (format nil " lisping-ai ~A" *model*) attr (1- w))
    (let* ((sel (tui-state-active ui))
           (panes (format nil " ~:[-~;*~]chat  ~:[-~;*~]weather "
                          (zerop sel) (= 1 sel))))
      (ui-grid-write g (max 1 (- (floor w 2) (floor (length panes) 2)))
                     y panes attr (1- w)))
    (let* ((now (multiple-value-list (decode-universal-time (get-universal-time))))
           (time (format nil "~2,'0d:~2,'0d " (nth 2 now) (nth 1 now)))
           (hint (if (tui-state-prefix ui)
                     "C-b cancel | q quit | 1/2 pane | PgUp/PgDn scroll"
                     "C-b prefix")))
      (ui-grid-write g (max 1 (- w (length hint) (length time) 1)) y
                     (concatenate 'string hint " " time) attr w))))
