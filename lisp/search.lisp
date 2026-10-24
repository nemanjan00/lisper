;;; search.lisp — search the chat scrollback (/find).
;;;
;;;   /find WORD    jump to the newest line containing WORD (case-insensitive)
;;;                 and highlight every match in the chat
;;;   /find         the next older match      /find -   the next newer one
;;;   Ctrl-P / Ctrl-N  older / newer match while a search is active
;;;   Esc           clear the highlight       /find off  the same
;;; The status bar shows "find 3/12" (nothing is written into the chat, so
;;; the search never matches its own report).  Only the lines the chat
;;; pane keeps (the last 400 buffer lines) are searched, as wrapped rows.

(defvar *find-term* nil "The word being searched for, or NIL.")
(defvar *find-pos* 0 "Which match is current, counted from the newest (0).")
(defvar *find-status* nil "Text of the status bar segment, or NIL.")
(defvar *find-chat-height* nil "Rows of the chat pane's last render.")

(defun find-rows (width)
  "Wrapped rows (chunk lists, top first) of the window the chat pane draws."
  (let* ((src (uibuf-lines (tui-ui-chat *tui*)))
         (n0 (length src)))
    (values (chat-rows (loop for i from (max 0 (- n0 400)) below n0 collect (aref src i))
                       width))))

(defun find-ranges (text term)
  "Start positions of the case-insensitive occurrences of TERM in TEXT."
  (let ((r nil) (pos 0) (len (length term)))
    (when (plusp len)
      (loop for p = (search term text :start2 pos :test #'char-equal)
            while p do (push p r) (setf pos (+ p len))))
    (nreverse r)))

(defun find-match-rows (rows term)
  "Indices of ROWS that contain TERM, ascending."
  (loop for row in rows for i from 0
        when (find-ranges (chunks-text row) term) collect i))

(defun find-highlight-line (chunks term)
  "CHUNKS with every occurrence of TERM drawn black on yellow."
  (let* ((text (chunks-text chunks))
         (starts (find-ranges text term)))
    (if (null starts)
        chunks
        (let* ((attrs (make-array (length text)))
               (hl (ui-attr 0 :bg 3))
               (k 0))
          (dolist (c chunks)
            (dotimes (i (length (car c))) (setf (aref attrs k) (cdr c)) (incf k)))
          (dolist (s starts)
            (loop for i from s below (+ s (length term)) do (setf (aref attrs i) hl)))
          (let ((out nil) (run-start 0))
            (loop for i from 1 to (length text)
                  do (when (or (= i (length text)) (/= (aref attrs i) (aref attrs run-start)))
                       (push (cons (subseq text run-start i) (aref attrs run-start)) out)
                       (setf run-start i)))
            (nreverse out))))))

(defun find-render-hook (lines height)
  "Called by the chat pane render with the visible rows: remembers HEIGHT
and returns LINES with the active search term highlighted."
  (setf *find-chat-height* height)
  (if *find-term*
      (mapcar (lambda (l) (if l (find-highlight-line l *find-term*) l)) lines)
      lines))

(defun find-clear ()
  (setf *find-term* nil *find-pos* 0 *find-status* nil)
  (ignore-errors (panes-mark-all-dirty) (tui-dirty))
  t)

(defun find-scroll-to (row n height)
  "Scroll the chat pane so ROW (of N) sits in the middle."
  (let ((p (pane-get "chat")))
    (when p
      (sb-thread:with-recursive-lock (*tui-lock*)
        (let ((end (+ row (ceiling height 2))))
          (setf (pane-scroll p) (max 0 (- n end)))
          (when (zerop (pane-scroll p)) (setf *tui-chat-new* 0))
          (setf (pane-needs-redraw p) 1)))
      (tui-dirty))))

(defun find-go (delta)
  "Move to the match DELTA steps older (negative: newer); wraps around."
  (let* ((width (or *tui-chat-pane-width* 80))
         (rows (find-rows width))
         (matches (coerce (find-match-rows rows *find-term*) 'vector))
         (m (length matches)))
    (cond ((zerop m)
           (setf *find-status* (format nil "find: no match for ~A" *find-term*)))
          (t (setf *find-pos* (mod (+ *find-pos* delta) m))
             (find-scroll-to (aref matches (- m 1 *find-pos*)) (length rows)
                             (or *find-chat-height* 20))
             (setf *find-status* (format nil "find ~D/~D" (- m *find-pos*) m))))
    (panes-mark-all-dirty)
    (tui-dirty)
    t))

(defun find-run (arg)
  (let ((a (string-trim " " (or arg ""))))
    (cond ((string-equal a "off") (find-clear))
          ((string= a "-")
           (if *find-term* (find-go -1) (setf *find-status* "find: nothing searched yet")))
          ((string= a "")
           (if *find-term* (find-go 1) (setf *find-status* "find: /find WORD")))
          (t (setf *find-term* a *find-pos* 0)
             (find-go 0)))
    (when *tui* (tui-dirty))))

(defun slash-find-chat (ui args)
  (declare (ignore ui))
  (find-run args))

(defun find-segment ()
  "Status bar segment (TEXT FG BG), or NIL."
  (and *find-term* *find-status* (list *find-status* 0 3)))

(defun find-key (ui key)
  "Key interceptor: Ctrl-P/Ctrl-N step through the matches, Esc clears.
True when the key was used."
  (declare (ignore ui))
  (when *find-term*
    (cond ((and (characterp key) (char= key #\Dle)) (find-go 1) t)    ; Ctrl-P: older
          ((and (characterp key) (char= key #\So)) (find-go -1) t)    ; Ctrl-N: newer
          ((and (eq key :escape) (not (and (fboundp 'interrupt-busy-p)
                                           (funcall 'interrupt-busy-p))))
           (find-clear)))))

(defun find-start ()
  "Called from init.lisp: register /find and the key interceptor."
  (define-slash "find" 'slash-find-chat "search the chat: /find WORD, then /find (older), /find - (newer), off"
    (lambda (ui partial) (declare (ignore ui partial)) '("off" "-")))
  (pushnew 'find-key *tui-key-interceptors*))
