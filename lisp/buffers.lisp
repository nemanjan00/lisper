;;;; lisp/buffers.lisp — line buffers and a character-cell grid the UI
;;;; renders into. A ULINE is a list of (text . attr) chunks; a UIBUF is a
;;;; capped, optionally wrapped vector of ULINES; a UIGRID is a flat
;;;; W x H field of chars + attrs that panes get composited into.

(in-package :lisping)

(defstruct uline chunks)

(defstruct uibuf
  name
  (lines (make-array 128 :adjustable t :fill-pointer 0))
  (cap 1500)
  (align :bottom)
  (wrap t)
  (last-attr -1))

(defun uibuf-append (buf chunks &key fresh)
  "Append (text . attr) chunks to BUF. With FRESH, start a new line."
  (when (and chunks (some (lambda (c) (plusp (length (car c)))) chunks))
    (let* ((attr (or (cdr (first chunks)) 7))
           (v (uibuf-lines buf))
           (mergeable (and (not fresh) (plusp (length v))
                           (= (uibuf-last-attr buf) attr))))
      (if mergeable
          (setf (uline-chunks (aref v (1- (length v))))
                (nconc (uline-chunks (aref v (1- (length v)))) chunks))
          (vector-push-extend (make-uline :chunks chunks) v))
      (setf (uibuf-last-attr buf) attr)
      (let ((excess (- (length v) (uibuf-cap buf))))
        (when (plusp excess)
          (replace v v :start2 excess)
          (decf (fill-pointer v) excess))))))

(defun uibuf-remove-matching (buf pred)
  "Remove every line matching PRED (a function of ULINE) from BUF.
Single downward pass: after a match at I the elements above I are
already tested, so the same index is never revisited. (Re-testing I
after DECF of the fill-pointer would read the stale slot past the end
of the active region and could match it again, double-decrementing the
fill-pointer and finally erroring in REPLACE with a :START2 past the
end — the boot crash on the queued->user line upgrade.)"
  (let ((v (uibuf-lines buf)))
    (loop for i from (1- (length v)) downto 0
          when (funcall pred (aref v i))
            do (replace v v :start1 i :start2 (1+ i))
               (decf (fill-pointer v)))))

(defun ui-split-lines (text)
  (loop with start = 0
        for i = (position #\Newline text :start start)
        collect (subseq text start i)
        while i
        do (setf start (1+ i))))

(defun uibuf-add-text (buf text attr &key fresh)
  (loop for p in (ui-split-lines text)
        for i from 0
        do (uibuf-append buf (list (cons p attr)) :fresh (or fresh (> i 0)))))

(defun ui-wrap-chunks (chunks width)
  "Word-wrap chunk text at WIDTH columns; returns a list of chunk-lists
(lines).  Breaks at the last space that fits (a word longer than the
line is cut at WIDTH); a line that starts with spaces keeps that indent
on its continuation lines."
  (when (plusp width)
    (let* ((text (apply #'concatenate 'string (mapcar #'car chunks)))
           (attrs (make-array (length text)))
           (n (length text))
           (indent (min (or (position #\Space text :test-not #'char=) 0)
                        (max 0 (floor width 2))))
           (out nil)
           (start 0)
           (first-line t))
      (let ((i 0))
        (dolist (ck chunks)
          (dotimes (k (length (car ck))) (setf (aref attrs i) (cdr ck)) (incf i))))
      (flet ((emit-line (from to pad)
               ;; chunks for text[from,to) preceded by PAD spaces
               (let ((cur nil) (a nil) (s nil))
                 (when (plusp pad)
                   (push (cons (make-string pad :initial-element #\Space)
                               (if (plusp n) (aref attrs (min from (1- n))) 7))
                         cur))
                 (loop for k from from below to
                       do (if (and s (eql a (aref attrs k)))
                              (vector-push-extend (char text k) s)
                              (progn
                                (when s (push (cons (coerce s 'string) a) cur))
                                (setf a (aref attrs k)
                                      s (make-array 1 :element-type 'character
                                                      :adjustable t :fill-pointer 1
                                                      :initial-element (char text k))))))
                 (when s (push (cons (coerce s 'string) a) cur))
                 (push (nreverse cur) out))))
        (loop while (< start n)
              do (let* ((pad (if first-line 0 indent))
                        (room (- width pad)))
                   (if (<= (- n start) room)
                       (progn (emit-line start n pad) (setf start n))
                       (let* ((brk (position #\Space text :start start :end (+ start room 1)
                                                          :from-end t))
                              ;; a space inside the indent is no break point
                              (brk (and brk (> brk (+ start (if first-line indent 0))) brk)))
                         (if brk
                             (progn (emit-line start brk pad)
                                    (setf start (or (position #\Space text :start brk
                                                                           :test-not #'char=)
                                                    n)))
                             (progn (emit-line start (+ start room) pad)
                                    (incf start room))))))
                 (setf first-line nil)))
      (nreverse out))))

(defun ui-truncate-chunks (chunks width)
  "Hard-truncate chunks to WIDTH columns."
  (when (plusp width)
    (let ((out nil) (len 0))
      (dolist (ck chunks)
        (let* ((s (car ck)) (m (min (length s) (- width len))))
          (when (plusp m) (push (cons (subseq s 0 m) (cdr ck)) out))
          (incf len m)))
      (nreverse out))))

(defstruct uigrid w h chars attrs)

(defun ui-grid-make (w h)
  (make-uigrid :w w :h h
               :chars (make-string (* w h) :initial-element #\ )
               :attrs (make-array (* w h) :element-type '(unsigned-byte 16)
                                  :initial-element 7)))

(defun ui-grid-put (g x y ch attr)
  (when (and (<= 0 x) (< x (uigrid-w g)) (<= 0 y) (< y (uigrid-h g)))
    (let ((i (+ x (* y (uigrid-w g)))))
      (setf (char (uigrid-chars g) i) ch
            (aref (uigrid-attrs g) i) attr))))

(defun ui-grid-write (g x y string attr max-w)
  (let ((n (min (length string) (max 0 (- (uigrid-w g) x)) max-w))
        (w (uigrid-w g)))
    (dotimes (k n)
      (let ((i (+ x k (* y w))))
        (setf (char (uigrid-chars g) i) (ui-safe-char (char string k))
              (aref (uigrid-attrs g) i) attr)))
    (+ x n)))

(defun ui-grid-write-chunks (g x y chunks max-w)
  (dolist (ck chunks x)
    (setf x (ui-grid-write g x y (car ck) (cdr ck) (max 0 (- max-w x))))))

(defparameter +ui-box-chars+ "─│┌┐└┘├┤┬┴┼")

(defun ui-draw-box (g x y w h attr &optional title)
  (when (or (< w 2) (< h 2)) (return-from ui-draw-box))
  (ui-grid-put g x y #\box_drawings_light_down_and_right attr)
  (ui-grid-put g (+ x w -1) y #\box_drawings_light_down_and_left attr)
  (ui-grid-put g x (+ y h -1) #\box_drawings_light_up_and_right attr)
  (ui-grid-put g (+ x w -1) (+ y h -1) #\box_drawings_light_up_and_left attr)
  (dotimes (k (- w 2))
    (ui-grid-put g (+ x 1 k) y #\box_drawings_light_horizontal attr)
    (ui-grid-put g (+ x 1 k) (+ y h -1) #\box_drawings_light_horizontal attr))
  (dotimes (k (- h 2))
    (ui-grid-put g x (+ y 1 k) #\box_drawings_light_vertical attr)
    (ui-grid-put g (+ x w -1) (+ y 1 k) #\box_drawings_light_vertical attr))
  (when (and title (plusp (length title)) (> w 4))
    (ui-grid-write g (+ x 2) y (subseq title 0 (min (length title) (- w 4)))
                   attr (- w 4))))

(defun ui-box-hconn (ch) (find ch "─┌┐├┤┬┴┼"))

(defun ui-box-vconn (ch) (find ch "│┌┐└┘├┤┬┴┼"))

(defun uifix-boxes (g)
  "Rewrite box-drawing chars so their edges connect to neighbours."
  (let ((w (uigrid-w g)) (h (uigrid-h g)) (chars (uigrid-chars g)))
    (dotimes (y h)
      (dotimes (x w)
        (let ((ch (char chars (+ x (* y w)))))
          (when (find ch +ui-box-chars+)
            (let* ((up (and (> y 0) (char chars (+ (* (1- y) w) x))))
                   (dn (and (< y (1- h)) (char chars (+ (* (1+ y) w) x))))
                   (lf (and (> x 0) (char chars (+ (1- x) (* y w)))))
                   (rt (and (< x (1- w)) (char chars (+ (1+ x) (* y w)))))
                   (n (and up (ui-box-vconn up)))
                   (s (and dn (ui-box-vconn dn)))
                   (e2 (and rt (ui-box-hconn rt)))
                   (w2 (and lf (ui-box-hconn lf)))
                   (fixed (cond ((and n s e2 w2) #\box_drawings_light_vertical_and_horizontal)
                                ((and n s e2) #\box_drawings_light_vertical_and_right)
                                ((and n s w2) #\box_drawings_light_vertical_and_left)
                                ((and e2 w2 s) #\box_drawings_light_down_and_horizontal)
                                ((and e2 w2 n) #\box_drawings_light_up_and_horizontal)
                                ((and e2 w2) #\box_drawings_light_horizontal)
                                ((and n s) #\box_drawings_light_vertical)
                                ((and n e2) #\box_drawings_light_up_and_right)
                                ((and n w2) #\box_drawings_light_up_and_left)
                                ((and s e2) #\box_drawings_light_down_and_right)
                                ((and s w2) #\box_drawings_light_down_and_left)
                                (t ch))))
              (setf (char chars (+ x (* y w))) fixed))))))))

(defun ui-grid-render (g)
  "Render a grid to a string with SGR codes. The cursor is positioned
explicitly at the start of every row, so one bad row cannot shift or
scroll the rest of the screen."
  (with-output-to-string (out)
    (let ((w (uigrid-w g))
          (prev -1))
      (dotimes (y (uigrid-h g))
        (format out "~C[~D;1H" #\Esc (1+ y))
        (dotimes (x w)
          (let ((i (+ x (* y w))))
            (write-string (ui-attr-string (aref (uigrid-attrs g) i) prev) out)
            (setf prev (aref (uigrid-attrs g) i))
            (write-char (char (uigrid-chars g) i) out)))
        (write-string (ui-attr-string 7 prev) out)
        (setf prev 7)))))

(defun ui-grid-diff-render (old new)
  "Render only the cells of NEW that differ from OLD: cursor-positioned
runs of changed cells, SGR emitted only where the attribute changes.
OLD nil or of different dimensions renders everything. Like UI-GRID-RENDER,
the output leaves the SGR state at attr 7, so the next diff frame may
assume it as its starting state."
  (with-output-to-string (out)
    (if (or (null old)
            (/= (uigrid-w old) (uigrid-w new))
            (/= (uigrid-h old) (uigrid-h new)))
        (write-string (ui-grid-render new) out)
        (let ((w (uigrid-w new))
              (h (uigrid-h new))
              (oc (uigrid-chars old))
              (nc (uigrid-chars new))
              (oa (uigrid-attrs old))
              (na (uigrid-attrs new))
              (prev 7))
          (dotimes (y h)
            (let ((runs nil)
                  (x 0))
              (loop while (< x w)
                    do (if (or (char/= (char oc (+ x (* y w)))
                                       (char nc (+ x (* y w))))
                               (/= (aref oa (+ x (* y w)))
                                   (aref na (+ x (* y w)))))
                           (let ((start x))
                             (loop while (and (< x w)
                                              (or (char/= (char oc (+ x (* y w)))
                                                          (char nc (+ x (* y w))))
                                                  (/= (aref oa (+ x (* y w)))
                                                      (aref na (+ x (* y w))))))
                                   do (incf x))
                             (push (cons start x) runs))
                           (incf x)))
              (dolist (run (nreverse runs))
                (format out "~C[~D;~DH" #\Esc (1+ y) (1+ (car run)))
                (loop for i from (car run) below (cdr run)
                      for idx = (+ i (* y w))
                      do (write-string (ui-attr-string (aref na idx) prev) out)
                         (setf prev (aref na idx))
                         (write-char (char nc idx) out)))))
          (write-string (ui-attr-string 7 prev) out)))))

(defun ui-safe-char (ch)
  "Control characters never go into a cell: they would move the cursor
mid-frame. Map them (and NUL) to a space."
  (if (or (char< ch #\Space) (char= ch #\Rubout)) #\Space ch))
