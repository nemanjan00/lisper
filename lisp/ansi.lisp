;;;; lisp/ansi.lisp — ANSI SGR attributes and parsing.
;;;; An ATTR is an 8-bit integer: bits 0-2 fg color, bit 3 fg bright,
;;;; bits 4-6 bg color (stored as color+1, 0 = default), bit 7 bg bright.

(in-package :lisping)

(defconstant +ui-fg-mask+ 15)

(defconstant +ui-bg-mask+ 240)

(defun ui-attr (fg &key bright bg bgbright)
  (logior (logand fg 7)
          (if bright 8 0)
          (if bg (ash (1+ (logand bg 7)) 4) 0)
          (if bgbright 128 0)))

(defvar *tui-status-arrows* (cons (code-char #xE0B0) (code-char #xE0B2))
  "Powerline separator glyphs: (right-pointing . left-pointing).  U+E0B0 /
U+E0B2 need a patched (Nerd/Powerline) font; for a plain font set this to
(cons #\\▶ #\\◀) or (cons #\\Space #\\Space).  Used by the status bar,
pane titles, the input prompt, queue rows and chat labels.")

(defun tui-seg-attr (fg bg)
  "Attr for a segment: FG 0-7 (7 = bright white), BG 0-7 or :GREY."
  (if (eq bg :grey)
      (ui-attr fg :bright (= fg 7) :bg 0 :bgbright t)
      (ui-attr fg :bright (= fg 7) :bg bg)))

(defun tui-arrow-attr (bg next-bg)
  "Attr for the arrow after a segment of colour BG: the arrow's
foreground is BG, its background the next segment's NEXT-BG (nil = the
terminal default)."
  (let ((fg (if (eq bg :grey) 0 bg))
        (bright (eq bg :grey)))
    (cond ((null next-bg) (ui-attr fg :bright bright))
          ((eq next-bg :grey) (ui-attr fg :bright bright :bg 0 :bgbright t))
          (t (ui-attr fg :bright bright :bg next-bg)))))

(defun tui-seg-chunks (text fg bg &optional next-bg)
  "Chunk list for one powerline segment: \" TEXT \" on BG, then the
arrow into NEXT-BG."
  (list (cons (format nil " ~A " text) (tui-seg-attr fg bg))
        (cons (string (car *tui-status-arrows*)) (tui-arrow-attr bg next-bg))))

(defstruct (tui-theme (:conc-name theme-))
  "A palette for the 8-bit ATTR slots.  FG and BG are 16-entry vectors of
(r g b), indexed by slot (0-7 normal, 8-15 bright).  The arrow of a
powerline segment uses the segment's bg slot as its fg, so slots 1-7
should be the same colour in both vectors.  DEFAULT-BG fills cells with
no bg slot (the screen background).  CACHE memoises SGR params per attr."
  name fg bg default-bg (cache (make-array 256 :initial-element nil)))

(defvar *tui-theme* nil
  "The active TUI-THEME, or NIL for the terminal's own 16 ANSI colours.")

(defvar *tui-themes* nil
  "Alist name-string -> TUI-THEME; (define-tui-theme ...) adds to it.")

(defun define-tui-theme (name &key fg bg default-bg)
  "Register theme NAME from vectors of 16 #xRRGGBB integers FG and BG
and the integer DEFAULT-BG."
  (flet ((rgb (n) (list (ldb (byte 8 16) n) (ldb (byte 8 8) n) (ldb (byte 8 0) n))))
    (let ((th (make-tui-theme :name name :fg (map 'vector #'rgb fg)
                              :bg (map 'vector #'rgb bg)
                              :default-bg (rgb default-bg))))
      (setf *tui-themes* (append (remove name *tui-themes* :key #'car :test #'string=)
                                 (list (cons name th))))
      th)))

;;; Dracula (draculatheme.com): bg 282a36, current line 44475a, fg f8f8f2,
;;; comment 6272a4, cyan 8be9fd, green 50fa7b, orange ffb86c, pink ff79c6,
;;; purple bd93f9, red ff5555, yellow f1fa8c.  Slot 0 is the dark ink used
;;; on coloured segments (and a darker panel bg); slot 8 is the comment
;;; colour as text and the current-line grey as a background.
(define-tui-theme
    "dracula"
  :fg  #(#x282a36 #xff5555 #x50fa7b #xf1fa8c #xbd93f9 #xff79c6 #x8be9fd #xf8f8f2
         #x6272a4 #xff6e6e #x69ff94 #xffb86c #xd6acff #xff92df #xa4ffff #xffffff)
  :bg  #(#x21222c #xff5555 #x50fa7b #xf1fa8c #xbd93f9 #xff79c6 #x8be9fd #xf8f8f2
         #x44475a #xff6e6e #x69ff94 #xffb86c #xd6acff #xff92df #xa4ffff #xffffff)
  :default-bg #x282a36)

(defun tui-select-attr ()
  "Attr of the selected row/option: dark ink on purple under a theme,
bright white on blue otherwise."
  (if *tui-theme* (ui-attr 0 :bg 4) (ui-attr 7 :bright t :bg 4)))

(defun ui-attr-bg-slot (attr)
  "Background slot 0-15 of ATTR, or NIL for the default.  Bits 4-6 hold
colour+1 and bit 7 the bright flag."
  (let ((v (logand (ash attr -4) 7)))
    (and (plusp v) (+ (1- v) (if (logbitp 7 attr) 8 0)))))

(defun ui-theme-codes (attr &optional (theme *tui-theme*))
  "SGR parameters (truecolor fg and bg) for ATTR under THEME, or NIL
without a theme."
  (when theme
    (or (aref (theme-cache theme) attr)
        (setf (aref (theme-cache theme) attr)
              (let* ((fg (aref (theme-fg theme) (logand attr 15)))
                     (slot (ui-attr-bg-slot attr))
                     (bg (if slot (aref (theme-bg theme) slot) (theme-default-bg theme))))
                (append (cons 38 (cons 2 fg)) (cons 48 (cons 2 bg))))))))

(defun ui-attr-codes (attr)
  "SGR parameters for ATTR: truecolor under *TUI-THEME*, else plain ANSI."
  (or (ui-theme-codes attr)
      (let ((fg (logand attr 7))
            (fb (logbitp 3 attr))
            (bg (ash attr -4))
            (bb (logbitp 7 attr))
            (codes nil))
        (when (plusp bg)
          (push (+ 39 bg (if bb 60 0)) codes))
        (push (+ 30 fg (if fb 60 0)) codes)
        (nreverse codes))))

(defun ui-attr-string (attr prev)
  (if (= attr prev)
      ""
      (format nil "~C[0m~C[~{~d~^;~}m" #\Esc #\Esc (ui-attr-codes attr))))

(defun ui-c256 (n)
  "Map a 256-color palette index to the nearest 3-bit color."
  (cond ((< n 16) n)
        ((< n 232)
         (let* ((m (- n 16))
                (r (floor m 36))
                (g (floor (mod m 36) 6))
                (b (mod m 6))
                (hi (max r g b))
                (lo (min r g b)))
           (if (<= (- hi lo) 1)
               (if (< (/ (+ r g b) 3.0) 2.5) 0 7)
               (+ (cond ((and (>= r g) (>= g b)) 3)
                        ((and (>= g r) (>= r b)) 2)
                        ((and (>= r b) (>= b g)) 1)
                        ((and (>= g b) (>= b r)) 2)
                        ((and (>= b r) (>= r g)) 5)
                        (t 4))
                  (if (>= hi 4) 8 0)))))
        (t (if (> (- n 232) 12) 15 8))))

(defun ui-sgr-apply (params attr)
  "Apply a list of SGR parameters to ATTR, returning the new ATTR."
  (let ((ps params))
    (loop while ps
          do (let ((p (pop ps)))
               (cond ((= p 0) (setf attr 7))
                     ((= p 1) (setf attr (logior attr 8)))
                     ((= p 22) (setf attr (logand attr (lognot 8))))
                     ((= p 39) (setf attr (logior (logand attr (lognot +ui-fg-mask+)) 7)))
                     ((= p 49) (setf attr (logand attr (lognot +ui-bg-mask+))))
                     ((<= 30 p 37)
                      (setf attr (logior (logand attr (lognot +ui-fg-mask+))
                                         (- p 30))))
                     ((<= 40 p 47)
                      (setf attr (logior (logand attr (lognot +ui-bg-mask+))
                                         (ash (1+ (- p 40)) 4))))
                     ((<= 90 p 97)
                      (setf attr (logior (logand attr (lognot +ui-fg-mask+))
                                         (+ 8 (- p 90)))))
                     ((<= 100 p 107)
                      (setf attr (logior (logand attr (lognot +ui-bg-mask+))
                                         (ash (1+ (- p 100)) 4) 128)))
                     ((or (= p 38) (= p 48))
                      (let* ((fgp (= p 38))
                             (mask (if fgp +ui-fg-mask+ +ui-bg-mask+))
                             (mode (and ps (pop ps))))
                        (cond ((and mode (= mode 5) ps)
                               (let ((c (ui-c256 (pop ps))))
                                 (setf attr (logior (logand attr (lognot mask))
                                                    (if fgp c
                                                        (logior (ash (1+ c) 4) 128))))))
                              ((and mode (= mode 2))
                               (dotimes (k 3) (when ps (pop ps)))))))
                     (t nil))))
    attr))

(defun ui-split-params (s)
  (if (zerop (length s))
      (list 0)
      (loop with start = 0
            for i = (position #\; s :start start)
            collect (parse-integer s :start start :end (or i (length s))
                                   :junk-allowed t)
            while i
            do (setf start (1+ i)))))

(defun str-replace (new old s)
  (with-output-to-string (out)
    (let ((start 0))
      (loop for i = (search old s :start2 start)
            while i
            do (write-string s out :start start :end i)
               (write-string new out)
               (setf start (+ i (length old))))
      (write-string s out :start start))))

(defun ui-parse-ansi (line)
  "Parse a string with embedded SGR codes into a list of (text . attr) chunks."
  (let ((chunks nil)
        (attr 7)
        (cur (make-string-output-stream))
        (i 0)
        (n (length line)))
    (labels ((flush ()
               (let ((s (get-output-stream-string cur)))
                 (when (plusp (length s)) (push (cons s attr) chunks)))))
      (loop while (< i n)
            do (let ((ch (char line i)))
                 (cond
                  ((char= ch #\Esc)
                   (let ((j (1+ i)))
                     (if (and (< j n) (char= (char line j) #\[))
                         (let ((k (1+ j)))
                           (loop while (and (< k n)
                                            (not (alpha-char-p (char line k))))
                                 do (incf k))
                           (when (< k n)
                             (when (char= (char line k) #\m)
                               (flush)
                               (setf attr
                                     (ui-sgr-apply (ui-split-params
                                                    (subseq line (1+ j) k))
                                                   attr)))
                             (setf i (1+ k))))
                         (setf i (1+ j)))))
                  ((or (char= ch #\Return) (char= ch #\Nul)) (incf i))
                  (t (write-char ch cur) (incf i)))))
      (flush))
    (or (nreverse chunks) (list (cons "" 7)))))
