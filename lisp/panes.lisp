;;;; lisp/panes.lisp -- the window manager: pane registry + split-tree
;;;; layout. A pane is a named object with a render function; the
;;;; layout is a tree of (:hsplit ratio a b) / (:vsplit ratio a b) /
;;;; (leaf name). The redraw thread renders only DIRTY panes into a
;;;; persistent canvas; the frame diff still sends only changed cells.
;;;;
;;;; Built-in kinds: :chat (the conversation, backed by *tui*'s uibuf),
;;;; :todo (the agent's todo list), :buffer (plain text uibuf),
;;;; :cmd (a shell command re-run every INTERVAL seconds by the ticker).

(in-package :lisping)

(declaim (ftype function tui-visible-lines tui-chat-add tui-dirty))

(declaim (special *tui* *tui-lock* *todos* *tui-prev-grid*))

;;; PANE is a DEFCLASS (not defstruct) so slots can be added and
;;; redefined on live instances: SBCL reinitializes existing instances
;;; on class redefinition, while a defstruct redefinition breaks them.
;;; Accessors keep the old defstruct names, so call sites are
;;; unchanged. Construct with (make-instance 'pane :name name).
(defclass pane ()
  ((name :initarg :name :accessor pane-name)
   (title :initform "" :accessor pane-title)
   (kind :initform :buffer :accessor pane-kind)   ; :chat :todo :buffer :cmd
   (buf :initform nil :accessor pane-buf)         ; uibuf for :buffer/:cmd
   (cmd :initform nil :accessor pane-cmd)         ; shell command for :cmd
   (interval :initform 10 :accessor pane-interval) ; seconds between :cmd re-runs
   (last-run :initform 0 :accessor pane-last-run)
   (render :initform nil :accessor pane-render)   ; fn (pane width height) -> chunk-line list,
                                          ; or a SYMBOL naming one — late binding, so
                                          ; redefining the renderer never leaves a stale
                                          ; function object in the registry
   (scroll :initform 0 :accessor pane-scroll)
   (needs-redraw :initform 1 :accessor pane-needs-redraw)))

(defvar *panes* (make-hash-table :test 'equal)
  "Registry of name -> pane.")

(defun default-layout ()
  "A FRESH default layout tree. Dragging mutates ratio cells in place,
so *LAYOUT* must never be assigned a quoted literal: a drag after a
reset would rewrite the constant the next reset returns to."
  (list :hsplit 0.78 (list 'leaf "chat") (list 'leaf "todo")))

(defvar *layout* (default-layout)
  "Tree of (:hsplit ratio a b) / (:vsplit ratio a b) / (leaf name).")

(defvar *pane-focus* "chat" "Name of the focused pane; scrolling keys apply to it.")

;;; Chat auto-scroll state: how many wrapped lines arrived while the
;;; user was scrolled up (the "↓ N new" marker), and the width the chat
;;; pane last rendered at (for wrapping new lines when pinning).
(defvar *tui-chat-new* 0)

(defvar *tui-chat-pane-width* nil)

(defvar *panes-canvas* nil "Persistent grid holding the panes' area.")

(defvar *panes-init-hooks* nil
  "Functions run at the end of PANES-INIT, after builtins and the saved
layout are in place.  PANES-INIT wipes *panes* and *layout* on every
UI (re)start, so any subsystem that owns a pane must re-register
itself here (the compactor's context pane does).")


;;; Lazy: *HOME* is NIL in test-boot's fresh process (only BOOT sets it),
;;; so SESSION-PATH can't run in a load-time defvar.
(defvar *panes-layout-file* nil "This session's layout.lisp (see PANES-LAYOUT-FILE).")

(defun panes-layout-file ()
  (or *panes-layout-file*
      (setf *panes-layout-file* (lisping-core:session-path "layout.lisp"))))

;;; ---------------------------------------------------------------
;;; Registry

(defun pane-register (name &key title kind cmd (interval 10))
  "Register (or replace) a pane. KIND is :chat :todo :buffer :cmd —
normalized to an UPPERCASE-name keyword, so a hand-edited or upcased
saved layout file can't smuggle in :BUFFER (which silently disables
the ticker and save/load round-trips).  Uppercase, not lowercase: the
source literals (:chat :todo ...) and every EQ comparison elsewhere
use uppercase keyword names, so a lowercase :todo never matches them
— that mismatch sent every pane to pane-buf-render and left all
pane content blank (2026-10-23)."
  (let ((p (or (gethash name *panes*) (make-instance 'pane :name name)))
        (k (or kind :buffer)))
    (setf (pane-title p) (or title name)
          (pane-kind p) (intern (string-upcase (if (symbolp k) (symbol-name k) (string k)))
                                :keyword)
          (pane-cmd p) cmd
          (pane-interval p) (max 1 (or interval 10))
          (pane-needs-redraw p) 1)
    (unless (eq (pane-kind p) :chat)
      (unless (pane-buf p) (setf (pane-buf p) (make-uibuf :cap 500))))
    (setf (pane-render p)
          ;; Symbols, not #'objects: redefining a renderer must take
          ;; effect without re-registering the pane.
          (case (pane-kind p)
            (:chat 'pane-chat-render)
            (:todo 'pane-todo-render)
            (:tasks 'pane-tasks-render)
            (:ask 'pane-ask-render)
            (t 'pane-buf-render)))
    (setf (gethash name *panes*) p)
    name))

(defun pane-unregister (name)
  (remhash name *panes*))

(defun pane-get (name)
  (gethash name *panes*))

(defun pane-dirty (name)
  "Mark a pane for re-render and ask the redraw thread for a frame."
  (let ((p (pane-get name)))
    (when p (setf (pane-needs-redraw p) 1)))
  (tui-dirty))

(defun panes-mark-all-dirty ()
  (maphash (lambda (_ p) (declare (ignore _)) (setf (pane-needs-redraw p) 1))
           *panes*))

(defun pane-of-kind (kind)
  "First registered pane of KIND in layout order, or NIL."
  (loop for name in (layout-pane-names)
        for p = (pane-get name)
        when (and p (eq (pane-kind p) kind)) return p))

(defun pane-scroll-by (name delta)
  (sb-thread:with-recursive-lock (*tui-lock*)
    (let ((p (pane-get name)))
      (when p
        (setf (pane-scroll p) (max 0 (+ (pane-scroll p) delta)))
        ;; Reaching the bottom clears the 'N new' marker.
        (when (and (eq (pane-kind p) :chat) (zerop (pane-scroll p)))
          (setf *tui-chat-new* 0))
        (setf (pane-needs-redraw p) 1))))
  (tui-dirty))


(defun pane-focus-pane (name)
  "Focus a pane (highlights its title); scrolling keys apply to it.
Takes *TUI-LOCK* (marks every pane dirty; the redraw reads them)."
  (sb-thread:with-recursive-lock (*tui-lock*)
    (when (pane-get name)
      (setf *pane-focus* name)
      (panes-mark-all-dirty)
      (tui-dirty)))
  *pane-focus*)


(defun layout-pane-names (&optional (tree *layout*))
  "Leaf names of the layout tree, in drawing order."
  (cond ((atom tree) nil)
        ((eq (car tree) 'leaf) (list (cadr tree)))
        ((member (car tree) '(:hsplit :vsplit))
         (append (layout-pane-names (third tree))
                 (layout-pane-names (fourth tree))))
        (t nil)))

(defun pane-focus-nth (n)
  "Focus the Nth pane (1-based) in layout order."
  (let ((names (layout-pane-names)))
    (when (<= 1 n (length names))
      (pane-focus-pane (nth (1- n) names)))))

(defun pane-focus-step (delta)
  "Move focus DELTA panes forward (or back) in layout order, wrapping."
  (let* ((names (layout-pane-names))
         (n (length names))
         (pos (position *pane-focus* names :test #'equal)))
    (when (plusp n)
      (pane-focus-pane (nth (mod (+ (or pos 0) delta) n) names)))))

;;; ---------------------------------------------------------------
;;; Render functions. Each returns top-first chunk-lists, at most
;;; HEIGHT lines, honoring and clamping the pane's scroll.

(defun pane-chat-render (pane w h)
  (when *tui*
    (tui-visible-lines pane (tui-ui-chat *tui*) w h)))

(defun pane-buf-render (pane w h)
  (tui-visible-lines pane (pane-buf pane) w h))

(defun pane-todo-render (pane w h)
  (let* ((lines
          (if (null *todos*)
              (list (list (cons "(no open todos)" 8)))
              ;; The item set with the todo "start" action is the one
              ;; being worked on; only when none is set (or it is
              ;; gone/done) does the first open item stand in.
              (let* ((set (and (boundp '*todo-current*)
                               (let ((c (symbol-value '*todo-current*)))
                                 (and c (member c *todos* :test #'eq) (not (cdr c)) c))))
                     (cur (or set (find-if-not #'cdr *todos*))))
                (loop for item in *todos*
                      for (text . done) = item
                      collect (list (cons (str-truncate
                                           (format nil "[~A] ~A" (cond (done "x") ((eq item cur) ">") (t " ")) text)
                                           w)
                                          (cond (done 8)
                                                ((eq item cur) (ui-attr 7 :bright t :bg 4))
                                                (t 7))))))))
         (n (length lines))
         (s (max 0 (min (pane-scroll pane) (max 0 (- n h))))))
    (setf (pane-scroll pane) s)
    (let* ((end (max 0 (- n s)))
           (start (max 0 (- end h))))
      (subseq lines start end))))

;;; ---------------------------------------------------------------
;;; Layout tree

(defun hsplit-left-width (w ratio)
  "Width of the left child when a W-wide rectangle is split
side-by-side with a 1-column separator gap: each child keeps a
10-column minimum."
  (min (max 10 (round (* (1- w) (or ratio 0.5)))) (- w 11)))

(defun layout-leaves (tree x y w h)
  "Map the layout tree over a rectangle; returns a list of
\(name x y w h) in drawing order. Side-by-side splits reserve one
column between the children for the separator (see
PANES-DRAW-SEPARATOR)."
  (cond ((atom tree) nil)
        ((eq (car tree) 'leaf)
         (list (list (cadr tree) x y w h)))
        ((member (car tree) '(:hsplit :vsplit))
         (destructuring-bind (_ ratio a b) tree
           (if (eq (car tree) :hsplit)
               (let ((wa (hsplit-left-width w ratio)))
                 (append (layout-leaves a x y wa h)
                         (layout-leaves b (+ x wa 1) y (- w wa 1) h)))
               (let ((ha (min (max 3 (round (* h (or ratio 0.5)))) (- h 3))))
                 (append (layout-leaves a x y w ha)
                         (layout-leaves b x (+ y ha) w (- h ha)))))))
        (t nil)))

(defun layout-split-replace (tree name direction ratio new-name)
  "Replace (leaf NAME) with (direction ratio (leaf NAME) (leaf NEW-NAME))."
  (cond ((atom tree) nil)
        ((and (eq (car tree) 'leaf) (equal (cadr tree) name))
         (list direction (or ratio 0.5) tree (list 'leaf new-name)))
        ((member (car tree) '(:hsplit :vsplit))
         (list (car tree) (cadr tree)
               (layout-split-replace (caddr tree) name direction ratio new-name)
               (layout-split-replace (cadddr tree) name direction ratio new-name)))
        (t tree)))

(defun leaf-p (tree)
  (and (consp tree) (eq (car tree) 'leaf)))

(defun layout-remove-leaf (tree name)
  "Replace the split holding (leaf NAME) with its other subtree."
  (cond ((atom tree) tree)
        ((eq (car tree) 'leaf) tree)
        (t
         (destructuring-bind (op ratio a b) tree
           (let ((a2 (layout-remove-leaf a name))
                 (b2 (layout-remove-leaf b name)))
             (cond ((and (leaf-p a) (equal (cadr a) name)) b2)
                   ((and (leaf-p b) (equal (cadr b) name)) a2)
                   (t (list op ratio a2 b2))))))))

(defun layout-names (tree)
  (cond ((atom tree) nil)
        ((eq (car tree) 'leaf) (list (cadr tree)))
        (t (append (layout-names (caddr tree)) (layout-names (cadddr tree))))))

(defun layout-valid-p (tree)
  "Every leaf names a registered pane, and at least one leaf exists."
  (and (layout-leaves tree 0 0 100 100)
       (every (lambda (n) (pane-get n)) (layout-names tree))))

(defun pane-at (x0 y0)
  "Name of the pane whose rect contains the 0-based point, or NIL."
  (when *panes-canvas*
    (dolist (rect (layout-leaves *layout* 0 0 (uigrid-w *panes-canvas*)
                                 (uigrid-h *panes-canvas*)))
      (destructuring-bind (name rx ry rw rh) rect
        (when (and (<= rx x0) (< x0 (+ rx rw))
                   (<= ry y0) (< y0 (+ ry rh)))
          (return name))))))

;;; ---------------------------------------------------------------
;;; Separator dragging (mouse): press on an hsplit's gap column grabs
;;; that split; motion updates its ratio; release saves the layout.

(defvar *pane-drag* nil
  "While an hsplit separator is dragged: (split-node region-x
region-width); the node's CADR is the ratio to mutate.")

(defvar *pane-drag-last-draw* 0
  "get-internal-real-time of the last drag repaint (throttle).")

(defun split-gap-at (x0 y0)
  "The split whose separator contains the 0-based point, as
(node region-x region-y region-w region-h direction), or NIL. For an
:hsplit the separator is its 1-column gap; for a :vsplit there is no
gap row, so the handle is the FIRST ROW of the lower child (its title
row) — that row sits exactly on the boundary. Same arithmetic as
LAYOUT-LEAVES, so the cell found is the one the user sees as the
separator."
  (when *panes-canvas*
    (labels ((walk (tree x y w h)
               (when (consp tree)
                 (if (eq (car tree) :hsplit)
                     (destructuring-bind (_ ratio a b) tree
                       (declare (ignore _))
                       (let ((wa (hsplit-left-width w ratio)))
                         (if (and (= x0 (+ x wa)) (<= y y0) (< y0 (+ y h)))
                             (list tree x y w h :hsplit)
                             (or (walk a x y wa h)
                                 (walk b (+ x wa 1) y (- w wa 1) h)))))
                     (when (eq (car tree) :vsplit)
                       (destructuring-bind (_ ratio a b) tree
                         (declare (ignore _))
                         (let ((ha (min (max 3 (round (* h (or ratio 0.5))))
                                        (- h 3))))
                           (if (and (= y0 (+ y ha)) (<= x x0) (< x0 (+ x w)))
                               (list tree x y w h :vsplit)
                               (or (walk a x y w ha)
                                   (walk b x (+ y ha) w (- h ha)))))))))))
      (walk *layout* 0 0 (uigrid-w *panes-canvas*) (uigrid-h *panes-canvas*)))))

(defun pane-drag-to (x0 y0)
  "Move the dragged split's separator to the 0-based point: an :hsplit
follows X0, a :vsplit follows Y0. Clamped so both children keep their
minimum (10 columns / 3 rows), then ratio = size / region-size, the
inverse of LAYOUT-LEAVES' split arithmetic."
  (when *pane-drag*
    (destructuring-bind (node rx ry rw rh dir) *pane-drag*
      (if (eq dir :hsplit)
          (let ((wa (min (max 10 (- x0 rx)) (- rw 11))))
            (setf (cadr node) (/ (float wa 1.0d0) (float (max 1 (1- rw)) 1.0d0))))
          (let ((ha (min (max 3 (- y0 ry)) (- rh 3))))
            (setf (cadr node) (/ (float ha 1.0d0) (float rh 1.0d0))))))))

;;; ---------------------------------------------------------------
;;; Drawing

(defun panes-ensure-canvas (w h)
  (when (or (null *panes-canvas*)
            (/= (uigrid-w *panes-canvas*) w)
            (/= (uigrid-h *panes-canvas*) h))
    (setf *panes-canvas* (ui-grid-make w h))
    (panes-mark-all-dirty)))

(defun pane-draw-into (g p x y w h)
  "Clear the pane's rect and draw its border + content — but only when
it needs a redraw; a clean pane's cells stay as they are. A render or
draw error is logged and drawn inside the pane instead of killing the
redraw thread."
  (when (plusp (pane-needs-redraw p))
    (unwind-protect
         (handler-case (pane-draw-into-1 g p x y w h)
           (serious-condition (c)
             (ignore-errors (tui-log-crash c))
             (ignore-errors
               (dotimes (j h)
                 (dotimes (i w)
                   (ui-grid-put g (+ x i) (+ y j) #\Space 7)))
               (ui-grid-write g x y
                              (format nil " pane error: ~A"
                                      (let ((s (substitute #\Space #\Newline (princ-to-string c))))
                                        (subseq s 0 (min 80 (length s)))))
                              3 (+ x w)))))
      (setf (pane-needs-redraw p) 0))))

(defun pane-draw-into-1 (g p x y w h)
  "The actual border + content drawing for PANE."
  (progn
    (dotimes (j h)
      (dotimes (i w)
        (ui-grid-put g (+ x i) (+ y j) #\Space 7)))
    (let ((focused (equal (pane-name p) *pane-focus*)))
      (dotimes (i w) (ui-grid-put g (+ x i) y #\─ (if focused 6 8)))
      ;; Title as a powerline segment: cyan when focused, grey otherwise.
      (ui-grid-write-chunks g x y
                            (tui-seg-chunks (pane-title p) (if focused 0 7) (if focused 6 :grey))
                            (+ x w))
      (let ((lines (handler-case
                       (let ((r (pane-render p)))
                         (funcall (if (symbolp r)
                                      (if (fboundp r) (symbol-function r) #'pane-buf-render)
                                      r)
                                  p w (1- h)))
                   (serious-condition (c)
                     ;; Log render errors, and show the failure in the
                     ;; pane instead of silently drawing nothing.
                     (tui-log-crash c)
                     (list (list (cons (format nil " render error: ~A" c) 3)))))))
        (when (eq (pane-kind p) :chat)
          (setf *tui-chat-pane-width* w))
        (when lines
          (loop for line in lines
                for j from 1
                while (< j h)
                do (ui-grid-write-chunks g x (+ y j) line (+ x w))))
        ;; "↓ N new" marker on the bottom edge while scrolled up.
        (when (and (eq (pane-kind p) :chat)
                   (plusp (pane-scroll p))
                   (plusp *tui-chat-new*))
          (let* ((label (format nil " ~D new " *tui-chat-new*))
                 (lx (max x (- (+ x w) (length label) 1))))
            (ui-grid-write g lx (+ y h -1)
                           (concatenate 'string "↓" label) 8 (+ x w))))))
    (setf (pane-needs-redraw p) 0)))

(defun panes-draw (g)
  "Render every dirty pane at its layout rect, then repaint the gap
columns between side-by-side panes with a dim vertical line (every
hsplit reserves one uncovered column — see hsplit-left-width)."
  (let ((rects (layout-leaves *layout* 0 0 (uigrid-w g) (uigrid-h g))))
    (dolist (rect rects)
      (destructuring-bind (name x y w h) rect
        (let ((p (pane-get name)))
          (when p (pane-draw-into g p x y w h)))))
    (panes-draw-gap-columns g rects)))

(defun panes-grid-char (g x y)
  "Char at (X, Y) in G, or #\\Space out of bounds."
  (if (and (<= 0 x) (< x (uigrid-w g)) (<= 0 y) (< y (uigrid-h g)))
      (char (uigrid-chars g) (+ x (* y (uigrid-w g))))
      #\Space))

(defun box-v-p (c) (and c (find c "│┬┴├┤┼")))  ; connects vertically
(defun box-h-p (c) (and c (find c "─┬┴├┤┼")))  ; connects horizontally

(defun panes-rect-cell-p (rects x y &optional title-row-only)
  "True when (X, Y) lies in one of RECTS (name x y w h); with
TITLE-ROW-ONLY, only on a rect's top (title) row."
  (some (lambda (r)
          (and (<= (second r) x) (< x (+ (second r) (fourth r)))
               (if title-row-only
                   (= y (third r))
                   (and (<= (third r) y) (< y (+ (third r) (fifth r)))))))
        rects))

(defun panes-gap-junction-char (g x y rects)
  "The box-drawing char for an uncovered (gap) cell at (X, Y).  Where the
vertical gap bar meets a pane's title line it becomes ┬/┴/├/┤/┼.
Connectivity comes from the layout RECTS, not from the drawn glyphs:
a title row is horizontal whatever its first cells show (powerline
title segments are not ─), and pane content (chat text may contain
box-drawing chars) never connects."
  (flet ((gap (cx cy) (and (< -1 cx (uigrid-w g)) (< -1 cy (uigrid-h g))
                           (not (panes-rect-cell-p rects cx cy)))))
    (let ((u (gap x (1- y)))
          (d (gap x (1+ y)))
          (l (panes-rect-cell-p rects (1- x) y t))
          (r (panes-rect-cell-p rects (1+ x) y t)))
      (cond ((and u d l r) #\┼)
            ((and u d r)   #\├)
            ((and u d l)   #\┤)
            ((and d l r)   #\┬)
            ((and u l r)   #\┴)
            (t             #\│)))))

(defun panes-draw-gap-columns (g rects)
  "Fill every cell no pane rect covers with a dim vertical bar — the
1-column gaps hsplit leaves between side-by-side panes, at any depth.
Where the bar meets a pane's horizontal title line the cell becomes the
proper junction (┬ ┴ ├ ┤ ┼; PANES-GAP-JUNCTION-CHAR reads the
neighbours), and a title-line cell a gap bar runs into from above or
below becomes ┴/┬/┼ too.  Coverage is checked PER CELL: a column
partially covered (e.g. by a full-width pane below a vsplit) must still
get its bar in the uncovered rows — the old per-column SOME let the
context pane's full-width rect suppress the separators above it.
(A cell is either pane-covered or a gap; vsplit reserves no gap rows.)"
  (let ((w (uigrid-w g))
        (h (uigrid-h g)))
    (flet ((gap-p (x y)
             (not (some (lambda (r)
                          (and (<= (second r) x)
                               (< x (+ (second r) (fourth r)))
                               (<= (third r) y)
                               (< y (+ (third r) (fifth r)))))
                        rects))))
      ;; Bars first, so the junction passes see their vertical neighbours.
      (dotimes (x w)
        (dotimes (y h)
          (when (gap-p x y)
            (ui-grid-put g x y #\│ 8))))
      (dotimes (x w)
        (dotimes (y h)
          (when (gap-p x y)
            (ui-grid-put g x y (panes-gap-junction-char g x y rects) 8))))
      ;; Title rows: where a gap bar ends at a pane's ─ title line from
      ;; above, that ─ becomes ┴.  The cell above must be an uncovered
      ;; GAP cell — a pane-covered neighbour is just chat text, which
      ;; may legitimately contain box-drawing chars.
      (dolist (r rects)
        (destructuring-bind (name rx ry rw rh) r
          (declare (ignore name rh))
          (dotimes (i rw)
            (let ((cx (+ rx i)))
              (when (and (eql (panes-grid-char g cx ry) #\─)
                         (plusp ry)
                         (gap-p cx (1- ry)))
                (ui-grid-put g cx ry #\┴
                             (aref (uigrid-attrs g) (+ cx (* ry w))))))))))))


(defun panes-junction-problems (layout w h)
  "Render only the gap columns of LAYOUT on a W×H grid whose title rows
start with a non-─ glyph (a powerline segment), then list every gap cell
that touches a title row but is not a junction, and every title cell
directly under a gap bar that is not ┴/┼.  NIL when the grid is right.
Independent of the draw code's connectivity logic: it only restates the
rule \"a separator meeting a title line is a ┬ ┴ ├ ┤ ┼, never a │\".
A cell is a gap when no rect covers it."
  (let* ((rects (layout-leaves layout 0 0 w h))
         (g (ui-grid-make w h))
         (problems nil))
    (dolist (r rects)
      (destructuring-bind (name rx ry rw rh) r
        (declare (ignore name rh))
        (dotimes (i rw)
          (ui-grid-put g (+ rx i) ry (if (< i 3) #\A #\─) 8))))
    (panes-draw-gap-columns g rects)
    (flet ((gap-p (x y) (and (< -1 x w) (< -1 y h) (not (panes-rect-cell-p rects x y))))
           (title-p (x y) (and (< -1 x w) (< -1 y h) (panes-rect-cell-p rects x y t))))
      (dotimes (y h)
        (dotimes (x w)
          (let ((c (panes-grid-char g x y)))
            (when (gap-p x y)
              (when (and (or (title-p (1- x) y) (title-p (1+ x) y))
                         (not (find c "┬┴├┤┼")))
                (push (list :gap-not-junction x y c) problems))
              (when (and (title-p (1- x) y) (title-p (1+ x) y) (not (find c "┬┴┼")))
                (push (list :between-titles x y c) problems)))
            (when (and (title-p x y) (gap-p x (1- y)) (not (find c "┴┼")))
              (push (list :title-under-bar x y c) problems))))))
    (nreverse problems)))

(defun panes-junction-selftest ()
  "Run PANES-JUNCTION-PROBLEMS over the default and the live layout at a
few sizes; WARN on any problem, so a regression fails the boot check."
  (dolist (layout (remove-duplicates
                   (list (default-layout) *layout*
                         '(:vsplit 0.85 (:hsplit 0.78 (leaf "chat") (leaf "todo"))
                           (:hsplit 0.68 (leaf "context") (leaf "tasks"))))
                   :test #'equal))
    (dolist (size '((208 40) (120 30) (80 24)))
      (let ((p (ignore-errors (panes-junction-problems layout (first size) (second size)))))
        (when p
          (warn "pane separator junctions wrong at ~Dx~D: ~S" (first size) (second size)
                (subseq p 0 (min 3 (length p)))))))))

(unless (boundp '*layout*) (defvar *layout* nil))
(when lisping-core:*test-boot* (panes-junction-selftest))

;;; ---------------------------------------------------------------
;;; Operations (also exposed as tools)

(defun tui-drop-frame ()
  "Drop all frame state so the next redraw repaints from scratch: fresh
panes canvas, every pane dirty, no diff against the previous frame.
Called after anything that moves, resizes or re-creates panes
(split/add/remove, focus, resize); TUI-FORCE-REDRAW (C-b r) is this
plus nothing else.  Takes *TUI-LOCK*: the redraw thread reads
*panes-canvas*/*layout* while holding it, so a mutation racing the
draw used to leave it staring at NIL ('NIL is not of type UIGRID',
tui-crash.log 21:05)."
  (sb-thread:with-recursive-lock (*tui-lock*)
    (setf *panes-canvas* nil
          *tui-prev-grid* nil)
    (panes-mark-all-dirty))
  (tui-dirty))


(defun pane-add (name &key title kind cmd (interval 10))
  (sb-thread:with-recursive-lock (*tui-lock*)
    (pane-register name :title title :kind kind :cmd cmd :interval interval)
    (panes-save)
    (tui-drop-frame)
    (pane-dirty name)))


(defun pane-split (name direction ratio new-name
                   &key title kind cmd (interval 10))
  "Split pane NAME, putting a new pane beside it. DIRECTION is
'h' (side by side) or 'v' (stacked). NEW-NAME may already exist.
Takes *TUI-LOCK* (mutates *layout* while the redraw thread draws)."
  (sb-thread:with-recursive-lock (*tui-lock*)
    (unless (pane-get name) (error "no pane named ~A" name))
    (unless (pane-get new-name)
      (pane-register new-name :title title :kind kind :cmd cmd :interval interval))
    (setf *layout* (layout-split-replace *layout* name
                                         (if (equal direction "v") :vsplit :hsplit)
                                         (or ratio 0.5) new-name))
    (panes-save)
    (tui-drop-frame)
    (format nil "split ~A ~A: added ~A (~A/~A)" name direction new-name
            (pane-kind (pane-get new-name))
            (or (pane-cmd (pane-get new-name)) "buffer"))))


(defun pane-remove (name)
  (sb-thread:with-recursive-lock (*tui-lock*)
    (when (equal name "chat") (error "the chat pane cannot be removed"))
    (setf *layout* (layout-remove-leaf *layout* name))
    (pane-unregister name)
    (when (equal *pane-focus* name) (setf *pane-focus* "chat"))
    (panes-save)
    (tui-drop-frame))
  (format nil "removed pane ~A" name))


;;; ---------------------------------------------------------------
;;; Persistence

(defun panes-save ()
  "Write layout + user pane definitions to the session's layout.lisp as
PURE DATA (one plist), so loading never evaluates code. Skips the write
during test-boot's fresh process (it shares the session directory, and a
transient write there could clobber the live layout)."
  (unless lisping-core:*test-boot*
    (ignore-errors
      (let ((*print-case* :downcase)
            (*print-readably* t))
        (with-open-file (s (panes-layout-file) :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
          (format s ";;; auto-generated by panes.lisp — pure data; read with *read-eval* nil.~%")
          (format s "(:layout ~S~% :panes ~S)~%"
                  *layout*
                  (let (specs)
                    (maphash
                     (lambda (_ p)
                       (declare (ignore _))
                       ;; Downcased kinds: an upcased *print-case* would
                       ;; write :BUFFER, which old images load back as a
                       ;; kind the ticker never matches.
                       (when (member (pane-kind p) '(:buffer :cmd))
                         (push (list :name (pane-name p)
                                     :title (pane-title p)
                                     :kind (pane-kind p)
                                     :cmd (pane-cmd p)
                                     :interval (pane-interval p))
                               specs)))
                     *panes*)
                    (nreverse specs))))))))

(defun panes-load-form (form)
  "Interpret one form from the layout file. New format is a single
(:layout tree :panes (spec ...)) plist; the old code format (setf /
pane-register forms) is accepted for migration."
  (when (consp form)
    (cond
      ((eq (car form) :layout)
       ;; The form itself is a plist (:layout tree :panes specs) — getf
       ;; on the whole form is immune to key order.
       (let ((tree (getf form :layout)))
         (when (consp tree) (setf *layout* tree)))
       (dolist (spec (getf form :panes))
         (when (and (consp spec) (stringp (getf spec :name)))
           ;; The file stores only :kind; re-registering must not clobber
           ;; a custom PANE-RENDER an init hook installed (the context
           ;; pane is :kind :buffer + PANE-CONTEXT-RENDER). Preserve the
           ;; render fn when the pane exists and its kind is unchanged.
           (let* ((name (getf spec :name))
                  (existing (pane-get name))
                  (render (when (and existing
                                     (getf spec :kind)
                                     (string-equal (string (pane-kind existing))
                                                   (string (getf spec :kind))))
                            (pane-render existing))))
             (pane-register name
                            :title (getf spec :title)
                            :kind (getf spec :kind)
                            :cmd (getf spec :cmd)
                            :interval (or (getf spec :interval) 300))
             (when render
               (setf (pane-render (pane-get name)) render))))))
      ((eq (car form) 'setf)
       (when (and (eq (second form) '*layout*)
                  (consp (third form))
                  (eq (car (third form)) 'quote))
         (setf *layout* (second (third form)))))
      ((eq (car form) 'pane-register)
       (apply #'pane-register (second form) (cddr form))))))

(defun panes-load ()
  "Read the layout file as DATA: *read-eval* is NIL so #. cannot run,
and reading happens in the LISPING package explicitly. Never evaluates."
  (when (probe-file (panes-layout-file))
    (handler-case
        (let ((*read-eval* nil)
              (*package* (find-package "LISPING")))
          (with-open-file (s (panes-layout-file))
            (loop for form = (read s nil nil)
                  while form do (panes-load-form form))))
      (error (c) (warn "panes-load: ~A" c)))))

;;; ---------------------------------------------------------------
;;; Command ticker: re-runs :cmd panes every INTERVAL seconds.

(defun panes-refresh-cmds ()
  "Two-phase so the SHELL call happens OUTSIDE *tui-lock*
(a slow command must not freeze the UI); only registry reads/writes
and the buffer write hold the lock."
  (let ((now (get-universal-time)))
    (let (due)
      (sb-thread:with-recursive-lock (*tui-lock*)
        (maphash
         (lambda (_ p)
           (declare (ignore _))
           (when (and (eq (pane-kind p) :cmd) (pane-cmd p)
                      (>= (- now (pane-last-run p)) (pane-interval p)))
             (setf (pane-last-run p) now)
             (push p due)))
         *panes*))
      (dolist (p due)
        (ignore-errors
          (multiple-value-bind (out code) (shell (pane-cmd p))
            (sb-thread:with-recursive-lock (*tui-lock*)
              (setf (pane-buf p) (make-uibuf :cap 500))
              (uibuf-add-text (pane-buf p)
                              (if (zerop code)
                                  out
                                  (format nil "~A (exit ~A)" out code))
                              8)
              (setf (pane-needs-redraw p) 1)
              (tui-dirty))))))))


(defun panes-ticker-loop ()
  (loop while *tui* do (ignore-errors (panes-refresh-cmds)) (sleep 1)))

;;; ---------------------------------------------------------------
;;; Init

(defun panes-init ()
  "Install builtins, load the saved layout, validate, start the ticker,
then run *PANES-INIT-HOOKS*: the init wipes *PANES*/*LAYOUT*, so every
subsystem that owns a pane (the compactor's context pane) re-registers
itself here or vanishes on UI restart."
  (setf *panes* (make-hash-table :test 'equal)
        *pane-focus* "chat"
        *panes-canvas* nil)
  (pane-register "chat" :title "chat" :kind :chat)
  (pane-register "todo" :title "todo" :kind :todo)
  (setf *layout* (default-layout))
  (panes-load)
  (unless (layout-valid-p *layout*)
    (setf *layout* (default-layout)))
  (unless (member "todo" (layout-names *layout*) :test #'equal)
    (setf *layout* `(:hsplit 0.78 ,*layout* (leaf "todo"))))
  (panes-mark-all-dirty)
  (sb-thread:make-thread #'panes-ticker-loop :name "tui-panes")
  (dolist (hook *panes-init-hooks*) (funcall hook)))


;;; ---------------------------------------------------------------
;;; Tools

(defun pane-tool (args)
  (let ((action (or (lisping-core::arg args "action") "list")))
    (ecase (intern (string-upcase action) :keyword)
      (:list
       (with-output-to-string (s)
         (format s "focus: ~A~%layout: ~S~%" *pane-focus* *layout*)
         (maphash (lambda (_ p)
                    (format s "  ~A [~A]~A title=~S scroll=~D~%"
                            (pane-name p) (pane-kind p)
                            (if (equal (pane-name p) *pane-focus*) "*" " ")
                            (pane-title p) (pane-scroll p)))
                  *panes*)))
      (:focus (pane-focus-pane (lisping-core::arg args "name")))
      (:add (pane-add (lisping-core::arg args "name")
                      :title (lisping-core::arg args "title")
                      :kind (intern (string-upcase (or (lisping-core::arg args "kind") "buffer")) :keyword)
                      :cmd (lisping-core::arg args "cmd")
                      :interval (or (lisping-core::arg args "interval") 10)))
      (:split (pane-split (lisping-core::arg args "name")
                          (or (lisping-core::arg args "direction") "h")
                          (lisping-core::arg args "ratio")
                          (lisping-core::arg args "new-name")
                          :title (lisping-core::arg args "title")
                          :kind (intern (string-upcase (or (lisping-core::arg args "kind") "buffer")) :keyword)
                          :cmd (lisping-core::arg args "cmd")
                          :interval (or (lisping-core::arg args "interval") 10)))
      (:remove (pane-remove (lisping-core::arg args "name"))))))

(defun layout-normalize-leaves (tree)
  "Rewrite (LEAF name) heads named \"LEAF\" in any package to
LISPING::LEAF: layout sexprs read from strings (the layout tool's SET)
must not depend on which package READ-FROM-STRING ran in."
  (if (atom tree)
      tree
      (mapcar (lambda (x)
                (if (and (symbolp x) (string-equal (symbol-name x) "LEAF"))
                    'leaf
                    (layout-normalize-leaves x)))
              tree)))

(defun layout-tool (args)
  (let ((action (or (lisping-core::arg args "action") "get")))
    (ecase (intern (string-upcase action) :keyword)
      (:get (format nil "~S" *layout*))
      (:set (let ((tree (layout-normalize-leaves
                         (read-from-string (lisping-core::arg args "sexpr")))))
              (if (layout-valid-p tree)
                  (progn (setf *layout* tree) (panes-save)
                         (tui-drop-frame) (tui-dirty)
                         (format nil "layout set: ~S" *layout*))
                  "refused: layout has unknown panes (create them first)")))
      (:reset (setf *layout* (default-layout))
              (panes-save) (tui-drop-frame) (tui-dirty)
              (format nil "layout reset: ~S" *layout*)))))

(lisping-core::register-tool
 "pane" "Manage TUI panes. list: one line per pane (name [kind] title scroll), * marks the focused pane. focus: focus pane `name`. add: register a pane without placing it — needs `name`; optional `title`, `kind` (buffer = static text, the default; cmd = run shell `cmd` every `interval` seconds and show its output). split: replace pane `name`'s leaf with a split — `direction` h = side by side, v = stacked; `ratio` is the existing pane's share of the space and the new pane `new-name` gets the rest (e.g. split todo v 0.8 hello puts hello under todo at 20%); `new-name` may be an already-registered pane (from add) or is created with the given title/kind/cmd/interval. remove: remove pane `name` and close its split; the chat pane cannot be removed. split/remove save the session layout file and redraw from scratch. Returns a status line; list returns the pane lines. Example: {\"action\":\"split\",\"name\":\"todo\",\"direction\":\"v\",\"ratio\":0.8,\"new-name\":\"hello\",\"title\":\"hello\"}."
 (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj
                      "action" (lisping-core::obj "type" "string"
                                                 "enum" #("list" "focus" "add" "split" "remove"))
                      "name" (lisping-core::obj "type" "string")
                      "new-name" (lisping-core::obj "type" "string")
                      "title" (lisping-core::obj "type" "string")
                      "kind" (lisping-core::obj "type" "string")
                      "cmd" (lisping-core::obj "type" "string")
                      "direction" (lisping-core::obj "type" "string")
                      "ratio" (lisping-core::obj "type" "number")
                      "interval" (lisping-core::obj "type" "integer"))
        "required" #("action"))
 'pane-tool)

(lisping-core::register-tool
 "layout" "Manage the TUI layout tree. get: return the current tree as a printed sexpr. set: replace it — `sexpr` is a string holding one sexpr of this grammar: TREE := (leaf \"pane-name\") | (:hsplit RATIO TREE TREE) | (:vsplit RATIO TREE TREE); :hsplit puts the first child left of the second (1-column separator between), :vsplit stacks it above, RATIO is the first child's share of the space (0..1). Every leaf must name a registered pane (see the pane tool); set refuses unknown names, otherwise applies, saves the session layout file and redraws. Example sexpr: (:vsplit 0.85 (:hsplit 0.75 (leaf \"chat\") (leaf \"todo\")) (leaf \"context\")) — chat left, todo right, context below. reset: back to the default chat+todo split. get's output is the same grammar (printed with package prefixes, e.g. lisping::leaf — both spellings are accepted on set)."
 (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj
                      "action" (lisping-core::obj "type" "string"
                                                 "enum" #("get" "set" "reset"))
                      "sexpr" (lisping-core::obj "type" "string"))
        "required" #("action"))
 'layout-tool)
