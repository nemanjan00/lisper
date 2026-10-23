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
   (render :initform nil :accessor pane-render)   ; fn (pane width height) -> chunk-line list
   (scroll :initform 0 :accessor pane-scroll)
   (needs-redraw :initform 1 :accessor pane-needs-redraw)))

(defvar *panes* (make-hash-table :test 'equal)
  "Registry of name -> pane.")

(defvar *layout* '(:hsplit 0.78 (leaf "chat") (leaf "todo"))
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


(defvar *panes-layout-file* (merge-pathnames "state/layout.lisp" lisping-core:*root*))

;;; ---------------------------------------------------------------
;;; Registry

(defun pane-register (name &key title kind cmd (interval 10))
  "Register (or replace) a pane. KIND is :chat :todo :buffer :cmd —
normalized to an UPPERCASE-name keyword, so a hand-edited or upcased
state/layout.lisp can't smuggle in :BUFFER (which silently disables
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
          (case (pane-kind p)
            (:chat #'pane-chat-render)
            (:todo #'pane-todo-render)
            (t #'pane-buf-render)))
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
          (let ((cur nil))
            (loop for (text . done) in *todos*
                  collect (list (cons (str-truncate
                                       (format nil "[~A] ~A" (if done "x" " ") text)
                                       w)
                                      (cond (done 8)
                                            ((null cur) (setf cur t)
                                             (ui-attr 7 :bright t :bg 4))
                                            (t 7)))))))
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
  "The hsplit whose 1-column separator gap contains the 0-based point,
as (node region-x region-width), or NIL. Walks the layout tree with the
same arithmetic as LAYOUT-LEAVES, so the column found is the one
PANES-DRAW-GAP-COLUMNS painted."
  (when *panes-canvas*
    (labels ((walk (tree x y w h)
               (when (consp tree)
                 (if (eq (car tree) :hsplit)
                     (destructuring-bind (_ ratio a b) tree
                       (declare (ignore _))
                       (let ((wa (hsplit-left-width w ratio)))
                         (if (and (= x0 (+ x wa)) (<= y y0) (< y0 (+ y h)))
                             (list tree x w)
                             (or (walk a x y wa h)
                                 (walk b (+ x wa 1) y (- w wa 1) h)))))
                     (when (eq (car tree) :vsplit)
                       (destructuring-bind (_ ratio a b) tree
                         (declare (ignore _))
                         (let ((ha (min (max 3 (round (* h (or ratio 0.5))))
                                        (- h 3))))
                           (or (walk a x y w ha)
                               (walk b x (+ y ha) w (- h ha))))))))))
      (walk *layout* 0 0 (uigrid-w *panes-canvas*) (uigrid-h *panes-canvas*)))))

(defun pane-drag-to (x0)
  "Move the dragged split's separator so its gap column is at 0-based
X0, clamped so both children keep HSPLIT-LEFT-WIDTH's 10-column
minimum."
  (when *pane-drag*
    (destructuring-bind (node rx rw) *pane-drag*
      (let ((wa (min (max 10 (- x0 rx)) (- rw 11))))
        (setf (cadr node) (/ (float wa 1.0d0) (float (max 1 (1- rw)) 1.0d0)))))))

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
    (let ((tattr (if (equal (pane-name p) *pane-focus*)
                     (ui-attr 7 :bright t :bg 4)
                     8)))
      (dotimes (i w) (ui-grid-put g (+ x i) y #\─ tattr))
      (ui-grid-write g (+ x 1) y (format nil " ~A " (pane-title p)) tattr (+ x w))
      (let ((lines (handler-case (funcall (pane-render p) p w (1- h))
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

(defun panes-draw-gap-columns (g rects)
  "Fill every cell no pane rect covers with a dim vertical bar — the
1-column gaps hsplit leaves between side-by-side panes, at any depth.
Coverage is checked PER CELL: a column partially covered (e.g. by a
full-width pane below a vsplit) must still get its bar in the uncovered
rows — the old per-column SOME let the context pane's full-width rect
suppress the separators above it.  (A cell is either pane-covered or
a gap; vsplit reserves no gap rows.)"
  (let ((w (uigrid-w g))
        (h (uigrid-h g)))
    (dotimes (x w)
      (dotimes (y h)
        (unless (some (lambda (r)
                        (and (<= (second r) x)
                             (< x (+ (second r) (fourth r)))
                             (<= (third r) y)
                             (< y (+ (third r) (fifth r)))))
                      rects)
          (ui-grid-put g x y (code-char 9474) 8))))))


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
  "Write layout + user pane definitions to state/layout.lisp as PURE
DATA (one plist), so loading never evaluates code. Skips the write
during test-boot's fresh process (same state/ directory, and a
transient file there would dirty the working tree)."
  (unless lisping-core:*test-boot*
    (ignore-errors
      (let ((*print-case* :downcase)
            (*print-readably* t))
        (with-open-file (s *panes-layout-file* :direction :output
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
       (let ((tree (getf (cdr form) :layout)))
         (when (consp tree) (setf *layout* tree)))
       (dolist (spec (getf (cdr form) :panes))
         (when (and (consp spec) (stringp (getf spec :name)))
           (pane-register (getf spec :name)
                          :title (getf spec :title)
                          :kind (getf spec :kind)
                          :cmd (getf spec :cmd)
                          :interval (or (getf spec :interval) 300)))))
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
  (when (probe-file *panes-layout-file*)
    (handler-case
        (let ((*read-eval* nil)
              (*package* (find-package "LISPING")))
          (with-open-file (s *panes-layout-file*)
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
  (setf *layout* '(:hsplit 0.78 (leaf "chat") (leaf "todo")))
  (panes-load)
  (unless (layout-valid-p *layout*)
    (setf *layout* '(:hsplit 0.78 (leaf "chat") (leaf "todo"))))
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
      (:set (let ((tree (read-from-string (lisping-core::arg args "sexpr"))))
              (if (layout-valid-p tree)
                  (progn (setf *layout* tree) (panes-save)
                         (tui-drop-frame) (tui-dirty)
                         (format nil "layout set: ~S" *layout*))
                  "refused: layout has unknown panes (create them first)")))
      (:reset (setf *layout* '(:hsplit 0.78 (leaf "chat") (leaf "todo")))
              (panes-save) (tui-drop-frame) (tui-dirty)
              (format nil "layout reset: ~S" *layout*)))))

(lisping-core::register-tool
 "pane" "Manage TUI panes. Actions: list | focus (name) | add (name, title, kind[buffer|cmd], cmd, interval) | split (name, direction h|v, ratio, new-name, title, kind, cmd, interval) | remove (name). The chat pane cannot be removed."
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
 "layout" "Manage the TUI layout tree. Actions: get | set (sexpr: tree of (:hsplit ratio a b) / (:vsplit ratio a b) / (leaf name)) | reset."
 (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj
                      "action" (lisping-core::obj "type" "string"
                                                 "enum" #("get" "set" "reset"))
                      "sexpr" (lisping-core::obj "type" "string"))
        "required" #("action"))
 'layout-tool)
