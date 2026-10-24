;;; slash.lisp — slash commands with an autocomplete popup (todo 9).
;;;
;;; Type "/" at the start of the input line and a popup lists the
;;; matching commands; Up/Down choose, Tab completes, Enter runs.
;;; Commands are rows of *SLASH-COMMANDS*: add one with DEFINE-SLASH.
;;;
;;; The completion engine is generic: a "source" returns candidates for
;;; the word under the cursor.  SLASH-SOURCE opens automatically while
;;; typing; sources in *COMPLETION-SOURCES* (path completion, todo 10)
;;; open on Tab.  The popup refreshes at draw time whenever the input
;;; text or cursor changed, so no hook inside TUI-NORMAL-KEY is needed.

(in-package :lisping)

;;; ---------------------------------------------------------------
;;; command table

(defvar *slash-commands* nil
  "List of (name fn doc args-fn).  FN takes (ui args-string).  ARGS-FN,
when non-nil, takes (ui partial-arg) and returns candidate strings for
the argument, offered in the popup after the command name.")

(defun define-slash (name fn doc &optional args-fn)
  "Register or replace slash command NAME (without the slash)."
  (setf *slash-commands*
        (append (remove name *slash-commands* :key #'first :test #'string=)
                (list (list name fn doc args-fn))))
  name)

(defun slash-find (name)
  (find name *slash-commands* :key #'first :test #'string=))

(defun slash-say (text)
  "Report TEXT in the chat as an info line."
  (emit :info :text text))

(defun slash-parse (text)
  "TEXT like \"/resume foo bar\" -> (values \"resume\" \"foo bar\"), or NIL
when TEXT is not a slash command (a path like /etc/passwd is not)."
  (when (and (> (length text) 1) (char= (char text 0) #\/)
             (not (find (char text 1) " /.~"))
             (not (find #\/ text :start 1 :end (position #\Space text))))
    (let ((sp (position #\Space text)))
      (values (subseq text 1 sp)
              (if sp (string-trim " " (subseq text sp)) "")))))

(defun slash-run (ui text)
  "Run TEXT as a slash command. Returns T when it was one (known or
not: an unknown command is reported, never sent to the model)."
  (multiple-value-bind (name args) (slash-parse text)
    (when name
      (let ((cmd (slash-find name)))
        (cond (cmd
               (unless (eq :keep-input
                           (handler-case (funcall (second cmd) ui args)
                             (error (c) (slash-say (format nil "/~A failed: ~A" name c)) nil)))
                 (tui-clear-input ui)))
              ;; Unknown: report it but keep the text, so the user can
              ;; fix it (or turn it into a plain message with //).
              (t (slash-say (format nil "Unknown command /~A — try /help; start with // to send a message that begins with a slash." name)))))
      t)))

;;; ---------------------------------------------------------------
;;; completion state: one popup, fed by sources

(defstruct completion
  items        ; list of (insert-string . display-string)
  (sel 0)      ; selected index
  (moved nil)  ; true once the user moved the highlight with Up/Down
  start        ; input index where the replaced word starts
  end)         ; input index where it ends

(defvar *completion* nil "The open completion popup, or NIL.")

(defvar *completion-seen* nil
  "(text . cursor) the popup was last refreshed for; a different input
state triggers a refresh at the next draw.")

(defvar *completion-sources* nil
  "Functions (ui text start end) -> list of (insert . display) or NIL,
tried in order on Tab; the first non-empty answer opens the popup.")

(defun completion-close ()
  (setf *completion* nil))

(defun completion-input-state (ui)
  (cons (copy-seq (coerce (tui-ui-input ui) 'string)) (tui-ui-input-cursor ui)))

(defun completion-word-bounds (ui)
  "Start/end of the word under the cursor (words split on spaces)."
  (let* ((in (tui-ui-input ui))
         (end (tui-ui-input-cursor ui))
         (start (let ((p (position-if (lambda (c) (member c '(#\Space #\Newline))) in :end end :from-end t)))
                  (if p (1+ p) 0))))
    (values start end)))

(defun slash-command-items (prefix)
  "Candidates for a command name PREFIX (no slash)."
  (loop for (name nil doc) in *slash-commands*
        when (and (<= (length prefix) (length name))
                  (string-equal prefix name :end2 (length prefix)))
          collect (cons (format nil "/~A " name)
                        (format nil "/~A  ~A" name (or doc "")))))

(defun slash-source (ui text start end)
  "Source for \"/command\" at the start of the input, and for the
argument of a command that has an ARGS-FN."
  (declare (ignore end))
  (when (and (plusp (length text)) (char= (char text 0) #\/))
    (let ((sp (position #\Space text)))
      (cond
        ;; still typing the command name
        ((and (zerop start) (null sp))
         (slash-command-items (subseq text 1)))
        ;; typing its argument
        ((and sp (plusp start))
         (let ((argf (fourth (slash-find (subseq text 1 sp)))))
           (when argf
             (loop for c in (funcall argf ui (string-left-trim " " (subseq text sp)))
                   collect (if (consp c) c (cons c c))))))))))

(defun completion-open (ui sources)
  "Ask SOURCES in order; open the popup with the first non-empty
answer (keeping the selection when still in range), else close it."
  (let ((text (coerce (tui-ui-input ui) 'string)))
    (multiple-value-bind (start end) (completion-word-bounds ui)
      (dolist (f sources (completion-close))
        (let ((items (and (plusp (length text))
                          (ignore-errors (funcall f ui text start end)))))
          (when items
            (return
              (setf *completion*
                    (make-completion
                     :items items :start start :end end)))))))))

(defun completion-replace-word (ui string)
  "Replace the popup's word in the input with STRING, cursor after it."
  (let* ((c *completion*)
         (in (tui-ui-input ui))
         (tail (subseq in (min (length in) (completion-end c)))))
    (setf (fill-pointer in) (completion-start c))
    (loop for ch across string do (vector-push-extend ch in))
    (loop for ch across tail do (vector-push-extend ch in))
    (setf (tui-ui-input-cursor ui) (+ (completion-start c) (length string)))))

(defun completion-common-prefix (items)
  "Longest prefix shared by the insert strings of ITEMS."
  (let ((p (car (first items))))
    (dolist (it (rest items) p)
      (let ((m (mismatch p (car it))))
        (when m (setf p (subseq p 0 m)))))))

(defun completion-extend (ui)
  "Vim-style first Tab: when every candidate shares a longer prefix
than the word typed so far, insert it and keep the popup open. Returns
T when it did."
  (let* ((c *completion*)
         (common (completion-common-prefix (completion-items c)))
         (typed (- (min (tui-ui-input-cursor ui) (length (tui-ui-input ui)))
                   (completion-start c))))
    (when (and (cdr (completion-items c)) (> (length common) typed))
      (completion-replace-word ui common)
      (setf (completion-end c) (tui-ui-input-cursor ui)
            (completion-sel c) 0)
      t)))

(defun completion-accept (ui)
  "Replace the word under the popup with the selected item."
  (let* ((c *completion*)
         (item (nth (completion-sel c) (completion-items c))))
    (completion-replace-word ui (car item))
    (completion-close)
    ;; Refresh now so a completed "/resume " immediately offers its
    ;; arguments; the state is recorded so the draw doesn't redo it.
    (completion-open ui '(slash-source))
    (setf *completion-seen* (completion-input-state ui))))

;;; ---------------------------------------------------------------
;;; keys

(defun slash-key (ui key)
  "Key interceptor: while the popup is open it owns Up/Down/Tab/Esc;
Tab with no popup asks the Tab sources (path completion)."
  (cond
    ((tui-queue-mode ui) (completion-close) nil)
    ((and *completion* (eq key :up))
     (setf (completion-moved *completion*) t)
     (setf (completion-sel *completion*)
           (mod (1- (completion-sel *completion*)) (length (completion-items *completion*))))
     t)
    ((and *completion* (eq key :down))
     (setf (completion-moved *completion*) t)
     (setf (completion-sel *completion*)
           (mod (1+ (completion-sel *completion*)) (length (completion-items *completion*))))
     t)
    ((and *completion* (characterp key) (char= key #\Tab))
     ;; Common prefix first; accept the highlighted item only when
     ;; there is nothing more to extend (or the user chose with Up/Down).
     (unless (and (not (completion-moved *completion*)) (completion-extend ui))
       (completion-accept ui))
     (setf *completion-seen* (completion-input-state ui))
     t)
    ((and *completion* (or (eq key :escape) (and (characterp key) (char= key #\Esc))))
     (completion-close)
     (setf *completion-seen* (completion-input-state ui))
     t)
    ((and (characterp key) (char= key #\Tab))
     (completion-open ui (cons 'slash-source *completion-sources*))
     ;; A single candidate completes at once, like vim; several that
     ;; share a longer prefix extend to it and leave the popup open.
     (when *completion*
       (if (null (cdr (completion-items *completion*)))
           (completion-accept ui)
           (completion-extend ui)))
     (setf *completion-seen* (completion-input-state ui))
     t)
    (t nil)))

(defvar *slash-enter-picks* nil
  "Command names (strings) whose popup is a picker: Enter takes the
highlighted item even when the highlight was not moved.")

(defun slash-enter-picks-p (text)
  (multiple-value-bind (name args) (slash-parse text)
    (and name (plusp (length (string-trim " " args)))
         (member name *slash-enter-picks* :test #'string=))))

(defun slash-submit (ui text)
  "Submit hook. Enter submits what was typed; the highlighted popup
item replaces it only when the user moved the highlight with Up/Down
(Tab is what accepts a suggestion). A text starting with // is a plain
message without one slash: returning a string makes TUI-SUBMIT-INPUT
send that instead of TEXT."
  (when (and *completion* (or (completion-moved *completion*)
                              (slash-enter-picks-p text))
             ;; Only a popup that belongs to this very input counts (a
             ;; keystroke may have arrived since the last redraw).
             (equal *completion-seen* (completion-input-state ui)))
    (let* ((start (completion-start *completion*))
           (item (nth (completion-sel *completion*) (completion-items *completion*)))
           (typed (subseq text (min (length text) start))))
      (when (and item (string/= (string-right-trim " " (car item)) typed))
        ;; TEXT arrives trimmed, so for "/model " (popup word at 7) it
        ;; is shorter than START: pad the gap back with spaces.
        (setf text (string-trim " " (concatenate 'string
                                                 (subseq text 0 (min start (length text)))
                                                 (make-string (max 0 (- start (length text)))
                                                              :initial-element #\Space)
                                                 (car item)))))))
  (completion-close)
  (slash-cache-reset)
  (cond ((and (> (length text) 1) (string= "//" text :end2 2))
         (subseq text 1))
        (t (slash-run ui text))))

;;; ---------------------------------------------------------------
;;; popup drawing (also the draw-time refresh)

(defun slash-draw-popup (g ui y w)
  "Overlay drawer: refresh the popup if the input changed, then paint
it in the rows ending just above Y (where the queue/input rows start)."
  (let ((state (completion-input-state ui)))
    (unless (equal state *completion-seen*)
      (setf *completion-seen* state)
      ;; Leaving "/command arg" typing drops the cached argument data.
      (unless (and (plusp (length (car state))) (char= (char (car state) 0) #\/)
                   (find #\Space (car state)))
        (slash-cache-reset))
      (if (tui-queue-mode ui)
          (completion-close)
          (completion-open ui '(slash-source)))))
  (when (and *completion* (completion-items *completion*))
    (let* ((items (completion-items *completion*))
           (n (length items))
           (rows (min n 8 (max 1 (- y 1))))
           (sel (min (completion-sel *completion*) (1- n)))
           (first-i (max 0 (min (- sel (1- rows)) (- n rows))))
           (top (- y rows)))
      (dotimes (k rows)
        (let* ((i (+ first-i k))
               (selp (= i sel))
               (line (format nil " ~A~A" (if selp "› " "  ") (cdr (nth i items)))))
          (ui-grid-write g 0 (+ top k)
                         (str-truncate (format nil "~vA" (1- w) line) w)
                         (if selp (ui-attr 0 :bg 6) (ui-attr 7 :bg 0))
                         w))))))

;;; ---------------------------------------------------------------
;;; built-in commands

(defun slash-help (ui args)
  (declare (ignore ui args))
  (slash-say
   (format nil "Slash commands:~{~%  /~A — ~A~}"
           (loop for (name nil doc) in *slash-commands*
                 collect name collect (or doc "")))))

(defvar *slash-cache* nil
  "(key . value): data a command's argument completion computed when
its popup opened, reused while the user keeps typing that argument.")

(defun slash-cache-reset () (setf *slash-cache* nil))

(defun slash-cached (key thunk)
  "THUNK's value, computed once per KEY until SLASH-CACHE-RESET."
  (if (and *slash-cache* (equal (car *slash-cache*) key))
      (cdr *slash-cache*)
      (cdr (setf *slash-cache* (cons key (funcall thunk))))))

(defun slash-session-names (ui partial)
  (declare (ignore ui))
  (loop for s in (slash-cached :sessions #'lisping-core:session-list)
        for n = (getf s :session)
        when (and (not (getf s :current))
                  (<= (length partial) (length n))
                  (string-equal partial n :end2 (length partial)))
          collect n))

(defun slash-resume (ui args)
  "/resume [name]: list the sessions, or switch to NAME."
  (declare (ignore ui))
  (if (string= args "")
      (slash-say
       (format nil "Sessions (/resume NAME to switch):~{~%~A~}"
               (loop for s in (lisping-core:session-list)
                     collect (format nil "  ~A~A  ~D msgs  ~A~A"
                                     (getf s :session)
                                     (if (getf s :current) " (this one)" "")
                                     (or (getf s :messages) 0)
                                     (or (getf s :title) "")
                                     (if (getf s :running) "  [running]" "")))))
      ;; SESSION-SWITCH restarts the image; it only returns (a string)
      ;; when the switch was refused.
      (let ((r (lisping-core:session-switch args)))
        (when (stringp r) (slash-say r)))))

(defun slash-clear-input (ui args)
  (declare (ignore args))
  (tui-clear-input ui))

(defun slash-install ()
  "Register the built-in commands and hooks. Idempotent."
  (define-slash "help" 'slash-help "list the slash commands")
  (define-slash "resume" 'slash-resume
                "list sessions, or /resume NAME to switch" 'slash-session-names)
  (define-slash "clear" 'slash-clear-input "clear the input line")
  (setf *tui-key-interceptors*
        (append (remove 'slash-key *tui-key-interceptors*) (list 'slash-key)))
  (setf *tui-submit-hooks*
        (append (remove 'slash-submit *tui-submit-hooks*) (list 'slash-submit)))
  (setf *tui-overlay-drawers*
        (append (remove 'slash-draw-popup *tui-overlay-drawers*) (list 'slash-draw-popup)))
  t)

(defun slash-start ()
  "Called from init.lisp once the UI exists."
  (unless lisping-core:*test-boot* (slash-install)))
