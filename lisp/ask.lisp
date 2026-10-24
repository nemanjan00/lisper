;;;; lisp/ask.lisp -- ask_user: blocking multiple-choice questions.
;;;;
;;;; One call can ask several QUESTIONS, each with a TITLE (short label),
;;;; a BODY (the actual question text), OPTIONS ({label, description?}),
;;;; a mode — radio (multi:false) or checklist (multi:true) — and OTHER
;;;; (allow a free-text answer, default true).  The ask pane (below chat)
;;;; shows one question at a time; the user moves the cursor with Up/Down,
;;;; picks with Enter (radio) or toggles with Space/1-9 (checklist) and
;;;; confirms with Enter, clicks an option row, and switches questions
;;;; with Left/Right.  Typing text + Enter gives a free-text Other answer
;;;; (the input line doubles as the Other editor; the Other row shows the
;;;; text live).  A queued message whose text parses as option numbers
;;;; (like 2, or 1,3 for a checklist) also answers; anything else waits.
;;;; ASK-START is called from init.lisp.

(in-package :lisping)

(defstruct askq
  title body options multi other
  (cursor 1) picked (selected nil) (other-text nil))
;; OPTIONS: list of (label . description-or-nil)
;; CURSOR: 1-based row; N+1 is the Other row (when OTHER)
;; PICKED: 1-based index (radio); SELECTED: sorted indices (checklist)

(defstruct ask
  questions (index 0) mailbox)

(defvar *ask-active* nil "The open question set (an ASK), or NIL.")

(defun ask-current-q (a) (nth (ask-index a) (ask-questions a)))
(defun askq-noptions (q) (length (askq-options q)))
(defun askq-other-row (q) (and (askq-other q) (1+ (askq-noptions q))))

(defun askq-answered-p (q)
  (or (askq-other-text q)
      (if (askq-multi q) (askq-selected q) (askq-picked q))))

(defun ask-all-answered-p (a)
  (every #'askq-answered-p (ask-questions a)))

(defun ask-split-lines (s)
  (loop with start = 0
        for pos = (position #\Newline s :start start)
        collect (subseq s start (or pos (length s)))
        while pos do (setf start (1+ pos))))

(defun askq-body-lines (q)
  (let ((b (askq-body q)))
    (and b (plusp (length b)) (ask-split-lines b))))

;;; ---------------------------------------------------------------
;;; pane

(defun pane-ask-render (pane w h)
  (let ((a *ask-active*))
    (if (null a)
        (list (list (cons "(no pending question)" 8)))
        (pane-window pane (ask-render-lines a w) h :top))))

(defun ask-render-lines (a w)
  "All lines of the question pane for the active ask A, at width W."
  (progn
    (progn
        (let* ((q (ask-current-q a))
               (n (askq-noptions q))
               (multi (askq-multi q))
               (nq (length (ask-questions a)))
               (typed (and *tui* (copy-seq (tui-ui-input *tui*)))))
          (append
           (list (list (cons (str-truncate
                              (format nil "? ~A~@[ (~D/~D)~]"
                                      (askq-title q)
                                      (and (> nq 1) (1+ (ask-index a)))
                                      (and (> nq 1) nq))
                              w)
                             (ui-attr 7 :bright t))))
           (loop for bl in (askq-body-lines q)
                 collect (list (cons (str-truncate bl w) 7)))
           (loop for (label . desc) in (askq-options q)
                 for i from 1
                 collect (list (cons (str-truncate
                                      (format nil "~A ~A ~A~@[ — ~A~]"
                                              (if (= i (askq-cursor q)) ">" " ")
                                              (if multi
                                                  (if (member i (askq-selected q)) "[x]" "[ ]")
                                                  (if (eql (askq-picked q) i) "(*)" "( )"))
                                              label desc)
                                      w)
                                     (if (= i (askq-cursor q))
                                         (tui-select-attr)
                                         7))))
           (when (askq-other q)
             (list (list (cons (str-truncate
                                (format nil "~A Other: ~A"
                                        (if (= (askq-cursor q) (1+ n)) ">" " ")
                                        (if (and typed (plusp (length typed)))
                                            typed
                                            (or (askq-other-text q) "…")))
                                w)
                               (if (= (askq-cursor q) (1+ n))
                                   (tui-select-attr)
                                   7)))))
           (list (list (cons (str-truncate
                              (concatenate 'string
                                           (if multi
                                               "up/dn move · space toggle · enter next"
                                               "up/dn move · enter pick")
                                           (when (askq-other q) " · type + enter = other")
                                           (when (> nq 1) " · left/right = question"))
                              w)
                             8))))))))

(defun ask-add-pane ()
  (unless (pane-get "ask")
    (pane-register "ask" :title "ask" :kind :ask))
  (unless (member "ask" (layout-pane-names) :test #'equal)
    (setf *layout* (layout-split-replace *layout* "chat" :vsplit 0.72 "ask"))
    (unless (member "ask" (layout-pane-names) :test #'equal)
      (setf *layout* (list :vsplit 0.72 *layout* (list 'leaf "ask"))))
    (panes-save)
    (panes-mark-all-dirty)
    (when *tui* (tui-dirty))))

(defun ask-remove-pane ()
  (when (member "ask" (layout-pane-names) :test #'equal)
    (setf *layout* (layout-remove-leaf *layout* "ask"))
    (panes-save)
    (panes-mark-all-dirty)
    (when *tui* (tui-dirty))))

;;; ---------------------------------------------------------------
;;; answering

(defun ask-refresh ()
  (pane-dirty "ask")
  (when *tui* (tui-dirty)))

(defun askq-label (q i)
  (car (nth (1- i) (askq-options q))))

(defun askq-result (q)
  "Alist for one question: (choice . label) / (choices labels) /
(other . text); a checklist may have choices AND other."
  (if (askq-multi q)
      (append (list (cons "choices"
                          (mapcar (lambda (i) (askq-label q i)) (askq-selected q))))
              (when (askq-other-text q)
                (list (cons "other" (askq-other-text q)))))
      (if (askq-other-text q)
          (list (cons "other" (askq-other-text q)))
          (list (cons "choice" (askq-label q (askq-picked q)))))))

(defun ask-finish (a)
  "All questions answered: build the result alist and deliver it to the
waiting tool."
  (let ((result
         (if (= 1 (length (ask-questions a)))
             (askq-result (first (ask-questions a)))
             (list (cons "answers"
                         (loop for q in (ask-questions a)
                               collect (cons (cons "title" (askq-title q))
                                             (askq-result q))))))))
    (setf *ask-active* nil)
    (sb-concurrency:send-message (ask-mailbox a) result)
    (ask-refresh)))

(defun ask-advance (a)
  "Move to the next unanswered question, or finish when none is left."
  (if (ask-all-answered-p a)
      (ask-finish a)
      (progn
        (setf (ask-index a)
              (or (position-if-not #'askq-answered-p (ask-questions a)
                                   :start (ask-index a))
                  (position-if-not #'askq-answered-p (ask-questions a))))
        (ask-refresh))))

(defun ask-move-cursor (delta)
  (let* ((a *ask-active*) (q (ask-current-q a)))
    (setf (askq-cursor q)
          (max 1 (min (+ (askq-noptions q) (if (askq-other q) 1 0))
                      (+ (askq-cursor q) delta)))))
  (ask-refresh))

(defun ask-toggle (i)
  (let* ((a *ask-active*) (q (ask-current-q a)))
    (when (and (askq-multi q) (<= 1 i (askq-noptions q)))
      (setf (askq-selected q)
            (if (member i (askq-selected q))
                (remove i (askq-selected q))
                (sort (cons i (askq-selected q)) #'<)))
      (ask-refresh))))

(defun ask-pick (i)
  "Radio: pick option I and advance.  Checklist: toggle option I."
  (let* ((a *ask-active*) (q (ask-current-q a)))
    (when (<= 1 i (askq-noptions q))
      (if (askq-multi q)
          (ask-toggle i)
          (progn
            (setf (askq-picked q) i
                  (askq-other-text q) nil)
            (ask-advance a))))))

(defun ask-space ()
  "Space on the cursor row: checklist toggles, radio picks and advances."
  (let* ((a *ask-active*) (q (ask-current-q a)))
    (when (<= (askq-cursor q) (askq-noptions q))
      (if (askq-multi q)
          (ask-toggle (askq-cursor q))
          (progn
            (setf (askq-picked q) (askq-cursor q)
                  (askq-other-text q) nil)
            (ask-advance a))))))

(defun ask-enter ()
  "Enter with an empty input line: radio picks the cursor option and
advances; a checklist advances when it has an answer."
  (let* ((a *ask-active*) (q (ask-current-q a)))
    (cond ((askq-multi q)
           (when (askq-answered-p q) (ask-advance a)))
          ((<= (askq-cursor q) (askq-noptions q))
           (setf (askq-picked q) (askq-cursor q)
                 (askq-other-text q) nil)
           (ask-advance a)))))

(defun ask-answer-other (text)
  "Typed text + Enter: the free-text Other answer for the current
question."
  (let* ((a *ask-active*) (q (ask-current-q a)))
    (when (askq-other q)
      (setf (askq-other-text q) text)
      (unless (askq-multi q) (setf (askq-picked q) nil))
      (ask-advance a))))

(defun ask-switch-question (delta)
  "Left/right: previous/next question.  T when there is more than one."
  (let ((a *ask-active*))
    (when (> (length (ask-questions a)) 1)
      (setf (ask-index a)
            (max 0 (min (1- (length (ask-questions a)))
                        (+ (ask-index a) delta))))
      (ask-refresh)
      t)))

;;; ---------------------------------------------------------------
;;; TUI interceptors

(defun ask-key (ui key)
  "Member of *TUI-KEY-INTERCEPTORS*: while questions are open, Up/Down
move the option cursor, Left/Right switch questions, digits 1-9
pick/toggle (only when the input line is empty), Space toggles/picks
the cursor row, and Enter submits — typed text is the Other answer, an
empty line picks (radio) or confirms (checklist).  Everything else
falls through to normal input-line editing.  T = consumed."
  (let ((a *ask-active*))
    (when a
      (pane-dirty "ask")          ; live Other text in the pane
      (let* ((q (ask-current-q a))
             (n (askq-noptions q))
             (input-empty (zerop (length (tui-ui-input ui)))))
        (cond
          ((eq key :up) (ask-move-cursor -1) t)
          ((eq key :down) (ask-move-cursor 1) t)
          ((eq key :left) (ask-switch-question -1))
          ((eq key :right) (ask-switch-question 1))
          ((and (characterp key) (digit-char-p key) input-empty
                (<= 1 (digit-char-p key) (min 9 n)))
           (ask-pick (digit-char-p key))
           t)
          ((and (characterp key) (char= key #\Space) input-empty)
           (ask-space)
           t)
          ((and (characterp key) (member key '(#\Return #\Newline)))
           (let ((text (string-trim " " (copy-seq (tui-ui-input ui)))))
             (if (plusp (length text))
                 (when (askq-other q)
                   (setf (fill-pointer (tui-ui-input ui)) 0
                         (tui-ui-input-cursor ui) 0)
                   (ask-answer-other text))
                 (ask-enter))
             t))
          (t nil))))))

(defun ask-mouse (ui key)
  "Member of *TUI-MOUSE-INTERCEPTORS*: a click on an option row in the
ask pane picks (radio) or toggles (checklist) it; a click on the Other
row moves the cursor there.  T = consumed."
  (declare (ignore ui))
  (let ((a *ask-active*))
    (when (and a *panes-canvas*)
      (destructuring-bind (_ b x y kind) key
        (declare (ignore _))
        (when (and (eq kind :press) (<= (logand b 3) 2) (zerop (logand b 64)))
          (let ((x0 (1- x)) (y0 (1- y)))
            (when (equal (pane-at x0 y0) "ask")
              (let* ((rect (find "ask" (layout-leaves (panes-layout-now) 0 0
                                                      (uigrid-w *panes-canvas*)
                                                      (uigrid-h *panes-canvas*))
                                 :key #'first :test #'equal))
                     (q (ask-current-q a))
                     (nb (length (askq-body-lines q)))
                     ;; Row 0 in the pane is its title line; content
                     ;; starts at row 1: question title, NB body lines,
                     ;; then options 1..N, then the Other row.
                     (row (and rect (- y0 (third rect))))
                     (i (and row (- row nb 1))))
                (cond ((and i (<= 1 i (askq-noptions q)))
                       (setf (askq-cursor q) i)
                       (ask-pick i)
                       t)
                      ((and row (askq-other q)
                            (= row (+ nb (askq-noptions q) 2)))
                       (setf (askq-cursor q) (1+ (askq-noptions q)))
                       (ask-refresh)
                       t)
                      (t nil))))))))))

;;; ---------------------------------------------------------------
;;; chat echo

(defun ask-echo (a)
  "Show the questions in the chat scrollback too (dim)."
  (loop for q in (ask-questions a)
        do (dolist (line (append (list (format nil "? ~A" (askq-title q)))
                                 (askq-body-lines q)
                                 (loop for (label . desc) in (askq-options q)
                                       for i from 1
                                       collect (format nil "  ~D. ~A~@[ — ~A~]"
                                                       i label desc))))
             (ignore-errors (lisping-core:emit :info :text line)))))

;;; ---------------------------------------------------------------
;;; numeric text answers (like 2, or 1,3) from queued messages

(defun ask-parse-indices (text n)
  "TEXT as a comma/space-separated list of valid 1-based option indices,
or NIL if it isn't one."
  (when (and (plusp (length text))
             (every (lambda (c) (or (digit-char-p c) (find c ", "))) text))
    (let ((parts (remove-if (lambda (s) (zerop (length s)))
                            (loop with start = 0
                                  for pos = (position-if (lambda (c) (find c ", ")) text
                                                         :start start)
                                  collect (subseq text start (or pos (length text)))
                                  while pos
                                  do (setf start (1+ pos))))))
      (block invalid
        (let ((indices (loop for part in parts
                             for d = (parse-integer part :junk-allowed t)
                             do (unless (and d (<= 1 d (min 9 n)))
                                  (return-from invalid nil))
                             collect d)))
          (and indices (remove-duplicates indices)))))))

;;; ---------------------------------------------------------------
;;; the blocking wait

(defun ask-wait (a)
  "Block (the agent thread) until every question is answered — by the
TUI interceptors, or by an inbox message that parses as option numbers
(like 2, or 1,3 for a checklist), which answers the current question.
Any other inbox message is deferred and re-queued afterwards, so a
question can never swallow a real message."
  (let ((defer '())
        (result nil))
    (loop until result do
      (let ((m (sb-concurrency:receive-message-no-hang (ask-mailbox a))))
        (when m (setf result m)))
      (loop for item = (sb-concurrency:receive-message-no-hang lisping-core:*inbox*)
            while item
            do (let ((indices (and (stringp item) (null result)
                                   (ask-parse-indices
                                    (string-trim '(#\Space #\Tab #\Newline #\Return) item)
                                    (askq-noptions (ask-current-q a))))))
                 (cond (indices
                        (ignore-errors
                          (lisping-core:emit :info
                                             :text (format nil "> answered by message: ~A" item)))
                        (let ((q (ask-current-q a)))
                          (if (askq-multi q)
                              (setf (askq-selected q) (sort (copy-list indices) #'<))
                              (setf (askq-picked q) (first indices))))
                        (ask-advance a))
                       (t (push item defer)))))
      (unless result (sleep 0.05)))
    (dolist (item (nreverse defer))
      (sb-concurrency:send-message lisping-core:*inbox* item))
    result))

;;; ---------------------------------------------------------------
;;; the tool

(defun ask-result-obj (alist)
  (apply #'lisping-core::obj
         (loop for (k . v) in alist
               append (list (intern (string-upcase k) :keyword)
                            (if (equal k "answers")
                                (mapcar #'ask-result-obj v)
                                v)))))

(defun ask-result-json (result)
  (lisping-core:json-encode (ask-result-obj result)))

(defun ask-parse-options (raw)
  (loop for o in (if (vectorp raw) (coerce raw 'list) raw)
        collect (cons (or (and (hash-table-p o) (gethash "label" o)) "?")
                      (and (hash-table-p o) (gethash "description" o)))))

(defun askq-from-hash (h)
  (make-askq :title (or (gethash "title" h) (gethash "body" h) "?")
             :body (gethash "body" h)
             :options (ask-parse-options (gethash "options" h))
             :multi (not (null (gethash "multi" h)))
             :other (multiple-value-bind (v present) (gethash "other" h)
                      (if present (not (null v)) t))))

(defun ask-user-tool (args)
  (if *ask-active*
      "{\"error\":\"a question is already open\"}"
      (let* ((rawqs (lisping-core::arg args "questions"))
             (questions
              (if (and rawqs (plusp (length rawqs)))
                  (loop for o in (if (vectorp rawqs) (coerce rawqs 'list) rawqs)
                        collect (if (hash-table-p o)
                                    (askq-from-hash o)
                                    (make-askq :title "?" :options nil)))
                  ;; Legacy single-question form.
                  (list (make-askq
                         :title (or (lisping-core::arg args "question") "")
                         :options (ask-parse-options (lisping-core::arg args "options"))
                         :multi (not (null (lisping-core::arg args "multi")))
                         :other (multiple-value-bind (v present) (gethash "other" args)
                                  (if present (not (null v)) t))))))
             (bad (find-if (lambda (q)
                             (or (zerop (length (string-trim " " (askq-title q))))
                                 (null (askq-options q))))
                           questions)))
        (cond (bad "{\"error\":\"every question needs a title/body and options\"}")
              (t
               (let ((a (make-ask :questions questions
                                  :mailbox (sb-concurrency:make-mailbox))))
                 (setf *ask-active* a)
                 (ask-add-pane)
                 (ask-echo a)
                 (unwind-protect
                      (ask-result-json (ask-wait a))
                   (setf *ask-active* nil)
                   (ask-remove-pane))))))))

;;; ---------------------------------------------------------------
;;; lifecycle

(defun ask-init-panes ()
  "No question can be open right after a (UI) restart — the tool call
was interrupted — so remove a stale ask leaf left in the saved layout."
  (when (member "ask" (layout-pane-names) :test #'equal)
    (setf *layout* (layout-remove-leaf *layout* "ask"))
    (panes-save)
    (panes-mark-all-dirty)
    (when *tui* (tui-dirty))))

(defun ask-start ()
  "Register the ask_user tool and the key/mouse interceptors.  Idempotent."
  (setf *panes-init-hooks* (remove 'ask-init-panes *panes-init-hooks*))
  (setf *panes-init-hooks* (append *panes-init-hooks* (list 'ask-init-panes)))
  (setf *tui-key-interceptors* (remove 'ask-key *tui-key-interceptors*))
  (setf *tui-key-interceptors* (append *tui-key-interceptors* (list 'ask-key)))
  (setf *tui-mouse-interceptors* (remove 'ask-mouse *tui-mouse-interceptors*))
  (setf *tui-mouse-interceptors* (append *tui-mouse-interceptors* (list 'ask-mouse)))
  (ask-init-panes)
  (lisping-core::register-tool
   "ask_user" "Ask the user one or more multiple-choice questions and BLOCK until all are answered. The ask pane (below chat) shows one question at a time; the user moves with Up/Down, picks with Enter (radio) or Space to toggle + Enter to confirm (checklist), clicks an option row, switches questions with Left/Right, and types text + Enter for a free-text Other answer. A queued message that parses as option numbers (like 2, or 1,3 for a checklist) answers the current question. Single question: pass question + options. Several: pass questions, a list of {title, body, options, multi?, other?} — title is the short label, body the actual question text. options: list of {label, description?}. multi (default false): checklist mode, several answers. other (default true): allow free text. Returns a JSON object: for one question {choice: label}, {choices: [a, b]}, or {other: text}; for several {answers: [{title: ..., choice|choices|other: ...}]}. Example: {question: Deploy now?, options: [{label: yes}, {label: no, description: keep staging}]} => {choice: yes}."
   (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj
                      "question" (lisping-core::obj "type" "string")
                      "options" (lisping-core::obj "type" "array"
                                                 "items" (lisping-core::obj "type" "object"))
                      "questions" (lisping-core::obj "type" "array"
                                                   "items" (lisping-core::obj "type" "object"))
                      "multi" (lisping-core::obj "type" "boolean")
                      "other" (lisping-core::obj "type" "boolean")))
   'ask-user-tool)
  t)