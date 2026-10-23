;;;; lisp/ask.lisp -- ask_user: a blocking multiple-choice question.
;;;;
;;;; The tool opens the "ask" pane (below chat) with numbered options and
;;;; blocks the agent thread on a mailbox.  The TUI fills the mailbox via
;;;; the interceptors (*TUI-KEY-INTERCEPTOR* / *TUI-MOUSE-INTERCEPTOR*):
;;;; digits 1-9 pick (or toggle, for multi), a mouse click on an option row
;;;; does the same, any typed text + Enter is a free-text Other answer, and
;;;; Enter alone confirms a multi selection.  A normal message typed or
;;;; queued while the question is open also answers it (a bare number
;;;; picks that option — multi accepts "1,3" — anything else is Other), so
;;;; nothing can get stuck.  ASK-START is called from init.lisp.

(in-package :lisping)

(defstruct (ask (:constructor %make-ask))
  question options multi other mailbox (selected nil))
;; OPTIONS: list of (label . description-or-nil)
;; SELECTED: sorted list of 1-based option indices (multi mode)

(defvar *ask-active* nil "The open question (an ASK), or NIL.")

;;; ---------------------------------------------------------------
;;; pane

(defun pane-ask-render (pane w h)
  (declare (ignore pane h))
  (let ((a *ask-active*))
    (if (null a)
        (list (list (cons "(no pending question)" 8)))
        (append
         (list (list (cons (str-truncate (format nil "? ~A" (ask-question a)) w)
                           (ui-attr 7 :bright t))))
         (loop for (label . desc) in (ask-options a)
               for i from 1
               collect (list (cons (str-truncate
                                    (format nil "~D. ~A~A~@[ — ~A~]"
                                            i
                                            (if (ask-multi a)
                                                (if (member i (ask-selected a)) "[x] " "[ ] ")
                                                "")
                                            label desc)
                                    w)
                                   (if (member i (ask-selected a))
                                       (ui-attr 7 :bright t)
                                       7))))
         (list (list (cons (str-truncate
                            (if (ask-multi a)
                                "1-9/click toggle · Enter confirm · type + Enter = other"
                                "1-9/click pick · type + Enter = other")
                            w)
                           8)))))))

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
;;; answering (called from TUI threads)

(defun ask-answer (result)
  "Deliver RESULT — ((\"choice\" . label)) / ((\"choices\" labels)) /
((\"other\" . text)) — to the waiting tool.  Only the first answer counts."
  (let ((a *ask-active*))
    (when a
      (setf *ask-active* nil)
      (sb-concurrency:send-message (ask-mailbox a) result)
      (pane-dirty "ask")
      (when *tui* (tui-dirty)))))

(defun ask-toggle (i)
  (let ((a *ask-active*))
    (when a
      (setf (ask-selected a)
            (if (member i (ask-selected a))
                (remove i (ask-selected a))
                (sort (cons i (ask-selected a)) #'<)))
      (pane-dirty "ask")
      (when *tui* (tui-dirty)))))

(defun ask-option-label (a i)
  (car (nth (1- i) (ask-options a))))

(defun ask-pick (i)
  "Pick option I (1-based): answer immediately, or toggle for multi."
  (let ((a *ask-active*))
    (when (and a (<= 1 i (length (ask-options a))))
      (if (ask-multi a)
          (ask-toggle i)
          (ask-answer (list (cons "choice" (ask-option-label a i))))))))

(defun ask-confirm-multi (a)
  (when (ask-selected a)
    (ask-answer (list (cons "choices"
                            (mapcar (lambda (i) (ask-option-label a i))
                                    (ask-selected a)))))))

;;; ---------------------------------------------------------------
;;; TUI interceptors

(defun ask-key (ui key)
  "*TUI-KEY-INTERCEPTOR*: while a question is open, digits 1-9 pick/toggle
and Enter submits (input text = Other; empty = confirm multi).  Everything
else falls through to the normal input-line editing.  T = consumed."
  (let ((a *ask-active*))
    (when a
      (let ((n (length (ask-options a))))
        (cond
          ((and (characterp key) (digit-char-p key)
                (<= 1 (digit-char-p key) (min 9 n)))
           (ask-pick (digit-char-p key))
           t)
          ((and (characterp key) (member key '(#\Return #\Newline)))
           (let ((text (string-trim " " (copy-seq (tui-ui-input ui)))))
             (cond ((plusp (length text))
                    (when (ask-other a)
                      (setf (fill-pointer (tui-ui-input ui)) 0
                            (tui-ui-input-cursor ui) 0)
                      (ask-answer (list (cons "other" text))))
                    t)
                   ((ask-multi a)
                    (ask-confirm-multi a)
                    t)
                   (t nil))))
          (t nil))))))

(defun ask-mouse (ui key)
  "*TUI-MOUSE-INTERCEPTOR*: a click on an option row in the ask pane
picks/toggles that option.  T = consumed."
  (declare (ignore ui))
  (let ((a *ask-active*))
    (when (and a *panes-canvas*)
      (destructuring-bind (_ b x y kind) key
        (declare (ignore _))
        (when (and (eq kind :press) (<= (logand b 3) 2) (zerop (logand b 64)))
          (let ((x0 (1- x)) (y0 (1- y)))
            (when (equal (pane-at x0 y0) "ask")
              (let* ((rect (find "ask" (layout-leaves *layout* 0 0
                                                      (uigrid-w *panes-canvas*)
                                                      (uigrid-h *panes-canvas*))
                                 :key #'first :test #'equal))
                     (row (and rect (- y0 (second rect)))))
                (when (and row (<= 1 row (length (ask-options a))))
                  (ask-pick row)
                  t)))))))))

;;; ---------------------------------------------------------------
;;; chat echo

(defun ask-echo (a)
  "Show the question in the chat scrollback too (dim), one event per line."
  (dolist (line (cons (format nil "? ~A" (ask-question a))
                      (loop for (label . desc) in (ask-options a)
                            for i from 1
                            collect (format nil "  ~D. ~A~@[ — ~A~]" i label desc))))
    (ignore-errors (lisping-core:emit :type :info :text line))))

;;; ---------------------------------------------------------------
;;; text answers (typed or queued messages while the question is open)

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

(defun ask-parse-text-answer (a text)
  "Interpret a user message as an answer: a bare number (multi: a list
like \"1,3\") picks options; anything else is a free-text Other answer;
NIL if neither works (then the message is delivered normally later)."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (or (let ((indices (ask-parse-indices trimmed (length (ask-options a)))))
          (cond ((null indices) nil)
                ((ask-multi a)
                 (list (cons "choices" (mapcar (lambda (i) (ask-option-label a i))
                                               indices))))
                ((null (rest indices))
                 (list (cons "choice" (ask-option-label a (first indices)))))
                (t nil)))
        (and (ask-other a) (plusp (length trimmed))
             (list (cons "other" trimmed))))))

;;; ---------------------------------------------------------------
;;; the blocking wait

(defun ask-wait (a)
  "Block (the agent thread) until the question is answered — by the TUI
interceptors, or by a normal message arriving in *INBOX* (typed or
queued), which counts as the answer so nothing can get stuck.  System
(kind'd) inbox messages are deferred and re-queued afterwards."
  (let ((defer '())
        (result nil))
    (loop until result do
      (let ((m (sb-concurrency:receive-message-no-hang (ask-mailbox a))))
        (when m (setf result m)))
      (loop for item = (sb-concurrency:receive-message-no-hang lisping-core:*inbox*)
            while item
            do (let ((parsed (and (stringp item) (null result)
                                  (ask-parse-text-answer a item))))
                 (cond (parsed
                        (ignore-errors
                          (lisping-core:emit :type :info
                                             :text (format nil "> answered by message: ~A" item)))
                        (setf result parsed))
                       (t (push item defer)))))
      (unless result (sleep 0.05)))
    (dolist (item (nreverse defer))
      (sb-concurrency:send-message lisping-core:*inbox* item))
    result))

;;; ---------------------------------------------------------------
;;; the tool

(defun ask-result-json (result)
  (lisping-core:json-encode
   (apply #'lisping-core::obj
          (loop for (k . v) in result
                append (list (intern (string-upcase k) :keyword) v)))))

(defun ask-user-tool (args)
  (if *ask-active*
      "{\"error\":\"a question is already open\"}"
      (let* ((question (or (lisping-core::arg args "question") ""))
             (raw (lisping-core::arg args "options"))
             (options (loop for o in (if (vectorp raw) (coerce raw 'list) raw)
                            collect (cons (or (and (hash-table-p o) (gethash "label" o)) "?")
                                          (and (hash-table-p o) (gethash "description" o)))))
             (multi (not (null (lisping-core::arg args "multi"))))
             (other (multiple-value-bind (v present) (gethash "other" args)
                      (if present (not (null v)) t)))
             (a (%make-ask :question question :options options :multi multi
                           :other other :mailbox (sb-concurrency:make-mailbox))))
        (cond ((zerop (length (string-trim " " question)))
               "{\"error\":\"question is empty\"}")
              ((null options)
               "{\"error\":\"options is empty\"}")
              (t
               (setf *ask-active* a)
               (ask-add-pane)
               (ask-echo a)
               (unwind-protect
                    (ask-result-json (ask-wait a))
                 (setf *ask-active* nil)
                 (ask-remove-pane)))))))

;;; ---------------------------------------------------------------
;;; lifecycle

(defun ask-init-panes ()
  "No question can be open right after a (UI) restart — the tool call was
interrupted — so remove a stale ask leaf left in the saved layout."
  (when (member "ask" (layout-pane-names) :test #'equal)
    (setf *layout* (layout-remove-leaf *layout* "ask"))
    (panes-save)
    (panes-mark-all-dirty)
    (when *tui* (tui-dirty))))

(defun ask-start ()
  "Register the ask_user tool and the key/mouse interceptors.  Idempotent."
  (setf *panes-init-hooks* (remove 'ask-init-panes *panes-init-hooks*))
  (setf *panes-init-hooks* (append *panes-init-hooks* (list 'ask-init-panes)))
  (setf *tui-key-interceptor* 'ask-key
        *tui-mouse-interceptor* 'ask-mouse)
  (ask-init-panes)
  (lisping-core::register-tool
   "ask_user" "Ask the user a multiple-choice QUESTION and BLOCK until they answer. The ask pane (below chat) shows numbered options; the user picks with keys 1-9 or a mouse click (multi: numbers/clicks toggle, Enter confirms), or types any text + Enter for a free-text Other answer. A normal message typed or queued while the question is open counts as the answer (a bare number picks that option; multi accepts \"1,3\"), so nothing gets stuck. options: list of {label, description?}. multi (default false): several answers. other (default true): allow free text. Returns a JSON object: {\"choice\":\"label\"}, {\"choices\":[\"a\",\"b\"]} (multi), or {\"other\":\"text\"}. Example: {\"question\":\"Deploy now?\",\"options\":[{\"label\":\"yes\"},{\"label\":\"no\",\"description\":\"keep staging\"}]} => {\"choice\":\"yes\"}."
   (lisping-core::obj "type" "object"
        "properties" (lisping-core::obj
                      "question" (lisping-core::obj "type" "string")
                      "options" (lisping-core::obj "type" "array"
                                                 "items" (lisping-core::obj "type" "object"))
                      "multi" (lisping-core::obj "type" "boolean")
                      "other" (lisping-core::obj "type" "boolean"))
        "required" #("question" "options"))
   'ask-user-tool)
  t)