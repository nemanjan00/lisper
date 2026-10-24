;;; compact.lisp — /compact [focus]: summarize the whole conversation in one
;;; model call and replace the history with that summary.
;;;
;;; Only the agent thread may touch *messages*, so /compact does not compact
;;; itself: it sets *COMPACT-PENDING* and, when idle, sends a kinded
;;; "compact" message so a turn starts.  COMPACT-BEFORE-REQUEST (first in
;;; *before-request*) sees the flag on the agent thread, asks the model for
;;; a summary in a separate call (a text transcript, so no tool
;;; definitions are needed), and swaps *messages* for
;;;   [compact] marker (x-kind "compact") + the current task, if any.
;;; The current task is the latest real user message and everything after
;;; it when it is still unanswered (a turn in progress); the cut is at that
;;; user message, so no tool call is orphaned.  The replaced messages go
;;; into *ctx-originals* as one stretch, so context_restore and
;;; context_search still find them.  The rewind guard of the compactor does
;;; not apply: the user asked for this.

(in-package :lisping)

(defvar *compact-pending* nil
  "NIL, or (:focus TEXT) once /compact has been asked for and not yet done.")

(defvar *compact-max-chars* 400000
  "Most characters of transcript sent to the summary call.")

(defparameter *compact-system*
  "You write handoff summaries. You get the transcript of a long conversation between a user and an AI coding agent (with tool calls and results, each message labeled [eN ...]). Write the summary the agent itself will continue from after its memory of the transcript is gone. Be concrete and complete: exact file paths, function names, line numbers, commands, ids, values. Never invent anything that is not in the transcript.")

(defparameter *compact-instructions*
  "Write the summary now, in these sections (omit one only if it is truly empty):
1. Goals: what the user is trying to achieve overall, and the standing instructions and rules they gave (quote the important ones).
2. Decisions and findings: what was decided, built, learned or ruled out, and why.
3. Todo state: the todo list below, and which item is in progress.
4. Files and code in play: paths, function names and line numbers, what each does or what was changed.
5. Open problems: bugs, failing checks, things promised but not done, risks.
6. The user's last request: verbatim if short, and whether it is answered.
7. Next steps.
Reference entries as eN where the details can be read back. Plain text, no preamble, at most about 1500 words.")

(defun compact-clip (s n)
  "S cut to about N chars, keeping the head and the tail."
  (let ((n (max 40 (round n))) (len (length s)))
    (if (<= len n)
        s
        (let ((head (round (* 2 n) 3)) (tail (- n (round (* 2 n) 3))))
          (format nil "~A [... ~D chars cut ...] ~A" (subseq s 0 head)
                  (- len head tail) (subseq s (- len tail)))))))

(defun compact-content-string (m)
  (let ((c (gethash "content" m)))
    (cond ((stringp c) c) ((null c) "") (t (princ-to-string c)))))

(defun compact-tool-calls (m)
  (let ((tcs (gethash "tool_calls" m)))
    (if (vectorp tcs) (coerce tcs 'list) nil)))

(defun compact-message-text (m scale)
  "One message as transcript text, or NIL when it has nothing to say."
  (let ((role (gethash "role" m)) (kind (gethash "x-kind" m)) (id (ctx-entry-id m)))
    (cond
      ((equal role "user")
       (format nil "[e~D ~A] ~A" id (if kind (format nil "system note, ~A" kind) "USER")
               (compact-clip (compact-content-string m) (* 4000 scale))))
      ((equal role "assistant")
       (with-output-to-string (s)
         (format s "[e~D ASSISTANT] ~A" id (compact-clip (compact-content-string m) (* 4000 scale)))
         (dolist (tc (compact-tool-calls m))
           (let ((f (gethash "function" tc)))
             (when (hash-table-p f)
               (format s "~%  -> call ~A ~A" (gethash "name" f)
                       (compact-clip (let ((a (gethash "arguments" f))) (if (stringp a) a ""))
                                     (* 500 scale))))))))
      ((equal role "tool")
       (format nil "[e~D TOOL RESULT] ~A" id (compact-clip (compact-content-string m) (* 1200 scale))))
      (t nil))))

(defun compact-transcript (messages)
  "MESSAGES as one transcript string no longer than *compact-max-chars*:
the per-message limits shrink until it fits."
  (let ((scale 1.0) text)
    (dotimes (i 6)
      (setf text (format nil "~{~A~^~%~%~}"
                         (remove nil (mapcar (lambda (m) (compact-message-text m scale)) messages))))
      (when (<= (length text) *compact-max-chars*) (return))
      (setf scale (/ scale 2)))
    (compact-clip text *compact-max-chars*)))

(defun compact-trigger-p (m)
  (equal (gethash "x-kind" m) "compact"))

(defun compact-final-assistant-p (m)
  "An assistant message that ends a turn: real text, no tool calls."
  (and (equal (gethash "role" m) "assistant")
       (not (lisping-core:synthetic-message-p m))
       (null (compact-tool-calls m))))

(defun compact-split (messages)
  "(values OLD TAIL): OLD is summarized, TAIL stays verbatim.  The trigger
messages of earlier /compact calls are dropped.  TAIL is the latest real
user message and what follows it, when no final reply answered it yet."
  (let* ((live (remove-if #'compact-trigger-p messages))
         (l (let ((*messages* live)) (ctx-latest-user-index))))
    (cond ((null l) (values live nil))
          ((find-if #'compact-final-assistant-p live :start (1+ l)) (values live nil))
          (t (values (subseq live 0 l) (subseq live l))))))

(defun compact-todo-text ()
  (if (null *todos*)
      "(no todos)"
      (format nil "~{~A~^~%~}"
              (loop for item in *todos* for i from 1
                    collect (format nil "~D. [~A] ~A" i (if (cdr item) "x" " ") (car item))))))

(defun compact-summary-body (old focus)
  (obj :model lisping-core:*model*
       :max_tokens 8000
       :messages (vector
                  (obj :role "system" :content *compact-system*)
                  (obj :role "user"
                       :content (format nil "--- CONVERSATION TRANSCRIPT ---~%~A~%--- END OF TRANSCRIPT ---~%~%~A~%~%Todo list right now:~%~A~@[~%~%The user asks you to focus the summary on: ~A~]"
                                        (compact-transcript old) *compact-instructions*
                                        (compact-todo-text)
                                        (and focus (plusp (length focus)) focus))))))

(defun compact-ask-model (old focus)
  "The summary text from one separate model call; signals an error without one."
  (let* ((message (lisping-core:default-complete (compact-summary-body old focus)))
         (text (and (hash-table-p message) (string-trim '(#\Space #\Newline) (compact-content-string message)))))
    (when (or (null text) (< (length text) 40))
      (error "the model returned no summary"))
    text))

(defun compact-marker (summary n first-id last-id idle)
  (let ((m (obj :role "user"
                :content (format nil "[compact] The user ran /compact. This is the summary of the conversation up to here, written from the full history:~%~%~A~%~%[The ~D messages it replaces (e~D … e~D) are kept in the session log: context_search finds anything in them, context_restore {\"entry\": \"e~D\"} reads one back.]~:[~;~%~%No new instruction came with this summary: reply with one short line saying the context was compacted and you are ready. Do not call tools.~]"
                                 summary n first-id last-id first-id idle))))
    (setf (gethash "x-kind" m) "compact")
    m))

(defun compact-overhead-tokens ()
  (round (+ (length (ctx-system-prompt-text)) (or *ctx-tools-chars* 0)) *ctx-chars-per-token*))

(defun compact-est-tokens (messages)
  (round (reduce #'+ (mapcar #'ctx-message-chars messages)) *ctx-chars-per-token*))

(defun compact-apply (old tail summary before-est)
  "Replace *messages* by marker + TAIL, keep OLD in the registry.  Returns
the new token estimate."
  (let* ((ids (mapcar #'ctx-entry-id old))
         (first-id (reduce #'min ids)) (last-id (reduce #'max ids))
         (marker (compact-marker summary (length old) first-id last-id (null tail)))
         (new (cons marker tail))
         (after (+ (compact-overhead-tokens) (compact-est-tokens new))))
    (ctx-entry-id marker)
    (dolist (m old) (setf (gethash (ctx-entry-id m) *ctx-originals*) (cons :message m)))
    ;; Stretches whose collapse marker is inside OLD are covered by the new one.
    (setf *ctx-stretches*
          (remove-if (lambda (st) (member (fourth st) ids)) *ctx-stretches*))
    (push (list first-id last-id (format nil "/compact: ~A" (ctx-short summary))
                (ctx-entry-id marker) (length old) before-est)
          *ctx-stretches*)
    (sb-sys:without-interrupts (setf *messages* new))
    (setf *ctx-requests-since-collapse* 0 *ctx-low-pt-streak* 0
          *ctx-prev-request-chars* nil *ctx-noted-percent* 0 *ctx-pending-note* nil)
    (ctx-save-state)
    (pane-dirty "context")
    after))

(defun compact-run (req body)
  "Do the compaction for request BODY; return the body to send."
  (multiple-value-bind (old tail) (compact-split *messages*)
    (when (< (length old) 2)
      (lisping-core:emit :info :text "⚙ nothing to compact yet")
      (return-from compact-run body))
    (let* ((usage lisping-core:*last-usage*)
           (pt (and (hash-table-p usage) (integerp (gethash "prompt_tokens" usage))
                    (gethash "prompt_tokens" usage)))
           (before (or pt (+ (compact-overhead-tokens) (compact-est-tokens *messages*))))
           (summary (compact-ask-model old (getf req :focus)))
           (summary-usage lisping-core:*last-usage*))
      ;; The summary call is a model call: count it, then forget its usage
      ;; (it is not the size of the conversation).
      (when (and (hash-table-p summary-usage) (fboundp 'usage-add))
        (funcall 'usage-add summary-usage lisping-core:*model*))
      (setf lisping-core:*last-usage* nil)
      (let ((after (compact-apply old tail summary before)))
        (lisping-core:emit :info :text (format nil "⚙ compacted ~A → ~A tokens (~D messages summarized~:[~;, current task kept~])"
                                               (ctx-fmt-k before) (ctx-fmt-k after) (length old) tail))
        ;; The request already built is stale: rebuild it from *messages*.
        (let ((old-messages (gethash "messages" body)))
          (when (vectorp old-messages)
            (setf (gethash "messages" body)
                  (coerce (cons (aref old-messages 0) (copy-list *messages*)) 'vector))))
        body))))

(defun compact-before-request (body)
  "First *before-request* hook: compact when /compact asked for it."
  (let ((req *compact-pending*))
    (if (null req)
        body
        (progn
          (setf *compact-pending* nil)
          (handler-case (compact-run req body)
            (error (c)
              (lisping-core:emit :error :text (format nil "/compact failed, the conversation is unchanged: ~A" c))
              body))))))

(defun compact-observe (event)
  "A turn ended while /compact was still waiting (it was asked for mid-turn
and no model call followed): start a turn for it now."
  (when (and (eq (getf event :type) :idle) *compact-pending*)
    (lisping-core:send-user-message
     "Compacting the conversation (/compact)." :kind "compact")))

(defun slash-compact (ui args)
  (declare (ignore ui))
  (let ((focus (string-trim " " (or args ""))))
    (cond (*compact-pending* (slash-say "A compaction is already waiting."))
          (t (setf *compact-pending* (list :focus focus))
             (if (lisping-core:turn-busy-p)
                 (slash-say "Compacting at the next model call of the running turn.")
                 (lisping-core:send-user-message
                  (format nil "Compacting the conversation (/compact)~:[~; — focus: ~A~]." (plusp (length focus)) focus)
                  :kind "compact"))))))

(defun compact-start ()
  "Called from init.lisp (after context-start, so the hook runs first)."
  (setf *before-request* (cons 'compact-before-request (remove 'compact-before-request *before-request*)))
  (pushnew 'compact-observe lisping-core:*observers*)
  (define-slash "compact" 'slash-compact
    "summarize the whole conversation in one model call and replace the history (/compact [focus])"))
