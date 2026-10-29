;;;; lisp/session-cmds.lisp -- /clear (fresh conversation) and #cmd (run a
;;;; shell command, send command + output to the model as a message).

(in-package :lisping)

(defvar *shell-input-timeout* 120 "Seconds a #cmd may run before it is killed.")

(defvar *shell-input-max-chars* 30000 "Longest output kept (head and tail) in the message.")

;;; ------------------------------------------------------------------ /clear

(defun clear-chat-pane ()
  "Empty the chat buffer and the structured chat state."
  (sb-thread:with-recursive-lock (*tui-lock*)
    (when *tui*
      (setf (fill-pointer (uibuf-lines (tui-ui-chat *tui*))) 0
            (tui-ui-shown-user *tui*) nil
            (tui-ui-pending-user *tui*) nil
            (tui-ui-scroll *tui*) 0))
    (setf *chat-run* nil *chat-pending* nil *chat-selected* nil
          *chat-row-owners* (vector) *tui-chat-new* 0)
    (let ((p (pane-of-kind :chat)))
      (when p (setf (pane-scroll p) 0 (pane-needs-redraw p) 1)))))

(defun clear-conversation ()
  "Forget the conversation, the compactor's stretches and queue, the goal
and the loop; the system prompt stays.  The session files keep the old
messages in the .jsonl log (context_search still finds them)."
  (when (lisping-core:turn-busy-p)
    (lisping-core:cancel-turn)
    (loop repeat 50 while (lisping-core:turn-busy-p) do (sleep 0.1)))
  (when (boundp '*goal*) (when (symbol-value '*goal*) (funcall 'goal-clear "cleared with /clear")))
  (when (and (boundp '*loop*) (symbol-value '*loop*)) (funcall 'loop-stop))
  (setf *compact-pending* nil)
  (sb-sys:without-interrupts
    (setf *messages* nil
          lisping-core:*last-usage* nil
          *ctx-stretches* nil
          *ctx-queue* nil
          *ctx-offloaded-count* 0
          *ctx-last-prompt-tokens* nil
          *ctx-last-est* 0
          *ctx-prev-request-chars* nil
          *ctx-noted-percent* 0
          *ctx-pending-note* nil
          *ctx-requests-since-collapse* 99
          *ctx-low-pt-streak* 0))
  (ignore-errors (ctx-save-state))
  (ignore-errors (lisping-core:session-save))
  (clear-chat-pane)
  (pane-dirty "context")
  (lisping-core:emit :info :text "conversation cleared: starting fresh (the old messages stay in the session log; context_search finds them)")
  t)

(defun slash-clear-conversation (ui args)
  (declare (ignore ui args))
  (clear-conversation)
  :keep-input)

;;; -------------------------------------------------------------------- #cmd

(defun shell-input-clip (out)
  (if (<= (length out) *shell-input-max-chars*)
      out
      (let ((h (floor *shell-input-max-chars* 2)))
        (format nil "~A~%... [~D chars cut] ...~%~A"
                (subseq out 0 h) (- (length out) *shell-input-max-chars*)
                (subseq out (- (length out) h))))))

(defun shell-input-message (cmd out code)
  (format nil "I ran `~A` in ~A (exit ~A~:[~;, timed out~]):~%```~%~A~%```"
          cmd (namestring (or lisping-core:*cwd* lisping-core:*root*)) code (eql code 124)
          (let ((s (string-right-trim '(#\Newline) (shell-input-clip out))))
            (if (zerop (length s)) "(no output)" s))))

(defun shell-input-run (cmd)
  "Run CMD in the session's directory (own thread), then send the result
to the model as a user message."
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (multiple-value-bind (out code)
             (lisping-core:shell
              (format nil "timeout ~D sh -c ~A" *shell-input-timeout* (lisping-core::shell-quote cmd)))
           (lisping-core:send-user-message (shell-input-message cmd out code)))
       (error (c) (lisping-core:emit :error :text (format nil "# ~A failed: ~A" cmd c)))))
   :name "shell-input"))

(defun shell-input-submit (ui text)
  "Submit hook.  #cmd (or # cmd) runs the command and sends it with its
output; ## starts a plain message that begins with one #."
  (cond ((and (> (length text) 1) (char= (char text 0) #\#) (char= (char text 1) #\#))
         (subseq text 1))
        ((and (> (length text) 1) (char= (char text 0) #\#))
         (let ((cmd (string-trim " " (subseq text 1))))
           (when (plusp (length cmd))
             (tui-clear-input ui)
             (lisping-core:emit :info :text (format nil "# running: ~A" cmd))
             (shell-input-run cmd)
             t)))
        (t nil)))

(defun session-cmds-start ()
  "Called from init.lisp after slash-start.  Idempotent."
  (define-slash "clear" 'slash-clear-conversation
    "start a fresh conversation (forget messages, goal, loop; the input line is kept)")
  (define-slash "new" 'slash-clear-conversation "same as /clear")
  (setf *tui-submit-hooks*
        (append (remove 'shell-input-submit *tui-submit-hooks*) (list 'shell-input-submit)))
  t)
