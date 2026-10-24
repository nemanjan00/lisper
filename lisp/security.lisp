;;; security.lisp — confirmation gate for risky tools after untrusted input (todo 28).
;;;
;;; The core fences the output of untrusted tools (web pages, MCP servers)
;;; and records them in *TURN-UNTRUSTED*.  While that list is non-NIL this
;;; turn has read text somebody else wrote, so anything that can run code
;;; or change files needs the user's say-so first.  The hook runs in the
;;; agent thread, so it asks the user itself (the ask_user machinery) and
;;; blocks: the decision never passes through text the model could have
;;; been talked into producing.  "Allow for this turn" is remembered until
;;; a tool call finds *TURN-UNTRUSTED* empty again (a new turn).

(in-package :lisping)

(defparameter *security-gated-tools*
  '("eval" "edit_file" "edit_files" "replace_form" "bg_start" "monitor_start"
    "pane" "layout" "mcp")
  "Tools that need confirmation once untrusted output is in the turn. pane
can add a command pane, mcp reload spawns configured commands; hardware
tools join this list when they exist.")

(defvar *security-turn-allowed* nil
  "True after the user allowed risky tools for the rest of this turn.")

(defun security-gated-p (name)
  (member name *security-gated-tools* :test #'string=))

(defun security-describe-call (name args)
  (let ((text (ignore-errors (lisping-core:json-encode args))))
    (format nil "~A ~A" name (str-truncate (or text "") 400))))

(defun security-question (name args)
  (lisping-core:obj
   "questions"
   (list (lisping-core:obj
          "title" (format nil "Allow ~A after untrusted input?" name)
          "body" (format nil "This turn read untrusted output (~{~A~^, ~}). It must not decide what runs. Call: ~A"
                         lisping-core:*turn-untrusted* (security-describe-call name args))
          "options" (list (lisping-core:obj "label" "Allow once")
                          (lisping-core:obj "label" "Allow for this turn")
                          (lisping-core:obj "label" "Deny"))
          "other" nil))))

(defun security-ask (name args)
  "Ask the user; :once, :turn or :deny."
  (let ((answer (ask-user-tool (security-question name args))))
    (cond ((search "Allow for this turn" answer) :turn)
          ((search "Allow once" answer) :once)
          (t :deny))))

(defun security-gate (name args)
  "*BEFORE-TOOL-CALL* hook: NIL lets the call through, a string refuses it."
  (cond ((null lisping-core:*turn-untrusted*)
         (setf *security-turn-allowed* nil)
         nil)
        ((or *security-turn-allowed* (not (security-gated-p name))) nil)
        (t (ecase (security-ask name args)
             (:turn (setf *security-turn-allowed* t) nil)
             (:once nil)
             (:deny (format nil "REFUSED: the user denied ~A. This turn read untrusted output (~{~A~^, ~}); do not retry the call unless the user asks for it."
                            name lisping-core:*turn-untrusted*))))))

(defun security-start ()
  "Install the gate. Idempotent."
  (setf lisping-core:*before-tool-call*
        (append (remove 'security-gate lisping-core:*before-tool-call*)
                (list 'security-gate)))
  t)
