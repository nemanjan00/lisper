;;;; lisp/agents.lisp -- read AGENTS.md into the system prompt at boot.
;;;;
;;;; Walks from the session's cwd up to the project root (the first
;;;; directory with a .git, else the home directory or /) and collects each
;;;; AGENTS.md on the way.  They are added to *system-prompt* between
;;;; [project:agents begin] / [project:agents end] markers, outermost file
;;;; first, so the nearest one has the last word.  A session's cwd is fixed
;;;; for the life of the process (session-switch starts another one), so
;;;; boot is the only moment that matters.  /agents lists what was loaded
;;;; and re-reads the files for the next boot's prompt too.

(in-package :lisping)

(defparameter *agents-file* "AGENTS.md")

(defparameter *agents-cap* 20000 "Characters kept per AGENTS.md.")

(defparameter *agents-begin* "[project:agents begin]")

(defparameter *agents-end* "[project:agents end]")

(defvar *agents-loaded* nil "Paths of the AGENTS.md files in the prompt, outermost first.")

(defun agents-dir (d)
  (string-right-trim "/" (namestring d)))

(defun agents-parent (dir)
  "Parent of the directory string DIR, or NIL at the top."
  (let ((p (position #\/ dir :from-end t)))
    (cond ((null p) nil)
          ((zerop p) (and (> (length dir) 1) "/"))
          (t (subseq dir 0 p)))))

(defun agents-find (cwd)
  "AGENTS.md paths from CWD up to the project root, outermost first."
  (let* ((home (ignore-errors (agents-dir (user-homedir-pathname))))
         (dir (agents-dir cwd)) (found nil))
    (loop while dir
          do (let ((f (format nil "~A/~A" (if (string= dir "/") "" dir) *agents-file*)))
               (when (probe-file f) (push f found)))
             (when (or (probe-file (format nil "~A/.git" dir)) (equal dir home))
               (return))
             (setf dir (agents-parent dir)))
    found))

(defun agents-read (path)
  (let ((text (ignore-errors (read-file path))))
    (when (and (stringp text) (plusp (length (string-trim '(#\Space #\Newline #\Tab) text))))
      (if (> (length text) *agents-cap*)
          (format nil "~A~%[truncated: ~D more characters]" (subseq text 0 *agents-cap*)
                  (- (length text) *agents-cap*))
          text))))

(defun agents-section (paths)
  "The prompt text for PATHS, or NIL when none has content."
  (let ((parts (loop for p in paths
                     for text = (agents-read p)
                     when text collect (format nil "--- ~A ---~%~A" p (string-right-trim '(#\Newline) text)))))
    (when parts
      (format nil "~%- ~A The project's AGENTS.md instructions (the user wrote them for agents working here; follow them, the nearest file last):~%~{~A~^~%~}~%~A"
              *agents-begin* parts *agents-end*))))

(defun agents-strip (prompt)
  "PROMPT without an earlier AGENTS.md section."
  (let* ((b (search *agents-begin* prompt))
         (e (and b (search *agents-end* prompt :start2 b))))
    (if e
        (concatenate 'string
                     (string-right-trim '(#\Newline #\Space #\-) (subseq prompt 0 b))
                     (subseq prompt (+ e (length *agents-end*))))
        prompt)))

(defun agents-load ()
  "Put the AGENTS.md files of the cwd into *system-prompt*, replacing an
earlier section. Returns the paths loaded."
  (when (and (stringp lisping-core:*system-prompt*) lisping-core:*cwd*)
    (let* ((paths (agents-find lisping-core:*cwd*))
           (section (agents-section paths))
           (base (agents-strip lisping-core:*system-prompt*)))
      (setf lisping-core:*system-prompt* (if section (concatenate 'string base section) base)
            *agents-loaded* (and section paths))
      *agents-loaded*)))

(defun slash-agents (ui args)
  (declare (ignore ui args))
  (agents-load)
  (slash-say (if *agents-loaded*
                 (format nil "AGENTS.md in the prompt (applies from the next request): ~{~A~^, ~}" *agents-loaded*)
                 (format nil "no AGENTS.md from ~A up to the project root" (agents-dir lisping-core:*cwd*))))
  t)

(defun agents-start ()
  "Load AGENTS.md into the prompt and install /agents. Idempotent."
  (agents-load)
  (unless lisping-core:*test-boot*
    (define-slash "agents" 'slash-agents "re-read AGENTS.md into the prompt")
    t))
