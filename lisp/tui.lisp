;;;; lisp/tui.lisp -- terminal UI: state, terminal control, start/stop.
;;;; Rendered in lisp/tui-render.lisp, driven by lisp/tui-input.lisp.
;;;;
;;;; One lock (*tui-lock*) protects all UI state. Only the redraw thread
;;;; writes to the screen. Other threads change state and call TUI-DIRTY.

(in-package :lisping)

(declaim (ftype function tui-install-hooks tui-key-loop tui-redraw-loop))

(defvar *tui* nil "The live TUI-UI, or NIL when the UI is not running.")
(defvar *tui-lock* (sb-thread:make-mutex))
(defvar *tui-cv* (sb-thread:make-waitqueue))
(defvar *tui-dirty* 0)

(defstruct tui-ui
  tty                                ; single :io fd-stream on /dev/tty
  (width 80) (height 24)
  (chat (make-uibuf :cap 2000))
  (input (make-array 64 :element-type 'character :adjustable t :fill-pointer 0))
  (prefix nil)       ; C-b pressed, waiting for the prefix command key
  (scroll 0)         ; chat scroll-back offset in lines
  (shown-user nil)   ; content of the last user message shown in chat
  (pending-user nil) ; message submitted by the UI, not yet echoed back
  threads)

(defun tui-dirty ()
  "Ask the redraw thread to redraw."
  (sb-thread:with-mutex (*tui-lock*)
    (incf *tui-dirty*)
    (sb-thread:condition-notify *tui-cv*)))

(defvar *tui-saved-termios* nil)
(defvar *tui-queued-text* nil "Text of the dimmed queued echo on screen.")

(defun tui-raw-mode (tty)
  "Put the terminal in raw-ish mode in-process (shell stty would hang:
the image runs in a background process group and gets SIGTTOU). Always
applies; saves the original termios for TUI-RESTORE-MODE, unless a saved
one is already live. Returns true if the mode verified by reading back."
  (let* ((fd (sb-sys:fd-stream-fd tty))
         (cur (sb-posix:tcgetattr fd)))
    ;; A saved termios is stale if the terminal is currently cooked
    ;; (e.g. a crash restored it): saving it again would restore wrong.
    (when (and *tui-saved-termios*
               (plusp (logand (sb-posix:termios-lflag cur) sb-posix:icanon)))
      (setf *tui-saved-termios* nil))
    (unless *tui-saved-termios*
      ;; Save a FRESH copy: CUR is about to be mutated into raw mode,
      ;; so saving it here would restore raw mode later.
      (setf *tui-saved-termios* (sb-posix:tcgetattr fd)))
    (setf (sb-posix:termios-lflag cur)
          (logand (sb-posix:termios-lflag cur)
                  (lognot (logior sb-posix:icanon sb-posix:echo sb-posix:isig
                                  sb-posix:iexten)))
          (sb-posix:termios-iflag cur)
          (logand (sb-posix:termios-iflag cur)
                  (lognot (logior sb-posix:icrnl sb-posix:inlcr sb-posix:igncr
                                  sb-posix:ixon))))
    (sb-posix:tcsetattr fd sb-posix:tcsanow cur)
    (let ((back (sb-posix:tcgetattr fd)))
      (zerop (logand (sb-posix:termios-lflag back)
                    (logior sb-posix:icanon sb-posix:echo sb-posix:isig))))))

(defun tui-restore-mode ()
  "Restore the terminal settings saved by TUI-RAW-MODE."
  (when *tui-saved-termios*
    (ignore-errors
      (sb-posix:tcsetattr (sb-sys:fd-stream-fd (tui-ui-tty *tui*))
                          sb-posix:tcsanow *tui-saved-termios*))
    (setf *tui-saved-termios* nil)))

(defun tty-write (string)
  (when *tui*
    (write-string string (tui-ui-tty *tui*))
    (finish-output (tui-ui-tty *tui*))))

(defun tty-size ()
  "Current terminal size as (values rows cols), or NIL."
  (ignore-errors
    (let ((out (shell "stty -F /dev/tty size")))
      (when (and (stringp out) (> (length out) 3))
        (let ((sp (position #\Space out)))
          (values (parse-integer out :end sp)
                  (parse-integer out :start (1+ sp))))))))

(defun tui-kill-threads ()
  "Terminate any stale tui-* worker threads. Old ones may be blocked in
read-char on their own /dev/tty stream and would steal keystrokes."
  (dolist (th (sb-thread:list-all-threads))
    (when (and (not (eq th sb-thread:*current-thread*))
               (search "tui-" (or (sb-thread:thread-name th) "")))
      (ignore-errors (sb-thread:terminate-thread th)))))

(defvar *tui-size-checked-at* 0 "Last time the terminal size was probed.")
(defvar *tui-winch-count* 0 "Bumped by the SIGWINCH handler.")
(defvar *tui-winch-seen* 0 "Value of *TUI-WINCH-COUNT* last handled.")

;; --- Redraw state -----------------------------------
(defvar *TUI-PREV-GRID* NIL "Grid string of the last frame drawn, or NIL.")
(defvar *TUI-PREV-CURSOR* NIL "(ROW COL) of the cursor last placed, or NIL.")
(defvar *TUI-CLOCK-MINUTE* NIL "Minute string last drawn; NIL forces a redraw.")
(defvar *TUI-FRAMES-TOTAL* 0 "Frames requested.")
(defvar *TUI-FRAMES-SKIPPED* 0 "Frames skipped because nothing changed.")
(defvar *TUI-FRAMES-FULL* 0 "Frames written in full (no diff base).")

;; --- Perf counters ----------------------------------
(defvar *tui-bytes-total* 0 "Terminal bytes written since TUI-START.")

(defun tui-install-sigwinch ()
  "Notice terminal resizes promptly instead of waiting for the poll."
  (ignore-errors
    (sb-sys:enable-interrupt sb-unix:sigwinch
      (lambda (sig code scp)
        (declare (ignore sig code scp))
        ;; Plain INCF, not ATOMIC-INCF: this SBCL's ATOMIC-INCF rejects
        ;; symbol places. A fixnum store is atomic in practice on x86-64,
        ;; and the 2s poll in TUI-MAYBE-UPDATE-SIZE is the safety net.
        (incf *tui-winch-count*)))))

(defun tui-maybe-update-size (ui)
  "Probe the terminal size at most every 2 seconds, or right away after
a SIGWINCH — TTY-SIZE spawns stty, which is too expensive per frame."
  (let ((now (get-universal-time)))
    (when (or (>= now (+ *tui-size-checked-at* 2))
              (> *tui-winch-count* *tui-winch-seen*))
      (setf *tui-size-checked-at* now
            *tui-winch-seen* *tui-winch-count*)
      (tui-update-size ui))))

(defun tui-update-size (ui)
  (multiple-value-bind (rows cols) (tty-size)
    (when (and rows cols (plusp rows) (plusp cols)
               (or (/= rows (tui-ui-height ui))
                   (/= cols (tui-ui-width ui))))
      (setf (tui-ui-height ui) rows
            (tui-ui-width ui) cols
            *tui-prev-grid* nil)         ; size changed: next frame is full
      (tty-write (format nil "~C[2J" #\Esc)))))

(defun tui-open-tty ()
  "Open /dev/tty as an unbuffered :io stream, or NIL."
  (ignore-errors
    (let ((fd (sb-posix:open "/dev/tty" sb-posix:o-rdwr)))
      (sb-sys:make-fd-stream fd :input t :output t :buffering :none
                             :external-format :utf-8
                             :auto-close t :name "tui-tty"))))

(defun tui-start ()
  "Take over the terminal and start the UI. No-op if there is no tty."
  (when *tui* (return-from tui-start nil))
  (let ((no (ignore-errors (sb-posix:getenv "LISPING_NO_TUI"))))
    (when (and no (plusp (length no))) (return-from tui-start nil)))
  (let ((tty (tui-open-tty)))
    (unless tty (return-from tui-start nil))
    (stop-terminal-input)
    (tui-kill-threads)
    (tui-install-sigwinch)
    ;; Fresh screen: with no remembered frame the first draw is a full
    ;; render (the terminal just entered a cleared alt screen), and old
    ;; counters from a previous run must not skew measurements.
    (setf *tui-prev-grid* nil
          *tui-prev-cursor* nil
          *tui-clock-minute* nil
          *tui-queued-text* nil
          *tui-frames-total* 0
          *tui-frames-skipped* 0
          *tui-frames-full* 0
          *tui-bytes-total* 0)
    (setf *tui* (make-tui-ui :tty tty))
    (unless (tui-raw-mode tty)
      (emit :error :text "TUI: terminal did not enter raw mode"))
    (let ((ui *tui*))
      (multiple-value-bind (rows cols) (tty-size)
        (when (and rows cols)
          (setf (tui-ui-height ui) rows
                (tui-ui-width ui) cols)))
      (tty-write (format nil "~C[?1049h~C[?25l~C[0m~C[2J~C[H" #\Esc #\Esc #\Esc #\Esc #\Esc))
      (setf (tui-ui-threads ui)
            (list (sb-thread:make-thread (lambda () (tui-key-loop ui))
                                         :name "tui-keys")
                  (sb-thread:make-thread (lambda () (tui-redraw-loop ui))
                                         :name "tui-redraw")))
      (tui-install-hooks)
      (tui-dirty)
      t)))

(defun tui-stop ()
  "Stop the UI and give the terminal back in a sane state."
  (let ((ui *tui*))
    (setf *tui* nil)
    (when ui
      (setf *emit* 'terminal-emit)
      (tui-kill-threads)
      (ignore-errors
        (write-string (format nil "~C[0m~C[?25h~C[?1049l" #\Esc #\Esc #\Esc)
                      (tui-ui-tty ui))
        (finish-output (tui-ui-tty ui)))
      (ignore-errors (close (tui-ui-tty ui)))
      (tui-restore-mode)
      (setf (tui-ui-threads ui) nil)
      (start-terminal-input)
      t)))

(defun tui-log-crash (condition)
  "Append the condition and a backtrace to state/tui-crash.log."
  (ignore-errors
    (with-open-file (s (merge-pathnames "state/tui-crash.log" *root*)
                       :direction :output :if-exists :append
                       :if-does-not-exist :create)
      (format s "~&--- TUI crash ~A ---~%" (get-universal-time))
      (format s "~A~%" condition)
      (sb-debug:print-backtrace :count 20 :stream s))))

(defun tui-crash (condition)
  "The UI broke: restore the terminal, keep the agent alive."
  (tui-log-crash condition)
  (let ((ui *tui*))
    (setf *tui* nil)
    (tui-kill-threads)
    (when ui
      (ignore-errors
        (write-string (format nil "~C[0m~C[?25h~C[?1049l~%TUI died: ~A~%"
                              #\Esc #\Esc #\Esc condition)
                      (tui-ui-tty ui))
        (finish-output (tui-ui-tty ui)))
      (ignore-errors (close (tui-ui-tty ui)))
      (tui-restore-mode)
      (setf *emit* 'terminal-emit)
      (start-terminal-input)))
  (terminal-emit (list :type :error
                       :text (format nil "TUI died and was stopped: ~A" condition))))
