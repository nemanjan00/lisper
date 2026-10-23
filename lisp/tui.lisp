;;;; lisp/tui.lisp -- tmux-inspired terminal UI: state, terminal control,
;;;; start/stop. Rendered in lisp/tui-render.lisp, driven by
;;;; lisp/tui-input.lisp (keys + weather).
;;;;
;;;; One lock (*tui-lock*) protects all UI state. Only the redraw thread
;;;; writes to the screen. Other threads change state and call TUI-DIRTY.

(in-package :lisping)

(declaim (ftype function tui-install-hooks tui-key-loop
                tui-weather-loop tui-redraw-loop))

(defvar *tui* nil "The live TUI-STATE, or NIL when the UI is not running.")
(defvar *tui-lock* (sb-thread:make-mutex))
(defvar *tui-cv* (sb-thread:make-waitqueue))
(defvar *tui-dirty* 0)

(defstruct tui-state
  tty                                ; single :io fd-stream on /dev/tty
  (width 80) (height 24)
  (chat (make-uibuf :cap 2000))
  (weather (make-uibuf :align :top))
  (weather-at 0)
  (input (make-array 64 :element-type 'character :adjustable t :fill-pointer 0))
  (active 0)         ; index of focused pane: 0 chat, 1 weather
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

(defun tui-raw-mode ()
  "Put the terminal in raw-ish mode in-process (shell stty would hang:
the image runs in a background process group and gets SIGTTOU). Saves
the original termios for TUI-RESTORE-MODE."
  (when (and *tui* (null *tui-saved-termios*))
    (let* ((fd (sb-sys:fd-stream-fd (tui-state-tty *tui*)))
           (saved (sb-posix:tcgetattr fd))
           (raw (sb-posix:tcgetattr fd)))
      (setf (sb-posix:termios-lflag raw)
            (logand (sb-posix:termios-lflag raw)
                    (lognot (logior sb-posix:icanon sb-posix:echo sb-posix:isig
                                    sb-posix:iexten)))
            (sb-posix:termios-iflag raw)
            (logand (sb-posix:termios-iflag raw)
                    (lognot (logior sb-posix:icrnl sb-posix:inlcr sb-posix:igncr
                                    sb-posix:ixon))))
      (sb-posix:tcsetattr fd sb-posix:tcsanow raw)
      (setf *tui-saved-termios* saved))))

(defun tui-restore-mode ()
  "Restore the terminal settings saved by TUI-RAW-MODE."
  (when *tui-saved-termios*
    (ignore-errors
      (sb-posix:tcsetattr (sb-sys:fd-stream-fd (tui-state-tty *tui*))
                          sb-posix:tcsanow *tui-saved-termios*))
    (setf *tui-saved-termios* nil)))

(defun tty-write (string)
  (when *tui*
    (write-string string (tui-state-tty *tui*))
    (finish-output (tui-state-tty *tui*))))

(defun tty-size ()
  "Current terminal size as (values rows cols), or NIL."
  (ignore-errors
    (let ((out (shell "stty -F /dev/tty size")))
      (when (and (stringp out) (> (length out) 3))
        (let ((sp (position #\Space out)))
          (values (parse-integer out :end sp)
                  (parse-integer out :start (1+ sp))))))))

(defun tui-update-size ()
  (multiple-value-bind (rows cols) (tty-size)
    (when (and rows cols (plusp rows) (plusp cols)
               (or (/= rows (tui-state-height *tui*))
                   (/= cols (tui-state-width *tui*))))
      (setf (tui-state-height *tui*) rows
            (tui-state-width *tui*) cols)
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
    (tui-raw-mode)
    (setf *tui* (make-tui-state :tty tty))
    (multiple-value-bind (rows cols) (tty-size)
      (when (and rows cols)
        (setf (tui-state-height *tui*) rows
              (tui-state-width *tui*) cols)))
    (tty-write (format nil "~C[?1049h~C[?25l~C[0m~C[2J~C[H" #\Esc #\Esc #\Esc #\Esc #\Esc))
    (setf (tui-state-threads *tui*)
          (list (sb-thread:make-thread 'tui-key-loop :name "tui-keys")
                (sb-thread:make-thread 'tui-weather-loop :name "tui-weather")
                (sb-thread:make-thread 'tui-redraw-loop :name "tui-redraw")))
    (tui-install-hooks)
    (tui-dirty)
    t))

(defun tui-stop ()
  "Stop the UI and give the terminal back in a sane state."
  (let ((ui *tui*))
    (setf *tui* nil)
    (when ui
      (setf *emit* 'terminal-emit)
      (dolist (th (tui-state-threads ui))
        (ignore-errors (sb-thread:terminate-thread th)))
      (ignore-errors
        (write-string (format nil "~C[0m~C[?25h~C[?1049l" #\Esc #\Esc #\Esc)
                      (tui-state-tty ui))
        (finish-output (tui-state-tty ui)))
      (ignore-errors (close (tui-state-tty ui)))
      (tui-stty "sane")
      (setf (tui-state-threads ui) nil)
      (start-terminal-input)
      t)))

(defun tui-crash (condition)
  "The UI broke: restore the terminal, keep the agent alive."
  (let ((ui *tui*))
    (setf *tui* nil)
    (when ui
      (ignore-errors
        (write-string (format nil "~C[0m~C[?25h~C[?1049l~%TUI died: ~A~%"
                              #\Esc #\Esc #\Esc condition)
                      (tui-state-tty ui))
        (finish-output (tui-state-tty ui)))
      (ignore-errors (close (tui-state-tty ui)))
      (tui-stty "sane")
      (setf *emit* 'terminal-emit)
      (start-terminal-input)))
  (terminal-emit (list :type :error
                       :text (format nil "TUI died and was stopped: ~A" condition))))
