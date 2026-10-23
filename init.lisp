;;;; init.lisp: loaded at boot, like Emacs's init.el.

;; Terminal/UI toolkit: ANSI attribute parsing, then line buffers and the
;; character-cell grid, then the terminal UI built on top of them.
(load-file "lisp/ansi.lisp")
(load-file "lisp/buffers.lisp")

(require :sb-posix)

;; Tools and the tmux-inspired terminal UI.
(load-file "lisp/edit.lisp")
(load-file "lisp/tui.lisp")
(load-file "lisp/tui-render.lisp")
(load-file "lisp/tui-input.lisp")

;; Take over the terminal when there is one (no-op otherwise; set
;; LISPING_NO_TUI=1 to opt out).
(tui-start)
