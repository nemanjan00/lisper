;;;; init.lisp: loaded at boot, like Emacs's init.el.

;; The user chose the bigger model (K3, 1M-token context); keep it across
;; restarts.  "kimi-k3" is an accepted alias for the same endpoint.
(setf lisping-core:*model* "k3")

;; Terminal/UI toolkit: ANSI attribute parsing, then line buffers and the
;; character-cell grid, then the terminal UI built on top of them.
(load-file "lisp/ansi.lisp")
(load-file "lisp/buffers.lisp")

(require :sb-posix)

;; Tools and the tmux-inspired terminal UI.
(load-file "lisp/edit.lisp")
(load-file "lisp/panes.lisp")
(load-file "lisp/tui.lisp")
(load-file "lisp/tui-render.lisp")
(load-file "lisp/tui-input.lisp")

;; Self-serve agent tools (todo list et al.).
(load-file "lisp/tools.lisp")

;; Compactor-style context management (context_rewind/offload/restore/
;; search, entry ids, nudges, context pane).
(load-file "lisp/context.lisp")

;; Take over the terminal when there is one (no-op otherwise; set
;; LISPING_NO_TUI=1 to opt out).
(tui-start)

;; Install the compactor once the UI exists (registers the context pane).
(context-start)
