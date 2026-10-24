;;;; init.lisp: loaded at boot, like Emacs's init.el.

;; The user switched to Claude Sonnet 5.5 through the local cli-proxy-api
;; (.env points OPENAI_BASE_URL there).  Back to Kimi: restore the kimi lines
;; in .env and set this to "k3" (K3, 1M-token context).
(setf lisping-core:*model* "claude-sonnet-5-5")

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

;; Background tasks/monitors and the ask_user question pane.
(load-file "lisp/tasks.lisp")
(load-file "lisp/ask.lisp")
(load-file "lisp/slash.lisp")
(load-file "lisp/pathcomp.lisp")

;; Take over the terminal when there is one (no-op otherwise; set
;; LISPING_NO_TUI=1 to opt out).
(tui-start)

;; Install the compactor once the UI exists (registers the context pane).
(context-start)

;; Task/monitor tools + the tasks pane, and the ask_user tool + its
;; key/mouse interceptors.
(tasks-start)
(ask-start)

;; Slash commands + autocomplete popup (after ask-start: the ask pane
;; must see keys first while a question is open).
(slash-start)
(pathcomp-start)
