;;;; init.lisp: loaded at boot, like Emacs's init.el.

;; The model and provider come from ~/.lisper/model.json (the core's
;; use-model writes it; keys are in ~/.lisper/auth.json): nothing to set here.

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
(load-file "lisp/markdown.lisp")

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
(load-file "lisp/activity.lisp")
(load-file "lisp/mcp.lisp")
(load-file "lisp/theme.lisp")
(load-file "lisp/nudge.lisp")
(load-file "lisp/models.lisp")
(load-file "lisp/history.lisp")
(load-file "lisp/copy.lisp")
(load-file "lisp/search.lisp")
(load-file "lisp/usage.lisp")
(load-file "lisp/powerline.lisp")
(load-file "lisp/interrupt.lisp")
(load-file "lisp/compact.lisp")

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
(models-start)
(activity-start)

;; MCP servers (~/.lisper/mcp.json): /mcp command, tools SERVER__TOOL.
(mcp-start)

;; Colour theme (Dracula by default; /theme off for plain ANSI).
(theme-start)
(powerline-start)   ; /powerline: nerd | triangles | flat, saved in ~/.lisper/powerline

;; Todo nudge after an idle turn with open todos (/nudge on|off).
(nudge-start)

;; /compact [focus]: summarize the whole conversation in one model call.
(compact-start)

;; Interrupt the running turn: Esc Esc, Ctrl-C on an empty input, /stop.
(interrupt-start)

;; Input history: Up/Down on an empty input, kept in ~/.lisper/history.jsonl.
(history-start)

;; /copy and Ctrl-Y: last reply / code block to the clipboard (OSC 52).
(copy-start)

;; /find: search the chat scrollback, highlight, Ctrl-P/Ctrl-N.
(find-start)

;; /usage: tokens, cache hit rate, rough cost; totals kept per session.
(usage-start)
