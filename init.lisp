;;;; init.lisp: loaded at boot, like Emacs's init.el.

;; Terminal/UI toolkit: ANSI attribute parsing, then line buffers and the
;; character-cell grid, then the terminal UI built on top of them.
(load-file "lisp/ansi.lisp")
(load-file "lisp/buffers.lisp")

(require :sb-posix)
