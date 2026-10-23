# Session notes

## Facts about this environment
Things I keep relearning on every restart — write them down once:

- **Reading files**: use a `with-open-file` + `read-line` loop. `uiop` and
  `alexandria` are not loaded (`(require :uiop)` works if you want it).
  My first attempt at `(uiop:read-file-string ...)` failed on the missing
  package.
- **shell is crippled for tty work**: commands get no stdin and run in a
  background process group, so `tty`/`stty` report "not a tty", and
  anything that *changes* terminal settings hangs forever (SIGTTOU).
  Do termios work in-process with `sb-posix:tcgetattr`/`tcsetattr` on the
  /dev/tty fd instead.
- **Editing files**: call my `edit_file` tool directly, not some
  `(edit-file-replace ...)` form inside `eval` — JSON tool args avoid
  Lisp string escaping entirely.
- **Crash forensics**: a crash message drawn on the tty is invisible to
  me (I can't read the screen back). Check `state/tui-crash.log` for the
  condition and backtrace.
- **defstruct in a live image**: redefining a defstruct with different
  slots fails; rename the struct or restart the image.

## TUI raw-mode fixes (previous boot)
Two bugs in `lisp/tui.lisp` kept raw mode from ever applying:

1. **Ordering bug**: `tui-start` called `(tui-raw-mode)` *before*
   `(setf *tui* ...)`, but `tui-raw-mode` began with `(when *tui* ...)`
   and silently did nothing. Result: keys echoed on top of the UI and
   Enter arrived as #\Newline. Fix: `tui-raw-mode` now takes the tty
   stream as an argument (no longer depends on `*tui*`), and `tui-start`
   sets `*tui*` first, then calls `(tui-raw-mode tty)`.

2. **Aliasing bug**: `tui-raw-mode` saved `cur` (the live termios object)
   into `*tui-saved-termios*` and then mutated that same object into raw
   mode — so `tui-restore-mode` would have restored raw mode. Fix: save a
   separate `(sb-posix:tcgetattr fd)` copy before mutating.

Verified after `tui-stop`/`tui-start`: lflag & (icanon|echo|isig) == 0 on
the live tty, and the saved termios still has icanon+echo set (cooked), so
restore works. Checked with:
`(sb-posix:termios-lflag (sb-posix:tcgetattr (sb-sys:fd-stream-fd (tui-state-tty *tui*))))`

## Core change to know about
Since that boot, user messages also reach `*emit*` as `:user` events, so
the chat pane can render them directly (no more scraping `*inbox*`).

## General lesson
Guard clauses like `(when *tui* ...)` that depend on state set by the
caller are order-sensitive landmines in a live image where you edit one
function at a time. Prefer passing arguments over reading global state
set elsewhere; it makes functions independently testable from the REPL.
