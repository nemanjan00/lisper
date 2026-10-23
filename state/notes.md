# Session notes

## Facts about this environment
Things I keep relearning on every restart — write them down once:

- **Reading files**: use a `with-open-file` + `read-sequence`/`read-line` loop.
  `uiop` and `alexandria` are not loaded (`(require :uiop)` works if wanted).
- **shell is crippled for tty work**: commands get no stdin and run in a
  background process group, so `tty`/`stty` report "not a tty", and anything
  that *changes* terminal settings hangs forever (SIGTTOU). Do termios work
  in-process with `sb-posix:tcgetattr`/`tcsetattr` on the /dev/tty fd instead.
- **Editing files**: call the `edit_file` tool directly, not a Lisp-level
  edit inside `eval` — JSON tool args avoid Lisp string escaping.
- **Crash forensics**: a crash message drawn on the tty is invisible to me.
  Check `state/tui-crash.log` for the condition and backtrace.
- **defstruct in a live image**: redefining a defstruct with different slots
  fails ("change in instance length"); rename the struct or restart. (The
  core now takes SBCL's use-new-definition restart on file reload, but an
  old running core does not.) I renamed `tui-state` -> `tui-ui` to apply the
  weather-removal slot change before a restart.

## Rules and boundaries
- **tmux boundary**: only ever `capture-pane` / `send-keys` to MY pane —
  `lisper:2` (the `bash run.sh` pane). All other tmux sessions/windows
  belong to the user and other agents: never send keys there, don't read
  them either. (Developer instruction, 2026-10-23.)

## Lessons, round 2 (2026-10-23 evening)
7. **SBCL mutexes are not recursive.** Any path that can nest
   with-mutex on the same lock (key dispatch → submit → show-queued)
   must use sb-thread:with-recursive-lock. Recursive + condition-wait is
   fine when the lock is held exactly once around the wait.
8. **ESC parsing: check the stream, not the fd.** One read pulls several
   sequences into the fd-stream's input buffer, so wait-until-fd-usable
   times out while the next byte is already buffered — you return
   :escape and the stray "[A" becomes text. Use read-char-no-hang first,
   fd-wait (~0.15 s) only for bytes still in flight; unread-char works
   on fd-streams (verified), so a non-sequence char goes back.
9. **WITH-OPEN-FILE :IF-DOES-NOT-EXIST NIL runs the body with the stream
   bound to NIL.** Any read then falls through to *STANDARD-INPUT* —
   the user's terminal — and blocks forever in the live image (passes
   the fresh-boot check because its stdin is empty). Guard the body
   with (WHEN S ...). This hung tools.lisp for the full 300 s timeout.
10. **`shell` runs dash, not bash: no brace expansion.** `{1..50}` is
    one literal word — printf emits the format once. Use bash -c for
    repeat-burst keystroke tests.
11. **Scroll clamping happens at draw time**: tui-visible-lines clamps
    the offset to [0, n-height] (wrapped-line count) and writes it back
    to the UI; otherwise a too-big offset silently eats the newest
    lines when the chat is shorter than the pane.

## Lessons from the UI work (expensive; do not relearn)

1. **Define helpers before their callers.** An edit whose body calls a
   not-yet-defined function loads with only a STYLE-WARNING and is accepted;
   the committed file then crashes the running UI at call time
   (`tui-maybe-update-size` undefined → redraw thread dead). After any edit
   touching UI code: check `state/tui-crash.log` and that `*tui*` is alive.
2. **`(coerce s 'string)` on a string returns THE SAME object**, and
   `string-trim` returns its argument when there is nothing to trim. Trimming
   the input buffer, resetting its fill-pointer and then sending the "trimmed"
   string sends "" (this caused the empty user message that forced a restart
   once). Use `(copy-seq ...)` when the result must outlive the buffer.
3. **Whole-file rewrites via eval's `file` argument can silently drop
   functions**: the running image still has the old definitions, so the load
   shows no undefined-function warning, and the file is committed incomplete
   (I lost `tui-draw-pane`/`tui-draw-status` this way). After a whole-file
   write, diff the defined defuns against the previous version.
4. **A consumed-flag counter that is never reset spins.** `*tui-dirty*` only
   grew; the redraw loop never blocked and burned a CPU core building grids
   to diff nothing. Reset the flag before acting on it (under the same lock
   that setters use).
5. **This SBCL's quirks**: `sb-thread:condition-wait` returns T even on
   timeout (test the condition, not the return value); `sb-ext:atomic-incf`
   rejects symbol places; `sb-ext:atomic-exchange` does not exist; a symbol
   proclaimed GLOBAL (defglobal) in the session cannot later be defvar'ed —
   rename instead.
6. **Frame discipline for SSH**: full-screen frames (~18KB) only on start,
   resize, C-b r. Diff each frame against the previous grid (synced with
   `ESC[?2026h/l`); skip the write entirely when nothing changed; the clock
   ticks once a minute by waking the redraw wait with the timeout computed as
   seconds-to-next-minute. Never spawn `stty` per frame — SIGWINCH flag +
   2s poll.

## TUI feature notes
- User messages echo dimmed as `you (queued)> ...` the moment they arrive
  (`:queued` event from the core, or `tui-submit-input` until the core has
  it); `tui-show-user` removes the dimmed line and appends the bright
  `you>` line when the `:user` event comes.
- Cursor: hidden at start (`?25l`), re-shown (`?25h`) inside every synced
  frame after positioning; column = prompt-len + input-len + 1.
- Struct is `tui-ui` (was `tui-state`); weather pane removed (experiment
  ended); prefix keys: C-b q quit, C-b r full redraw, C-u/C-c clear input,
  C-d exit/backspace, PgUp/PgDn scroll chat.
