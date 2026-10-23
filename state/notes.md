# Session notes

<!-- Hygiene pass 2026-10-24 (todo 8): every lesson below carries evidence
(commit hash, in-image test, or [unverified - drop at next review]).
The dash/brace-expansion lesson was contradicted by direct test and
corrected. Do the same pass each review. -->

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
  weather-removal slot change before a restart. Re-verified
  2026-10-24 in-image: 'change in instance length' error.

## Rules and boundaries
- **tmux boundary**: only ever `capture-pane` / `send-keys` to MY pane —
  `lisper:2` (the `bash run.sh` pane). All other tmux sessions/windows
  belong to the user and other agents: never send keys there, don't read
  them either. (Developer instruction, 2026-10-23.)

## Lessons, round 2 (2026-10-23 evening)
7. **SBCL mutexes are not recursive.** Any path that can nest
   with-mutex on the same lock (key dispatch → submit → show-queued)
   must use sb-thread:with-recursive-lock. Evidence: grab-mutex
   :waitp nil on a self-held mutex signals 'Recursive lock attempt'
   (verified in-image 2026-10-24); fix commit 6e46d72 (Use recursive
   *tui-lock*). Recursive + condition-wait is
   fine when the lock is held exactly once around the wait.
8. **ESC parsing: check the stream, not the fd.** One read pulls several
   sequences into the fd-stream's input buffer, so wait-until-fd-usable
   times out while the next byte is already buffered — you return
   :escape and the stray "[A" becomes text. Use read-char-no-hang first,
   fd-wait (~0.15 s) only for bytes still in flight; unread-char works
   on fd-streams (verified), so a non-sequence char goes back. Evidence: state/tmp/keys.log,
   commit ae3f78c (session 2026-10-24).
9. **WITH-OPEN-FILE :IF-DOES-NOT-EXIST NIL runs the body with the stream
   bound to NIL.** Any read then falls through to *STANDARD-INPUT* —
   the user's terminal — and blocks forever in the live image (passes
   the fresh-boot check because its stdin is empty). Guard the body
   with (WHEN S ...). This hung tools.lisp for the full 300 s timeout. The NIL-binding
   half verified in-image 2026-10-24; the stdin-block half is standard CL.
10. **`shell` runs bash 5.3 as /bin/sh: brace expansion WORKS**
    (`echo {1..3}` prints `1 2 3`, verified 2026-10-24; /bin/sh is a
    symlink to bash 5.3.20). An earlier version of this note claimed
    dash / no brace expansion - contradicted by direct test, dropped.
    Test the construct, don't assume POSIX minimums.
11. **Scroll clamping happens at draw time**: tui-visible-lines clamps
    the offset to [0, n-height] (wrapped-line count) and writes it back
    to the UI; otherwise a too-big offset silently eats the newest
    lines when the chat is shorter than the pane. Evidence: commit
    b76aa80 (Scroll clamp).

## Pane system (lisp/panes.lisp, built 2026-10-23)
- **tui-start no-ops while *tui* is alive.** After changing draw/render code,
  `(tui-stop)` then `(tui-start)` — a plain tui-start leaves the old UI
  running the new code with unregistered panes (blank screen, no crash).
  Verify with `tmux capture-pane -t lisper:2 -p | head` after every step.
- Panes: `*panes*` registry (name → pane struct), `*layout*` tree of
  `(:hsplit ratio a b)` / `(:vsplit ratio a b)` / `(leaf name)`.
  Kinds: :chat :todo :buffer :cmd (cmd re-runs a shell command every
  INTERVAL via the "tui-panes" ticker thread).
- Only panes with `pane-needs-redraw` redraw into the persistent
  `*panes-canvas*`; input+status rows are composed on top; the frame
  diff still sends only changed cells.
- Scrolling keys apply to `*pane-focus*`; wheel scrolls the pane under
  the pointer; click focuses a pane. Tools: `pane` (list/focus/add/
  split/remove) and `layout` (get/set/reset). Layout persists to
  state/layout.lisp.
- Slot named `dirty` in a defstruct collides with a same-named function
  (the accessor) — name slots `needs-redraw` instead.

## Lessons from the UI work (expensive; do not relearn)

1. **Define helpers before their callers.** An edit whose body calls a
   not-yet-defined function loads with only a STYLE-WARNING and is accepted;
   the committed file then crashes the running UI at call time
   (`tui-maybe-update-size` undefined → redraw thread dead). After any edit
   touching UI code: check `state/tui-crash.log` and that `*tui*` is alive. Mechanism
   verified 2026-10-24: a call to an undefined function compiles with
   only a WARNING (no error), so a file can load 'clean'.
2. **`(coerce s 'string)` on a string returns THE SAME object**, and
   `string-trim` returns its argument when there is nothing to trim. Trimming
   the input buffer, resetting its fill-pointer and then sending the "trimmed"
   string sends "" (this caused the empty user message that forced a restart
   once). Use `(copy-seq ...)` when the result must outlive the buffer.
   Identity half verified in-image 2026-10-24: (eq (coerce s 'string) s)
   and string-trim of an already-trimmed string both return the input.
3. **Whole-file rewrites via eval's `file` argument can silently drop
   functions**: the running image still has the old definitions, so the load
   shows no undefined-function warning, and the file is committed incomplete
   (I lost `tui-draw-pane`/`tui-draw-status` this way). After a whole-file
   write, diff the defined defuns against the previous version.
   [unverified anecdote - drop at next review]
4. **A consumed-flag counter that is never reset spins.** `*tui-dirty*` only
   grew; the redraw loop never blocked and burned a CPU core building grids
   to diff nothing. Reset the flag before acting on it (under the same lock
   that setters use).
   [unverified anecdote - drop at next review]
5. **This SBCL's quirks** (all re-verified 2026-10-24):
   condition-wait with :timeout returns NIL on a plain timeout, BUT
   commit 9faf8fe documents a timeout return that comes back WITHOUT
   the lock held - bail out and re-acquire instead of releasing;
   sb-ext:atomic-incf rejects plain variable places ('Invalid first
   argument'; wants a defglobal / struct slot / car of a cons...);
   sb-ext:atomic-exchange does not exist (reader error - use
   atomic-update); a defglobal'ed symbol cannot later be defvar'ed
   (fresh-SBCL test: unhandled condition) - rename instead.
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

## Session 2026-10-24 (cursor editing, pane gaps, auto-scroll, context rule)
- **Context rule (developer, verified): this endpoint silently truncates**
  requests that exceed the window — no error, oldest context dropped. Watch
  `*last-usage*` prompt_tokens and the :thinking context readout. Past 150k:
  compact old work into notes.md and restart (compactor tools are the
  primary defense; the restart rule is the fallback).
- **Keys on this terminal** (screen-256color inside tmux, evidence:
  keys.log after `tmux send-keys Home`): Home = ESC[1~, End = ESC[4~; also
  map 7~/8~. Arrows: ESC[A-D or SS3 ESC O A-D. Unknown CSI params are now
  logged to keys.log as (:unknown-csi params final) — check there first when
  a key 'does nothing'.
- **Never block on read-char inside an escape sequence** (evidence: keys.log
  :ESCAPE runs, ae3f78c): a split sequence blocked the csi reader, which then
  swallowed bytes of the *next* key. Per-byte timeout (0.05 s) with bail-out.
- **send-keys to lisper:2 lands in my own input line; Enter submits a real
  user message to myself.** Clear test keys with C-u, never send Enter unless
  I mean to message myself. Forensics: state/tmp/keys.log (parsed keys), or
  instrument tui-read-key live to log raw chars.
- **tmux capture output was echoing into chat** via the tool-result preview:
  tui-chat-result now shows only `<- N lines' for >2-line results (4d78742).
- **Auto-scroll**: scroll = offset from bottom. New lines while scrolled up
  bump pane-scroll + *tui-chat-new* (tui-chat-note-new), marker `↓ N new` on
  the pane's last content row, cleared when scroll hits 0 (229e96d).
- **Pane gaps**: hsplit reserves a 1-column gap; panes-draw-separator repaints
  a dim │ every frame (no diff cost) so stale text can't linger (229e96d).

## Session 2026-10-24 evening (compactor installed, first real collapse)
- **lisp/context.lisp is the compactor** (ported from the user's ~/compactor/README.md):
  context_rewind / context_offload / context_restore / context_search tools;
  entry ids via *before-request* ([entry N] prefix on the request copy only —
  stored messages stay clean); auto-offload of tool results over 20000 chars
  (head+tail kept, original in the registry); threshold notes edge-triggered
  (+5 points, *ctx-window* 120000); todo-done milestone nudge via the *emit*
  watcher; context pane shows size + collapses. Wired in init.lisp:
  (load-file "lisp/context.lisp") then (context-start) AFTER (tui-start).
  Rewind guard: never the latest real user message or anything after it.
- **First real collapse: entries e3…e136 (~134 messages) after prompt hit
  ~177k tokens** — this endpoint silently truncates; *last-usage*
  prompt_tokens lags one request behind the collapse.
- **Pane renderers MUST return chunk lists** ((list (cons text attr)) per
  line); plain strings crash the redraw thread (took the whole UI down once).
  Fault isolation since added: handler-case in pane-draw-into,
  lisp/panes.lisp:271.
- **Pane fault isolation** (panes.lisp): pane-draw-into wraps pane-draw-into-1
  in handler-case, logs to tui-crash.log, draws 'pane error: ...' in the pane.
- No COPY-HASH-TABLE in this CL (fboundp check 2026-10-24: absent) — ctx-copy-message copies the hash by hand.
- context_restore of a collapsed tool message re-inserts it as a QUOTED user
  message, so it can never orphan a tool_call the API would then reject/drop.
- **Never put a literal tilde in a format string** (~/, ~D eat the prose);
  use write-line for notes and prose.
- Testing the input line live: `tmux send-keys -t lisper:2 "text"` types without Enter; Home/End/C-u as key names; then `tmux capture-pane -t lisper:2 -p`. Input row is line 61 (of 63) at 208-col width. [verified 2026-10-24, h-scroll todo 13]

## 22:38 nested separators (todo 27, won't-do)
- Reproduced live: setting *layout* to (:vsplit 0.85 (:hsplit 0.78 chat (:hsplit 0.5 todo weather)) context) made panes-draw-gap-columns paint EVERY cell as a gap — whole screen became │ chars. So the rect/gap test fails wholesale for hsplit nested inside the right branch of an hsplit (three sibling columns).
- Note: vsplit nested inside hsplit (the normal todo/weather stack) works fine — nesting per se isn't broken, only sibling columns under a nested hsplit.
- Likely panes-draw-gap-columns only considers top-level hsplit boundaries; never debugged further (timeboxed, LOW). Debug on a COPY of the canvas, never the live one — restoring took a tui-drop-frame + tui-force-redraw + reset of *layout* and a panes-save (layout file had been overwritten with the broken tree).
- Unrelated fixes landed same session: cmd pane shows '(exit N)' only on non-zero exit (lisp/panes.lisp panes-refresh-cmds); weather pane cmd now wttr.in/Belgrade?format=%%l:+%%C+%%t+%%w+%%p (%%C text instead of double-width %%c emoji).
