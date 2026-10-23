# lisper

A Lisp image that builds its own harness, the way Emacs is built out of Emacs
Lisp. The kernel (`core.lisp`, plain SBCL, no libraries) gives the model a
single tool, `eval`, plus `register-tool` to add more. Everything else is
written by the model itself, while it runs: its terminal UI, its file-editing
tool, its notes. That code lives in `init.lisp` and `lisp/`.

## The experiment

lisper is an experiment in a self-developing harness. The question: if an agent
starts with almost nothing, just a way to run code in its own live process,
can it grow the harness it works in, the way people grow an Emacs config?

**Setup.**

- The kernel stays small and unopinionated: an agent loop, `eval`,
  `register-tool`, file persistence, a trace, and a supervisor. No built-in
  editing tools, no UI, no context management, no streaming. Where the model
  might want something different, the kernel exposes a hook instead of a
  feature.
- The model (Kimi K2.7 in the first runs, then K3 with a 1M context) is told what it is and how the
  image works, then given tasks. The first one was a tmux-like terminal UI with
  a chat pane and a wttr.in weather pane.
- A second agent acts as its manager: it watches the session's `trace.log`, steers the
  model through the session's `queue.txt`, reads the model's screen through tmux, and
  changes the kernel when the trace shows the kernel is in the way. It doesn't
  write the model's features.

**What it built in the first two hours**, all of it in its own files: an ANSI
color parser, line buffers and a character grid, a tmux-style layout with a
live weather pane and an input line, raw terminal handling, a crash log for
its own UI, an `edit_file` tool for small edits, and notes to itself.

**What went wrong**, and is the interesting part:

- *Parentheses.* Long forms with one paren out of place were its most common
  failure, and retyping whole files to fix one paren kept introducing new ones.
  It got past this by checking forms with the Lisp reader and by building
  `edit_file`.
- *Misremembered APIs.* It reached for functions that don't exist in SBCL
  (`define-alien-struct`, `uiop` without loading it) and lost turns on each.
- *Working blind.* It can't see its own screen, so UI bugs were invisible to it
  until it logged crashes to a file and drove its UI through tmux.
- *Live-image bugs.* Stale threads from earlier starts stealing keystrokes; a
  string that aliased the input buffer, so every submitted message became
  empty; a failed edit that rolled back the file but left a declaration behind
  in the running image.
- *Forgetting.* Its conversation doesn't survive a restart, and until it kept
  notes it relearned the same facts each time.

**How the kernel changed in response.** Each of these came from something in
the trace: automatic saving of definitions was replaced by Emacs-style files
that are kept only if they load cleanly; error messages got hints for
misremembered names; a message queue, `:user` events and a trace file were
added; blank messages are dropped so one can't block every request. Each time
the rule was the same: fix what the kernel got wrong, expose what the model
needs, and leave the feature to the model.

**Open questions.** Can it keep its context small on its own, if the kernel
tells it how big the context is? Can it cope with a hang, which the supervisor
doesn't catch yet? And the longer goal: a keyboard-less lab-PC interface, a
window manager it can be talked to, that drives electronics instruments as
tools.

## What it looks like

Everything in this screen was written by the model: the panes and separators,
the drag-resizable layout, the tool-call summaries, the context pane fed by its
own compactor, the status line with its todo count and the user messages held
until its turn ends. (Cropped: a todo pane sits to the right.) Here it is
building its own background-task and `ask_user` tools (captured with
`tmux capture-pane`, mid-turn):

```
─ chat ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
k3 is thinking... (context 74,724 tokens, 56,832 cached)
  -> eval: shell
  <- 7 lines
k3 is thinking... (context 75,106 tokens, 74,496 cached)
  -> eval: shell
  <- 12 lines
k3 is thinking... (context 75,506 tokens, 75,008 cached)
All pieces confirmed. Implementing in dependency order — first the hooks in existing files:
  -> edit_files: {"edits":[{"new":"(defvar *tui-stop-hooks* nil\n  \"Functions run by TUI-STOP before the …
  edited lisp/tui.lisp, lisp/tui-input.lisp, lisp/panes.lisp (5 edits; Wrote and loaded lisp/tui.lisp, lisp/tui-input.lisp, lisp/panes.lisp; committed together.)
k3 is thinking... (context 75,730 tokens, 75,264 cached)
Hooks in. Now the tasks file:
  -> eval: in-package
  <- 33 lines
k3 is thinking... (context 80,995 tokens, 75,520 cached)
It committed (style warnings are just forward references within the file — I'll declaim them away after). Now ask.lisp:
  -> eval: in-package
  <- 10 lines
k3 is thinking... (context 85,469 tokens, 80,896 cached)
─ context ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
context: 85k tokens (8% of 1M, 43% of budget)
+4.4k est since · 130 messages
rewind e200…e246: DONE (user confirmed "looks ok" e249): blank todo+context pa (9.9k)
rewind e68…e157: All 4 review todos DONE and archived via todo clear (user sa (15k)
rewind e17…e26: edit_files verification COMPLETE: (1) write-files-and-load e (450)
rewind e14…e15: read-file-string is undefined in LISPING (style warning + UN (370)
rewind e1…e8: Verified edit_files with new core: (1) (fboundp 'lisping-cor (400)
>
  lisping-ai k3 [3 todos open] chat [2 held]
```

## The power of rewind

The kernel has no context management. It only reports the context size and
leaves `*messages*` to the model. The model built a compactor
(`lisp/context.lisp`): every message gets an entry id, and `context_rewind`
collapses a finished stretch (`e68…e157`) into a one-line summary it writes
itself, while `context_offload` moves a big tool result to a file. The
originals are kept, so `context_restore` can bring one back. That's the
`rewind …` lines in the context pane above.

Measured over one run (768 requests, 50.6M input tokens):

- 21 rewinds and 16 offloads, and only one restore: the summaries were almost
  always enough.
- A rewind costs fresh (uncached) tokens once, because the prompt changes from
  that point on, and saves on every later request. One shrank the context from
  109.6k to 31.8k tokens: 20.5k fresh tokens once, then 77.8k fewer tokens on
  each following request. A typical one costs 1–6k once and saves 5–78k per
  request.
- Of the 50.6M input tokens only about 1.8M were fresh. Kimi's prefix cache hit
  about 99.7% of what it could cache.
- Before the compactor, on K2.7, requests averaged 76.8k tokens. With it, on
  K3, the working context sits around 5–20k (recent requests 5–7k), with 1M
  available. It isn't trying to fit the window. It keeps only what the current
  task needs.

So a long-running session neither ends nor slows down: finished work collapses
into a few lines, and the cache stays warm because only the tail changes.

## Sessions and `~/.lisper`

One session is one process: the same code, with its own conversation and
working directory. Code lives in the repo; everything else lives in
`~/.lisper/` (`LISPER_HOME`):

    ~/.lisper/
      notes.md                    shared data (home-path)
      sessions/main/              per session (session-path)
        messages.json, meta.json  conversation and cwd, saved after every step
        queue.txt, trace.log      message queue and live trace
        todos.txt, layout.lisp    the model's per-session state

- `LISPER_SESSION=name ./run.sh` starts or resumes session `name`
  (default `main`). Several sessions can run at once. Each one's whole-file
  writes are refused if another session changed the file since it last looked.
- `LISPER_CWD=dir` sets the session's working directory, where `shell` runs.
  It's saved with the session.
- A restart (crash, SIGUSR1, `restart-image`) resumes the conversation where it
  was. The conversation is saved before tool calls run, so even a tool that
  kills the process leaves the turn recoverable. `LISPER_FRESH=1` starts
  without the saved conversation. If the image crashes three times in a row
  soon after starting, the supervisor sets the conversation aside and starts
  fresh.
- On exit, `run.sh` prints the command that resumes the session. Inside, the
  kernel offers `(session-list)` (every session with its cwd, size, first
  message and whether a process has it) and `(session-switch name)`, which
  saves this session and has the supervisor start the other one in its place.
  The model builds the `/resume` command on top of them.
- `~/.lisper` is its own git repo, committed after every turn (transcripts and
  logs excluded), so a bad rewrite of the model's notes can be undone.

## Run

    cp .env.example .env              # OPENAI_BASE_URL, OPENAI_API_KEY, MODEL
    ./run.sh                          # supervised; talk to it in the terminal
    tail -f ~/.lisper/sessions/main/trace.log           # watch it think
    echo "a message" >> ~/.lisper/sessions/main/queue.txt   # reach it from anywhere

Requires SBCL (`.tool-versions` pins it for asdf), git and curl. Any
OpenAI-compatible Chat Completions endpoint works; it was built against Kimi.

- `LISPING_DOTENV=path` reads settings from another file.
- `LISPING_HEADLESS=1` starts it without terminal input.
- `LISPING_NO_TUI=1` skips the UI the model built for itself.
- Messages sent while the model is mid-turn wait until it's done, then arrive
  one per turn. Start a message with `!` to interrupt: it goes in before the
  model's next call.
- `pkill -USR1 -f boot.lisp` restarts the image through the supervisor;
  SIGTERM stops it (and the supervisor with it).

## How persistence works

Like Emacs:

- At boot the core loads `init.lisp`, like `init.el`, and it loads the
  model's other files with `load-file`. A form that fails is reported to the
  model and skipped.
- `eval` with only `code` changes the running image, like `M-:`.
- `eval` with a `file` argument writes the file verbatim, like saving a buffer.
  Before the running image sees it, a fresh SBCL boots with the new file in
  place (`test-boot`); only if that's clean is the file loaded into the running
  image and committed, like `eval-buffer`. Otherwise the old version is put
  back and the image is untouched. A function that's called but never defined
  anywhere also counts as a problem.
- git keeps the history. If a boot crashes, the supervisor (`run.sh`) rolls the
  files back to the last commit that booted (tag `last-good`).

## What the model can change

The core's own functions are locked, but its hooks are plain variables and
functions the model may replace: `*system-prompt*`, `*emit*` (where events go),
`*complete*` (how the model is called), `*before-request*`, `*messages*` (its
own context), and the terminal and queue input loops. Every event is also
written to the session's `trace.log` by the core, whatever the model changes.

## Layout

- `core.lisp`: the kernel. `boot.lisp` loads it; `run.sh` supervises it.
- `init.lisp`, `lisp/`: written and maintained by the model.
- `hooks/pre-commit`: boot-checks every commit (installed by `run.sh`).
- `~/.lisper/`: notes, sessions, traces, queues. See above.
