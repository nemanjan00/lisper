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
- The model (Kimi K2.7 in the first runs) is told what it is and how the
  image works, then given tasks. The first one was a tmux-like terminal UI with
  a chat pane and a wttr.in weather pane.
- A second agent acts as its manager: it watches `state/trace.log`, steers the
  model through `state/queue.txt`, reads the model's screen through tmux, and
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

## Run

    cp .env.example .env              # OPENAI_BASE_URL, OPENAI_API_KEY, MODEL
    ./run.sh                          # supervised; talk to it in the terminal
    tail -f state/trace.log           # watch its reasoning and tool calls
    echo "a message" >> state/queue.txt   # reach it from anywhere

Requires SBCL (`.tool-versions` pins it for asdf), git and curl. Any
OpenAI-compatible Chat Completions endpoint works; it was built against Kimi.

- `LISPING_DOTENV=path` reads settings from another file.
- `LISPING_HEADLESS=1` starts it without terminal input.
- `LISPING_NO_TUI=1` skips the UI the model built for itself.

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
written to `state/trace.log` by the core, whatever the model changes.

## Layout

- `core.lisp`: the kernel. `boot.lisp` loads it; `run.sh` supervises it.
- `init.lisp`, `lisp/`: written and maintained by the model.
- `state/notes.md`: the model's notes to itself across restarts.
- `state/sessions/`, `state/trace.log`, `state/queue.txt`: transcripts, the
  live trace and the message queue (not committed).
