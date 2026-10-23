# lisper

A Lisp image that builds its own harness, the way Emacs is built out of Emacs
Lisp. The kernel (`core.lisp`, plain SBCL, no libraries) gives the model a
single tool, `eval`, plus `register-tool` to add more. Everything else is
written by the model itself, while it runs: its terminal UI, its file-editing
tool, its notes. That code lives in `init.lisp` and `lisp/`.

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
- `eval` with a `file` argument writes the file verbatim and loads it, like
  saving a buffer and running `eval-buffer`. It's kept and committed only if it
  loads with no errors or warnings; otherwise the old version is put back. So
  the files only ever hold code that loaded, and what loads at boot is what ran.
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
