# lisping-ai

A Lisp image that builds its own harness, the way Emacs is built out of Emacs
Lisp. The kernel (`core.lisp`) gives the model one tool, `eval`, plus
`register-tool`. Everything else, including its UI, the model builds itself.

    ./run.sh                          # talk in the terminal; Ctrl-D quits
    LISPING_HEADLESS=1 ./run.sh       # no terminal input (once it has its own UI)
    LISPING_DOTENV=path/.env ./run.sh # OPENAI_BASE_URL, OPENAI_API_KEY, MODEL
    tail -f state/trace.log           # watch its reasoning and tool calls

Persistence works like Emacs:

- `init.lisp` is loaded at boot, like `init.el`, and loads the AI's other files.
- `eval` with only `code` changes the running image, like `M-:`.
- `eval` with `file` writes the file and loads it, like saving a buffer and
  `eval-buffer`; it is kept and committed only if it loads cleanly.
- `state/` holds its notes and data; `state/sessions/` holds transcripts.
- The supervisor rolls files back to tag `last-good` if a boot crashes.
