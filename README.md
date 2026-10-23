# lisping-ai

A Lisp image that builds its own harness. The kernel (`core.lisp`) gives the model
one tool, `eval`, plus `register-tool` and `persist`. Everything else, including
its UI, the model builds for itself.

    ./run.sh                          # talk in the terminal; Ctrl-D quits
    LISPING_HEADLESS=1 ./run.sh       # no terminal input (once it has its own UI)
    LISPING_DOTENV=path/.env ./run.sh # OPENAI_BASE_URL, OPENAI_API_KEY, MODEL

- `extensions.lisp`: what the model has built, replayed on every boot.
- `state/`: its own files; `state/sessions/` holds transcripts.
- `git log`: one commit per thing it kept. The supervisor rolls back to tag
  `last-good` if a boot crashes.
