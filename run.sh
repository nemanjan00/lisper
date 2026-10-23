#!/usr/bin/env bash
# Supervisor for lisping-ai.
#
# Exit 0 stops, exit 42 (restart-image) restarts. Any other exit is a crash:
# the image is restarted, and if it crashed while booting, the AI's files
# (everything outside state/) are first rolled back to the last commit that
# booted (tag last-good).
set -u
cd "$(dirname "$0")"

# Model settings come from ./.env (see .env.example), or the file named by LISPING_DOTENV.
BOOT_GRACE=15      # seconds; a crash sooner than this counts as a boot crash
MAX_BOOT_CRASHES=3
QUICK=300          # seconds; crashes sooner than this after start count toward a crash loop
MAX_QUICK_CRASHES=3

# On the way out, say how to come back to this session.
resume_hint() {
  [ -f "$SESSION_DIR/messages.json" ] || return 0
  local cmd="$PWD/run.sh"
  [ "$SESSION" = main ] || cmd="LISPER_SESSION=$(printf %q "$SESSION") $cmd"
  [ -n "${LISPER_HOME_SET:-}" ] && cmd="LISPER_HOME=$(printf %q "$LISPER_HOME") $cmd"
  echo "[lisper] session $SESSION saved. Resume it with:"
  echo "  $cmd"
}
trap resume_hint EXIT
trap 'exit 0' INT

[ -n "${LISPER_HOME:-}" ] && LISPER_HOME_SET=1
LISPER_HOME="${LISPER_HOME:-$HOME/.lisper}"
SESSION="${LISPER_SESSION:-main}"
SESSION_DIR="$LISPER_HOME/sessions/$SESSION"

# session-switch writes the next session's name (and optionally a cwd for a new
# one) to $LISPER_HOME/switch, then restarts; start that session in its place.
switch_session() {
  local file="$LISPER_HOME/switch" name cwd
  [ -f "$file" ] || return 0
  { read -r name; read -r cwd; } < "$file"
  rm -f "$file"
  [[ "$name" =~ ^[A-Za-z0-9_-]+$ ]] || return 0
  SESSION="$name"; SESSION_DIR="$LISPER_HOME/sessions/$SESSION"
  export LISPER_SESSION="$SESSION"
  if [ -n "$cwd" ]; then export LISPER_CWD="$cwd"; else unset LISPER_CWD; fi
  echo "[supervisor] switching to session $SESSION"
}
resume() { mkdir -p "$SESSION_DIR" && printf '%s\n' "$1" >> "$SESSION_DIR/resume.txt"; }

# The pre-commit hook boot-checks every commit, however it's made. It's kept in
# hooks/ and (re)installed on every start, so a clone gets it and deleting it
# from .git/hooks doesn't stick.
install_hook() {
  [ -d .git/hooks ] || return 0
  cmp -s hooks/pre-commit .git/hooks/pre-commit 2>/dev/null ||
    { cp hooks/pre-commit .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit; }
}

# The last step down: if the image keeps crashing soon after start, the
# resumed conversation may be what crashes it, so set it aside (not deleted)
# and boot this session fresh.
quick_crashes=0
forget_session() {
  local saved="$SESSION_DIR/messages.json"
  [ -f "$saved" ] || return 0
  local kept="$SESSION_DIR/messages.crashed-$(date +%Y%m%d-%H%M%S).json"
  mv "$saved" "$kept"
  echo "[supervisor] $quick_crashes quick crashes in a row; starting the session fresh"
  resume "[supervisor] The image crashed $quick_crashes times in a row soon after starting, so this session's conversation was set aside ($kept) and you start fresh. Your files and notes are untouched."
}

boot_crashes=0
while true; do
  install_hook
  started=$(date +%s)
  sbcl --noinform --disable-debugger --load boot.lisp
  code=$?
  [ -t 0 ] && stty sane 2>/dev/null   # a UI may have left the terminal in raw mode
  case $code in
    0) exit 0 ;;
    42) boot_crashes=0; quick_crashes=0; switch_session; continue ;;
  esac

  if (( $(date +%s) - started < QUICK )); then
    quick_crashes=$((quick_crashes + 1))
    if (( quick_crashes >= MAX_QUICK_CRASHES )); then forget_session; quick_crashes=0; fi
  else
    quick_crashes=0
  fi

  if (( $(date +%s) - started >= BOOT_GRACE )); then
    echo "[supervisor] image crashed (exit $code), restarting"
    resume "[supervisor] The image crashed with exit code $code and was restarted."
    boot_crashes=0
    continue
  fi

  boot_crashes=$((boot_crashes + 1))
  if (( boot_crashes > MAX_BOOT_CRASHES )); then
    echo "[supervisor] image keeps crashing on boot even after rollback, giving up"
    exit 1
  fi
  if git rev-parse -q --verify last-good >/dev/null &&
     ! git diff --quiet last-good -- . ':(exclude)state'; then
    echo "[supervisor] crashed on boot (exit $code), rolling files back to last-good"
    git restore --source=last-good --staged --worktree -- . ':(exclude)state' &&
      git commit -q -m "supervisor: roll back files after boot crash (exit $code)"
    resume "[supervisor] The image crashed while booting (exit $code), so your files were rolled back to the last commit that booted. See git log for what was undone."
  else
    echo "[supervisor] crashed on boot (exit $code), retrying"
  fi
  sleep 1
done
