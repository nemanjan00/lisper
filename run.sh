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

trap 'exit 0' INT

resume() { mkdir -p state && printf '%s\n' "$1" >> state/resume.txt; }

boot_crashes=0
while true; do
  started=$(date +%s)
  sbcl --noinform --disable-debugger --load boot.lisp
  code=$?
  [ -t 0 ] && stty sane 2>/dev/null   # a UI may have left the terminal in raw mode
  case $code in
    0) exit 0 ;;
    42) boot_crashes=0; continue ;;
  esac

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
