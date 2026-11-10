#!/usr/bin/env bash
# Test the script that keeps a Zed tasks file in step with a Maak file.
#
# SPDX-License-Identifier: MIT
#
# Usage: contrib/test/maak-zed.sh
#
# Needs bash and coreutils (cksum, cmp, stat); python3 is used, when it exists,
# to check that what was written is JSON.  Every project it writes lives in a
# temporary directory.

set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
script="$here/../zed/maak-zed"
helper="$here/../bin/maak-tasks"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

export MAAK_TASKS="$helper"

pass=0

check() {
  local description="$1" text="$2" pattern="$3"
  if printf '%s' "$text" | grep -qE -- "$pattern"; then
    pass=$((pass + 1))
  else
    printf 'FAIL: %s\n  wanted: %s\n  in: %s\n' "$description" "$pattern" "$text" >&2
    exit 1
  fi
}

check_not() {
  local description="$1" text="$2" pattern="$3"
  if printf '%s' "$text" | grep -qE -- "$pattern"; then
    printf 'FAIL: %s\n  did not want: %s\n  in: %s\n' "$description" "$pattern" "$text" >&2
    exit 1
  else
    pass=$((pass + 1))
  fi
}

check_status() {
  local description="$1" wanted="$2" actual="$3"
  if [ "$wanted" = "$actual" ]; then
    pass=$((pass + 1))
  else
    printf 'FAIL: %s\n  wanted status: %s\n  got: %s\n' \
      "$description" "$wanted" "$actual" >&2
    exit 1
  fi
}

project=$work/project
mkdir -p "$project/sub"
tasks=$project/.zed/tasks.json

# One task, so the first write has something to write.
cat >"$project/maak.scm" <<'EOF'
(define-module (tasks)
  #:export (build))

(define (build)
  "Compile everything."
  (display "built\n"))
EOF

# Two tasks, so a second write has something new.
cat >"$work/two-tasks.scm" <<'EOF'
(define-module (tasks)
  #:export (build deploy))

(define (build)
  "Compile everything."
  (display "built\n"))

(define (deploy)
  "Ship the build."
  (display "shipped\n"))
EOF

mtime() { stat -c %y -- "$1"; }

# ---- sync writes the file where the Maak file is ---------------------------

output=$(cd "$project" && "$script" sync)
check "sync says it wrote the file" "$output" 'wrote .*\.zed/tasks\.json \(1 tasks?\)'
check "the file is where Zed looks" "$(test -f "$tasks" && echo present)" '^present$'
check "the task is labelled with its docstring" \
  "$(cat "$tasks")" '"label":"maak: build — Compile everything\."'
check "the label runs the task in the worktree root" \
  "$(cat "$tasks")" "\"cwd\":\"\\\$ZED_WORKTREE_ROOT\""
check "the generated list carries the watch task" \
  "$(cat "$tasks")" '"label":"maak: watch tasks"'

if command -v python3 >/dev/null 2>&1; then
  check "the file is JSON" "$(python3 -m json.tool "$tasks" >/dev/null 2>&1 && echo valid)" '^valid$'
fi

# ---- a file that is already right is left alone ---------------------------

before=$(mtime "$tasks")
output=$(cd "$project" && "$script" sync)
check "a second sync says nothing was written" "$output" 'is up to date'
check "an up-to-date file keeps its mtime" "$(mtime "$tasks")" "^$(printf '%s' "$before" | sed 's/[][\\.^$*+?(){}|]/\\&/g')\$"

# ---- a changed Maak file is picked up ------------------------------------

cp "$work/two-tasks.scm" "$project/maak.scm"
output=$(cd "$project/sub" && "$script" sync)
check "sync runs from a subdirectory" "$output" 'wrote '
check_not "nothing is written next to the subdirectory" \
  "$(ls -A "$project/sub")" '\.zed'
check "the new task is there" "$(cat "$tasks")" '"label":"maak: deploy — Ship the build\."'
check "the mtime moved with the content" \
  "$(test "$(mtime "$tasks")" != "$before" && echo moved)" '^moved$'
check "the file is named with the number of tasks" "$output" '\(2 tasks\)'

# ---- --file names a Maak file, and bad input writes nothing ---------------

output=$(cd "$work" && "$script" sync --file "$project/maak.scm")
check "--file syncs the file it names" "$output" 'is up to date'

printf '(define (build "unterminated\n' >"$project/maak.scm"
status=0
output=$(cd "$project" && "$script" sync 2>&1) || status=$?
check_status "a Maak file that does not read fails" 1 "$status"
check "the failure says the task file was left alone" "$output" 'left alone'
check "the old task file survives the failed run" "$(cat "$tasks")" '"label":"maak: deploy'

# ---- nothing to sync, and nothing to do ----------------------------------

mkdir -p "$work/empty"
status=0
output=$(cd "$work/empty" && "$script" sync 2>&1) || status=$?
check_status "no maak.scm anywhere fails" 2 "$status"
check "the failure names maak.scm" "$output" 'no maak\.scm here or above'

status=0
output=$("$script" 2>&1) || status=$?
check_status "a missing command fails" 2 "$status"
check "the failure names the commands" "$output" 'sync or watch'

# ---- watch rewrites the file when the Maak file changes -------------------

cp "$work/two-tasks.scm" "$project/maak.scm"
log=$work/watch.log
(cd "$project" && exec "$script" watch --interval 0.2 >"$log" 2>&1) &
watcher=$!

for _ in $(seq 1 50); do
  [ -f "$tasks" ] && break
  sleep 0.1
done
check "watch writes before it waits" "$(test -f "$tasks" && echo written)" '^written$'
check "watch starts from the current tasks" "$(cat "$tasks")" '"label":"maak: deploy'

# A task the first write could not have known about.
cat >"$project/maak.scm" <<'EOF'
(define-module (tasks)
  #:export (build deploy release))

(define (build) "Compile everything." (display "built\n"))
(define (deploy) "Ship the build." (display "shipped\n"))
(define (release) "Tag the release." (display "tagged\n"))
EOF

seen=no
for _ in $(seq 1 50); do
  if grep -q 'maak: release' "$tasks" 2>/dev/null; then
    seen=yes
    break
  fi
  sleep 0.1
done
check "watch picks up the new task" "$seen" '^yes$'

rm -rf -- "$project/.zed"
seen=no
for _ in $(seq 1 50); do
  if [ -f "$tasks" ]; then
    seen=yes
    break
  fi
  sleep 0.1
done
check "watch puts a deleted task file back" "$seen" '^yes$'

kill -TERM "$watcher"
status=0
wait "$watcher" || status=$?
check_status "watch stops cleanly on SIGTERM" 0 "$status"
check "watch names what it watches" "$(cat "$log")" 'watching .*maak\.scm'
check "watch says it stopped" "$(cat "$log")" 'stopped'

printf 'ok: %d checks passed\n' "$pass"
