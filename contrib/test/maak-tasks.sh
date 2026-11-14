#!/usr/bin/env bash
# Test the helper the editor integrations read their tasks from.
#
# SPDX-License-Identifier: MIT
#
# Usage: contrib/test/maak-tasks.sh
#
# The assertions are grep, so the test needs nothing but bash and the helper
# itself.  A JSON parser would read better, but this must run on the machine of
# whoever is hacking on an integration, not only where the project's shell is.
#
# The last group asks the question the reader exists to answer: does the helper
# describe a file the way maak itself does?  Both readings are printed for the
# same file and compared byte for byte, so a shape one reader understands and the
# other does not fails here rather than in an editor.

set -euo pipefail

# `cd' prints where it landed when it consults CDPATH, so its own output is
# dropped here: what is wanted is the path the subshell ends up in, not a
# transcript of getting there.
into() {
  (cd -- "$1" >/dev/null 2>&1 && pwd)
}

helper="$(into "$(dirname "$0")/../bin")/maak-tasks"
fixture="$(into "$(dirname "$0")/fixture")/maak.scm"
launcher="$(into "$(dirname "$0")/../..")/scripts/maak"
project="$(into "$(dirname "$0")/../..")"

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

json="$("$helper" --file "$fixture")"
names="$("$helper" --file "$fixture" --names)"
preview="$("$helper" --file "$fixture" --preview deploy)"
words="$("$helper" --file "$fixture" --words deploy)"

# Exports are the tasks; helpers, values and unexported procedures are not.
check "the exported tasks are listed" "$names" '^build$'
check "a define* task is listed" "$names" '^deploy$'
check "a lambda task is listed" "$names" '^report$'
check "an exported rest task is listed" "$names" '^echo$'
check_not "an unexported procedure is not a task" "$names" 'private-helper'
check_not "a value definition is not a task" "$names" 'build-directory'
check "a task is listed once" "$(printf '%s\n' "$names" | grep -c '^build$')" '^1$'

# A hidden task is exported and still not offered: it is work the other tasks do,
# not work a person picks.
check_not "a hidden task is left out of the names" "$names" '^tidy$'
check_not "a hidden task is left out of the JSON" "$json" '"name":"tidy"'
check "a hidden task can still be previewed" \
  "$("$helper" --file "$fixture" --preview tidy)" '^tidy'

# The last definition wins, and its line is where the definition starts.
check "the last definition is the task" "$json" '"name":"build","doc":"Compile everything.","line":55'
check_not "the shadowed docstring is gone" "$json" 'Compile the project\.'

# Every kind of argument is read the way maak binds it, with the prose the
# docstring's `Args:' section gives it.
check "a required argument" "$json" '"name":"target","kind":"required","default":"","hasDefault":false'
check "an optional argument keeps its default" "$json" '"name":"env","kind":"optional","default":"\\"dev\\"","hasDefault":true'
check "a keyword argument keeps its keyword" "$json" '"name":"jobs","kind":"key","default":"1","hasDefault":true,"keyword":"#:jobs"'
check "a #:rest argument" "$json" '"name":"extra","kind":"rest"'
check "a dotted tail is a rest argument" "$json" '"name":"words","kind":"rest"'
check "a lambda's argument" "$json" '"name":"day","kind":"required"'
check "an argument carries its prose" "$json" 'the host to ship to, as HOST\[:PORT\]\.'
check "a multi-line docstring is one string" "$json" 'Two lines, so the preview indents both\.'

# What a task declares about itself.
check "a task keeps what it needs first" "$json" '"depends":\["build"\]'
check "a task keeps an example invocation" "$json" '"example":"maak deploy prod.example.com"'
check "a task with no metadata declares nothing" "$json" '"name":"echo","doc":"Echo every word\.","line":[0-9]+,"depends":\[\],"example":""'

# The other modes, which keep a shell integration out of JSON.
check "preview names the command" "$preview" 'deploy target \[env="dev"\]'
check "preview names the keyword it cannot pass" "$preview" 'jobs keyword -- default: 1'
check "preview says what to type for an argument" "$preview" 'the host to ship to'
check "preview says what a task needs first" "$preview" 'needs: build'
check "preview says how to invoke it" "$preview" 'example: maak deploy prod.example.com'
check "preview leaves the argument section out of the documentation" \
  "$(printf '%s\n' "$preview" | grep -c '^  Args:$')" '^0$'
check "words prints the promptable arguments, in order" "$words" '^required	target		0$'
check "words prints an optional argument's default" "$words" '^optional	env	"dev"	1$'
check "words marks the keyword a picker must not ask for" "$words" '^key	jobs	1	1$'
check "fzf prints name, signature and summary" \
  "$("$helper" --file "$fixture" --fzf | grep '^report')" '^report	day	Print the report for DAY\.$'
check "zed labels a task with its docstring" \
  "$("$helper" --file "$fixture" --zed)" '"label":"maak: report — Print the report for DAY\."'
check "zed runs the task in the worktree root" \
  "$("$helper" --file "$fixture" --zed)" "\"cwd\":\"\\\$ZED_WORKTREE_ROOT\""
check_not "zed leaves a hidden task out" "$("$helper" --file "$fixture" --zed)" 'maak: tidy'

# The file is found upward from a directory, which is how an editor asks.
found="$( (cd -- "$(dirname "$fixture")" >/dev/null 2>&1 && "$helper" --names) )"
check "the nearest maak.scm is found upward" "$found" '^deploy$'

# A file that cannot be read to the end is refused, not half listed: an editor
# would otherwise show a list that is quietly short, and the Zed writer would
# overwrite a good task file with it.
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
printf '(define (build "unterminated\n' >"$work/broken.scm"
status=0
output=$("$helper" --file "$work/broken.scm" 2>&1) || status=$?
check "a file that does not read is refused" "$status" '^2$'
check "the refusal names the file" "$output" 'broken\.scm does not read to the end'

# ====== The two readers agree ======
#
# `maak --list --json' asks the loaded module what its tasks are and completes
# that with the file's lambda lists; the helper asks only the file.  The bytes
# below are the same, or one of the two is lying about a task.
if [ -x "$launcher" ] && command -v guile >/dev/null 2>&1; then
  for file in "$fixture" "$project/maak.scm"; do
    if [ ! -f "$file" ]; then
      continue
    fi
    by_maak=$("$launcher" --list --json -f "$file" 2>/dev/null)
    by_helper=$("$helper" --file "$file" 2>/dev/null)
    if [ "$by_maak" = "$by_helper" ]; then
      pass=$((pass + 1))
    else
      printf 'FAIL: maak and the helper describe %s differently\n' "$file" >&2
      printf '  maak:   %s\n' "$by_maak" >&2
      printf '  helper: %s\n' "$by_helper" >&2
      exit 1
    fi
  done
  check "the project's own Maak file is described" \
    "$("$helper" --file "$project/maak.scm" --names)" '^agents$'
else
  printf 'skipped: the agreement checks need the launcher and guile on PATH\n' >&2
fi

printf 'ok: %d checks passed\n' "$pass"
