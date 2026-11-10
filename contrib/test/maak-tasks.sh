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

set -euo pipefail

helper="$(cd "$(dirname "$0")/../bin" && pwd)/maak-tasks"
fixture="$(cd "$(dirname "$0")/fixture" && pwd)/maak.scm"

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

# The last definition wins, and its line is where the definition starts.
check "the last definition is the task" "$json" '"name":"build","doc":"Compile everything.","line":38'
check_not "the shadowed docstring is gone" "$json" 'Compile the project\.'

# Every kind of argument is read the way maak binds it.
check "a required argument" "$json" '"name":"target","kind":"required","default":"","hasDefault":false'
check "an optional argument keeps its default" "$json" '"name":"env","kind":"optional","default":"\\"dev\\"","hasDefault":true'
check "a keyword argument keeps its keyword" "$json" '"name":"jobs","kind":"key","default":"1","hasDefault":true,"keyword":"#:jobs"'
check "a #:rest argument" "$json" '"name":"extra","kind":"rest"'
check "a dotted tail is a rest argument" "$json" '"name":"words","kind":"rest"'
check "a lambda's argument" "$json" '"name":"day","kind":"required"'
check "a multi-line docstring is one string" "$json" 'Two lines, so the preview indents both\.'

# The other modes, which keep a shell integration out of JSON.
check "preview names the command" "$preview" 'deploy target \[env="dev"\]'
check "preview names the keyword it cannot pass" "$preview" 'jobs keyword -- default: 1'
check "words prints the promptable arguments, in order" "$words" '^required	target		0$'
check "words prints an optional argument's default" "$words" '^optional	env	"dev"	1$'
check "words marks the keyword a picker must not ask for" "$words" '^key	jobs	1	1$'
check "fzf prints name, signature and summary" \
  "$("$helper" --file "$fixture" --fzf | grep '^report')" '^report	day	Print the report for DAY\.$'
check "zed labels a task with its docstring" \
  "$("$helper" --file "$fixture" --zed)" '"label":"maak: report — Print the report for DAY\."'
check "zed runs the task in the worktree root" \
  "$("$helper" --file "$fixture" --zed)" "\"cwd\":\"\\\$ZED_WORKTREE_ROOT\""

# The file is found upward from a directory, which is how an editor asks.
found="$(cd "$(dirname "$fixture")" && "$helper" --names)"
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

printf 'ok: %d checks passed\n' "$pass"
