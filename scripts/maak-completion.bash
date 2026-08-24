#!/usr/bin/env bash

# Copyright © Josep Bigorra <jjbigorra@gmail.com>

# maak is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# maak is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with maak. If not, see <https://www.gnu.org/licenses/>.

_maak() {
  local cur prev words cword
  _get_comp_words_by_ref -n : cur prev words cword 2>/dev/null || {
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD - 1]}"
    words=("${COMP_WORDS[@]}")
    cword=$COMP_CWORD
  }

  local opts="-f --file -h --help -l --list -n --dry-run -q --quiet"

  # Complete options if the current token starts with a hyphen
  if [[ "$cur" == -* ]]; then
    mapfile -t COMPREPLY < <(compgen -W "$opts" -- "$cur")
    return 0
  fi

  # Complete files if the previous token was -f or --file
  if [[ "$prev" == "-f" || "$prev" == "--file" ]]; then
    mapfile -t COMPREPLY < <(compgen -f -- "$cur")
    return 0
  fi

  # Locate the maak file: check CLI args first, then search parent directories
  local maak_file=""
  local i
  for ((i = 1; i < ${#words[@]} - 1; i++)); do
    if [[ "${words[i]}" == "-f" || "${words[i]}" == "--file" ]]; then
      maak_file="${words[i + 1]}"
      break
    fi
  done

  if [[ -z "$maak_file" ]]; then
    local dir="$PWD"
    while [[ -n "$dir" ]]; do
      if [[ -f "$dir/maak.scm" ]]; then
        maak_file="$dir/maak.scm"
        break
      fi
      [[ "$dir" == "/" ]] && break
      dir="${dir%/*}"
      [[ -z "$dir" ]] && dir="/"
    done
  fi

  [[ -f "$maak_file" ]] || return 0

  # Extract tasks adhering strictly to maak visibility and export rules
  local tasks
  tasks=$(awk '
        BEGIN { has_exports = 0; in_export = 0 }
        {
            # Strip comments
            sub(/;.*/, "")

            # Detect #:export section
            if (/#:export/) { in_export = 1 }

            if (in_export) {
                line = $0
                sub(/.*#:export/, "", line)
                while (match(line, /[-a-zA-Z0-9_+=!*?\/<>]+/)) {
                    sym = substr(line, RSTART, RLENGTH)
                    exports[sym] = 1
                    has_exports = 1
                    line = substr(line, RSTART + RLENGTH)
                }
                if (index($0, ")")) { in_export = 0 }
            }

            # Match procedure definitions: (define (task-name ...
            if (match($0, /^[ \t]*\([ \t]*define[ \t]+\([ \t]*[-a-zA-Z0-9_+=!*?\/<>]+/)) {
                str = $0
                sub(/^[ \t]*\([ \t]*define[ \t]+\([ \t]*/, "", str)
                match(str, /^[-a-zA-Z0-9_+=!*?\/<>]+/)
                task = substr(str, RSTART, RLENGTH)
                if (task != "" && !(task in seen)) {
                    seen[task] = 1
                    defined_order[++count] = task
                }
            }
        }
        END {
            for (i = 1; i <= count; i++) {
                t = defined_order[i]
                # Open Fallback (no exports/empty export) or Strict Lock (must be in exports)
                if (!has_exports || (t in exports)) {
                    print t
                }
            }
        }' "$maak_file")

  # Handle comma-separated multi-task completion (e.g. maak task1,tas<TAB>)
  local prefix=""
  local search_term="$cur"
  if [[ "$cur" == *","* ]]; then
    prefix="${cur%*,},"
    search_term="${cur##*,}"
  fi

  mapfile -t COMPREPLY < <(compgen -P "$prefix" -W "$tasks" -- "$search_term")
  return 0
}

complete -F _maak maak
