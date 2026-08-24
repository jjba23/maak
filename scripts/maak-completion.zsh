#!/usr/bin/env zsh
#compdef maak

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
    local -a specs
    local -A opt_args

    specs=(
        '(-f --file)'{-f,--file}'[Specify path to the Maak file]:Maak file:_files'
        '(-h --help)'{-h,--help}'[Display help screen and exit]'
        '(-l --list)'{-l,--list}'[List available tasks in the Maak file]'
        '(-n --dry-run)'{-n,--dry-run}'[Dry-run mode (print shell commands without executing)]'
        '(-q --quiet)'{-q,--quiet}'[Quiet mode (suppress logging output)]'
        '*:task:_maak_tasks'
    )

    _arguments -s -S $specs
}

_maak_tasks() {
    # Check -f/--file option first, otherwise walk up directories for maak.scm
    local maak_file="${opt_args[-f]:-${opt_args[--file]}}"

    if [[ -z "$maak_file" ]]; then
        local dir="$PWD"
        while [[ -n "$dir" ]]; do
            if [[ -f "$dir/maak.scm" ]]; then
                maak_file="$dir/maak.scm"
                break
            fi
            [[ "$dir" == "/" ]] && break
            dir="${dir:h}"
        done
    fi

    [[ -f "$maak_file" ]] || return 1

    # Extract tasks & descriptions adhering strictly to maak.el visibility rules
    local -a tasks
    tasks=(${(f)"$(awk '
        BEGIN { has_exports = 0; in_export = 0 }
        {
            # Strip comments
            sub(/;.*/, "")

            # Detect #:export (...) block
            if (/#:export/) { in_export = 1 }
            if (in_export) {
                line = $0
                sub(/.*#:export/, "", line)
                while (match(line, /[a-zA-Z0-9_+*\/<>=!?-]+/)) {
                    sym = substr(line, RSTART, RLENGTH)
                    exports[sym] = 1
                    has_exports = 1
                    line = substr(line, RSTART + RLENGTH)
                }
                if (index($0, ")")) { in_export = 0 }
            }

            # Match procedure definitions: (define (task-name arg1 ...)
            if (match($0, /^[ \t]*\([ \t]*define[ \t]+\([ \t]*[a-zA-Z0-9_+*\/<>=!?-]+/)) {
                str = $0
                sub(/^[ \t]*\([ \t]*define[ \t]+\([ \t]*/, "", str)
                match(str, /^[a-zA-Z0-9_+*\/<>=!?-]+/)
                task = substr(str, RSTART, RLENGTH)

                if (task != "") {
                    if (!(task in seen)) {
                        seen[task] = 1
                        defined_order[++count] = task

                        # Extract signature arguments
                        sig = str
                        sub(/\).*/, "", sig)
                        sub(/^[a-zA-Z0-9_+*\/<>=!?-]+[ \t]*/, "", sig)
                        gsub(/[ \t]+/, " ", sig)
                        gsub(/^ | $/, "", sig)
                        task_args[task] = sig
                    }
                    last_task = task
                }
            }

            # Capture docstrings if defined directly after function header
            if (last_task != "" && match($0, /^[ \t]*"([^"]+)"/, m)) {
                doc = m[1]
                gsub(/:/, "-", doc) # Avoid breaking Zsh description delimiter
                task_doc[last_task] = doc
                last_task = ""
            } else if (last_task != "" && $0 !~ /^[ \t]*$/) {
                last_task = ""
            }
        }
        END {
            for (i = 1; i <= count; i++) {
                t = defined_order[i]
                # Enforce Open Fallback vs Strict Lock rule
                if (!has_exports || (t in exports)) {
                    desc = task_doc[t]
                    args = task_args[t]
                    if (desc != "" && args != "") {
                        info = "<" args "> " desc
                    } else if (desc != "") {
                        info = desc
                    } else if (args != "") {
                        info = "<" args ">"
                    } else {
                        info = "Maak task"
                    }
                    # Format as task:description for Zsh _describe
                    print t ":" info
                }
            }
        }' "$maak_file")"})

    # Complete comma-separated task chains (e.g., maak task1,task2)
    if (( ${#tasks} )); then
        _sequence -s , _describe -t tasks 'maak task' tasks
    fi
}

_maak "$@"
