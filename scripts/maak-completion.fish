#!/usr/bin/env fish

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

# Clear previous completions for maak
complete -c maak -e

# Disable default file completion unless -f/--file is given
complete -c maak -f

# Define CLI options
complete -c maak -s f -l file -r -F -d "Specify path to the Maak file"
complete -c maak -s h -l help -d "Display help screen and exit"
complete -c maak -s l -l list -d "List available tasks in the Maak file"
complete -c maak -s n -l dry-run -d "Dry-run mode (print shell commands without executing)"
complete -c maak -s q -l quiet -d "Quiet mode (suppress logging output)"

# Dynamic task completion function
function __fish_maak_targets
    # Parse command line buffer to check for -f/--file
    set -l cmdline (commandline -opc)
    set -l maak_file ""

    set -l i 1
    set -l len (count $cmdline)
    while test $i -lt $len
        set -l arg $cmdline[$i]
        if test "$arg" = "-f" -o "$arg" = "--file"
            set -l next_idx (math $i + 1)
            set maak_file $cmdline[$next_idx]
            break
        end
        set i (math $i + 1)
    end

    # Walk up directory tree to locate dominating maak.scm if -f is absent
    if test -z "$maak_file"
        set -l dir (pwd)
        while test -n "$dir" -a "$dir" != "/"
            if test -f "$dir/maak.scm"
                set maak_file "$dir/maak.scm"
                break
            end
            set dir (string replace -r '/[^/]*$' '' $dir)
            if test -z "$dir"
                set dir "/"
            end
        end
        if test -z "$maak_file" -a -f "/maak.scm"
            set maak_file "/maak.scm"
        end
    end

    test -f "$maak_file"; or return 0

    # Extract tasks & descriptions adhering strictly to maak.el visibility rules
    set -l tasks (awk '
        BEGIN { has_exports = 0; in_export = 0 }
        {
            sub(/;.*/, "")

            # Detect #:export (...) block
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

            # Match procedure definitions: (define (task-name arg1 ...)
            if (match($0, /^[ \t]*\([ \t]*define[ \t]+\([ \t]*[-a-zA-Z0-9_+=!*?\/<>]+/)) {
                str = $0
                sub(/^[ \t]*\([ \t]*define[ \t]+\([ \t]*/, "", str)
                match(str, /^[-a-zA-Z0-9_+=!*?\/<>]+/)
                task = substr(str, RSTART, RLENGTH)

                if (task != "") {
                    if (!(task in seen)) {
                        seen[task] = 1
                        defined_order[++count] = task

                        # Extract signature arguments
                        sig = str
                        sub(/\).*/, "", sig)
                        sub(/^[-a-zA-Z0-9_+=!*?\/<>]+[ \t]*/, "", sig)
                        gsub(/[ \t]+/, " ", sig)
                        gsub(/^ | $/, "", sig)
                        task_args[task] = sig
                    }
                    last_task = task
                }
            }

            # Capture docstring string literal
            if (last_task != "" && match($0, /^[ \t]*"([^"]+)"/, m)) {
                doc = m[1]
                gsub(/\t/, " ", doc)
                task_doc[last_task] = doc
                last_task = ""
            } else if (last_task != "" && $0 !~ /^[ \t]*$/) {
                last_task = ""
            }
        }
        END {
            for (i = 1; i <= count; i++) {
                t = defined_order[i]
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
                    # Fish menu format: candidate <TAB> description
                    print t "\t" info
                }
            }
        }' "$maak_file")

    # Handle comma-separated multi-task completion (e.g. maak fmt,quick<TAB>)
    set -l token (commandline -ct)
    if string match -q "*,*" -- "$token"
        set -l prefix (string replace -r '[^,]*$' '' -- "$token")
        for item in $tasks
            echo "$prefix$item"
        end
    else
        for item in $tasks
            echo "$item"
        end
    end
end

# Bind task completion generator
complete -c maak -a "(__fish_maak_targets)"