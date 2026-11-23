;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The shell completions, generated from one description.
;;
;; A shell completion knows two things: which options the program takes, and
;; which words the project's Maak file answers with.  The first is written once,
;; here, and rendered for each shell; the second is not known until a Maak file
;; is read, so it is asked of maak itself (`maak --list=names' or
;; `maak --list=completion') rather than parsed out of the file a second time.
;;
;; That second half is the point of the rewrite.  The scripts used to find task
;; names with an awk program over maak.scm, which had to guess at `#:export',
;; macros, task files in JSON and TOML, and the tasks a used module brings in --
;; and guessed wrong about every one of them.  maak already answers the question
;; a completion is asking, so the scripts run it and read the answer.
;;
;; The table below is what a shell script cannot have: the value each option
;; takes, so `-f <TAB>' offers a file and `--list= <TAB>' offers a layout.  A
;; spec in `test/veritas/unit/completion.scm' keeps it in step with
;; `maak-option-spec', which is the option grammar the rest of maak obeys.

;;; Code:

(define-module (maak completion)
  ;; Declarative, so the generated text is the same in every build: nothing here
  ;; reads the environment or the clock.
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (completion-shells completion-options completion-script
            completion-option-words))

;; The shells a script is generated for, in the order the help screen names them.
(define completion-shells '(bash zsh fish))

;; The options, in the order `maak --help' prints them: the long name, the short
;; name (#f when there is none), what the value is, and the line a completion
;; menu shows beside it.
;;
;; The kind is the whole reason this table exists rather than the option grammar
;; being read out of `(maak main)': a shell has to know that `-f' wants a file,
;; that `-C' wants a directory, that `--list=' wants one of the layouts and that
;; `-j' wants a number.  `completion-option-kinds' below maps each kind to what
;; each shell does with it.
;;
;; A description must carry no `:', `[' or `]': zsh reads its menu text inside
;; brackets and splits a candidate from its description on a colon, and the
;; description arrives through all three generators unchanged.
(define completion-options
  '(("--file" "-f" file "Specify the path to the Maak file")
    ("--directory" "-C" directory "Change to a directory before reading the Maak file")
    ("--env-file" "-e" file "Read NAME=VALUE assignments from a file")
    ("--no-env-file" #f flag "Read no environment file at all")
    ("--banner" #f flag "Print the greeting banner")
    ("--no-banner" #f flag "Skip the greeting banner")
    ("--help" "-h" flag "Display the help screen and exit")
    ("--ignore-errors" "-i" flag "Keep going after a failure, and still succeed")
    ("--jobs" "-j" number "Run up to this many tasks at the same time")
    ("--keep-going" "-k" flag "Keep going after a failure")
    ("--list" "-l" layout "List the tasks of the Maak file")
    ("--list-width" #f number "Fold a listing to this many columns")
    ("--json" #f flag "Print the listing or the plan as JSON")
    ("--plan" #f flag "Print the plan a run would execute, without running it")
    ("--watch" #f flag "Re-run the named tasks whenever the tree changes")
    ("--explain" #f flag "Explain a task, what it calls and which environment file wins")
    ("--completion" #f shell "Print the completion script for a shell")
    ("--tasks" "-t" tasks "Run the tasks named in a comma separated list")
    ("--load-path" "-L" directory "Add a directory to the Guile load path")
    ("--dry-run" "-n" flag "Print the commands a run would execute, without running them")
    ("--no-print-directory" #f flag "Do not report the directories -C enters")
    ("--quiet" "-q" flag "Suppress logging output")
    ("--version" "-v" flag "Print the version and exit")
    ("--debug" #f flag "Print the Guile backtrace on an error")))

;; The layouts `--list=STYLE' takes, beside the name in the table above.
(define completion-layouts '(table plain long names json completion))

;; ====== What each shell is told ======

;; The command a completion runs to learn the project's tasks.  `completion' is
;; the layout a menu reads: one task a line, the name, a tab, and the first line
;; of the task's docstring.  `names' is the same thing without the menu text,
;; which is all bash has room for.
(define (task-command style)
  (format #f "maak --list=~a" style))

(define (union-words words)
  "Return WORDS as one space separated string, for a shell that splits on it."
  (string-join (map (lambda (word)
                      (if (symbol? word) (symbol->string word) word))
                    words)
               " "))

(define (completion-option-words)
  "Return every option word the completions offer, short before long."
  (append-map (lambda (option)
                (match option
                  ((long short kind description)
                   (if short (list short long) (list long)))))
              completion-options))

;; ====== Bash ======
;;
;; bash offers no descriptions in its menu, so the tasks are asked for by name.
;; The one thing bash needs beyond the words is which option wants a value that
;; is not a word at all -- a path, a directory, a number -- because it would
;; otherwise fall back to completing a file after `-j'.

(define (bash-lines)
  "Return the bash completion script as a list of lines."
  (append
   (list
    "# bash completion for maak."
    "#"
    "# Generated by `maak --completion bash'.  Edit src/maak/completion.scm and run"
    "# `maak completions'; this file is a copy of what that task writes."
    ""
    (format #f "_maak_styles=(~a)" (union-words completion-layouts))
    (format #f "_maak_shells=(~a)" (union-words completion-shells))
    (format #f "_maak_options=(~a)" (union-words (completion-option-words)))
    ""
    "# The tasks of the Maak file the line is about, one name per line."
    "#"
    "# maak answers this rather than this script reading the file: a task a macro"
    "# defines, one a used module brings in, and one a task file in JSON, TOML or"
    "# YAML describes are all tasks a run would find.  The -f/--file and"
    "# -C/--directory given before the cursor are handed over, so `maak -C sub'"
    "# offers the tasks of sub, which is what a run there would use."
    "#"
    "# shortcut: a value written as one word (-Csub, --file=x) is not read here;"
    "# upgrade when someone asks for it, since the two-word spelling is the rest."
    "_maak_tasks() {"
    "  local -a selection=()"
    "  local i"
    "  for ((i = 1; i < COMP_CWORD; i++)); do"
    "    case \"${COMP_WORDS[i]}\" in"
    "      -f | --file | -C | --directory)"
    "        if [[ -n \"${COMP_WORDS[i + 1]:-}\" ]]; then"
    "          selection+=(\"${COMP_WORDS[i]}\" \"${COMP_WORDS[i + 1]}\")"
    "          ((i++))"
    "        fi"
    "        ;;"
    "    esac"
    "  done"
    (format #f "  ~a \"${selection[@]}\" 2>/dev/null" (task-command 'names))
    "}"
    ""
    "# Whether a `--' before the cursor left the line with tasks alone."
    "_maak_after_separator() {"
    "  local i"
    "  for ((i = 1; i < COMP_CWORD; i++)); do"
    "    case \"${COMP_WORDS[i]}\" in"
    "      --) return 0 ;;"
    "      -*) ;;"
    "      *) return 1 ;;"
    "    esac"
    "  done"
    "  return 1"
    "}"
    ""
    "_maak() {"
    "  local cur=\"${COMP_WORDS[COMP_CWORD]}\""
    "  local prev=\"${COMP_WORDS[COMP_CWORD - 1]}\""
    "  local prefix=\"\" stem=\"${cur}\" name"
    "  COMPREPLY=()"
    ""
    "  # The layout of `--list=STYLE', after the `='."
    "  if [[ \"${cur}\" == --list=* ]]; then"
    "    mapfile -t COMPREPLY < <(compgen -P \"--list=\" -W \"${_maak_styles[*]}\" \\"
    "      -- \"${cur#--list=}\")"
    "    return 0"
    "  fi"
    ""
    "  case \"${prev}\" in"
    "    -C | --directory | -L | --load-path)"
    "      mapfile -t COMPREPLY < <(compgen -d -- \"${cur}\")"
    "      return 0"
    "      ;;"
    "    -f | --file | -e | --env-file)"
    "      mapfile -t COMPREPLY < <(compgen -f -- \"${cur}\")"
    "      return 0"
    "      ;;"
    "    --completion)"
    "      mapfile -t COMPREPLY < <(compgen -W \"${_maak_shells[*]}\" -- \"${cur}\")"
    "      return 0"
    "      ;;"
    "    -j | --jobs | --list-width)"
    "      # A number, which the reader knows better than the shell."
    "      return 0"
    "      ;;"
    "    -t | --tasks)"
    "      # A comma separated task list, completed below like any other task."
    "      ;;"
    "  esac"
    ""
    "  if [[ \"${cur}\" == -* ]] && ! _maak_after_separator; then"
    "    mapfile -t COMPREPLY < <(compgen -W \"${_maak_options[*]}\" -- \"${cur}\")"
    "    return 0"
    "  fi"
    ""
    "  # A comma continues a task list, as in `maak fmt,quick'."
    "  if [[ \"${cur}\" == *,* ]]; then"
    "    prefix=\"${cur%,*},\""
    "    stem=\"${cur##*,}\""
    "  fi"
    ""
    "  while IFS= read -r name; do"
    "    if [[ -n \"${name}\" && \"${name}\" == \"${stem}\"* ]]; then"
    "      COMPREPLY+=(\"${prefix}${name}\")"
    "    fi"
    "  done < <(_maak_tasks)"
    ""
    "  return 0"
    "}"
    ""
    "complete -F _maak maak")
   '()))

;; ====== Zsh ======
;;
;; zsh has a menu with descriptions and a description of its own for each
;; option, so both are written out: `_arguments' is told what each option's
;; value is, and the tasks arrive as `name:summary' words for `_describe'.

(define (zsh-value-spec kind)
  "Return the `_arguments' part that follows an option name, for KIND."
  (case kind
    ((flag) "")
    ((file) ":Maak file:_files")
    ((directory) ":directory:_directories")
    ((layout) (format #f ":layout:(~a)" (union-words completion-layouts)))
    ((shell) (format #f ":shell:(~a)" (union-words completion-shells)))
    ((tasks) ":task:_maak_tasks")
    ((number) ":N:")
    (else ":value:")))

(define (zsh-option-lines option)
  "Return the `_arguments' specs that describe OPTION, long and short."
  (match option
    ((long short kind description)
     (let* (;; The exclusion list keeps zsh from offering a long option and its
            ;; short twin on one line; an option with no short name needs none.
            (mutex (if short (format #f "(~a ~a)" short long) ""))
            ;; `--list' takes its value only after an `=', which `_arguments'
            ;; spells with a `-' following the `='.
            (long-word (if (eq? kind 'layout)
                           (format #f "~a=-" long)
                           long)))
       (append
        (list (format #f "'~a~a[~a]~a'" mutex long-word description
                      (zsh-value-spec kind)))
        (if short
            (list (format #f "'~a~a[~a]'" mutex short description))
            '()))))))

(define (zsh-lines)
  "Return the zsh completion script as a list of lines."
  (append
   (list
    "#compdef maak"
    "#"
    "# zsh completion for maak."
    "#"
    "# Generated by `maak --completion zsh'.  Edit src/maak/completion.scm and run"
    "# `maak completions'; this file is a copy of what that task writes."
    ""
    "_maak() {"
    "    # `_arguments' rewrites `words' while an action runs, so the line as it was"
    "    # typed is kept here for the task action below to read."
    "    local -a maak_line=(\"${words[@]}\")"
    "    local maak_position=${CURRENT}"
    "    _arguments -s -S \\")
   (map (lambda (line)
          (format #f "        ~a \\" line))
        (append-map zsh-option-lines completion-options))
   (list
    "        '*:task:_maak_tasks'"
    "}"
    ""
    "# The tasks of the Maak file the line is about, as `name:summary' words."
    "#"
    "# maak answers this rather than this script reading the file, so a task a"
    "# macro defines or a used module brings in is offered like any other, and the"
    "# -f/--file and -C/--directory given before the cursor decide which file is"
    "# read.  `_sequence' walks a comma separated task list one word at a time."
    "_maak_tasks() {"
    "    local -a entries"
    "    local -a selection"
    "    local i"
    "    for ((i = 2; i < maak_position; i++)); do"
    "        case \"${maak_line[i]}\" in"
    "            -f|--file|-C|--directory)"
    "                if (( i + 1 < maak_position )); then"
    "                    selection+=(\"${maak_line[i]}\" \"${maak_line[i + 1]}\")"
    "                    (( i++ ))"
    "                fi"
    "                ;;"
    "        esac"
    "    done"
    (format #f "    entries=(${(f)\"$(~a ${selection[@]} 2>/dev/null)\"})"
            (task-command 'completion))
    "    # `_describe' reads a candidate and its description apart on a colon."
    "    entries=(\"${(@)entries//$'\\t'/:}\")"
    "    (( ${#entries} )) || return 1"
    "    _sequence -s , _describe -t maak-tasks 'maak task' entries"
    "}"
    ""
    "# Autoloaded from fpath, zsh calls `_maak' itself; sourced by hand, the"
    "# binding is made here."
    "if [[ \"${funcstack[1]}\" == \"_maak\" ]]; then"
    "    _maak \"$@\""
    "else"
    "    compdef _maak maak 2>/dev/null || true"
    "fi")))

;; ====== Fish ======
;;
;; fish reads its own completion rules one option at a time, so the table is
;; written out as `complete' lines.  Its menu has room for the description, which
;; is why the tasks are asked for as `name<TAB>summary'.

(define (fish-quote text)
  "Return TEXT inside double quotes, which fish reads as one word."
  (format #f "\"~a\"" text))

(define (fish-option-lines option)
  "Return the `complete' lines that describe OPTION."
  (match option
    ((long short kind description)
     (let* ((names (string-append
                    (if short (format #f "-s ~a " (string-drop short 1)) "")
                    (format #f "-l ~a" (string-drop long 2))))
            (menu (fish-quote description)))
       (case kind
         ((flag)
          (list (format #f "complete -c maak ~a -d ~a" names menu)))
         ((file)
          (list (format #f "complete -c maak ~a -r -F -d ~a" names menu)))
         ((directory)
          (list (format #f "complete -c maak ~a -r -a \"(__fish_complete_directories)\" -d ~a"
                        names menu)))
         ((layout)
          ;; `--list' on its own is a flag, and its layout is written after an
          ;; `='; fish has no optional argument, so the menu of layouts is
          ;; offered only while the word being typed already carries the `='.
          (append
           (list (format #f "complete -c maak ~a -f -d ~a" names menu))
           (list (format #f "complete -c maak ~a -x -a ~a -n \"string match -q -- '~a=*' (commandline -ct)\" -d ~a"
                         names
                         (fish-quote (union-words completion-layouts))
                         long
                         (fish-quote "Lay the listing out this way")))))
         ((shell)
          (list (format #f "complete -c maak ~a -x -a ~a -d ~a"
                        names
                        (fish-quote (union-words completion-shells))
                        menu)))
         ((tasks)
          (list (format #f "complete -c maak ~a -x -a \"(__fish_maak_targets)\" -d ~a"
                        names menu)))
         (else
          (list (format #f "complete -c maak ~a -x -d ~a" names menu))))))))

(define (fish-lines)
  "Return the fish completion script as a list of lines."
  (append
   (list
    "# fish completion for maak."
    "#"
    "# Generated by `maak --completion fish'.  Edit src/maak/completion.scm and run"
    "# `maak completions'; this file is a copy of what that task writes."
    ""
    "# Start from nothing, and offer no file name where maak expects a task."
    "complete -c maak -e"
    "complete -c maak -f"
    "")
   (append-map fish-option-lines completion-options)
   (list
    ""
    "# The tasks of the Maak file the line is about, as candidates with the first"
    "# line of the task's docstring beside them."
    "#"
    "# maak answers this rather than this script reading the file, so a task a"
    "# macro defines or a used module brings in is offered like any other, and the"
    "# -f/--file and -C/--directory given before the cursor decide which file is"
    "# read."
    "function __fish_maak_tasks"
    "    set -l words (commandline -opc)"
    "    set -l selection"
    "    for i in (seq (count $words))"
    "        switch $words[$i]"
    "            case -f --file -C --directory"
    "                if test (count $words) -gt $i"
    "                    set -a selection $words[$i] $words[(math $i + 1)]"
    "                end"
    "        end"
    "    end"
    (format #f "    ~a $selection 2>/dev/null" (task-command 'completion))
    "end"
    ""
    "# The same candidates, carrying on a comma separated task list, as in"
    "# `maak fmt,quick'."
    "function __fish_maak_targets"
    "    set -l token (commandline -ct)"
    "    set -l prefix \"\""
    "    set -l stem $token"
    "    if string match -q \"*,*\" -- $token"
    "        set prefix (string replace -r '[^,]*$' '' -- $token)"
    "        set stem (string replace -r '^.*,' '' -- $token)"
    "    end"
    "    for line in (__fish_maak_tasks)"
    "        set -l name (string replace -r '\\t.*$' '' -- $line)"
    "        string match -q -- \"$stem*\" $name; or continue"
    "        if test -n \"$prefix\""
    "            printf '%s%s\\t%s\\n' $prefix $name \\"
    "                (string replace -r '^[^\\t]*\\t' '' -- $line)"
    "        else"
    "            printf '%s\\n' $line"
    "        end"
    "    end"
    "end"
    ""
    "complete -c maak -a \"(__fish_maak_targets)\" -d \"Maak task\"")))

;; ====== The one entry point ======

(define (completion-script shell)
  "Return the completion script for SHELL, a symbol among `completion-shells'.

The text is what `scripts/maak-completion.*' is, byte for byte: the checked-in
files are copies of this, which is what keeps three hand-edited scripts from
drifting apart from the interface they describe."
  (case shell
    ((bash) (string-append (string-join (bash-lines) "\n") "\n"))
    ((zsh) (string-append (string-join (zsh-lines) "\n") "\n"))
    ((fish) (string-append (string-join (fish-lines) "\n") "\n"))
    (else
     (raise-maak-error
      (format #f "unknown shell: ~a" shell)
      #:hint (format #f "`--completion' takes one of ~a."
                      (string-join (map symbol->string completion-shells)
                                   ", "))))))
