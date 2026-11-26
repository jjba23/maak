;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Elixir helpers: `mix' as an argv a task can inspect, the three task lines a
;; project's automation actually writes -- `mix test', `mix format' and
;; `mix release' -- the words that run a command in a sub-directory or for each
;; application of an umbrella, and the two files a release reads.
;;
;; Mix is the build tool and stays the build tool: it owns the compiler, the
;; dependency graph, the environment and the release.  What this module adds is
;; the task layer around it, so the automation is a Maak file rather than a
;; shell script around `mix'.
;;
;; Every builder is pure.  `mix-test-argv' returns the words a `mix test'
;; invocation consists of, so a test asserts the command line without Elixir
;; installed, and `mix-test' feeds that list to `run', so `--dry-run' prints it
;; and `--plan' records it.  The option names are the ones `mix help TASK'
;; documents, checked against Elixir 1.19.3; an option this module does not name
;; travels as an argument.
;;
;; Mix recognizes only `--help' and `--version' before the task name, so every
;; other switch comes after it.  That is why `mix-argv' places its switches
;; between the task and the arguments rather than in front of the task, which is
;; how `mix --no-compile test' fails while `mix test --no-compile' runs.
;;
;; The two readers answer the questions a release task asks -- what is this
;; application called, what version is it, what does it require, what is pinned
;; -- from `mix.exs' and `mix.lock' themselves, with no mix, no Erlang and no
;; compilation.  They read the files rather than evaluate them, so they answer
;; for the subset documented on each one and refuse a value the project
;; computes: a reader that guessed at `version: version()' would tag a release
;; with something nobody wrote.
;;
;; The programs are parameters, honoured from MAAK_MIX then MIX and MAAK_ELIXIR
;; then ELIXIR.  A value may carry a wrapper (`"asdf exec mix"'), split into
;; words the way an unquoted shell word is, so a task can pin a version manager
;; for one command.

;;; Code:

(define-module (maak elixir)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (mix-command
            elixir-command
            mix-argv
            mix
            mix-output
            mix-cmd-argv
            mix-test-argv
            mix-test
            mix-format-argv
            mix-format
            mix-release-argv
            mix-release
            mix-deps
            mix-deps-get
            mix-compile
            mix-run
            mix-clean
            mix-version
            elixir-version
            mix-project-file
            mix-lock-file
            mix-project
            mix-project-name
            mix-project-version
            mix-project-elixir
            mix-project-umbrella?
            mix-lock-versions
            mix-lock-version))

;; ====== The programs ======

(define (env-value names fallback)
  "Return the first non-empty value among environment NAMES, or FALLBACK."
  (let loop ((names names))
    (if (null? names)
        fallback
        (let ((value (getenv (car names))))
          (if (and value (not (string=? value "")))
              value
              (loop (cdr names)))))))

(define mix-command
  (make-parameter (env-value '("MAAK_MIX" "MIX") "mix")))

(define elixir-command
  (make-parameter (env-value '("MAAK_ELIXIR" "ELIXIR") "elixir")))

(define (program-words command what)
  "Return COMMAND as the words of a program invocation, or raise.

A value may carry a wrapper, so it is split the way an unquoted shell word is
split; an empty value is a maak error rather than a command with no program."
  (let ((words (and (string? command)
                    (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "~a must name a program, got ~s" what command)
       #:hint "Set MAAK_MIX or MAAK_ELIXIR, or pass the program as a string."))
    words))

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write '(\"slow\" \"integration\") for the filters."))
    items))

(define (require-string value what)
  (unless (and (string? value) (not (string=? value "")))
    (raise-maak-error
     (format #f "~a takes a non-empty string, got ~s" what value)
     #:hint "Name the value as a string, or leave the option out."))
  value)

(define* (number-string value what #:key (zero? #f))
  "Return VALUE as a decimal string, refusing a number Mix would not take.

ZERO? allows the number `0', which `mix test --seed 0' takes and documents: it
turns the random order off."
  (let ((number (cond ((exact-integer? value) value)
                      ((and (string? value) (string->number value))
                       (string->number value))
                      (else #f))))
    (unless (and number
                 (exact-integer? number)
                 (>= number (if zero? 0 1)))
      (raise-maak-error
       (format #f "~a takes ~a, got ~s"
               what (if zero? "a number" "a positive number") value)
       #:hint (format #f "Write ~a as an integer." what)))
    (number->string number)))

;; ====== The mix command line ======

(define* (mix-argv task
                   #:key
                   (arguments '())
                   (force? #f)
                   (no-archives-check? #f)
                   (no-compile? #f)
                   (no-deps-check? #f)
                   (no-elixir-version-check? #f)
                   (no-start? #f))
  "Return the argv that runs the mix TASK, without running it.

The switches come after TASK and before ARGUMENTS, because Mix recognizes only
`--help' and `--version' in front of the task name and every other switch
belongs to the task that documents it.  The five `no-' checks and FORCE? are
the ones the tasks a project's automation runs all accept; an option this
builder does not name travels in ARGUMENTS, as the words Mix reads.

ARGS is the task's own arguments, so `(mix-argv \"ecto.migrate\" #:arguments
'(\"-r\" \"Repo\"))' is the command Mix documents.

@example
(mix-argv \"compile\" #:force? #t)
@result{} (\"mix\" \"compile\" \"--force\")

(mix-argv \"test\" #:no-start? #t #:arguments '(\"test/a_test.exs\"))
@result{} (\"mix\" \"test\" \"--no-start\" \"test/a_test.exs\")
@end example"
  (unless (and (string? task) (not (string=? task "")))
    (raise-maak-error
     (format #f "mix needs a task, got ~s" task)
     #:hint "Write (mix-compile), or (mix \"ecto.migrate\" #:arguments '(\"-r\" \"Repo\"))."))
  (append (program-words (mix-command) "mix")
          (list task)
          (if force? (list "--force") '())
          (if no-archives-check? (list "--no-archives-check") '())
          (if no-compile? (list "--no-compile") '())
          (if no-deps-check? (list "--no-deps-check") '())
          (if no-elixir-version-check? (list "--no-elixir-version-check") '())
          (if no-start? (list "--no-start") '())
          (string-list arguments "mix")))

(define (mix task . arguments)
  "Run the mix TASK with ARGUMENTS, as separate words.

This is the short form: the words after TASK are arguments, so `(mix-compile
\"--force\")' passes Mix's own `--force'.  Use `mix-argv' when the named
switches are wanted, and `run' to execute what it returns.

@example
(mix \"deps.get\" \"--only\" \"prod\")
@end example"
  (run (mix-argv task #:arguments arguments)))

(define* (argv-output argv #:key (allow-failure? #f) (what "mix"))
  "Run ARGV and return its standard output as a string.

The query runs even in a dry run: it reads the installed tool rather than
changing the project.  A failure raises a maak error naming the command, or
comes back as #f when the caller asked for the exit status instead."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     (format #f "~a-output expects a non-empty list of strings" what)
     #:hint "Build it with mix-argv, which returns exactly that."))
  (let* ((line (argv->string argv))
         (port (open-input-pipe line))
         (output (get-string-all port))
         (status (close-pipe port))
         (code (status:exit-val status)))
    (cond
     ((and code (zero? code)) output)
     (allow-failure? #f)
     ((not code)
      (raise-maak-error
       (format #f "~a was terminated by a signal: ~a" what line)
       #:hint "Run the command above by hand to read its own message."))
     (else
      (raise-maak-error
       (format #f "~a failed (exit code ~a): ~a" what code line)
       #:hint "Run the command above to read its own message.")))))

(define* (mix-output argv #:key (allow-failure? #f))
  "Run the mix ARGV and return its standard output as a string.

See `argv-output' for what a query does in a dry run and on failure."
  (argv-output argv #:allow-failure? allow-failure?))

;; ====== One command, one directory, or one umbrella application ======
;;
;; `mix cmd' is how Mix runs something that is not a Mix task, and it is the
;; only way to say "in that directory": Mix has no global `--cd' of its own.  In
;; an umbrella the applications are named instead, and the option that names
;; them lives on `mix do' -- `mix cmd --app' is deprecated in favour of it.

(define* (mix-cmd-argv command #:key (cd #f) (apps '()) (shell? #f))
  "Return the argv that runs COMMAND through `mix cmd', without running it.

COMMAND is the program and its words, one string each, and it is not read by a
shell unless SHELL? asks for it: with `#:shell? #t' Mix expands the words the
way a shell would, which is what a pipe or a `$VAR' needs.

CD runs the command in that directory, which is `mix cmd --cd DIR'.  APPS names
applications of an umbrella instead, and answers with `mix do --app NAME ...
cmd COMMAND', which is what Mix documents: `mix cmd --app' is deprecated.  A
directory and a list of applications are two different runs, so naming both is
a maak error.

@example
(mix-cmd-argv '(\"make\" \"test\") #:cd \"third-party\")
@result{} (\"mix\" \"cmd\" \"--cd\" \"third-party\" \"make\" \"test\")

(mix-cmd-argv '(\"mix\" \"test\") #:apps '(\"web\" \"api\"))
@result{} (\"mix\" \"do\" \"--app\" \"web\" \"--app\" \"api\" \"cmd\" \"mix\" \"test\")
@end example"
  (let ((command (string-list command "mix-cmd"))
        (apps (string-list apps "mix-cmd's #:apps")))
    (when (null? command)
      (raise-maak-error
       "mix-cmd needs a command to run"
       #:hint "Write (mix-cmd-argv '(\"make\" \"test\")), or use a wrapper such as mix-test."))
    (when (and cd (pair? apps))
      (raise-maak-error
       "mix-cmd takes a directory or a list of apps, not both"
       #:hint "Write #:cd \"DIR\" outside an umbrella, or #:apps '(\"web\") for the applications of one."))
    (if (pair? apps)
        (append (program-words (mix-command) "mix")
                (list "do")
                (append-map (lambda (app)
                              (list "--app" (require-string app "mix-cmd's #:apps")))
                            apps)
                (list "cmd")
                command)
        (append (program-words (mix-command) "mix")
                (list "cmd")
                (if cd (list "--cd" (require-string cd "mix-cmd's #:cd")) '())
                (if shell? (list "--shell") '())
                command))))

;; ====== mix test ======
;;
;; The test line is the one a project writes most often and the one with the
;; most options, so it gets a builder of its own.  The filters Mix lets a caller
;; repeat are `--only', `--include' and `--exclude': a list of them is one flag
;; each, which is how Mix applies several filters at once.

(define (filter-words flag filters what)
  "Return FILTERS as repeated FLAG VALUE words."
  (append-map (lambda (filter) (list flag (require-string filter what)))
              (string-list filters what)))

(define* (mix-test-argv #:key
                        (arguments '())
                        (only '())
                        (include '())
                        (exclude '())
                        (seed #f)
                        (max-failures #f)
                        (slowest #f)
                        (partitions #f)
                        (repeat-until-failure #f)
                        (trace? #f)
                        (cover? #f)
                        (failed? #f)
                        (stale? #f)
                        (force? #f)
                        (warnings-as-errors? #f))
  "Return the argv that runs `mix test', without running it.

ONLY, INCLUDE and EXCLUDE take a filter -- a tag such as `\"integration\"' or a
file with a line number -- or a list of them, and a list becomes one flag per
filter, which is how `mix test --only slow --only integration' reads.  SEED
takes a number, and `0' turns the random order off.  MAX-FAILURES, SLOWEST,
PARTITIONS and REPEAT-UNTIL-FAILURE take a positive number.

TRACE?, COVER?, FAILED?, STALE?, FORCE? and WARNINGS-AS-ERRORS? are the switches
that only say yes.  ARGUMENTS are the files and lines to run, appended last,
where `mix test' reads them.

STALE? is asked for rather than assumed: `mix test --stale' runs only the tests
that reference a module which changed, and a suite that quietly ran a subset is
not what a task in a release asks for.

@example
(mix-test-argv #:warnings-as-errors? #t #:cover? #t)
@result{} (\"mix\" \"test\" \"--cover\" \"--warnings-as-errors\")

(mix-test-argv #:only '(\"slow\" \"integration\") #:seed 0)
@result{} (\"mix\" \"test\" \"--seed\" \"0\" \"--only\" \"slow\" \"--only\" \"integration\")
@end example"
  (append (program-words (mix-command) "mix")
          (list "test")
          (if trace? (list "--trace") '())
          (if failed? (list "--failed") '())
          (if stale? (list "--stale") '())
          (if cover? (list "--cover") '())
          (if warnings-as-errors? (list "--warnings-as-errors") '())
          (if force? (list "--force") '())
          (if seed
              (list "--seed" (number-string seed "mix test's #:seed" #:zero? #t))
              '())
          (if max-failures
              (list "--max-failures"
                    (number-string max-failures "mix test's #:max-failures"))
              '())
          (if slowest
              (list "--slowest" (number-string slowest "mix test's #:slowest"))
              '())
          (if partitions
              (list "--partitions"
                    (number-string partitions "mix test's #:partitions"))
              '())
          (if repeat-until-failure
              (list "--repeat-until-failure"
                    (number-string repeat-until-failure
                                   "mix test's #:repeat-until-failure"))
              '())
          (filter-words "--only" only "mix test's #:only")
          (filter-words "--include" include "mix test's #:include")
          (filter-words "--exclude" exclude "mix test's #:exclude")
          (string-list arguments "mix test")))

(define (mix-test . arguments)
  "Run `mix test' with ARGUMENTS -- files, lines and Mix's own switches.

@example
(mix-test \"test/a_test.exs:12\")
(mix-test \"--warnings-as-errors\")
@end example"
  (run (mix-test-argv #:arguments arguments)))

;; ====== mix format ======

(define* (mix-format-argv #:key
                          (arguments '())
                          (check-formatted? #f)
                          (dry-run? #f)
                          (migrate? #f)
                          (dot-formatter #f)
                          (stdin-filename #f))
  "Return the argv that runs `mix format', without running it.

CHECK-FORMATTED? asks whether the files are already formatted and fails when
they are not, which is the gate a CI runs; DRY-RUN? reports what would change
without writing it.  DOT-FORMATTER names the formatter configuration to read,
`.formatter.exs' by default.  STDIN-FILENAME is the path the input on standard
input is formatted as, which is what an editor passes when it formats a buffer
that is not saved.

ARGUMENTS are the files and patterns, appended last, where `mix format' reads
them; without any, the formatter's own `:inputs' are formatted.

@example
(mix-format-argv #:check-formatted? #t)
@result{} (\"mix\" \"format\" \"--check-formatted\")

(mix-format-argv #:dot-formatter \".formatter.exs\" #:arguments '(\"lib\" \"test\"))
@result{} (\"mix\" \"format\" \"--dot-formatter\" \".formatter.exs\" \"lib\" \"test\")
@end example"
  (append (program-words (mix-command) "mix")
          (list "format")
          (if check-formatted? (list "--check-formatted") '())
          (if dry-run? (list "--dry-run") '())
          (if migrate? (list "--migrate") '())
          (if dot-formatter
              (list "--dot-formatter"
                    (require-string dot-formatter "mix format's #:dot-formatter"))
              '())
          (if stdin-filename
              (list "--stdin-filename"
                    (require-string stdin-filename "mix format's #:stdin-filename"))
              '())
          (string-list arguments "mix format")))

(define (mix-format . arguments)
  "Format the project with `mix format', passing ARGUMENTS.

@example
(mix-format)
(mix-format \"--check-formatted\")
@end example"
  (run (mix-format-argv #:arguments arguments)))

;; ====== mix release ======

(define* (mix-release-argv #:optional name
                           #:key
                           (arguments '())
                           (overwrite? #f)
                           (quiet? #f)
                           (path #f)
                           (version #f)
                           (force? #f)
                           (no-archives-check? #f)
                           (no-compile? #f)
                           (no-deps-check? #f)
                           (no-elixir-version-check? #f))
  "Return the argv that assembles a release, without assembling it.

NAME is the release to assemble, which the project declares under `releases:'
in `mix.exs'; without one, `mix release' assembles the release the project
names.  OVERWRITE? replaces the files of an existing release instead of
prompting.  PATH is where the release is written, VERSION is the version to
label it with, and QUIET? keeps the progress off standard output.

The `no-' checks and FORCE? are the ones `mix help release' documents; a release
is normally assembled with `MIX_ENV=prod', which is the task's environment to
set:

@example
(run (mix-release-argv \"example\" #:overwrite? #t) #:env '((\"MIX_ENV\" . \"prod\")))

(mix-release-argv #:path \"dist\" #:quiet? #t)
@result{} (\"mix\" \"release\" \"--path\" \"dist\" \"--quiet\")
@end example"
  (append (program-words (mix-command) "mix")
          (list "release")
          (if name (list (require-string name "mix-release")) '())
          (if overwrite? (list "--overwrite") '())
          (if path (list "--path" (require-string path "mix release's #:path")) '())
          (if quiet? (list "--quiet") '())
          (if version
              (list "--version" (require-string version "mix release's #:version"))
              '())
          (if force? (list "--force") '())
          (if no-archives-check? (list "--no-archives-check") '())
          (if no-compile? (list "--no-compile") '())
          (if no-deps-check? (list "--no-deps-check") '())
          (if no-elixir-version-check? (list "--no-elixir-version-check") '())
          (string-list arguments "mix release")))

(define (mix-release . arguments)
  "Assemble a release with `mix release', passing ARGUMENTS.

The words are what `mix release' takes: a release name first, then its
switches.  A release is normally assembled with `MIX_ENV=prod', so a task
usually sets it:

@example
(run (list \"mix\" \"release\" \"--overwrite\") #:env '((\"MIX_ENV\" . \"prod\")))
@end example"
  (run (append (mix-release-argv) arguments)))

;; ====== The common commands ======
;;
;; A wrapper is one line so that `maak --list' shows a task that reads like the
;; command it runs.  Any option the wrapper does not name travels as an
;; argument; the structured builders are the `-argv' procedures above.

(define (mix-deps . arguments)
  "List the project's dependencies and their status, passing ARGUMENTS."
  (run (mix-argv "deps" #:arguments arguments)))

(define (mix-deps-get . arguments)
  "Fetch the unavailable or out-of-date dependencies, passing ARGUMENTS.

A locked install is `(mix-deps-get \"--check-locked\")', which is what a CI job
wants: it raises instead of changing `mix.lock'."
  (run (mix-argv "deps.get" #:arguments arguments)))

(define (mix-compile . arguments)
  "Compile the project, passing ARGUMENTS.

`(mix-compile \"--warnings-as-errors\")' is the gate that turns a compiler
warning into a failure."
  (run (mix-argv "compile" #:arguments arguments)))

(define (mix-run . arguments)
  "Run the current application, passing ARGUMENTS.

`(mix-run \"--no-start\")' compiles without starting the application, and the
words after `--' belong to the program, as in `(mix-run \"--\" \"--help\")'."
  (run (mix-argv "run" #:arguments arguments)))

(define (mix-clean . arguments)
  "Delete the generated application files, passing ARGUMENTS."
  (run (mix-argv "clean" #:arguments arguments)))

;; ====== What the installed Elixir says ======
;;
;; Both programs print an Erlang/OTP banner above their own line, so the answer
;; is read from the line that names them rather than from the output as a whole.

(define (reported-version command what prefix)
  "Return the third-party version COMMAND reports on its PREFIX line."
  (let* ((text (argv-output (append (program-words command what)
                                    (list "--version"))
                            #:what what))
         (line (find (lambda (candidate) (string-prefix? prefix candidate))
                     (string-split text #\newline))))
    (unless line
      (raise-maak-error
       (format #f "~a --version printed no line starting with ~a" what prefix)
       #:hint "Run it by hand to read what the program prints."))
    (let ((words (string-tokenize line)))
      (if (and (pair? words) (pair? (cdr words)))
          (cadr words)
          (raise-maak-error
           (format #f "~a --version printed ~s, which carries no version" what line)
           #:hint "Run it by hand to read what the program prints.")))))

(define (mix-version)
  "Return the Mix version the configured program reports, as a number string.

The answer is the one `Mix ...' line of `mix --version', not the Erlang/OTP
banner above it, so a task can assert on it.

@example
(mix-version)
@result{} \"1.19.3\"
@end example"
  (reported-version (mix-command) "mix" "Mix "))

(define (elixir-version)
  "Return the Elixir version the configured program reports.

@example
(elixir-version)
@result{} \"1.19.3\"
@end example"
  (reported-version (elixir-command) "elixir" "Elixir "))

;; ====== The project and the lock ======
;;
;; Both files are read as text rather than evaluated, so the documented subset
;; is the contract: a field written as a literal is answered for, and a value
;; the project computes is refused with the field named.  A release that tagged
;; a guessed version would be worse than a task that stopped.

(define mix-project-file
  (make-parameter "mix.exs"))

(define mix-lock-file
  (make-parameter "mix.lock"))

(define (read-text path)
  "Return the whole of PATH, or raise when it is not there."
  (unless (file-exists? path)
    (raise-maak-error
     (format #f "no ~a in this directory" path)
     #:hint "Run the task at the project's root, or point `mix-project-file' or `mix-lock-file' at the file."))
  (call-with-input-file path get-string-all))

(define (without-comments text)
  "Return TEXT with Elixir's `#' comments removed.

A `#' inside a string is part of the string and a backslash escapes the
character after it, so both are tracked.  Comments are removed before anything
is matched because the file `mix new' writes leaves a commented out dependency
carrying a `tag:' and a `git:' that would otherwise be read as fields of the
project."
  (let loop ((chars (string->list text))
             (in-string? #f)
             (escaped? #f)
             (kept '()))
    (cond
     ((null? chars) (list->string (reverse kept)))
     (escaped? (loop (cdr chars) in-string? #f (cons (car chars) kept)))
     ((and in-string? (char=? (car chars) #\\))
      (loop (cdr chars) in-string? #t (cons (car chars) kept)))
     ((char=? (car chars) #\")
      (loop (cdr chars) (not in-string?) #f (cons (car chars) kept)))
     ((and (not in-string?) (char=? (car chars) #\#))
      (let skip ((rest chars))
        (cond
         ((null? rest) (list->string (reverse kept)))
         ((char=? (car rest) #\newline)
          (loop (cdr rest) #f #f (cons #\newline kept)))
         (else (skip (cdr rest))))))
     (else (loop (cdr chars) in-string? #f (cons (car chars) kept))))))

(define field-pattern
  (make-regexp "([a-z_][A-Za-z0-9_]*)[ \t]*:"))

;; The words Elixir uses to open a block are written as `keyword:' too, and
;; `defp version, do: \"1.0.0\"' would otherwise answer as a field called `do'.
(define block-keywords
  '("do" "else" "after" "rescue" "catch"))

(define (literal-after text index)
  "Return the literal value TEXT holds after INDEX, or #f when it computes one.

A literal is a string, an atom or an integer -- what a project list writes for
its own name, version and requirement.  Anything else, a function call above
all, is #f, because answering with a guess is what a release must not do."
  (let ((rest (substring text (min index (string-length text)))))
    (cond
     ((string-match "^[ \t]*\"([^\"\\\\\n]*)\"" rest)
      => (lambda (found) (match:substring found 1)))
     ((string-match "^[ \t]*:([A-Za-z_][A-Za-z0-9_]*)" rest)
      => (lambda (found) (match:substring found 1)))
     ((string-match "^[ \t]*(-?[0-9]+)" rest)
      => (lambda (found) (match:substring found 1)))
     (else #f))))

(define (fields-of text)
  "Return the literal fields of TEXT as (KEY . VALUE) pairs, in file order.

The first occurrence of a key wins, which is the project list in a `mix.exs'
that `mix new' wrote: everything a file adds below it -- a release, an
application's options -- is written after the project's own fields."
  ;; The matches are found in the text without its comments, and read out of
  ;; that same text: a match carries an offset, and the two texts are not the
  ;; same length.
  (let ((clean (without-comments text)))
    (let loop ((matches (list-matches field-pattern clean))
               (seen '())
               (fields '()))
      (if (null? matches)
          (reverse fields)
          (let* ((match (car matches))
                 (key (match:substring match 1)))
            (if (or (member key seen) (member key block-keywords))
                (loop (cdr matches) seen fields)
                (let ((value (literal-after clean (match:end match 0))))
                  (loop (cdr matches)
                        (cons key seen)
                        (if value (cons (cons key value) fields) fields)))))))))

(define* (mix-project #:optional (path (mix-project-file)))
  "Return the literal fields of PATH's project list, as (KEY . VALUE) pairs.

The file is read, never run: `app: :example' answers as `(\"app\" .
\"example\")', a string stays a string and a number is turned into one.  A field
whose value is computed -- `deps: deps()', `start_permanent: Mix.env() ==
:prod' -- is left out here, because this is the list a reader can answer for;
the named readers refuse such a field by name instead.

@example
(mix-project)
@result{} ((\"app\" . \"example\") (\"version\" . \"0.3.1\") (\"elixir\" . \"~> 1.15\"))
@end example"
  (fields-of (read-text path)))

(define* (project-field path field)
  "Return FIELD of PATH's project list, or raise naming why it has no answer."
  (let* ((text (without-comments (read-text path)))
         (match (find (lambda (candidate)
                        (string=? field (match:substring candidate 1)))
                      (list-matches field-pattern text))))
    (cond
     ((not match)
      (raise-maak-error
       (format #f "~a has no ~a field" path field)
       #:hint (format #f "Write ~a: ... in the project list." field)))
     ((literal-after text (match:end match 0)))
     (else
      (raise-maak-error
       (format #f "~a computes ~a, so it is not a value this reader answers with"
               path field)
       #:hint (format #f "Write ~a as a string, or run mix for the value." field))))))

(define* (mix-project-name #:optional (path (mix-project-file)))
  "Return the application name PATH declares, as the atom's name without its colon.

@example
(mix-project-name)
@result{} \"example\"
@end example"
  (project-field path "app"))

(define* (mix-project-version #:optional (path (mix-project-file)))
  "Return the version PATH's project list declares.

A version the project computes is refused rather than guessed: a release that
tagged a number nobody wrote is worse than a task that stopped.

@example
(mix-project-version)
@result{} \"0.3.1\"
@end example"
  (project-field path "version"))

(define* (mix-project-elixir #:optional (path (mix-project-file)))
  "Return the Elixir requirement PATH's project list declares.

The answer is the requirement as written, `\"~> 1.15\"' for example, which is
what `mix compile' checks the running Elixir against."
  (project-field path "elixir"))

(define* (mix-project-umbrella? #:optional (path (mix-project-file)))
  "Return whether PATH is an umbrella project, which is whether it names `apps_path'."
  (let ((fields (mix-project path)))
    (and (assoc "apps_path" fields) #t)))

;; ====== What mix.lock pins ======
;;
;; The lock is an Elixir map literal, one entry per dependency:
;;
;;   "decimal" => {:hex, :decimal, "2.1.1", "checksum", [:mix], [], "hexpm", "checksum"}
;;   "local"   => {:git, "https://example.com/x.git", "0bed9c8", []}
;;
;; The third element is a version for a hex dependency and a revision for a git
;; one, which is why the reader carries the kind beside the value instead of
;; calling every third element a version.

(define lock-entry-pattern
  (make-regexp "\"([A-Za-z0-9_.-]+)\"[ \t\n]*=>[ \t\n]*\\{:([a-z]+),[ \t\n]*"))

(define (lock-entries path)
  "Return (NAME KIND VERSION-OR-#F) for every dependency PATH pins, in file order."
  (let ((text (read-text path)))
    (map (lambda (match)
           (let* ((name (match:substring match 1))
                  (kind (match:substring match 2))
                  (rest (substring text (min (match:end match 0)
                                             (string-length text))))
                  (version
                   (and (string=? kind "hex")
                        (let ((found (string-match
                                      "^:?[A-Za-z0-9_.]+,[ \t\n]*\"([^\"]+)\"" rest)))
                          (and found (match:substring found 1))))))
             (list name kind version)))
         (list-matches lock-entry-pattern text))))

(define* (mix-lock-versions #:optional (path (mix-lock-file)))
  "Return the version each hex dependency of PATH is pinned at, in file order.

A dependency fetched from git carries a revision rather than a version, so it is
left out; `mix-lock-version' names one and says which it is.

@example
(mix-lock-versions)
@result{} ((\"decimal\" . \"2.1.1\") (\"phoenix\" . \"1.7.10\"))
@end example"
  (filter-map (lambda (entry)
                (and (string=? (cadr entry) "hex")
                     (cons (car entry) (caddr entry))))
              (lock-entries path)))

(define* (mix-lock-version name #:optional (path (mix-lock-file)))
  "Return the version PATH pins NAME at.

@example
(mix-lock-version \"phoenix\")
@result{} \"1.7.10\"
@end example"
  (require-string name "mix-lock-version")
  (let* ((entries (lock-entries path))
         (entry (find (lambda (candidate) (string=? name (car candidate)))
                      entries)))
    (cond
     ((not entry)
      (raise-maak-error
       (format #f "~a pins no dependency called ~a" path name)
       #:hint (format #f "It pins: ~a"
                      (string-join (map car entries) ", "))))
     ((not (string=? (cadr entry) "hex"))
      (raise-maak-error
       (format #f "~a pins ~a from ~a, which carries a revision rather than a version"
               path name (cadr entry))
       #:hint "Only a hex dependency has a version to answer with."))
     (else (caddr entry)))))
