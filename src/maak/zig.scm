;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>
;; Copyright © FuncProgLinux <softwarelibre@urutau-ltd.org>

;;; Commentary:
;;
;; Zig helpers: `zig build' as an argv a task can inspect, the direct compiler
;; and formatter commands, and the output a query returns.
;;
;; Zig already knows how to build a Zig project: `build.zig' declares the
;; steps, the cache and the dependency graph, and maak adds no second build
;; engine beside it.  What this module adds is the rest of a task -- the build
;; steps and their options as a list, the direct commands a build step may not
;; cover, and the program as a parameter -- so a project's automation is a Maak
;; file rather than a shell script around `zig'.
;;
;; `zig-build-argv' is pure: it returns the words of a `zig build' invocation,
;; so a test asserts the command line without Zig installed, and `zig-build'
;; feeds that list to `run', so `--dry-run' prints it and `--plan' records it.
;; Options the builder does not name travel as `#:arguments', and project
;; options travel as `#:options', which is how a `-D' a build.zig invented
;; still reaches the build.
;;
;; The program is a parameter, honoured from MAAK_ZIG then ZIG.  A value may
;; carry a wrapper (`"sccache zig"'), which is split into words the way an
;; unquoted shell word is, so a task can pin one for a single command.
;;
;; Zig's `uninstall' step is not implemented: 0.16.0 panics with a link to
;; issue 14943.  `zig-build-uninstall' says so instead of aborting the build
;; runner, and `zig-build-clean' removes the local `zig-out' prefix with
;; guards that keep it inside the project.
;;
;; The named build options are the ones Zig 0.16.0 documents, which is the
;; version this module was tested against.  A flag a newer Zig spells
;; differently still reaches the build through `#:arguments'.

;;; Code:

(define-module (maak zig)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (zig-command
            zig-argv
            zig
            zig-output
            zig-build-argv
            zig-build
            zig-build-install
            zig-build-test
            zig-build-run
            zig-build-uninstall
            zig-build-clean
            zig-build-steps
            zig-build-exe
            zig-build-lib
            zig-build-obj
            zig-test
            zig-fmt
            zig-fetch
            zig-version))

;; ====== The program ======

(define (env-value names fallback)
  "Return the first non-empty value among environment NAMES, or FALLBACK."
  (let loop ((names names))
    (if (null? names)
        fallback
        (let ((value (getenv (car names))))
          (if (and value (not (string=? value "")))
              value
              (loop (cdr names)))))))

(define zig-command
  (make-parameter (env-value '("MAAK_ZIG" "ZIG") "zig")))

(define (program-words command what)
  "Return COMMAND as the words of a program invocation, or raise."
  (let ((words (and (string? command)
                    (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "~a must name a program, got ~s" what command)
       #:hint "Set MAAK_ZIG, or pass the program as a string."))
    words))

;; ====== The command line ======

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write '(\"test\" \"run\") for the steps."))
    items))

(define (require-string value what)
  (unless (and (string? value) (not (string=? value "")))
    (raise-maak-error
     (format #f "~a takes a non-empty string, got ~s" what value)
     #:hint "Name the value as a string, or leave the option out."))
  value)

(define (choice what value allowed)
  "Return VALUE after checking it is one of ALLOWED."
  (unless (member value allowed)
    (raise-maak-error
     (format #f "zig's ~a takes ~a, got ~s"
             what (string-join allowed ", ") value)
     #:hint (format #f "Write #:~a \"~a\"." what (car allowed))))
  value)

(define (option flag value what)
  "Return FLAG and VALUE as two words, or nothing when VALUE is #f."
  (if value
      (list flag (require-string value what))
      '()))

(define (build-option-words options)
  "Return OPTIONS as `-D' words.

An entry is a string such as `\"foo=bar\"', or a pair such as
`(\"foo\" . \"bar\")', which is how an alist already stores it."
  (unless (list? options)
    (raise-maak-error
     (format #f "zig's #:options takes a list, got ~s" options)
     #:hint "Write '((\"foo\" . \"bar\")) or '(\"foo=bar\")."))
  (map (lambda (entry)
         (cond
          ((pair? entry)
           (format #f "-D~a=~a"
                   (require-string (car entry) "zig's #:options")
                   (require-string (cdr entry) "zig's #:options")))
          ((string? entry) (string-append "-D" entry))
          (else
           (raise-maak-error
            (format #f "~s is not a zig build option" entry)
            #:hint "Write a \"name=value\" string or a (name . value) pair."))))
       options))

(define (fetch-words fetch)
  "Return the words of the `--fetch' mode FETCH names."
  (cond
   ((not fetch) '())
   ((eq? fetch #t) (list "--fetch"))
   ((member fetch '("needed" "all"))
    (list (string-append "--fetch=" fetch)))
   (else
    (raise-maak-error
     (format #f "zig's #:fetch takes #t, \"needed\" or \"all\", got ~s" fetch)
     #:hint "Write #:fetch #t, or #:fetch \"all\" to fetch lazy dependencies."))))

(define* (zig-argv subcommand #:key (arguments '()))
  "Return the argv that runs zig SUBCOMMAND with ARGUMENTS, without running it.

@example
(zig-argv \"version\")
@result{} (\"zig\" \"version\")

(zig-argv \"fmt\" #:arguments '(\"src\"))
@result{} (\"zig\" \"fmt\" \"src\")
@end example"
  (unless (and (string? subcommand) (not (string=? subcommand "")))
    (raise-maak-error
     (format #f "zig needs a subcommand, got ~s" subcommand)
     #:hint "Write (zig \"version\"), or (zig-build)."))
  (append (program-words (zig-command) "zig")
          (list subcommand)
          (string-list arguments "zig")))

(define (zig subcommand . arguments)
  "Run zig SUBCOMMAND with ARGUMENTS."
  (run (zig-argv subcommand #:arguments arguments)))

(define* (zig-build-argv #:optional (steps '())
                         #:key
                         (arguments '())
                         (optimize #f)
                         (target #f)
                         (cpu #f)
                         (options '())
                         (prefix #f)
                         (prefix-lib-dir #f)
                         (prefix-exe-dir #f)
                         (prefix-include-dir #f)
                         (build-file #f)
                         (cache-dir #f)
                         (global-cache-dir #f)
                         (zig-lib-dir #f)
                         (search-prefixes '())
                         (systems '())
                         (fetch #f)
                         (summary #f)
                         (color #f)
                         (jobs #f)
                         (verbose? #f))
  "Return the argv that runs `zig build' with STEPS, without running it.

STEPS is optional: omit it, or pass an empty list, to build Zig's default
install step.  A step name is a string, and several steps are a list of them.
The steps come first, then the build options, then ARGUMENTS as they are, so an
option this builder does not name still reaches the build.

OPTIMIZE is one of `Debug', `ReleaseSafe', `ReleaseFast' or `ReleaseSmall'.
TARGET and CPU become `-Dtarget' and `-Dcpu'; OPTIONS carries the project
options `build.zig' declares, as `\"name=value\"' strings or `(name . value)'
pairs.  SUMMARY is one of Zig's modes (`all', `new', `failures', `line' or
`none') and COLOR is `auto', `off' or `on'.

The named options are the ones Zig 0.16.0 documents, the version these helpers
are tested against.  A flag a newer Zig spells differently still reaches the
build through ARGUMENTS.

@example
(zig-build-argv #:optimize \"ReleaseSafe\")
@result{} (\"zig\" \"build\" \"-Doptimize=ReleaseSafe\")

(zig-build-argv '(\"test\")
                #:optimize \"ReleaseSafe\"
                #:target \"aarch64-linux-gnu\"
                #:options '((\"foo\" . \"bar\")))
@result{} (\"zig\" \"build\" \"test\" \"-Doptimize=ReleaseSafe\"
          \"-Dtarget=aarch64-linux-gnu\" \"-Dfoo=bar\")
@end example"
  (let ((steps (string-list steps "zig-build"))
        (arguments (string-list arguments "zig-build")))
    (when optimize
      (choice "optimize" optimize
              '("Debug" "ReleaseSafe" "ReleaseFast" "ReleaseSmall")))
    (when summary
      (choice "summary" summary '("all" "new" "failures" "line" "none")))
    (when color
      (choice "color" color '("auto" "off" "on")))
    (unless (or (not jobs) (and (exact-integer? jobs) (> jobs 0)))
      (raise-maak-error
       (format #f "zig takes a positive job count, got ~s" jobs)
       #:hint "Write #:jobs 4, or omit it for Zig's default."))
    (append
     (program-words (zig-command) "zig")
     (list "build")
     steps
     (if optimize (list (string-append "-Doptimize=" optimize)) '())
     (if target (list (string-append "-Dtarget=" target)) '())
     (if cpu (list (string-append "-Dcpu=" cpu)) '())
     (build-option-words options)
     (option "--prefix" prefix "zig-build")
     (option "--prefix-lib-dir" prefix-lib-dir "zig-build")
     (option "--prefix-exe-dir" prefix-exe-dir "zig-build")
     (option "--prefix-include-dir" prefix-include-dir "zig-build")
     (option "--build-file" build-file "zig-build")
     (option "--cache-dir" cache-dir "zig-build")
     (option "--global-cache-dir" global-cache-dir "zig-build")
     (option "--zig-lib-dir" zig-lib-dir "zig-build")
     (append-map (lambda (path) (list "--search-prefix" (require-string
                                                         path "zig-build")))
                 (string-list search-prefixes "zig-build"))
     (append-map (lambda (path) (list "--system" (require-string
                                                  path "zig-build")))
                 (string-list systems "zig-build"))
     (fetch-words fetch)
     (if summary (list "--summary" summary) '())
     (if color (list "--color" color) '())
     (if jobs (list (format #f "-j~a" jobs)) '())
     (if verbose? (list "--verbose") '())
     arguments)))

;; ====== The common commands ======
;;
;; A wrapper is one line so that `maak --list' shows a task that reads like the
;; command it runs.  Any option the wrapper does not name travels as an
;; argument; the structured builder is `zig-build-argv'.

(define (zig-build . steps)
  "Build STEPS with `zig build', passing STEPS and arguments as words.

Without a step, `zig build' installs the default install step.  An option
travels as the word it is, so `(zig-build \"test\" \"-Doptimize=ReleaseSafe\")'
is the command Zig documents.

@example
(zig-build)
(zig-build \"test\")
@end example"
  (run (zig-build-argv steps)))

(define (zig-build-install . arguments)
  "Run the `install' build step, passing ARGUMENTS."
  (apply zig-build "install" arguments))

(define (zig-build-test . arguments)
  "Run the `test' build step, passing ARGUMENTS."
  (apply zig-build "test" arguments))

(define (zig-build-run . arguments)
  "Run the `run' build step, passing ARGUMENTS.

A word that belongs to the program rather than to the build goes after `--',
as in `(zig-build-run \"--\" \"--help\")'."
  (apply zig-build "run" arguments))

(define (zig-build-uninstall . arguments)
  "Refuse to run `zig build uninstall', which Zig has not implemented.

Zig 0.16.0's uninstall step panics with
`TODO implement https://github.com/ziglang/zig/issues/14943'; calling it
would abort the build runner rather than remove anything.  For a local
`zig-out' prefix, use `zig-build-clean'; for an external prefix, remove it
with the package manager that installed it."
  (raise-maak-error
   "zig build uninstall is not implemented by Zig"
   #:hint "Use (zig-build-clean) for the local zig-out prefix, or remove an external prefix yourself; see https://github.com/ziglang/zig/issues/14943."))

(define (local-prefix path)
  "Return PATH normalised after checking it stays inside the project."
  (require-string path "zig-build-clean")
  (when (string-prefix? "/" path)
    (raise-maak-error
     (format #f "zig-build-clean takes a path relative to the project, got ~s"
             path)
     #:hint "Write #:prefix \"zig-out\", or remove an external prefix yourself."))
  (let ((normalised
         (let loop ((parts (string-split path #\/)) (kept '()))
           (cond
            ((null? parts) (string-join (reverse kept) "/"))
            ((member (car parts) '("" ".")) (loop (cdr parts) kept))
            ((string=? (car parts) "..")
             (raise-maak-error
              (format #f "zig-build-clean refuses a path that leaves the project: ~s"
                      path)
              #:hint "Write #:prefix \"zig-out\" or a directory under it."))
            (else (loop (cdr parts) (cons (car parts) kept)))))))
    (when (string=? normalised "")
      (raise-maak-error
       (format #f "zig-build-clean refuses to remove the project root: ~s" path)
       #:hint "Write #:prefix \"zig-out\" or a directory under it."))
    normalised))

(define* (zig-build-clean #:key (prefix "zig-out") (verbose? #f))
  "Remove a local Zig install PREFIX, `zig-out' by default.

This is the working replacement for Zig's unimplemented `uninstall' step,
and it is deliberately narrower: PREFIX must be a path inside the project,
so `/usr/local' and `$HOME/.local' are never removed by accident.  A path
that is absolute, names the project root, or contains `..' is a maak error.
An external prefix is the package manager's to remove.

@example
(zig-build-clean)
(zig-build-clean #:prefix \"build/out\")
@end example"
  (let* ((root (getcwd))
         (target (in-vicinity root (local-prefix prefix))))
    (delete-file-recursively target #:verbose? verbose?)))

(define (zig-build-steps . arguments)
  "Print the steps the project's `build.zig' offers, passing ARGUMENTS."
  (apply zig-build "--list-steps" arguments))

;; ====== The direct commands ======

(define (zig-build-exe . arguments)
  "Build an executable from source with `zig build-exe', passing ARGUMENTS."
  (apply zig "build-exe" arguments))

(define (zig-build-lib . arguments)
  "Build a library from source with `zig build-lib', passing ARGUMENTS."
  (apply zig "build-lib" arguments))

(define (zig-build-obj . arguments)
  "Build an object file with `zig build-obj', passing ARGUMENTS."
  (apply zig "build-obj" arguments))

(define (zig-test . arguments)
  "Run the tests in one source file with `zig test', passing ARGUMENTS.

This is Zig's direct test command, which takes a file.  A project whose
`build.zig' declares a test step runs all its tests with `zig-build-test'.
"
  (apply zig "test" arguments))

(define (zig-fmt . arguments)
  "Format the sources with `zig fmt', passing ARGUMENTS."
  (apply zig "fmt" arguments))

(define (zig-fetch . arguments)
  "Copy a package into Zig's global cache with `zig fetch', passing ARGUMENTS."
  (apply zig "fetch" arguments))

;; ====== Asking zig ======

(define* (zig-output argv #:key (allow-failure? #f))
  "Run the zig ARGV and return its standard output as a string.

The query runs even in a dry run: it reads the installed tool rather than
changing the project.  A failure raises a maak error naming the command, or
comes back as #f when the caller asked for the exit status instead."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     "zig-output expects a non-empty list of strings"
     #:hint "Build it with zig-argv, which returns exactly that."))
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
       (format #f "zig was terminated by a signal: ~a" line)
       #:hint "Run the command above by hand to read zig's own message."))
     (else
      (raise-maak-error
       (format #f "zig failed (exit code ~a): ~a" code line)
       #:hint "Run the command above to read zig's own message.")))))

(define (zig-version)
  "Return the version line the installed zig prints, trimmed."
  (string-trim-both (zig-output (zig-argv "version"))))
