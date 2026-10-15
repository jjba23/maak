;; maak

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; maak is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; maak is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with maak.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The command line entry point.
;;
;; Two things matter here beyond parsing the options.  First, a user-facing
;; failure must read as one line, not as a Guile backtrace: every error maak
;; raises itself is a `&maak-error' (see `(maak dsl)'), and this module prints
;; those as a short message with a hint.  Second, the load path is assembled
;; from several sources -- the `-L' flag, `MAAK_LOAD_PATH', and the directory
;; of the maak file itself -- so a project can require its own modules without
;; exporting GUILE_LOAD_PATH by hand.

;;; Code:

(define-module (maak main)
  #:declarative? #f
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 getopt-long)
  #:use-module (maak maak)
  #:use-module (maak dsl)
  #:use-module (maak version)
  ;; The error reporting helpers are readable from here so the test suite can
  ;; exercise the messages a user actually sees without starting a process.
  ;; They are *re*-exported: they are defined in `(maak dsl)', and an `#:export'
  ;; of a name that came in from another module binds a second, empty one here
  ;; that then shadows the import.
  #:re-export (exception->message error-message-string)
  #:export (main maak-option-spec maak-default-file resolve-maak-file
                 resolve-banner? resolve-env-files truthy-string?
                 resolve-print-directory?
                 report-maak-error report-generic-error))

(define maak-default-file
  "./maak.scm")

;; The option grammar shared with `scripts/maak'.  Keep the two in step: the
;; wrapper recognises the same flags so a mistyped option is caught before
;; Guile even starts.
(define maak-option-spec
  '((file (single-char #\f)
          (value #t)
          (required? #f))
    (resources (single-char #\r)
               (value #t)
               (required? #f))
    (list (single-char #\l)
          (value #f)
          (required? #f))
    (dry-run (single-char #\n)
             (value #f)
             (required? #f))
    (quiet (single-char #\q)
           (value #f)
           (required? #f))
    (tasks (single-char #\t)
           (value #t)
           (required? #f))
    ;; make's `-C': change to DIR before the Maak file, its environment files
    ;; and the tasks are looked at.  Several directories are given as one
    ;; colon separated list, as with `-L', and they are entered in order.
    (directory (single-char #\C)
               (value #t)
               (required? #f))
    ;; make's `-k': a task that fails stops nothing else.
    (keep-going (single-char #\k)
                (value #f)
                (required? #f))
    ;; make's `-i': the same, except that a run full of failures still succeeds.
    (ignore-errors (single-char #\i)
                   (value #f)
                   (required? #f))
    ;; make's `--no-print-directory', the switch that turns off the `-w' lines:
    ;; a run that changes directory says so unless it is asked not to.
    (no-print-directory (value #f)
                        (required? #f))
    ;; `getopt-long' keeps only the last occurrence of a single-valued
    ;; option, so several directories are given as one colon separated list,
    ;; exactly like PATH.  The wrapper script joins repeated -L flags that way.
    (load-path (single-char #\L)
               (value #t)
               (required? #f))
    ;; Environment files are named one per flag by the wrapper, which joins
    ;; them with ':' the way `-L' does.
    (env-file (single-char #\e)
              (value #t)
              (required? #f))
    (no-env-file (value #f)
                 (required? #f))
    (banner (value #f)
            (required? #f))
    (no-banner (value #f)
               (required? #f))
    (version (single-char #\v)
             (value #f)
             (required? #f))
    (debug (value #f)
           (required? #f))
    (help (single-char #\h)
          (value #f)
          (required? #f))))

;; ====== Error reporting ======
;;
;; Reading a caught error into one line -- `exception->message' and
;; `error-message-string' -- lives in `(maak dsl)', beside the error types,
;; because the library needs it too: a task handed words it does not take is
;; re-raised by the library with a hint of its own, and that hint has to carry
;; the message Guile raised.  They are still exported from here, so a caller
;; that imports `(maak main)' finds them where it always did.

(define (report-maak-error error)
  "Print a `&maak-error' ERROR as a short, red, backtrace-free message."
  (format (current-error-port)
          "\n\x1b[0;31mmaak: error:\x1b[0m ~a\n"
          (maak-error-message error))
  (when (maak-error-hint error)
    (format (current-error-port)
            "        hint: ~a\n"
            (maak-error-hint error))))

(define (report-generic-error key vals)
  "Print an unexpected error.

An unexpected failure is a bug in maak, or a genuine error in the maak file,
rather than something maak can explain, so the message says so and points at
the way to get the backtrace -- and, when it is a missing module, at the way to
put it on the load path."
  (let ((message (exception->message key vals))
        (unbound (string-prefix? "Unbound variable: "
                                (exception->message key vals))))
    (format (current-error-port)
            "\n\x1b[0;31mmaak: error:\x1b[0m ~a\n"
            message)
    (when (string-contains message "no code for module")
      (format (current-error-port)
              "        hint: that module is not on the load path; add its directory with -L/--load-path or the MAAK_LOAD_PATH environment variable.\n"))
    (when unbound
      (format (current-error-port)
              "        hint: ~a is not bound here. A `use-modules' written inside a task body has no effect; import the module at the top level of the Maak file, or load it at run time with `resolve-module' and `module-ref'.\n"
              (substring message (string-length "Unbound variable: "))))
    (format (current-error-port)
            "        hint: rerun with MAAK_DEBUG=1 (or --debug) for the full backtrace.\n")))

;; ====== Environment files ======

(define (truthy-string? str)
  "Return #t when STR is set to something other than 0, false or no.

A switch that lives in the environment has to be given a value, and every
convention for `off' is accepted rather than only the empty string."
  (and str
       (not (member (string-downcase (string-trim str))
                    '("" "0" "false" "no")))
       #t))

(define (resolve-banner? options)
  "Return whether the greeting banner should be printed for OPTIONS.

The `--no-banner' flag wins over `--banner', which wins over the environment:
an explicit `--banner' is how a reader of a script asks for the banner back
after `MAAK_NO_BANNER' was exported for the shell as a whole."
  (cond
    ((option-ref options 'no-banner #f) #f)
    ((option-ref options 'banner #f) #t)
    ((truthy-string? (getenv "MAAK_NO_BANNER")) #f)
    (else #t)))

(define (resolve-print-directory? options)
  "Return whether the directories `-C/--directory' enters should be reported.

A run that changes directory says where it ended up, because a task that ran
somewhere else is the kind of thing that is worth knowing without asking.  The
report is what make's `-w' prints, and `--no-print-directory' is the switch of
make's that turns it off -- useful when the output of a run is read by
something other than a person.  `--quiet' asks for the same silence, among
other things, and so does not bring the lines back."
  (cond
    ((option-ref options 'no-print-directory #f) #f)
    ((option-ref options 'quiet #f) #f)
    (else #t)))

(define (resolve-env-files options file)
  "Return the environment files to read for OPTIONS and maak file FILE.

`--env-file' names them explicitly and, when given, is the only source;
otherwise `MAAK_ENV_FILE' is read, and failing that a `.env' beside the maak
file.  `--no-env-file' turns the whole mechanism off, which is what a
reproducible run wants -- the same command, no matter what a checkout happens
to contain."
  (cond
    ((option-ref options 'no-env-file #f) '())
    ((option-ref options 'env-file #f)
     => (lambda (value)
          (split-load-path value)))
    ((truthy-string? (getenv "MAAK_ENV_FILE"))
     (split-load-path (getenv "MAAK_ENV_FILE")))
    (else
     (let ((beside (and file
                        (format #f "~a/.env" (dirname file)))))
       (if (and beside (file-exists? beside)) (list beside) '())))))

;; ====== Load paths ======

(define (apply-load-paths! cli-paths file)
  "Add every load path of CLI-PATHS, the environment and FILE to `%load-path'.

CLI-PATHS come first so an explicit `-L' wins over everything else, then
`MAAK_LOAD_PATH', then the directory of the maak file so it can require
modules that live beside it.  Directories that do not exist are skipped."
  (let* ((file-dir (and file (dirname file)))
         (resolved (append cli-paths
                           (environment-load-paths)
                           (if (and file-dir
                                    (not (string=? file-dir ""))
                                    (not (string=? file-dir ".")))
                               (list file-dir) '()))))
    ;; `add-load-path!' prepends, so walk backwards to keep the order above.
    (for-each (lambda (dir)
                (when (and dir (file-exists? dir))
                  (add-load-path! dir)))
              (reverse resolved))
    resolved))

;; ====== Options ======

(define (normalize-args args)
  "Drop a leading `--' separator from ARGS.

A launcher commonly forwards the user's arguments after `--' so that Guile
itself does not interpret flags such as -h.  `getopt-long' would take that
separator as the end of the options, so it is removed here."
  (cond
    ((null? args) args)
    ((string=? "--" (car args)) (cdr args))
    ((and (pair? (cdr args))
          (string=? "--" (cadr args)))
     (cons (car args) (cddr args)))
    (else args)))

(define (parse-options args)
  "Parse ARGS, turning a bad command line into a helpful maak error."
  (catch #t
    (lambda ()
      (getopt-long (normalize-args args) maak-option-spec))
    (lambda (key . vals)
      (raise-maak-error
       (format #f "could not parse the command line: ~a"
               (exception->message key vals))
       #:hint "Run `maak --help` to see the accepted options."))))

(define (resolve-maak-file file)
  "Return the absolute path of the maak file to load, or raise a maak error.

FILE defaults to `maak-default-file' when the command line did not name one.
The path is canonicalised because `load' resolves a relative name against the
directory of the module that calls it -- from this module, `./maak.scm' would
be searched for as `maak/maak.scm' and could load a completely different file."
  (let ((path (or file maak-default-file)))
    (unless (file-exists? path)
      (raise-maak-error
       (format #f "Maak file not found: ~a" path)
       #:hint "Create it, or name another one with -f/--file."))
    (canonicalize-path path)))

(define (print-help resources)
  "Print the help screen, taken from `help.txt' in RESOURCES when available."
  (let ((help-file (and resources
                         (format #f "~a/help.txt" resources))))
    (if (and help-file
             (file-exists? help-file))
        (display (call-with-input-file help-file get-string-all))
        (display "Usage: maak [OPTIONS] [-- [TASKS]... [TASK_ARGS]...]\n"))))

;; ====== Several tasks in one run ======

(define* (enter-directories! directories #:key (silent? #f))
  "Change to each of the colon separated DIRECTORIES in turn, in order.

SILENT? keeps the change of directory unannounced, which is what
`--no-print-directory' asks for, and what `--quiet' asks for before the quiet
parameter is in place.  A `-C' without either reports every directory it
enters, because a task that ran somewhere else is worth knowing about.

This is make's `-C/--directory': everything read after it -- the default
`./maak.scm', a relative `-f', the task arguments -- is read against the
directory it changed to, which is what makes a recursive `maak -C build' work.
A directory that is not there is an error rather than a silent fallback, so
`maak -C typo -- deploy' cannot deploy whatever the current directory holds."
  (for-each (lambda (directory)
              (unless (file-exists? directory)
                (raise-maak-error
                 (format #f "no such directory: ~a" directory)
                 #:hint "`-C/--directory DIR' changes to DIR before the Maak file is read."))
              (catch #t
                (lambda () (chdir directory))
                (lambda (key . vals)
                  (raise-maak-error
                   (format #f "could not change to directory: ~a (~a)"
                           directory (exception->message key vals))
                   #:hint "Check that the directory exists and that you may enter it.")))
              (unless (or silent? (quiet?))
                (log-info "maak: Entering directory '~a'\n" (getcwd))))
            (split-load-path directories)))

(define (report-task-failure task key . vals)
  "Print the failure of TASK and let the run carry on, for `--keep-going'.

The line reads like the one a run that stops prints, with the task named,
because a failure in the middle of a run of several has to say which one it
was.  KEY and VALS are the key and values the exception arrived with, which is
the shape `find-maak-error' reads: Guile hands a raised condition over as
`(%exception <the condition>)'."
  (let ((error (find-maak-error key vals)))
    (if error
        (begin
          (format (current-error-port)
                  "\n\x1b[0;31mmaak: error (task ~a):\x1b[0m ~a\n"
                  task
                  (maak-error-message error))
          (when (maak-error-hint error)
            (format (current-error-port)
                    "        hint: ~a\n"
                    (maak-error-hint error))))
        (report-generic-error key vals))))

(define (report-failed-tasks failed ignore-errors?)
  "Decide what a run does about the tasks in FAILED that did not succeed.

Each failure has already been printed where it happened.  IGNORE-ERRORS? turns
the lot into a warning, so the run still succeeds; otherwise the run fails with
one line naming the tasks that did not."
  (cond
    ((null? failed) #f)
    (else
     (let ((names (string-join (map symbol->string failed) ", ")))
       (if ignore-errors?
           (log-info "maak: warning: ~a task~a failed and --ignore-errors was given: ~a\n"
                     (length failed)
                     (if (null? (cdr failed)) "" "s")
                     names)
           (raise-maak-error
            (format #f "~a task~a failed: ~a"
                    (length failed)
                    (if (null? (cdr failed)) "" "s")
                    names)
            #:hint "Each failure is reported above with the message the task gave it."))))))

;; ====== Running ======

(define (run-maak options)
  "Carry out the action OPTIONS select: help, version, listing, or tasks."
  (let ((resources (option-ref options
                               'resources #f))
        (should-list (option-ref options
                                 'list #f))
        (should-version (option-ref options
                                    'version #f))
        (should-help (option-ref options
                                 'help #f))
        (should-dry-run (option-ref options
                                    'dry-run #f))
        (should-quiet (option-ref options
                                  'quiet #f)))
    (cond
      (should-help
       (print-help resources)
       0)
      (should-version
       (print-version)
       0)
      (else
       ;; `-C' comes first, before the Maak file is found and before an
       ;; environment file beside it is looked for, so a relative `-f' means
       ;; what it says inside the directory entered.
       (enter-directories! (option-ref options 'directory #f)
                           #:silent? (not (resolve-print-directory? options)))
       (let* ((file (resolve-maak-file (option-ref options
                                                   'file #f)))
              (raw-tasks (filter (lambda (task)
                                   (not (equal? task "")))
                                 (string-split (option-ref options
                                                           'tasks "")
                                               #\,)))
              (string-tasks (if (null? raw-tasks)
                                '("default") raw-tasks))
              (tasks (map string->symbol string-tasks))
              (task-args (option-ref options
                                     '() '()))
              (env-files (resolve-env-files options file))
              ;; `--ignore-errors' is `--keep-going' and then some: nothing
              ;; stops the run either way, and only the exit status differs.
              (keep-going? (or (option-ref options 'keep-going #f)
                               (option-ref options 'ignore-errors #f)))
              (ignore-errors? (option-ref options 'ignore-errors #f)))
         (apply-load-paths! (split-load-path (option-ref options
                                                           'load-path #f))
                            file)
         (parameterize ((dry-run? should-dry-run)
                        (quiet? should-quiet)
                        (banner? (resolve-banner? options)))
           ;; The environment is read before the maak file is loaded, so a task
           ;; file can already consult the variables it names.
           (for-each (lambda (env-file)
                       (unless (quiet?)
                         (log-info "  Reading environment from: ~a\n" env-file))
                       (load-env-file env-file))
                     env-files)
           (when (banner?)
             (log-separator)
             (log-info "Maak ~a: the infinitely extensible command runner\n"
                       maak-version)
             (log-info "Loading tasks from Maak file: ~a\n" file))
           (load file)
           (if should-list
               (list-tasks #:as-is? #f)
               (begin
                 (unless (quiet?)
                   (log-info "Executing Maak tasks: ~a\n"
                             (string-join (map symbol->string
                                               tasks) " =>")))
                 (report-failed-tasks
                  (run-tasks tasks
                             #:task-args task-args
                             #:keep-going? keep-going?
                             #:on-failure report-task-failure)
                  ignore-errors?)))
           0))))))

(define* (main #:key (args (command-line)))
  "Entry point for the maak command line interface.

Parse ARGS and either print help or the version, list the tasks, or run the
requested tasks.  Returns the process exit status: 0 on success, 1 when the
maak file or a task failed.  User-facing errors print as one line; set
`MAAK_DEBUG' (or pass --debug) to see the Guile backtrace instead."
  (let ((debug? (or (getenv "MAAK_DEBUG")
                    (and (member "--debug" args) #t))))
    (if debug?
        ;; With debugging on, let the exception reach Guile's own top level
        ;; handler, which prints the full backtrace.
        (run-maak (parse-options args))
        (catch #t
          (lambda ()
            (run-maak (parse-options args)))
          (lambda (key . vals)
            (let ((error (find-maak-error key vals)))
              (if error
                  (report-maak-error error)
                  (report-generic-error key vals)))
            1)))))
