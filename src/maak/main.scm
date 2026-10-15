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
  ;; The error reporting helpers are exported so the test suite can exercise
  ;; the messages a user actually sees, without starting a whole process.
  #:export (main maak-option-spec maak-default-file resolve-maak-file
                 exception->message error-message-string
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
    ;; `getopt-long' keeps only the last occurrence of a single-valued
    ;; option, so several directories are given as one colon separated list,
    ;; exactly like PATH.  The wrapper script joins repeated -L flags that way.
    (load-path (single-char #\L)
               (value #t)
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

(define (first-string-in lst)
  "Return the first string found in LST, or #f.

Guile spells an error raised with `error' as a key followed by a message and
its irritants; the message is the only part worth showing."
  (cond
    ((null? lst) #f)
    ((string? (car lst)) (car lst))
    (else (first-string-in (cdr lst)))))

(define (error-message-string key vals)
  "Return the most descriptive string Guile attached to a caught error.

The shape of the arguments depends on who raised the error.  A call to `error'
arrives as (#f MESSAGE () #f), where the message is the second element, while a
primitive failure such as `(car 1)' arrives as (#f MESSAGE (1) (1)) whose
*first* string is only the name of the procedure -- reporting that as the
message would hide the one sentence the user needs.  The second element is the
message in both shapes, so it is preferred, and the remaining values are
searched only when it is not a string, as when `throw' is handed a key and a
message but no irritants."
  (let* ((preferred (and (pair? vals)
                         (pair? (cdr vals))
                         (string? (cadr vals))
                         (cadr vals)))
         (candidates (if (string? key) vals (cons key vals))))
    (or preferred (first-string-in candidates))))

(define (fill-template message parts)
  "Fill the error TEMPLATE MESSAGE with the irritants that follow it in PARTS.

Guile keeps the arguments of a `scm-error' style message in a list of their
own, so the entry right after the message is the argument list: a failed module
lookup leaves `((no such module))' there, which formats as `(no such module)',
and `(error \"boom\" 'x 'y)' leaves `(x y)'.  A few raises hand over a single
value instead, which is wrapped so it still formats.  When the template cannot
be filled at all it is returned as it stands: showing a raw `~S' is better than
raising a second error while reporting the first."
  (let* ((after (cond
                  ((member message parts) => cdr)
                  (else '())))
         (irritants (cond
                      ((null? after) '())
                      ((list? (car after)) (car after))
                      (else (list (car after))))))
    (if (null? irritants)
        ;; Nothing to fill the template with, and `format' complains loudly on
        ;; the error port before it raises, so it is not even asked.  A message
        ;; that is only a template is still more use than silence.
        message
        (catch #t
          (lambda () (apply format #f message irritants))
          (lambda args message)))))

(define (exception->message key vals)
  "Return a one-line human message for a caught error.

It prefers an attached exception message, then the message among the raising
values, and only then falls back to the raw key.  Guile raises many errors with
a format *template* (for example `no code for module ~S') plus the arguments;
those are filled in here so the user never sees a bare `~S'."
  (let* ((object (if (exception? key) key
                     (if (pair? vals) (car vals) #f)))
         (attached (and (exception? object)
                        (exception-with-message? object)
                        (exception-message object)))
         (message (or attached
                      (error-message-string key vals))))
    (cond
      (attached attached)
      ((not (string? message)) (format #f "~a" key))
      ((string-contains message "~")
       (fill-template message (cons key vals)))
      (else message))))

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
                                     '() '())))
         (apply-load-paths! (split-load-path (option-ref options
                                                           'load-path #f))
                            file)
         (parameterize ((dry-run? should-dry-run)
                        (quiet? should-quiet))
           (unless (quiet?)
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
                 (run-tasks tasks
                            #:task-args task-args)))
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
