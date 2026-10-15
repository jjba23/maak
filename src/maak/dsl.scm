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

(define-module (maak dsl)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 getopt-long)
  #:export (log-message log-info
                        log-error
                        log-separator
                        get-module-var
                        delete-file-recursively
                        manifest-shell
                        program-shell
                        mkdir-p
                        bold-output
                        cp
                        mv
                        quiet?
                        dry-run?
                        banner?
                        remkdir-p
                        maak-proc?
                        maak-task?                        $
                        ~
                        environment-assignments
                        decorate-command
                        time-machine
                        time-machine-manifest-shell
                        time-machine-program-shell
                        syscall
                        cat
                        ;; ====== Errors ======
                        &maak-error
                        make-maak-error
                        maak-error?
                        maak-error-message
                        maak-error-hint
                        raise-maak-error
                        find-maak-error
                        task-error
                        exception->message
                        error-message-string
                        ;; ====== Load paths ======
                        maak-load-path
                        add-load-path!
                        add-load-paths!
                        with-load-path
                        split-load-path
                        environment-load-paths
                        ;; ====== Pretty strings ======
                        string-lines
                        unlines
                        unwords
                        blank-line?
                        leading-whitespace-count
                        dedent-string
                        indent-string
                        text
                        ;; ====== Guix shells and containers ======
                        guix-shell
                        guix-container
                        guix-pack))

(define (log-message msg)
  (unless (quiet?)
    (display (format #f "\n~a" msg))))

(define (log-separator)
  (log-message "
--------------------------------------------------------------
"))

(define (log-info msg . args)
  (log-message (format #f "~a"
                       (apply format
                              (append (list #f msg) args)))))

(define (log-error msg . args)
  (log-message (format #f "~a"
                       (apply format
                              (append (list #f msg) args)))))

;; ====== Maak errors ======
;;
;; A maak error is a user-facing failure: a missing task, an unreadable maak
;; file, a shell command that exited non-zero.  They carry a message and an
;; optional hint, and the top level (`maak main') prints them without the Guile
;; backtrace that would otherwise bury the one line that matters.  Everything
;; the program raises on purpose goes through `raise-maak-error'.

(define-exception-type &maak-error &error
  make-maak-error maak-error?
  (message maak-error-message)
  (hint maak-error-hint))

(define* (raise-maak-error message #:key (hint #f))
  "Raise a `&maak-error' with MESSAGE and an optional HINT.

MESSAGE is the single line the user reads; HINT, when given, is printed
underneath as a suggestion for what to do next.  Prefer this over `error' in
library code so the CLI can keep the report short."
  (raise-exception (make-maak-error message hint)))

(define (find-maak-error key vals)
  "Return the `&maak-error' among KEY and VALS, or `#f'.

Guile hands a `catch' handler the raising key followed by its values; a
`raise-exception' of our condition type arrives as the first value, so both are
searched.  This is what lets `main' tell our own messages apart from internal
errors."
  (let loop ((objs (cons key vals)))
    (cond
      ((null? objs) #f)
      ((maak-error? (car objs)) (car objs))
      (else (loop (cdr objs))))))

(define* (task-error message #:key (hint #f) (args '()))
  "Raise a user-facing error from a task.

This is what a task calls when it cannot do its job: the message is printed as
one line, the hint (when given) underneath it, and no Guile backtrace follows.
ARGS, when given, are handed to `format' to build the message, so a task can
report the value that made it give up.

@example
(task-error \"no compiled artefact for ~a\" #:args (list host))
(task-error \"the deploy key is missing\"
            #:hint \"Set DEPLOY_KEY, or run maak deploy --dry-run.\")
@end example"
  (raise-maak-error (if (null? args)
                        (format #f "~a" message)
                        (apply format #f message args))
                    #:hint hint))

;; ====== Reading a caught error ======
;;
;; What `catch' hands over is not yet a sentence: a raise arrives as a key and
;; a list of values whose shape depends on who raised it, and Guile spells many
;; of them as a `format' template plus the arguments to fill it with.  The two
;; readers below turn that into the one line a user reads.  They live beside the
;; error types rather than in the command line, because the library needs them
;; where the command line does: `main' reports a task's failure, and the library
;; re-raises the one it can explain itself -- a task given words it does not take.

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

(define dry-run?
  (make-parameter #f))

(define quiet?
  (make-parameter #f))

;; Whether the greeting banner and the "loading tasks from" line are printed.
;; It is a parameter rather than a plain flag so a task can turn it off for a
;; run it starts itself, and on again afterwards.
(define banner?
  (make-parameter #t))

;; ====== Command decorations ======
;;
;; `$' builds one shell command line, and the things a task often needs around
;; a command -- running it as root, in a modified environment, without output,
;; in the background, or without minding whether it fails -- are all
;; decorations of that line.  They are applied in one place and in one order,
;; the order someone would write by hand, so a command printed by `--dry-run'
;; can be pasted straight into a shell:
;;
;;   [sudo] [env A=1 B=2] COMMAND [>/dev/null 2>&1] [&]
;;
;; Mindering whether a command fails is deliberately *not* one of them: `cmd ||
;; true' would tell the shell to report success, and maak would then have no
;; way to tell a task whether its command actually worked.  `#:allow-failure?'
;; instead reads the real exit code and returns it, so the task keeps the
;; decision and the printed command stays the one that runs.

(define (environment-assignments env)
  "Return ENV as a list of `KEY=VALUE' strings.

ENV may be #f, a single string, a list of strings, or an alist of pairs, so a
caller can pass whatever shape it already has.  An entry that is not a pair is
used as it stands, which is what makes the plain string list useful when a
value cannot be written as a literal."
  (cond
    ((not env) '())
    ((string? env) (list env))
    ((list? env)
     (map (lambda (entry)
            (if (pair? entry)
                (format #f "~a=~a" (car entry) (cdr entry))
                (format #f "~a" entry)))
          env))
    (else '())))

(define* (decorate-command cmd
                           #:key (env '()) (sudo? #f) (dev-null? #f)
                           (async? #f))
  "Return CMD with the decorations a task asked for, applied in a fixed order.

The environment comes first and goes through `env' rather than as a `KEY=VALUE'
prefix, because everything after a `guix shell --' is executed directly: a
prefix there would be looked up as a program name and fail with 127.

A backgrounded command is never waited for, so it never reports a status: the
command counts as started rather than as succeeded.  Whether a failure is
allowed is not a decoration of the command line at all; `syscall' reads the
exit code, which a `|| true' in the line would have hidden."
  (let* ((assignments (environment-assignments env))
         (with-env (if (null? assignments)
                       cmd
                       (format #f "env ~a ~a"
                               (string-join assignments " ") cmd)))
         (with-sudo (if sudo? (format #f "sudo ~a" with-env) with-env))
         (with-output (if dev-null?
                          (format #f "~a >/dev/null 2>&1" with-sudo)
                          with-sudo)))
    (if async? (format #f "~a &" with-output) with-output)))

(define* (syscall cmd
                  #:key (allow-failure? #f) (async? #f))
  "Executes a shell command and checks its exit code.

@sc{arguments}:
@itemize @bullet

@item @strong{cmd} (string):
The shell command to be executed.

@item @strong{#:allow-failure?} (boolean, default #f):
If #t, a non-zero exit code does not raise: it comes back as @code{#f}, the way
`cmd || true' would keep a shell going, except that the task also learns that
the command failed.

@item @strong{#:async?} (boolean, default #f):
If #t, the command is not waited for and its status is not checked.

@end itemize

@sc{returns}:
@code{#t} if the command completes with an exit code of 0, @code{#f} if it
failed and failures are allowed.

@sc{errors}:
  Signals an error if the command returns a non-zero exit code,
  providing the command and the exit code in the error message."
  (cond
    ((dry-run?)
     (unless (quiet?)
       (display (bold-output (format #f "\n[DRY-RUN]: ~a\n" cmd)))) #t)
    (else (unless (quiet?)
            (log-info "Executing: ~a\n" cmd))
          (let* ((exit-val (system cmd))
                 (exit-code (status:exit-val exit-val)))
            (cond
              ((zero? exit-code) #t)
              ;; A signalled command (segfault, SIGTERM) has no exit code; say
              ;; so rather than reporting a misleading number.
              ((not exit-code)
               (if allow-failure?
                   #f
                   (raise-maak-error
                    (~ "command was terminated by a signal: ~a" cmd)
                    #:hint
                    "The command above did not exit normally.  Re-run it by hand to see why.")))
              (else
               (if allow-failure?
                   #f
                   (raise-maak-error
                    (~ "command failed (exit code ~a): ~a" exit-code cmd)
                    #:hint
                    "Fix the command above, or run `maak --dry-run' to inspect it first."))))))))

(define* ($ cmd-parts
            #:key (verbose? #f)
            (join " ")
            (env '())
            (sudo? #f)
            (dev-null? #f)
            (allow-failure? #f)
            (async? #f))
  "Executes a shell command constructed from a list of strings.

@example
($ '(\"echo -n\" \"hello-world\"))

;; as root, in the background, quietly, and without caring whether it fails
($ '(\"systemctl daemon-reload\")
   #:sudo? #t #:async? #t #:dev-null? #t #:allow-failure? #t)
@end example

@sc{arguments}:
@itemize @bullet

@item @strong{cmd-parts} (list):
A list of strings that will be joined to form the command.

@item @strong{#:verbose?} (boolean, default #f):
If #t, logs the generated command before executing it.

@item @strong{#:join} (string, default \" \"):
A string to use as a separator when joining the command parts.

@item @strong{#:env} (alist, list of strings, or string, default '()):
Environment variables to set for this command only, given as an alist
(@code{((\"CI\" . \"1\"))}), as @code{\"KEY=VALUE\"} strings, or as one such
string.  They are passed through @code{env} and reach the command itself.

@item @strong{#:sudo?} (boolean, default #f):
If #t, the command runs through @code{sudo}.

@item @strong{#:dev-null?} (boolean, default #f):
If #t, both standard output and standard error are discarded to
@code{/dev/null}.

@item @strong{#:allow-failure?} (boolean, default #f):
If #t, a non-zero exit code does not raise: @code{#f} is returned instead, so a
task can decide what a failure means.  The command line itself is left alone,
which is what lets the task be told the command failed.

@item @strong{#:async?} (boolean, default #f):
If #t, the command is started with a trailing @code{&} and not waited for.

@end itemize

@sc{returns}:
The result of the @code{syscall} function: @code{#t} if the command completes
with an exit code of 0, @code{#f} if it failed and @code{#:allow-failure?} was
given."
  (let ((cmd (decorate-command (string-join cmd-parts join)
                               #:env env
                               #:sudo? sudo?
                               #:dev-null? dev-null?
                               #:async? async?)))
    (cond
      ((dry-run?)
       (unless (quiet?)
         (display (bold-output (format #f "\n[DRY-RUN]: ~a\n" cmd)))) #t)
      (else (begin
              (when (and verbose?
                         (not (quiet?)))
                (log-info "  Execute command:\n~a\n" cmd))
              (syscall cmd
                      #:allow-failure? allow-failure?
                      #:async? async?))))))

(define-syntax-rule (get-module-var setting)
  "Get a variable from the currently loaded maak file module."
  (let* ((val (module-variable (resolve-module '(maak)) setting)))
    (catch #t
           (lambda ()
             (variable-ref val))
           (lambda (key . args)
             #f))))

(define (maak-task? x)
  "Predicate that indicates whether it is a valid maak task."
  (let* ((proc (variable-ref x)))
    (and proc
         (maak-proc? proc))))

(define (maak-proc? x)
  "Predicate that indicates whether a procedure is a valid maak proc."
  (procedure? x))

(define* (delete-file-recursively dir
                                  #:key (verbose? #f))
  "Delete a directory and all its contents recursively.
Runs `rm -rfv DIR` using the `$` command wrapper. This means:
The deletion is recursive.It forces removal without prompting (`-f`).It prints removed files (`-v`).

@sc{arguments}:
@itemize @bullet

@item
@strong{dir} (string):
The path of the directory (or file) to delete.

@item @strong{#:verbose?} (boolean, default #f):
If #t, logs the generated command before executing it.
@end itemize

@sc{returns}:
  #t if the command succeeded.

@sc{errors}:
  Signals an error if the underlying shell command exits with a non-zero status."
  ($ (list (~ "rm -rfv ~a" dir))
     #:verbose? verbose?))

(define* (mkdir-p dir
                  #:key (verbose? #f))
  "Create a directory and all necessary parent directories.
Executes `mkdir -pv DIR` using the `$` wrapper. This means:
Parent directories are created as needed (`-p`).
The operation prints each created directory (`-v`).

@sc{arguments}:
@itemize @bullet

@item @strong{dir}:
The path of the directory to create.

@item @strong{#:verbose?} (boolean, default #f):
If #t, logs the generated command before executing it.

@end itemize

@sc{returns}:
  #t if the command completed successfully.

@sc{errors}:
  Signals an error if the underlying shell command exits with a non-zero code."
  ($ (list (~ "mkdir -pv ~a" dir))))

(define* (remkdir-p dir
                    #:key (verbose? #f))
  "Remove a directory if it exists, then recreate it.
This results in an empty directory, regardless of its previous state.
Calls `delete-file-recursively` then `mkdir-p` on DIR.

@sc{arguments}:
@itemize @bullet

@item @strong{dir}:
The path of the directory to recreate.

@item @strong{#:verbose?} (boolean, default #f):
If #t, logs the generated command before executing it.
@end itemize

@sc{returns}: #t if the operations succeed.

@sc{errors}:
  Signals an error if any underlying shell command fails."
  (delete-file-recursively dir
                           #:verbose? verbose?)
  (mkdir-p dir
           #:verbose? verbose?))

(define* (mv x y
             #:key (verbose? #f))
  "Move or rename a file or directory.
  Executes `mv -v X Y` using the `$` wrapper. The `-v` flag prints
  each move operation performed.

@sc{arguments}:

@itemize @bullet
@item @strong{x} (string):
The source file or directory.

@item  @strong{y} (string):
The destination path.

@item @strong{#:verbose?} (boolean, default #f):
If #t, logs the generated command before executing it.

@end itemize

@sc{returns}: #t if the command succeeds.

@sc{errors}:
  Signals an error if the underlying shell command returns a non-zero exit code."
  ($ (list (~ "mv -v ~a ~a" x y))
     #:verbose? verbose?))

(define* (cat file
              #:key (verbose? #f))
  ($ (list (~ "cat ~a" file))
     #:verbose? verbose?))

(define* (cp x
             y
             #:key (recursive? #t)
             (force? #f)
             (verbose? #f)
             (verbose-copy? #t))
  "Copy a file or directory from X to Y.

@sc{arguments}:
  x: Source file or directory.
  y: Destination path.

  recursive?: If #t, copy directories recursively (`-r`). Defaults to #t.
  force?: If #t, overwrite existing files without prompting (`-f`). Defaults to #f.
  verbose?: If #t, logs the final shell command before execution. Defaults to #f.
  verbose-copy?: If #t, enables verbose output from `cp` (`-v`). Defaults to #t.

  Constructs and executes a command of the form:

    cp [OPTIONS] X Y

  using the `$` wrapper, ensuring errors are caught and logged.

@sc{returns}: #t if the copy succeeds.

@sc{errors}:
  Signals an error if the underlying `cp` command exits with a non-zero status."
  ($ (list (~ "cp ~a ~a ~a ~a ~a"
              (if recursive? "-r" "")
              (if force? "-f" "")
              (if verbose-copy? "-v" "")
              x
              y))
     #:verbose? verbose?))

(define* (~ #:rest args)
  "A utility function to format a string (interpolation). Alias for (apply format #f ARGS).

@example
(~ \"Hello world, my name is ~a and I am ~a years old\"
    \"Joe\"
    30)
@end example

@sc{arguments}:
@itemize @bullet

@item @strong{args} (list):
A list of arguments where the first element is the format string
and the subsequent elements are the values to be substituted.

@end itemize

@sc{returns}:
  A newly allocated string resulting from the formatting operation."
  (apply format
         (append '(#f) args)))

(define* (time-machine cmd-parts
                       #:key (verbose? #f)
                       (join " ")
                       (channels "channels.scm")
                       (env '())
                       (sudo? #f)
                       (dev-null? #f)
                       (allow-failure? #f)
                       (async? #f))
  "Execute a command using a pinned Guix revision via `guix time-machine`.

@sc{arguments}:
  cmd-parts: A list of strings representing the command to run
             after the `guix time-machine` invocation.

  #:verbose?: If #t, logs the final constructed command before running it.
            Defaults to #f.
  #:join: The string used to join command segments when constructing
        the final command. Defaults to a single space.
  #:channels: Path to the channels file used to pin the Guix revision.
            Defaults to \"channels.scm\".

  Constructs and executes a command of the form:
@example
    guix time-machine --channels=CHANNELS -- <cmd ...>
@end example

  using the `$` wrapper to run the resulting shell command safely.

@sc{returns}:  #t if the command succeeds.

@sc{errors}:
  Signals an error if the underlying command exits with a non-zero status."
  ($ (append (list (~ "guix time-machine --channels=~a" channels) "--")
             cmd-parts)
     #:verbose? verbose?
     #:join join
     #:env env
     #:sudo? sudo?
     #:dev-null? dev-null?
     #:allow-failure? allow-failure?
     #:async? async?))

(define* (manifest-shell cmd-parts
                         #:key (verbose? #f)
                         (pure? #f)
                         (join " ")
                         (manifest "manifest.scm")
                         (env '())
                         (sudo? #f)
                         (dev-null? #f)
                         (allow-failure? #f)
                         (async? #f))
  "Execute a command inside a Guix shell defined by a manifest file.

@sc{arguments}:

  cmd-parts: A list of strings representing the command to run
             inside the Guix shell environment.


  #:verbose?: If #t, logs the fully constructed command before execution.
            Defaults to #f.
  #:pure?: If #t, it will run in a pure isolated shell (clean).
            Defaults to #f.
  #:join: The string separator used when joining command segments.
        Defaults to a single space.
  #:manifest: Path to the manifest file (in Scheme) describing the
            package environment. Defaults to \"manifest.scm\".

  Constructs and executes a command of the form:
@example
    guix shell -m MANIFEST -- <cmd ...>
@end example

  using the `$` wrapper to enforce proper execution and error handling.

@sc{returns}: #t if the command succeeds.

@sc{errors}:
  Signals an error if the underlying shell command exits with a non-zero status."
  ($ (append (list (~ "guix shell -m ~a" manifest))
             ;; An empty segment would leave a double space in the printed
             ;; command, which `--dry-run' shows and a user copies.
             (if pure? (list "--pure") '())
             (list "--") cmd-parts)
     #:verbose? verbose?
     #:join join
     #:env env
     #:sudo? sudo?
     #:dev-null? dev-null?
     #:allow-failure? allow-failure?
     #:async? async?))

(define* (time-machine-manifest-shell cmd-parts
                                      #:key (verbose? #f)
                                      (pure? #f)
                                      (join " ")
                                      (channels "channels.scm")
                                      (manifest "manifest.scm")
                                      (env '())
                                      (sudo? #f)
                                      (dev-null? #f)
                                      (allow-failure? #f)
                                      (async? #f))
  "Execute a command inside a Guix shell defined by a manifest file,
using a pinned Guix revision via `guix time-machine`.

@sc{arguments}:
  cmd-parts: A list of strings representing the command to run inside
             the pinned Guix shell environment.

  #:verbose?: If #t, logs the composed command before execution.
            Defaults to #f.
  #:pure?: If #t, it will run in a pure isolated shell (clean).
            Defaults to #f.
  join: String used to join command segments. Defaults to a single space.
  #:channels: Path to the channels file used to pin the Guix revision.
            Defaults to \"channels.scm\".
  #:manifest: Path to the manifest file describing the package environment.
            Defaults to \"manifest.scm\".

  Constructs and executes a command of the form:
@example
    guix time-machine --channels=CHANNELS -- shell -m MANIFEST -- <cmd ...>
@end example

  This provides a reproducible shell environment defined by the manifest file
  and pinned to the Guix revision specified in the channels file.

@sc{returns}: #t if the command succeeds.

@sc{errors}:
  Signals an error if any underlying shell command exits with a non-zero status."
  ($ (append (list (~ "guix time-machine --channels=~a" channels) "--"
                   (~ "shell -m ~a" manifest))
             (if pure? (list "--pure") '())
             (list "--") cmd-parts)
     #:verbose? verbose?
     #:join join
     #:env env
     #:sudo? sudo?
     #:dev-null? dev-null?
     #:allow-failure? allow-failure?
     #:async? async?))

(define* (program-shell cmd-parts
                        #:key (verbose? #f)
                        (pure? #f)
                        (join " ")
                        (file "guix.scm")
                        (env '())
                        (sudo? #f)
                        (dev-null? #f)
                        (allow-failure? #f)
                        (async? #f))
  "Execute a command inside a Guix shell defined by a program file.

@sc{arguments}:
  cmd-parts: A list of strings representing the command to run
             inside the Guix shell environment.

  #:verbose?: If #t, logs the full constructed command before execution.
            Defaults to #f.

  #:join: String used to join command segments. Defaults to a single space.
  #:file: Path to the Guix program or manifest file defining the shell
        environment. Defaults to \"guix.scm\".


  Constructs and executes a command of the form:
@example
    guix shell -f FILE -- <cmd ...>
@end example

  using the `$` wrapper to safely run the command and check for errors.

@sc{returns}: #t if the command succeeds.

@sc{errors}:
  Signals an error if the underlying shell command exits with a non-zero status."
  ($ (append (list (~ "guix shell -f ~a" file))
             (if pure? (list "--pure") '())
             (list "--") cmd-parts)
     #:verbose? verbose?
     #:join join
     #:env env
     #:sudo? sudo?
     #:dev-null? dev-null?
     #:allow-failure? allow-failure?
     #:async? async?))

(define* (time-machine-program-shell cmd-parts
                                     #:key (verbose? #f)
                                     (pure? #f)
                                     (join " ")
                                     (channels "channels.scm")
                                     (file "guix.scm")
                                     (env '())
                                     (sudo? #f)
                                     (dev-null? #f)
                                     (allow-failure? #f)
                                     (async? #f))
  "Execute a command inside a Guix shell defined by a program file,
using a pinned Guix revision via `guix time-machine`.

@sc{arguments}:
  cmd-parts: A list of strings representing the command to run inside
             the pinned Guix shell environment.

  #:verbose?: If #t, logs the composed command before execution.
            Defaults to #f.
  #:pure?: If #t, it will run in a pure isolated shell (clean).
            Defaults to #f.
  #:join: String used to join command segments. Defaults to a single space.
  #:channels: Path to the channels file used to pin the Guix revision.
            Defaults to \"channels.scm\".
  #:file: Path to the Guix program file (e.g. guix.scm) defining the environment.
        Defaults to \"guix.scm\".


  Constructs and executes a command of the form:
@example
    guix time-machine --channels=CHANNELS -- shell -f FILE -- <cmd ...>
@end example

This provides a reproducible development environment defined by the program
file and pinned to the Guix revision specified in the channels file.

@sc{returns}: #t if the command succeeds.

@sc{errors}:
  Signals an error if any underlying shell command exits with a non-zero status."
  ($ (append (list (~ "guix time-machine --channels=~a" channels) "--"
                   (~ "shell -f ~a" file))
             (if pure? (list "--pure") '())
             (list "--") cmd-parts)
     #:verbose? verbose?
     #:join join
     #:env env
     #:sudo? sudo?
     #:dev-null? dev-null?
     #:allow-failure? allow-failure?
     #:async? async?))

;; ====== GUILE_LOAD_PATH facilities ======
;;
;; A maak file often needs to load a module that lives next to it, or in a
;; sibling `lib' directory.  Rather than asking every user to export
;; GUILE_LOAD_PATH by hand, maak offers the load path from three angles:
;;
;;   * the `-L'/`--load-path' command line flag (see `maak main'),
;;   * the `MAAK_LOAD_PATH' environment variable (a colon separated list),
;;   * and the `add-load-path!' / `with-load-path' builtins for tasks.
;;
;; All three funnel into Guile's own `%load-path', so they compose with
;; whatever GUILE_LOAD_PATH the user already has.

(define maak-load-path
  (make-parameter '()))

(define (split-load-path str)
  "Split a `PATH'-style colon separated STR into a list of directories.

An empty entry is dropped, so a trailing or doubled colon does not add the
current directory twice."
  (if (or (not str) (string=? str "")) '()
      (filter (lambda (part)
                (not (string=? part "")))
              (string-split str #\:))))

(define (environment-load-paths)
  "Return the directories named by `MAAK_LOAD_PATH'.

`GUILE_LOAD_PATH' is already honoured by Guile itself and is deliberately not
merged here; this is the maak-specific extra so a task can add paths without
clobbering the user's global setting."
  (split-load-path (getenv "MAAK_LOAD_PATH")))

(define (add-load-path! dir)
  "Prepend DIR to Guile's load path and record it as a maak load path.

Returns the canonical name of the directory that was added, which makes it
usable in a `let' when a task wants to report what it did.  This is a
procedure rather than the `add-to-load-path' syntax because a task computes
the directory at run time."
  (set! %load-path (cons dir %load-path))
  (maak-load-path (cons dir (maak-load-path)))
  dir)

(define (add-load-paths! dirs)
  "Prepend every directory of DIRS to the load path, in order.

Returns the list of directories that were added."
  (for-each add-load-path! (reverse dirs))
  dirs)

(define* (with-load-path dirs thunk)
  "Call THUNK with DIRS added to the load path, restoring it afterwards.

DIRS is a list of directory names.  The load path is saved and restored even
when THUNK raises, so a failing task does not leave the process with a
polluted search path.

@example
(with-load-path '(\"./lib\" \"./src\")
  (lambda () (use-modules (my project))))
@end example"
  (let ((saved %load-path))
    (dynamic-wind
     (lambda ()
       (for-each (lambda (dir)
                   (set! %load-path (cons dir %load-path))) dirs))
     thunk
     (lambda ()
       (set! %load-path saved)))))

;; ====== Pretty multiline strings ======
;;
;; Guile has no heredoc, so a long message ends up as a string spanning several
;; source lines -- with the source indentation baked into the value.  These
;; helpers strip that indentation back out so a docstring (or a message printed
;; by a task) reads as it was meant to.

(define (string-lines str)
  "Split STR on newlines into a list of lines."
  (string-split str #\newline))

(define (unlines lines)
  "Join LINES with newlines, adding no trailing newline."
  (string-join lines "\n"))

(define (unwords words)
  "Join WORDS with single spaces."
  (string-join words " "))

(define (blank-line? str)
  "Return #t when STR is empty or only whitespace."
  (string=? (string-trim str) ""))

(define (leading-whitespace-count str)
  "Return the number of leading space and tab characters in STR."
  (let loop ((i 0))
    (if (and (< i (string-length str))
             (let ((c (string-ref str i)))
               (or (char=? c #\space)
                   (char=? c #\tab))))
        (loop (+ i 1)) i)))

(define (dedent-string str)
  "Remove the common leading indentation from a multiline STR.

Tabs are expanded to four spaces, leading and trailing blank lines are
removed, and the smallest indentation shared by the remaining lines is
stripped from all of them.  Blank lines in the middle are preserved as empty
lines.

@example
(dedent-string \"   hello world\")
@result{} \"hello world\"
@end example"
  (let* ((expanded (string-replace-substring str "\t" "    "))
         (raw (string-split expanded #\newline))
         ;; Drop blank lines at both ends, keeping the ones in between.
         (trimmed (reverse (drop-while blank-line?
                                        (reverse (drop-while blank-line?
                                                             raw)))))
         (indents (delete #f
                          (map (lambda (line)
                                 (if (blank-line? line) #f
                                     (leading-whitespace-count line)))
                               trimmed)))
         (margin (if (null? indents) 0
                     (apply min indents))))
    (string-join (map (lambda (line)
                        (if (>= (string-length line) margin)
                            (substring line margin)
                            ""))
                      trimmed)
                 "\n")))

(define* (indent-string str #:key (by 2) (first? #t))
  "Indent every line of STR by BY spaces.

When FIRST? is #f the first line is left flush, which is what aligning a
continuation underneath an existing prefix needs."
  (let ((pad (make-string by #\space))
        (lines (string-split str #\newline)))
    (string-join (if first?
                     (map (lambda (line)
                            (string-append pad line)) lines)
                     (cons (car lines)
                           (map (lambda (line)
                                  (string-append pad line)) (cdr lines))))
                 "\n")))

;; The `text' macro returns a multiline string built from its LINE arguments,
;; de-indented.  Each argument is one line; the result has the common source
;; indentation removed, so a block such as
;;
;;   (text "first"
;;         "second")
;;
;; prints as two flush-left lines rather than carrying the indentation of the
;; source file.  It is a macro (and so has no `procedure-documentation') because
;; each line is an ordinary expression rather than a literal.
(define-syntax-rule (text line ...)
  (dedent-string (string-join (list line ...) "\n")))

;; ====== Guix shells, containers and packs ======

(define* (guix-shell cmd-parts
                     #:key (packages '())
                     (pure? #f)
                     (manifest #f)
                     (file #f)
                     (join " ")
                     (verbose? #f)
                     (env '())
                     (sudo? #f)
                     (dev-null? #f)
                     (allow-failure? #f)
                     (async? #f))
  "Run CMD-PARTS inside a Guix shell made of PACKAGES.

This is the package-list sibling of `manifest-shell' and `program-shell': the
environment is described inline rather than in a manifest or a `guix.scm'.
PACKAGES may be a list of strings (\"guile\", \"coreutils\") or a single
string, which is used as-is.

@example
(guix-shell '(\"guile -c '(+ 1 2)'\") #:packages '(\"guile\"))
@end example"
  (let ((pkgs (if (list? packages) packages (list packages))))
    ($ (append (list "guix shell")
               (if pure? (list "--pure") '())
               (if manifest (list (~ "-m ~a" manifest)) '())
               (if file (list (~ "-f ~a" file)) '())
               pkgs
               (list "--") cmd-parts)
       #:join join
       #:verbose? verbose?
       #:env env
       #:sudo? sudo?
       #:dev-null? dev-null?
       #:allow-failure? allow-failure?
       #:async? async?)))

(define* (guix-container cmd-parts
                         #:key (manifest "manifest.scm")
                         (pure? #f)
                         (network? #f)
                         (share '())
                         (expose '())
                         (join " ")
                         (verbose? #f)
                         (env '())
                         (sudo? #f)
                         (dev-null? #f)
                         (allow-failure? #f)
                         (async? #f))
  "Run CMD-PARTS in a Guix container defined by MANIFEST.

A container isolates the process from the host: no network unless NETWORK? is
#t, no access to the home directory unless it is SHARE'd.  SHARE and EXPOSE
are lists of `HOST=DEST' (or `HOST') strings passed through as `-S' and `-E'.

@example
(guix-container '(\"maak test\")
                #:manifest \"manifest.scm\"
                #:share '(\"/home/joe\"))
@end example"
  ($ (append (list (~ "guix shell --container -m ~a" manifest))
             (if pure? (list "--pure") '())
             (if network? (list "--network") '())
             (map (lambda (s)
                    (~ "-S ~a" s)) share)
             (map (lambda (s)
                    (~ "-E ~a" s)) expose)
             (list "--") cmd-parts)
     #:join join
     #:verbose? verbose?
     #:env env
     #:sudo? sudo?
     #:dev-null? dev-null?
     #:allow-failure? allow-failure?
     #:async? async?))

(define* (guix-pack cmd-parts
                    #:key (file "guix.scm")
                    (format "docker")
                    (image-tag #f)
                    (symlinks '())
                    (extra-packages '())
                    (join " ")
                    (verbose? #f)
                    (env '())
                    (sudo? #f)
                    (dev-null? #f)
                    (allow-failure? #f)
                    (async? #f))
  "Build a `guix pack' of the program in FILE.

FORMAT is any `guix pack --format' value (\"docker\", \"squashfs\",
\"tarball\").  IMAGE-TAG names a docker image, SYMLINKS are `TARGET=SOURCE'
entries for `-S', and EXTRA-PACKAGES are appended to the pack.

@example
(guix-pack '() #:format \"docker\" #:image-tag \"maak\"
           #:symlinks '(\"/bin=bin\") #:extra-packages '(\"bash\"))
@end example"
  ($ (append (list (~ "guix pack --file=~a" file)
                   (~ "--format=~a" format))
             (if image-tag (list (~ "--image-tag=~a" image-tag)) '())
             (map (lambda (s)
                    (~ "-S ~a" s)) symlinks)
             extra-packages)
     #:join join
     #:verbose? verbose?
     #:env env
     #:sudo? sudo?
     #:dev-null? dev-null?
     #:allow-failure? allow-failure?
     #:async? async?))

(define (bold-output x)
  (format #f "\x1b[1m~a\x1b[0m" x))
