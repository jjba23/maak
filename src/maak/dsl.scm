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
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 getopt-long)
  #:export (log-message log-info
                        log-error
                        log-separator
                        get-module-var
                        delete-file-recursively
                        rm-rf
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
                        elevation-command
                        detect-elevation-command
                        command-on-path?
                        time-machine
                        time-machine-manifest-shell
                        time-machine-program-shell
                        syscall
                        cat
                        ;; ====== Programs and background jobs ======
                        run
                        run-pipeline
                        stage
                        stage?
                        pipeline-result?
                        pipeline-ok?
                        pipeline-outputs
                        pipeline-errors
                        shell-quote
                        job?
                        job-pid
                        job-command
                        job-done?
                        job-exit-code
                        wait-job
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
;; a command -- running it as root, in a modified environment or without its
;; output -- are all decorations of that line.  They are applied in one place
;; and in one order, the order someone would write by hand, so a command
;; printed by `--dry-run' can be pasted straight into a shell:
;;
;;   [sudo|doas] [env A=1 B=2] COMMAND [>/dev/null 2>&1]
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

;; ====== Privilege escalation ======
;;
;; A task that needs root should not have to decide whether the machine uses
;; `sudo' or `doas'.  The command is a parameter, detected once from `PATH'
;; (`doas' wins when both are installed) and overridable through
;; `MAAK_ELEVATE' or lexically with `parameterize'.  Every `#:sudo?' in maak
;; goes through it, so a doas user never needs sudo installed to run a task that
;; changes something as root.

(define (executable-file? path)
  "Return #t when PATH names an executable file."
  (catch #t
    (lambda () (and (file-exists? path) (access? path X_OK)))
    (lambda args #f)))

(define (command-on-path? command path)
  "Return #t when COMMAND is an executable in one of the directories in PATH."
  (and (string? command)
       (not (string=? command ""))
       (let ((dirs (if path (string-split path #\:) '())))
         (any (lambda (dir)
                (executable-file?
                 (string-append (if (string=? dir "") "." dir)
                                "/" command)))
              dirs))))

(define* (detect-elevation-command
          #:key (path (getenv "PATH")) (preferred (getenv "MAAK_ELEVATE")))
  "Return the command maak should use to run something as root.

PREFERRED, when non-empty, wins unchanged, which is how `MAAK_ELEVATE' lets a
user name an escalation program maak has never heard of.  Otherwise `doas' is
chosen when it is on PATH, and `sudo' when it is not: `doas' is the smaller
tool and the one a machine that has it usually means, while `sudo' remains the
fallback every other system understands.

PATH is the search path, `PATH' itself by default, so the function can be
tested without changing the process environment.

@example
(detect-elevation-command #:path \"/usr/bin:/bin\")     ;; => \"sudo\"
(detect-elevation-command #:preferred \"doas -n\")      ;; => \"doas -n\"
@end example"
  (cond
    ((and preferred (not (string=? preferred ""))) preferred)
    ((command-on-path? "doas" path) "doas")
    (else "sudo")))

(define elevation-command
  (make-parameter (detect-elevation-command)))

(define (elevation-arguments)
  "Return `elevation-command' as the words of an argv.

The command may carry options (for example @code{MAAK_ELEVATE=doas -n}), so
it is split on whitespace, and the result is what `run' and a pipeline stage
put before the program.  An empty command means no escalation at all, which is
how a test or an unprivileged container turns `#:sudo?' into a no-op."
  (if (not (string? (elevation-command)))
      '()
      (string-tokenize (string-trim-both (elevation-command)))))

(define* (decorate-command cmd
                           #:key (env '()) (sudo? #f) (dev-null? #f))
  "Return CMD with the decorations a task asked for, applied in a fixed order.

The environment comes first and goes through `env' rather than as a `KEY=VALUE'
prefix, because everything after a `guix shell --' is executed directly: a
prefix there would be looked up as a program name and fail with 127.

Whether a failure is allowed is not a decoration of the command line at all;
`syscall' reads the exit code, which a `|| true' in the line would have hidden.
Backgrounding is not one either: a trailing `&' leaves nothing to wait on, so
`#:async?' forks instead and hands back a job (see `run')."
  (let* ((assignments (environment-assignments env))
         (with-env (if (null? assignments)
                       cmd
                       (format #f "env ~a ~a"
                               (string-join assignments " ") cmd)))
         (escalation (string-join (elevation-arguments) " "))
         (with-sudo (if (and sudo? (not (string=? escalation "")))
                        (format #f "~a ~a" escalation with-env)
                        with-env)))
    (if dev-null?
        (format #f "~a >/dev/null 2>&1" with-sudo)
        with-sudo)))

;; ====== Quoting and arguments ======
;;
;; `$' takes one string and leaves it to the shell; `run' takes the words it
;; needs as separate strings and starts the program directly.  The boundary
;; between the two is where a path with a space stops being three arguments.

(define (shell-quote str)
  "Return STR quoted so that a shell reads it as one literal word.

The POSIX recipe: wrap the value in single quotes and turn every embedded
single quote into the four characters backslash-quote-quote-quote.  Unlike
`~s', this is safe for spaces, tabs, `$', backticks and newlines."
  (string-append "'"
                 (string-join (string-split str #\')
                              (string-append "'" (string #\\ #\') "'"))
                 "'"))

(define (shell-safe-argument? arg)
  "Return #t when ARG can be printed without quotes and read back unchanged."
  (and (not (string=? arg ""))
       (every (lambda (c)
                (or (char-alphabetic? c)
                    (char-numeric? c)
                    (memv c '(#\- #\_ #\. #\/ #\= #\: #\@ #\% #\+ #\,))))
              (string->list arg))))

(define (argv->string argv)
  "Return ARGV as a command line a shell would read back as the same words.

Used for the dry-run line and for logging; what actually runs is the ARGV list
itself, so quoting is only there to make the printed command copyable."
  (string-join (map (lambda (arg)
                      (if (shell-safe-argument? arg) arg (shell-quote arg)))
                    argv)
               " "))

;; ====== Programs and background jobs ======
;;
;; A shell's trailing `&' starts a command and forgets it: there is no pid to
;; wait on and no status to read, so a task cannot tell a server that started
;; from one that died at once.  `spawn-process' forks and execs instead, which
;; is what lets `run' keep the program and its arguments separate and gives
;; `#:async?' something to hand back.

(define-record-type <job>
  (make-job pid command)
  job?
  (pid job-pid)
  (command job-command)
  ;; #f means "not reaped yet".  0 is a real wait status, so it cannot stand
  ;; for one.
  (status job-status set-job-status!))

(define* (spawn-process argv #:key (dev-null? #f) (stdin-null? #f))
  "Start ARGV in a child process and return its pid.

ARGV is a non-empty list of strings whose first element is the program.  The
child execs it directly, so no shell interprets the arguments.  With DEV-NULL?
its standard output and standard error are connected to `/dev/null'; with
STDIN-NULL? its standard input is.  A program that cannot be executed exits
127, the code a shell uses for a command it cannot find."
  ;; A child inherits whatever the parent still has buffered in its ports, so
  ;; that output would be written twice unless it is flushed before the fork.
  (force-output (current-output-port))
  (force-output (current-error-port))
  (let ((pid (primitive-fork)))
    (if (zero? pid)
        (begin
          (when stdin-null?
            (let ((null (open-file "/dev/null" "r")))
              (dup2 (port->fdes null) 0)
              (close-port null)))
          (when dev-null?
            (let ((null (open-file "/dev/null" "w")))
              (dup2 (port->fdes null) 1)
              (dup2 (port->fdes null) 2)
              (close-port null)))
          (catch #t
            (lambda () (apply execlp (car argv) argv))
            (lambda (key . args) (primitive-exit 127)))
          ;; Not reached: `execlp' either replaces the process or raises.
          (primitive-exit 127))
        pid)))

(define (reap-process pid)
  "Wait for PID and return its wait status."
  (cdr (waitpid pid 0)))

(define (reap-job job)
  "Wait for JOB when it has not been reaped yet and return its wait status."
  (or (job-status job)
      (let ((status (reap-process (job-pid job))))
        (set-job-status! job status)
        status)))

(define (job-done? job)
  "Return #t when JOB has finished, without waiting for it.

A finished job is reaped here, so a background process that a task never waits
for does not stay a zombie for the rest of the run."
  (cond
    ((job-status job) #t)
    (else
     (let ((result (waitpid (job-pid job) WNOHANG)))
       ;; WAITPID reports (0 . _) while the child is still running; that cdr is
       ;; not a status and must not be read.
       (if (zero? (car result))
           #f
           (begin
             (set-job-status! job (cdr result))
             #t))))))

(define (job-exit-code job)
  "Wait for JOB when needed and return its exit code.

Returns #f when the process was terminated by a signal, which is also what
`status:exit-val' says about such a status."
  (status:exit-val (reap-job job)))

(define (wait-job job)
  "Wait for JOB to finish and return #t when it exited with code 0.

Returns #f for a non-zero exit or a signal.  Waiting twice is fine: the status
is remembered after the first wait."
  (let ((code (job-exit-code job)))
    (and code (zero? code))))

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
If #t, the command is started in the background and a job record is returned
instead of a boolean.  The command still runs through a shell, so pipes and
redirections keep working, but the fork is maak's: `job-pid', `job-done?',
`wait-job' and `job-exit-code' can read the process.

@end itemize

@sc{returns}:
@code{#t} if the command completes with an exit code of 0, @code{#f} if it
failed and failures are allowed, and a job record when @code{#:async?} was
given.

@sc{errors}:
  Signals an error if the command returns a non-zero exit code,
  providing the command and the exit code in the error message."
  (cond
    ((dry-run?)
     (unless (quiet?)
       (display (bold-output (format #f "\n[DRY-RUN]: ~a~a\n"
                                     cmd
                                     (if async? " &" "")))))
     #t)
    (else (unless (quiet?)
            (log-info "Executing: ~a\n" cmd))
          (if async?
              ;; A shell's `&' redirects a background job's stdin from
              ;; /dev/null; the fork has to do it itself to keep that.
              (make-job (spawn-process (list "sh" "-c" cmd)
                                       #:stdin-null? #t)
                        cmd)
              (let* ((exit-val (system cmd))
                     (exit-code (status:exit-val exit-val)))
                (cond
                  ((zero? exit-code) #t)
                  ;; A signalled command (segfault, SIGTERM) has no exit code;
                  ;; say so rather than reporting a misleading number.
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
                        "Fix the command above, or run `maak --dry-run' to inspect it first.")))))))))

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
If #t, the command runs through the configured privilege escalation command --
@code{doas} when it is installed, @code{sudo} otherwise, or whatever
@code{MAAK_ELEVATE} names.  See @code{elevation-command}.

@item @strong{#:dev-null?} (boolean, default #f):
If #t, both standard output and standard error are discarded to
@code{/dev/null}.

@item @strong{#:allow-failure?} (boolean, default #f):
If #t, a non-zero exit code does not raise: @code{#f} is returned instead, so a
task can decide what a failure means.  The command line itself is left alone,
which is what lets the task be told the command failed.

@item @strong{#:async?} (boolean, default #f):
If #t, the command is started in the background and a job record is returned
(see @code{run}), instead of a boolean.

@end itemize

@sc{returns}:
The result of the @code{syscall} function: @code{#t} if the command completes
with an exit code of 0, @code{#f} if it failed and @code{#:allow-failure?} was
given, and a job record when @code{#:async?} was given."
  (let ((cmd (decorate-command (string-join cmd-parts join)
                               #:env env
                               #:sudo? sudo?
                               #:dev-null? dev-null?)))
    (cond
      ((dry-run?)
       (unless (quiet?)
         (display (bold-output (format #f "\n[DRY-RUN]: ~a~a\n"
                                       cmd
                                       (if async? " &" "")))))
       #t)
      (else (begin
              (when (and verbose?
                         (not (quiet?)))
                (log-info "  Execute command:\n~a\n" cmd))
              (syscall cmd
                      #:allow-failure? allow-failure?
                      #:async? async?))))))

(define* (run argv
             #:key (verbose? #f)
             (env '())
             (sudo? #f)
             (dev-null? #f)
             (allow-failure? #f)
             (async? #f)
             (expand? #f))
  "Run a program and its arguments without a shell.

ARGV is a list of strings: the program to run, followed by one string per
argument.  Nothing is split or interpolated, so a path with a space in it stays
one argument.  This is the default way to run a program; `$' is for the lines
that genuinely need a shell -- pipes, redirections, globs, `$(...)' and
`VAR=value' prefixes.

With @code{#:expand? #t} the arguments are joined with spaces and handed to
the shell, so wildcards, `~' and `$VAR' are expanded the way a shell expands
them.  That is opt-in because the shell sees words rather than arguments: a
path with a space in it becomes several words again, which is exactly what a
caller asking for a pattern wants and exactly what the default prevents.

@example
(run (list \"mkdir\" \"-p\" build-dir))
(run (list \"git\" \"commit\" \"-m\" message))

;; start a server in the background and wait for it later
(let ((server (run (list \"guix\" \"shell\" \"--\" \"run-server\")
                   #:async? #t)))
  (log-info \"server started as ~a\n\" (job-pid server))
  (wait-job server))
@end example

@sc{arguments}:
@itemize @bullet

@item @strong{argv} (list):
The program and its arguments, each a string.

@item @strong{#:verbose?} (boolean, default #f):
If #t, logs the command before running it.

@item @strong{#:env} (alist, list of strings, or string, default '()):
Environment variables for this program only, in the shapes `$' accepts.

@item @strong{#:sudo?} (boolean, default #f):
If #t, the program runs through the configured privilege escalation command --
@code{doas} when it is installed, @code{sudo} otherwise, or whatever
@code{MAAK_ELEVATE} names.  See @code{elevation-command}.

@item @strong{#:dev-null?} (boolean, default #f):
If #t, standard output and standard error are connected to @code{/dev/null}.

@item @strong{#:allow-failure?} (boolean, default #f):
If #t, a non-zero exit code does not raise: @code{#f} is returned instead.

@item @strong{#:async?} (boolean, default #f):
If #t, the program is started in the background and a job record is returned.

@item @strong{#:expand?} (boolean, default #f):
If #t, ARGV is joined into one shell line and the shell expands wildcards,
`~' and `$VAR' in it.  The arguments are then words rather than literals.

@end itemize

@sc{returns}:
@code{#t} if the program exits with code 0, @code{#f} if it failed and
failures were allowed, and a job record when @code{#:async?} was given.

@sc{errors}:
Signals a maak error when the program exits non-zero and failures are not
allowed."
  (when (or (not (list? argv))
            (null? argv)
            (not (every string? argv)))
    (raise-maak-error
     "run expects a non-empty list of strings, starting with the program"
     #:hint "Write (run (list \"mkdir\" \"-p\" dir)) instead of one long command."))
  (if expand?
      ;; The words are handed to the shell as written, which is what expands
      ;; `*', `~' and `$VAR'.  Everything else is the same as `$'.
      ($ (list (string-join argv " "))
         #:verbose? verbose?
         #:env env
         #:sudo? sudo?
         #:dev-null? dev-null?
         #:allow-failure? allow-failure?
         #:async? async?)
      (let* ((assignments (environment-assignments env))
         (parts (append (if sudo? (elevation-arguments) '())
                        (if (null? assignments)
                            '()
                            (append (list "env") assignments))
                        argv))
         (command (argv->string argv)))
    (cond
      ((dry-run?)
       (unless (quiet?)
         (display (bold-output (format #f "\n[DRY-RUN]: ~a~a\n"
                                       (argv->string parts)
                                       (if async? " &" "")))))
       #t)
      (else
       (when (and verbose? (not (quiet?)))
         (log-info "  Execute command:\n~a\n" command))
       ;; A shell's `&' redirects a background job's stdin from /dev/null;
       ;; the fork has to do it itself to keep that.
       (let ((pid (spawn-process parts
                                 #:dev-null? dev-null?
                                 #:stdin-null? async?)))
         (if async?
             (make-job pid command)
             (let* ((status (reap-process pid))
                    (code (status:exit-val status)))
               (cond
                 ((and code (zero? code)) #t)
                 ;; A signalled program has no exit code; say so rather than
                 ;; reporting a misleading number.
                 ((not code)
                  (if allow-failure?
                      #f
                      (raise-maak-error
                       (~ "command was terminated by a signal: ~a" command)
                       #:hint
                       "The program above did not exit normally.  Re-run it by hand to see why.")))
                 (else
                  (if allow-failure?
                      #f
                      (raise-maak-error
                       (~ "command failed (exit code ~a): ~a" code command)
                       #:hint
                       "Fix the command above, or run `maak --dry-run' to inspect it first."))))))))))))

(define* (pipeline-stage-pid argv input output #:key (dev-null? #f))
  "Start one stage of a pipeline and return its pid.

INPUT is the read side of the pipe from the previous stage, or #f to inherit
standard input; OUTPUT is the pipe to the next stage, or #f to inherit standard
output.  A `pipe' is a pair of ports, so the child closes both of them after
the `dup2' and no stage keeps another stage's end open.  With DEV-NULL? its
standard output is connected to @code{/dev/null} instead, which is where the
copy process of a captured last stage puts what it has already written."
  ;; Flush before forking, so the parent's buffered output is not written a
  ;; second time by the child.
  (force-output (current-output-port))
  (force-output (current-error-port))
  (let ((pid (primitive-fork)))
    (if (zero? pid)
        (begin
          (when input
            (dup2 (port->fdes input) 0)
            (close-port input))
          (when dev-null?
            (let ((null (open-file "/dev/null" "w")))
              (dup2 (port->fdes null) 1)
              (close-port null)))
          (when output
            (dup2 (port->fdes (cdr output)) 1)
            (close-port (car output))
            (close-port (cdr output)))
          (catch #t
            (lambda () (apply execlp (car argv) argv))
            (lambda (key . args) (primitive-exit 127)))
          ;; Not reached: `execlp' either replaces the process or raises.
          (primitive-exit 127))
        pid)))

;; ====== Pipelines ======
;;
;; A pipeline is a list of stages, each a program and its arguments.  A stage
;; may carry the decorations `run' takes -- its own environment, and `sudo?' --
;; because a middle stage is often the one that needs them: `sort' reading a
;; locale, or `tee' writing where only root may write.  A stage may also name a
;; file its output is kept in, and the pipeline as a whole may capture what
;; every stage wrote, which is how a task reads a result out of a pipeline
;; instead of running it a second time.  The pipeline takes an environment of
;; its own for all its stages, and it is the pipeline, not the stage, that can
;; be started as a background job: a stage cannot finish before the stage after
;; it has read what it wrote.

(define-record-type <stage>
  (make-stage argv env sudo? tee)
  stage?
  (argv stage-argv)
  (env stage-env)
  (sudo? stage-sudo?)
  (tee stage-tee))

(define* (stage argv #:key (env '()) (sudo? #f) (tee #f))
  "Describe one stage of a pipeline, with the options `run' takes.

ARGV is a program and its arguments, in the shape `run' takes them.  ENV sets
names for this stage alone, in the shapes `$' accepts; they are added to the
pipeline's own environment, later names winning.  With SUDO? the stage runs
through the configured privilege escalation command (see `elevation-command').
TEE names a file that the stage's standard output is also
written to, the way `tee' does it: the stage after this one reads the same
bytes, and the file keeps them for the task to look at once the pipeline is
done.

@example
(run-pipeline (list (stage (list \"grep\" \"-v\" \"^#\")
                           #:env '(\"LC_ALL=C\")
                           #:tee \"clean.txt\")
                    (stage (list \"sort\" \"-u\"))))
@end example

A stage is a description only; `run-pipeline' starts it."
  (when (or (not (list? argv))
            (null? argv)
            (not (every string? argv)))
    (raise-maak-error
     "stage expects a non-empty list of strings, starting with the program"
     #:hint "Write (stage (list \"grep\" pattern) #:env '(\"LC_ALL=C\"))."))
  (when (and tee (or (not (string? tee)) (string=? tee "")))
    (raise-maak-error
     (format #f "stage expects #:tee to name a file, got ~s" tee)
     #:hint "Write (stage (list \"grep\" pattern) #:tee \"clean.txt\")."))
  (make-stage argv env sudo? tee))

(define (pipeline-stage-spec? spec)
  "Return #t when SPEC is something `run-pipeline' can run as a stage."
  (or (stage? spec)
      (and (list? spec) (pair? spec) (every string? spec))))

(define (pipeline-stage-argv spec pipeline-env)
  "Return SPEC as the argv to exec, with PIPELINE-ENV and its own options.

The command line is built the way `run' builds one: the privilege escalation
command first, then `env' with the assignments, then the program and its
arguments.  That is what lets an assignment reach a stage inside a pipeline,
where the shell that usually reads it is not there."
  (let* ((argv (if (stage? spec) (stage-argv spec) spec))
         (assignments (append (environment-assignments pipeline-env)
                              (environment-assignments
                               (if (stage? spec) (stage-env spec) '()))))
         (sudo? (and (stage? spec) (stage-sudo? spec))))
    (append (if sudo? (elevation-arguments) '())
            (if (null? assignments)
                '()
                (append (list "env") assignments))
            argv)))

(define (pipeline-record-file index)
  "Return a temporary file of this run's own for the output of stage INDEX.

It is only ever used for the span of one pipeline: the bytes are read back when
every stage has finished and the file is removed then, so the name does not
have to be unique beyond the process that made it."
  (format #f "~a/maak-pipeline-~a-~a.log"
          (or (getenv "TMPDIR") "/tmp") (getpid) index))

;; A step is a stage after the environment has been folded into its command
;; line, together with the file its output is recorded in, if any.  Recording
;; is what makes the stage worth a copy process: its output has to reach the
;; next stage and the file at the same time, from a process the task itself
;; never waits on by name.
(define-record-type <pipeline-step>
  (make-pipeline-step argv record-path temporary?)
  pipeline-step?
  (argv pipeline-step-argv)
  (record-path pipeline-step-record-path)
  ;; #t when RECORD-PATH is a file of ours, made because the pipeline's output
  ;; is captured, rather than one the task asked for with `#:tee'.
  (temporary? pipeline-step-temporary?))

(define-record-type <pipeline-result>
  (make-pipeline-result outputs errors)
  pipeline-result?
  (outputs pipeline-outputs)
  (errors pipeline-errors))

(define (pipeline-ok? result)
  "Return #t when every stage of the pipeline that produced RESULT succeeded.

RESULT is what `run-pipeline' returns when asked to capture the output, whether
or not failures were allowed."
  (null? (pipeline-errors result)))

(define (pipeline-steps stages env capture?)
  "Return STAGES as the steps to run, with ENV folded in and files to record.

A stage records its standard output when it names a file with `#:tee', and
every stage records it when CAPTURE? is #t; a captured stage that named no
file of its own gets a temporary one."
  (let loop ((remaining stages) (index 0) (steps '()))
    (if (null? remaining)
        (reverse steps)
        (let* ((spec (car remaining))
               (tee (and (stage? spec) (stage-tee spec)))
               (record (or tee
                           (and capture? (pipeline-record-file index)))))
          (loop (cdr remaining)
                (+ index 1)
                (cons (make-pipeline-step (pipeline-stage-argv spec env)
                                          record
                                          (and record (not tee)))
                      steps))))))

(define (pipeline-rendered steps)
  "Return STEPS as the shell line that would do the same thing.

A stage recorded in a file of the task's own is followed by the `tee' that
plays the copy process's part, so the line can be pasted into a shell.  A
captured stage writes to a temporary file the task never sees, which is left
out: the commands themselves are what the line is for."
  (string-join
   (append-map (lambda (step)
                 (let ((argv (argv->string (pipeline-step-argv step)))
                       (record (pipeline-step-record-path step)))
                   (if (and record (not (pipeline-step-temporary? step)))
                       (list argv (argv->string (list "tee" record)))
                       (list argv))))
               steps)
   " | "))

(define (pipeline-check-records! steps)
  "Make sure every step can write the file its output is recorded in.

STEPS are the steps of a pipeline that is about to run.  The files are created
here, before any stage is started, so a path that cannot be written is reported
against the stage that would have lost its output rather than as a pipeline
that failed for no stated reason."
  (let loop ((remaining steps) (position 1))
    (unless (null? remaining)
      (let* ((step (car remaining))
             (path (pipeline-step-record-path step)))
        (when path
          (catch #t
            (lambda ()
              (close-port (open-file path "w")))
            (lambda (key . args)
              (raise-maak-error
               (format #f "stage ~a (~a) cannot record its output to ~a: ~a"
                       position
                       (argv->string (pipeline-step-argv step))
                       path
                       (exception->message key args))
               #:hint
               "Point #:tee at a file the task may write, or drop it and read the pipeline's output another way."))))
        (loop (cdr remaining) (+ position 1))))))

;; A stage whose output is recorded cannot write to the next stage and to the
;; file by itself, so a `tee' process stands between them: what the stage wrote
;; goes on to the next stage unchanged and into the file as well.  That is the
;; `tee' from coreutils rather than a copy written here, because the copy has
;; to outlive the fork that starts it.  A Maak process may have threads of its
;; own -- the test runner runs its cases in fibers -- and a child forked from
;; such a process that then uses the Guile runtime can wait forever on a lock
;; the thread it was forked over still holds.  `exec' starts a program with no
;; such history, which is also why every stage is a fork and an exec.
(define (pipeline-tap-pid input next record-path dev-null?)
  "Start the process that copies INPUT to NEXT and RECORD-PATH.

NEXT is the pipe to the next stage, or #f when nothing follows the tap.  With
DEV-NULL? the copy's own output goes to @code{/dev/null}: the bytes are wanted
in the file and nowhere else.  Without it, and with NEXT #f, the copy inherits
standard output, which is what makes `#:tee' on the last stage print the line
as well as keep it.  Returns the copy process's pid."
  (pipeline-stage-pid (list "tee" record-path)
                      input
                      next
                      #:dev-null? dev-null?))

(define (pipeline-stage-failure-lines steps statuses)
  "Return a sentence for each step in STEPS that did not exit with 0.

STEPS and STATUSES are in the same order.  A stage killed by a signal is
reported as such, because it has no exit code to name."
  (let scan ((remaining steps)
             (status-list statuses)
             (position 1)
             (failures '()))
    (cond
      ((null? remaining) (reverse failures))
      (else
       (let ((code (status:exit-val (car status-list))))
         (scan (cdr remaining)
               (cdr status-list)
               (+ position 1)
               (if (and code (zero? code))
                   failures
                   (cons (format #f "stage ~a (~a) ~a"
                                 position
                                 (argv->string (pipeline-step-argv (car remaining)))
                                 (if code
                                     (format #f "exited with code ~a" code)
                                     "was terminated by a signal"))
                         failures))))))))

(define (pipeline-tap-failure-lines steps taps)
  "Return a sentence for each copy process that could not write its file.

TAPS holds the index of the step each copy process serves, next to its status.
A copy killed by a signal is not reported: the stage after it stopped reading,
as `head' does, and that is the stage's business.  A copy that exited with a
code could not open or write the file, which the pipeline reports against the
stage whose output is missing."
  (let loop ((remaining taps) (failures '()))
    (if (null? remaining)
        (reverse failures)
        (let* ((entry (car remaining))
               (index (car entry))
               (code (status:exit-val (cdr entry)))
               (step (list-ref steps index)))
          (loop (cdr remaining)
                (if (or (not code) (zero? code))
                    failures
                    (cons (format #f "stage ~a (~a) could not record its output to ~a"
                                  (+ index 1)
                                  (argv->string (pipeline-step-argv step))
                                  (pipeline-step-record-path step))
                          failures)))))))

(define (pipeline-captured-outputs steps capture?)
  "Return what each stage wrote, in stage order, reading our own files.

Every recorded step has a file; the ones this pipeline made are removed here,
after their bytes have been read, whether the pipeline succeeded or not."
  (if (not capture?)
      '()
      (map (lambda (step)
             (let ((path (pipeline-step-record-path step)))
               (if (not path)
                   ""
                   (let ((text (if (file-exists? path)
                                   (call-with-input-file path get-string-all)
                                   "")))
                     (when (pipeline-step-temporary? step)
                       (catch #t
                         (lambda () (delete-file path))
                         (lambda args #f)))
                     text))))
           steps)))

(define (pipeline-run steps capture?)
  "Run STEPS connected by pipes and return a <pipeline-result>.

Each stage's standard output is the next stage's standard input, and the
pipeline is done when every stage has finished.  A step that records its output
has a `tee' process between it and the next stage, so the two run at the same
time rather than one filling a file the other has to wait for.  With CAPTURE?
the result holds what every stage wrote, read back from the recorded files."
  (let loop ((remaining steps) (input #f) (stages '()) (taps '()) (index 0))
    (if (null? remaining)
        (let ((statuses (map reap-process (reverse stages)))
              (tap-statuses (map (lambda (entry)
                                   (cons (car entry) (reap-process (cdr entry))))
                                 (reverse taps))))
          (make-pipeline-result
           (pipeline-captured-outputs steps capture?)
           (append (pipeline-stage-failure-lines steps statuses)
                   (pipeline-tap-failure-lines steps tap-statuses))))
        (let* ((step (car remaining))
               (last? (null? (cdr remaining)))
               (record (pipeline-step-record-path step))
               (stage-out (if (or record (not last?)) (pipe) #f))
               (pid (pipeline-stage-pid (pipeline-step-argv step)
                                        input stage-out)))
          (when input (close-port input))
          ;; The write end has to leave the parent before a copy process is
          ;; forked, or that process keeps the pipe open and the reader never
          ;; sees the end of the stage's output.
          (when stage-out (close-port (cdr stage-out)))
          (cond
            ;; Nothing to record: the pipe is simply the next stage's input.
            ((not record)
             (loop (cdr remaining)
                   (and stage-out (car stage-out))
                   (cons pid stages)
                   taps
                   (+ index 1)))
            (else
             (let* ((next (and (not last?) (pipe)))
                    (tap (pipeline-tap-pid (car stage-out)
                                           next
                                           record
                                           (and last? capture?))))
               (close-port (car stage-out))
               (when next (close-port (cdr next)))
               (loop (cdr remaining)
                     (and next (car next))
                     (cons pid stages)
                     (cons (cons index tap) taps)
                     (+ index 1)))))))))

(define (spawn-pipeline steps)
  "Run STEPS in a background process and return its pid.

The process exits 0 when every stage succeeded and 1 otherwise.  A failure is
reported by that status rather than by printing, which is what `run' does for a
background program too: the task that holds the job decides what a failure
means."
  (force-output (current-output-port))
  (force-output (current-error-port))
  (let ((pid (primitive-fork)))
    (if (zero? pid)
        (begin
          ;; A raise inside the worker would print a backtrace over whatever
          ;; the terminal is showing, so it becomes the exit status instead.
          (let ((failures (catch #t
                            (lambda ()
                              (pipeline-errors (pipeline-run steps #f)))
                            (lambda args '("the pipeline could not be started")))))
            (force-output (current-output-port))
            (force-output (current-error-port))
            (primitive-exit (if (null? failures) 0 1))))
        pid)))

(define* (run-pipeline stages
                       #:key (verbose? #f) (allow-failure? #f)
                       (env '()) (async? #f) (capture? #f))
  "Run a pipeline of programs without a shell.

STAGES is a non-empty list of stages.  A stage is an argument list shaped like
the ARGV of `run' -- the program first, then one string per argument -- or a
@code{stage} form, which adds the options `run' takes for that stage alone.
The standard output of each stage is the standard input of the next, and the
whole pipeline is done when every stage has finished.

@example
(run-pipeline (list (list \"cat\" file)
                    (list \"grep\" \"-v\" \"^#\")
                    (list \"sort\" \"-u\")))

;; a stage with an environment and a file of its own, and one that needs root
(run-pipeline (list (stage (list \"sort\" \"-u\")
                           #:env '(\"LC_ALL=C\")
                           #:tee \"sorted.txt\")
                    (stage (list \"tee\" log) #:sudo? #t)))

;; keep what every stage wrote for the task to look at
(define result (run-pipeline (list (list \"git\" \"diff\" \"--cached\")
                                   (list \"grep\" \"^+\"))
                             #:capture? #t
                             #:allow-failure? #t))
(pipeline-outputs result)   ;; one string per stage, in order
(pipeline-ok? result)       ;; #f when a stage failed
@end example

Unlike a shell's `|', which reports the exit status of the last stage alone, a
pipeline that fails anywhere reports the stages that failed and raises; pass
@code{#:allow-failure? #t} to get @code{#f} instead and decide in the task.
This is the pipefail behaviour, and it is the one that does not let a failing
producer hide behind a successful consumer.

A pipeline's output can be looked at afterwards in two ways.  A stage with
@code{#:tee} keeps a copy of what it wrote in a file the task names, and the
stage after it reads the same bytes while it runs: nothing is held back until
the pipeline ends.  @code{#:capture? #t} records every stage the same way, in
temporary files, and hands the bytes back in the result instead of printing
them; the last stage's output is then the last entry of
@code{pipeline-outputs}.  Capturing is the way to make a decision from what a
pipeline produced -- a count of changed files, the list a `grep' matched --
without running the pipeline twice.

@sc{arguments}:
@itemize @bullet

@item @strong{stages} (list):
The stages to run, each an argument list or a @code{stage}.

@item @strong{#:verbose?} (boolean, default #f):
If #t, logs the pipeline, decorations included, before running it.

@item @strong{#:allow-failure?} (boolean, default #f):
If #t, a failing stage does not raise: @code{#f} is returned instead (or the
result, when the output was captured), so the task can decide what it means.

@item @strong{#:env} (alist, list of strings, or string, default '()):
Environment variables for every stage of the pipeline, in the shapes `$'
accepts.  A stage's own @code{#:env} is added to these, later names winning.

@item @strong{#:capture?} (boolean, default #f):
If #t, every stage's standard output is recorded and returned in a
@code{<pipeline-result>}, and the pipeline prints nothing itself.  The result's
@code{pipeline-outputs} holds one string per stage, in stage order, and
@code{pipeline-ok?} says whether every stage succeeded.  A stage that also has
@code{#:tee} records into that file, which is left in place.

@item @strong{#:async?} (boolean, default #f):
If #t, the whole pipeline is started in the background and a job record is
returned.  It is the pipeline that becomes a job, not a stage: a stage cannot
finish before the stage after it has read what it wrote.  The job's exit code
is 0 when every stage succeeded and 1 otherwise, whatever
@code{#:allow-failure?} said.  An asynchronous pipeline cannot capture its
output, because the bytes would be read in the background process; give the
stage a @code{#:tee} file instead.

@end itemize

@sc{returns}:
@code{#t} when every stage exits with code 0, @code{#f} when a stage failed and
failures were allowed, a @code{<pipeline-result>} when @code{#:capture?} was
given, and a job record when @code{#:async?} was given.

@sc{errors}:
Signals a maak error naming the failing stage or stages when a pipeline fails
and failures are not allowed, and when @code{#:capture?} and @code{#:async?}
are asked for together."
  (when (or (not (list? stages))
            (null? stages)
            (not (every pipeline-stage-spec? stages)))
    (raise-maak-error
     "run-pipeline expects a non-empty list of stages: argument lists or (stage ...) forms"
     #:hint
     "Write (run-pipeline (list (list \"cat\" file) (stage (list \"grep\" pattern) #:env '(\"LC_ALL=C\"))))."))
  (when (and async? capture?)
    (raise-maak-error
     "a background pipeline cannot capture its output"
     #:hint
     "Wait for the job and capture a foreground pipeline, or give the stage a file with (stage argv #:tee file)."))
  (let* ((steps (pipeline-steps stages env capture?))
         (rendered (pipeline-rendered steps)))
    (cond
      ((dry-run?)
       (unless (quiet?)
         (display (bold-output (format #f "\n[DRY-RUN]: ~a~a\n"
                                       rendered
                                       (if async? " &" "")))))
       ;; Nothing ran, so nothing was written: the result is the shape a task
       ;; expects, with every stage having said nothing.
       (if capture?
           (make-pipeline-result (map (lambda (step) "") steps) '())
           #t))
      (else
       (when (and verbose? (not (quiet?)))
         (log-info "  Execute pipeline:\n~a\n" rendered))
       (pipeline-check-records! steps)
       (if async?
           (make-job (spawn-pipeline steps) rendered)
           (let ((result (pipeline-run steps capture?)))
             (cond
               ((pipeline-ok? result) (if capture? result #t))
               (allow-failure? (if capture? result #f))
               (else
                (raise-maak-error
                 (format #f "pipeline failed: ~a"
                         (string-join (pipeline-errors result) ", "))
                 #:hint
                 "Fix the failing stage above, or run `maak --dry-run' to inspect the pipeline first.")))))))))

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
                                  #:key (verbose? #f) (expand? #f))
  "Delete a directory and all its contents recursively.
Runs `rm -rfv DIR` through `run`, so DIR reaches `rm` as one argument however
it is spelled.  The deletion is recursive, forced and verbose.

With EXPAND? #t the shell expands DIR, so `build/*.o' names every matching
file; the same opt-in applies to the other file helpers via `run'.

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
  (run (list "rm" "-rfv" dir)
       #:verbose? verbose? #:expand? expand?))

(define* (rm-rf path #:key (verbose? #f) (expand? #f))
  "Delete PATH recursively, forcefully and verbosely.

A short spelling of @code{delete-file-recursively}, for the times a task says
@code{rm -rfv build} in the shell.  It is an alias rather than a second
implementation, so it takes the same @code{#:verbose?} and @code{#:expand?}
keywords and behaves the same way.

@example
(rm-rf \"build\")
(rm-rf \"build/*.o\" #:expand? #t)
@end example"
  (delete-file-recursively path
                           #:verbose? verbose?
                           #:expand? expand?))

(define* (mkdir-p dir
                  #:key (verbose? #f) (expand? #f))
  "Create a directory and all necessary parent directories.
Executes `mkdir -pv DIR` through `run`.  Parent directories are created as
needed and each created directory is printed.  A DIR that contains spaces is
one directory, not several.

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
  (run (list "mkdir" "-pv" dir)
       #:verbose? verbose? #:expand? expand?))

(define* (remkdir-p dir
                    #:key (verbose? #f) (expand? #f))
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
                           #:verbose? verbose?
                           #:expand? expand?)
  (mkdir-p dir
           #:verbose? verbose?
           #:expand? expand?))

(define* (mv x y
             #:key (verbose? #f) (expand? #f))
  "Move or rename a file or directory.
  Executes `mv -v X Y` through `run`, so X and Y reach `mv` as one argument
each.  The `-v` flag prints each move operation performed.

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
  (run (list "mv" "-v" x y)
       #:verbose? verbose? #:expand? expand?))

(define* (cat file
              #:key (verbose? #f) (expand? #f))
  "Print FILE to standard output."
  (run (list "cat" file)
       #:verbose? verbose? #:expand? expand?))

(define* (cp x
             y
             #:key (recursive? #t)
             (force? #f)
             (verbose? #f)
             (verbose-copy? #t)
             (expand? #f))
  "Copy a file or directory from X to Y.

@sc{arguments}:
  x: Source file or directory.
  y: Destination path.

  recursive?: If #t, copy directories recursively (`-r`). Defaults to #t.
  force?: If #t, overwrite existing files without prompting (`-f`). Defaults to #f.
  verbose?: If #t, logs the final shell command before execution. Defaults to #f.
  verbose-copy?: If #t, enables verbose output from `cp` (`-v`). Defaults to #t.
  expand?: If #t, the shell expands X and Y, so a wildcard names the files it
           matches. Defaults to #f, which passes both as one argument each.

  Constructs and executes a command of the form:

    cp [OPTIONS] X Y

  through `run`, so X and Y reach `cp` as one argument each and a path with a
  space in it is copied rather than split.

@sc{returns}: #t if the copy succeeds.

@sc{errors}:
  Signals an error if the underlying `cp` command exits with a non-zero status."
  (run (append (list "cp")
               (if recursive? (list "-r") '())
               (if force? (list "-f") '())
               (if verbose-copy? (list "-v") '())
               (list x y))
       #:verbose? verbose? #:expand? expand?))

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
