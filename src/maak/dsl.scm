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
                        remkdir-p
                        maak-proc?
                        maak-task?
                        $
                        ~
                        time-machine
                        time-machine-manifest-shell
                        time-machine-program-shell
                        syscall
                        cat))

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

(define dry-run?
  (make-parameter #f))

(define quiet?
  (make-parameter #f))

(define* (syscall cmd)
  "Executes a shell command and checks its exit code.

@sc{arguments}:
@itemize @bullet

@item @strong{cmd} (string):
The shell command to be executed.

@end itemize

@sc{returns}:
@code{#t} if the command completes with an exit code of 0.

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
            (if (zero? exit-code) #t
                (error "Command failed" cmd exit-code))))))

(define* ($ cmd-parts
            #:key (verbose? #f)
            (join " "))
  "Executes a shell command constructed from a list of strings.

@example
($ '(\"echo -n\" \"hello-world\"))
@end example

@sc{arguments}:
@itemize @bullet

@item @strong{cmd-parts} (list):
A list of strings that will be joined to form the command.

@item @strong{#:verbose?} (boolean, default #f):
If #t, logs the generated command before executing it.

@item @strong{#:join} (boolean, default \" \"):
A string to use as a separator when joining the command parts.

@end itemize

@sc{returns}:
The result of the @code{syscall} function, which is @code{#t} if the command completes with an exit code of 0."
  (let ((cmd (string-join cmd-parts join)))
    (cond
      ((dry-run?)
       (unless (quiet?)
         (display (bold-output (format #f "\n[DRY-RUN]: ~a\n" cmd)))) #t)
      (else (begin
              (when (and verbose?
                         (not (quiet?)))
                (log-info "  Execute command:\n~a\n" cmd))
              (syscall cmd))))))

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
                       (channels "channels.scm"))
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
     #:verbose? verbose?))

(define* (manifest-shell cmd-parts
                         #:key (verbose? #f)
                         (pure? #f)
                         (join " ")
                         (manifest "manifest.scm"))
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
  ($ (append (list (~ "guix shell -m ~a" manifest)
                   (if pure? "--pure" "") "--") cmd-parts)
     #:verbose? verbose?))

(define* (time-machine-manifest-shell cmd-parts
                                      #:key (verbose? #f)
                                      (pure? #f)
                                      (join " ")
                                      (channels "channels.scm")
                                      (manifest "manifest.scm"))
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
                   (~ "shell -m ~a" manifest)
                   (if pure? "--pure" "") "--") cmd-parts)
     #:verbose? verbose?))

(define* (program-shell cmd-parts
                        #:key (verbose? #f)
                        (pure? #f)
                        (join " ")
                        (file "guix.scm"))
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
  ($ (append (list (~ "guix shell -f ~a" file)
                   (if pure? "--pure" "") "--") cmd-parts)
     #:verbose? verbose?))

(define* (time-machine-program-shell cmd-parts
                                     #:key (verbose? #f)
                                     (pure? #f)
                                     (join " ")
                                     (channels "channels.scm")
                                     (file "guix.scm"))
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
                   (~ "shell -f ~a" file)
                   (if pure? "--pure" "") "--") cmd-parts)
     #:verbose? verbose?))

(define (bold-output x)
  (format #f "\x1b[1m~a\x1b[0m" x))
