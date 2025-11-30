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
  (display (format #f "\n~a" msg)))

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

(define* (syscall cmd)
  "Executes a shell command and checks its exit code.

Args:
  cmd: A string representing the shell command to be executed.

Returns:
  #t if the command completes with an exit code of 0.

Errors:
  Signals an error if the command returns a non-zero exit code,
  providing the command and the exit code in the error message."
  (let* ((exit-val (system cmd))
         (exit-code (status:exit-val exit-val)))
    (cond
      ((equal? 0 exit-code)
       #t)
      (else (error (format #f "
Non-zero exit code when running!
Command: ~a
Exit code: ~a
" cmd exit-code))))))

(define* ($ cmd-parts
            #:key (verbose? #f)
            (join " "))
  "Executes a shell command constructed from a list of strings.
  
Args:
  cmd-parts: A list of strings that will be joined to form the command.
  #:key:
    verbose?: A boolean. If true, the command to be executed will be logged
             to the info stream before execution.
    join: A string to use as a separator when joining the command parts.
          Defaults to a space ("
  ").

Returns:
  The result of the `syscall` function, which typically is the exit status
  of the executed command."
  (let* ((cmd (string-join cmd-parts join)))
    (when verbose?
      (log-info "  Execute command:\n~a\n" cmd))
    (syscall cmd)))

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
  (and (procedure? x)
       (equal? '(0 0 #f)
               (procedure-minimum-arity x))))

(define* (delete-file-recursively dir
                                  #:key (verbose? #f))
  "Delete a directory and all its contents recursively.

Args:
  dir: The path of the directory (or file) to delete.

Keyword arguments:
  verbose?: If #t, logs the generated command before executing it.
            Defaults to #f.

Behavior:
  Runs `rm -rfv DIR` using the `$` command wrapper. This means:
    - The deletion is recursive.
    - It forces removal without prompting (`-f`).
    - It prints removed files (`-v`).

Returns:
  #t if the command succeeded.

Errors:
  Signals an error if the underlying shell command exits with a non-zero status."
  ($ (list (~ "rm -rfv ~a" dir))
     #:verbose? verbose?))

(define* (mkdir-p dir
                  #:key (verbose? #f))
  "Create a directory and all necessary parent directories.

Args:
  dir: The path of the directory to create.

Keyword arguments:
  verbose?: If #t, logs the generated command before executing it.
            Defaults to #f.

Behavior:
  Executes `mkdir -pv DIR` using the `$` wrapper. This means:
    - Parent directories are created as needed (`-p`).
    - The operation prints each created directory (`-v`).

Returns:
  #t if the command completed successfully.

Errors:
  Signals an error if the underlying shell command exits with a non-zero code."
  ($ (list (~ "mkdir -pv ~a" dir))))

(define* (remkdir-p dir
                    #:key (verbose? #f))
  "Remove a directory if it exists, then recreate it.

Args:
  dir: The path of the directory to recreate.

Keyword arguments:
  verbose?: If #t, logs each underlying command before executing it.
            Defaults to #f.

Behavior:
  Performs two steps:
    1. Calls `delete-file-recursively` on DIR.
    2. Calls `mkdir-p` to recreate DIR.

This results in an empty directory, regardless of its previous state.

Returns:
  #t if both operations succeed.

Errors:
  Signals an error if any underlying shell command fails."
  (delete-file-recursively dir
                           #:verbose? verbose?)
  (mkdir-p dir
           #:verbose? verbose?))

(define* (mv x y
             #:key (verbose? #f))
  "Move or rename a file or directory.

Args:
  x: The source file or directory.
  y: The destination path.

Keyword arguments:
  verbose?: If #t, logs the generated command before executing it.
            Defaults to #f.

Behavior:
  Executes `mv -v X Y` using the `$` wrapper. The `-v` flag prints
  each move operation performed.

Returns:
  #t if the command succeeds.

Errors:
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

Args:
  x: Source file or directory.
  y: Destination path.

Keyword arguments:
  recursive?: If #t, copy directories recursively (`-r`). Defaults to #t.
  force?: If #t, overwrite existing files without prompting (`-f`). Defaults to #f.
  verbose?: If #t, logs the final shell command before execution. Defaults to #f.
  verbose-copy?: If #t, enables verbose output from `cp` (`-v`). Defaults to #t.

Behavior:
  Constructs and executes a command of the form:

    cp [OPTIONS] X Y

  using the `$` wrapper, ensuring errors are caught and logged.

Returns:
  #t if the copy succeeds.

Errors:
  Signals an error if the underlying `cp` command exits with a non-zero status."
  ($ (list (~ "cp ~a ~a ~a ~a ~a"
              (if recursive? "-r" "")
              (if force? "-f" "")
              (if verbose-copy? "-v" "")
              x
              y))
     #:verbose? verbose?))

(define* (~ #:rest args)
  (apply format
         (append '(#f) args)))

(define* (time-machine cmd-parts
                       #:key (verbose? #f)
                       (join " ")
                       (channels "channels.scm"))
  "Execute a command using a pinned Guix revision via `guix time-machine`.

Args:
  cmd-parts: A list of strings representing the command to run
             after the `guix time-machine` invocation.

Keyword arguments:
  verbose?: If #t, logs the final constructed command before running it.
            Defaults to #f.
  join: The string used to join command segments when constructing
        the final command. Defaults to a single space.
  channels: Path to the channels file used to pin the Guix revision.
            Defaults to \"channels.scm\".

Behavior:
  Constructs and executes a command of the form:

    guix time-machine --channels=CHANNELS -- <cmd ...>

  using the `$` wrapper to run the resulting shell command safely.

Returns:
  #t if the command succeeds.

Errors:
  Signals an error if the underlying command exits with a non-zero status."
  ($ (append (list (~ "guix time-machine --channels=~a" channels) "--")
             cmd-parts)
     #:verbose? verbose?))

(define* (manifest-shell cmd-parts
                         #:key (verbose? #f)
                         (join " ")
                         (manifest "manifest.scm"))
  "Execute a command inside a Guix shell defined by a manifest file.

Args:
  cmd-parts: A list of strings representing the command to run
             inside the Guix shell environment.

Keyword arguments:
  verbose?: If #t, logs the fully constructed command before execution.
            Defaults to #f.
  join: The string separator used when joining command segments.
        Defaults to a single space.
  manifest: Path to the manifest file (in Scheme) describing the
            package environment. Defaults to \"manifest.scm\".

Behavior:
  Constructs and executes a command of the form:

    guix shell -m MANIFEST -- <cmd ...>

  using the `$` wrapper to enforce proper execution and error handling.

Returns:
  #t if the command succeeds.

Errors:
  Signals an error if the underlying shell command exits with a non-zero status."
  ($ (append (list (~ "guix shell -m ~a" manifest) "--") cmd-parts)
     #:verbose? verbose?))

(define* (time-machine-manifest-shell cmd-parts
                                      #:key (verbose? #f)
                                      (join " ")
                                      (channels "channels.scm")
                                      (manifest "manifest.scm"))
  "Execute a command inside a Guix shell defined by a manifest file,
using a pinned Guix revision via `guix time-machine`.

Args:
  cmd-parts: A list of strings representing the command to run inside
             the pinned Guix shell environment.

Keyword arguments:
  verbose?: If #t, logs the composed command before execution.
            Defaults to #f.
  join: String used to join command segments. Defaults to a single space.
  channels: Path to the channels file used to pin the Guix revision.
            Defaults to \"channels.scm\".
  manifest: Path to the manifest file describing the package environment.
            Defaults to \"manifest.scm\".

Behavior:
  Constructs and executes a command of the form:

    guix time-machine --channels=CHANNELS --       shell -m MANIFEST -- <cmd ...>

  This provides a reproducible shell environment defined by the manifest file
  and pinned to the Guix revision specified in the channels file.

Returns:
  #t if the command succeeds.

Errors:
  Signals an error if any underlying shell command exits with a non-zero status."
  ($ (append (list (~ "guix time-machine --channels=~a" channels) "--"
                   (~ "shell -m ~a" manifest) "--") cmd-parts)
     #:verbose? verbose?))

(define* (program-shell cmd-parts
                        #:key (verbose? #f)
                        (join " ")
                        (file "guix.scm"))
  "Execute a command inside a Guix shell defined by a program file.

Args:
  cmd-parts: A list of strings representing the command to run
             inside the Guix shell environment.

Keyword arguments:
  verbose?: If #t, logs the full constructed command before execution.
            Defaults to #f.
  join: String used to join command segments. Defaults to a single space.
  file: Path to the Guix program or manifest file defining the shell
        environment. Defaults to \"guix.scm\".

Behavior:
  Constructs and executes a command of the form:

    guix shell -f FILE -- <cmd ...>

  using the `$` wrapper to safely run the command and check for errors.

Returns:
  #t if the command succeeds.

Errors:
  Signals an error if the underlying shell command exits with a non-zero status."
  ($ (append (list (~ "guix shell -f ~a" file) "--") cmd-parts)
     #:verbose? verbose?))

(define* (time-machine-program-shell cmd-parts
                                     #:key (verbose? #f)
                                     (join " ")
                                     (channels "channels.scm")
                                     (file "guix.scm"))
  "Execute a command inside a Guix shell defined by a program file,
using a pinned Guix revision via `guix time-machine`.

Args:
  cmd-parts: A list of strings representing the command to run inside
             the pinned Guix shell environment.

Keyword arguments:
  verbose?: If #t, logs the composed command before execution.
            Defaults to #f.
  join: String used to join command segments. Defaults to a single space.
  channels: Path to the channels file used to pin the Guix revision.
            Defaults to \"channels.scm\".
  file: Path to the Guix program file (e.g. guix.scm) defining the environment.
        Defaults to \"guix.scm\".

Behavior:
  Constructs and executes a command of the form:

    guix time-machine --channels=CHANNELS --       shell -f FILE -- <cmd ...>

  This provides a reproducible development environment defined by the program
  file and pinned to the Guix revision specified in the channels file.

Returns:
  #t if the command succeeds.

Errors:
  Signals an error if any underlying shell command exits with a non-zero status."
  ($ (append (list (~ "guix time-machine --channels=~a" channels) "--"
                   (~ "shell -f ~a" file) "--") cmd-parts)
     #:verbose? verbose?))

(define (bold-output x)
  (format #f "\x1b[1m~a\x1b[0m" x))

