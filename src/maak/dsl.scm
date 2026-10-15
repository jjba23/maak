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
                        cat
                        ;; ====== Errors ======
                        &maak-error
                        make-maak-error
                        maak-error?
                        maak-error-message
                        maak-error-hint
                        raise-maak-error
                        find-maak-error
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
            (cond
              ((zero? exit-code) #t)
              ;; A signalled command (segfault, SIGTERM) has no exit code; say
              ;; so rather than reporting a misleading number.
              ((not exit-code)
               (raise-maak-error
                (~ "command was terminated by a signal: ~a" cmd)
                #:hint
                "The command above did not exit normally.  Re-run it by hand to see why."))
              (else
               (raise-maak-error
                (~ "command failed (exit code ~a): ~a" exit-code cmd)
                #:hint
                "Fix the command above, or run `maak --dry-run' to inspect it first.")))))))

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
                     (verbose? #f))
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
       #:verbose? verbose?)))

(define* (guix-container cmd-parts
                         #:key (manifest "manifest.scm")
                         (pure? #f)
                         (network? #f)
                         (share '())
                         (expose '())
                         (join " ")
                         (verbose? #f))
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
     #:verbose? verbose?))

(define* (guix-pack cmd-parts
                    #:key (file "guix.scm")
                    (format "docker")
                    (image-tag #f)
                    (symlinks '())
                    (extra-packages '())
                    (join " ")
                    (verbose? #f))
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
     #:verbose? verbose?))

(define (bold-output x)
  (format #f "\x1b[1m~a\x1b[0m" x))
