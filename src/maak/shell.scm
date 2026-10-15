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
;; The shell helpers that sit on top of `(maak dsl)'.  Where the core module
;; offers the primitives (`$', `syscall', `cp', a few file moves), this one
;; collects the everyday conveniences a task reaches for: locating a program
;; before relying on it, quoting a value safely into a command, changing
;; directory for the duration of a thunk, and reading environment variables.
;;
;; Every helper here honours `dry-run?' and `quiet?' through the `$'/`syscall'
;; it is built on, so a dry run prints instead of acting.

;;; Code:

(define-module (maak shell)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (shell-quote
            which
            command-exists?
            require-command
            capture
            touch
            ln-s
            chmod-file
            with-directory
            cd
            env-get
            env-set!
            env-unset!
            with-env
            with-env-vars
            env-file-entries
            load-env-file
            home-dir
            tmp-dir
            read-file
            write-file))

;; ====== Quoting and program lookup ======

(define (shell-quote str)
  "Return STR quoted so that a shell reads it as one literal word.

The POSIX recipe: wrap the value in single quotes and turn every embedded
single quote into the four characters backslash-quote-quote-quote.  Unlike
`~s', this is safe for spaces, tabs, `$', backticks and newlines."
  (string-append "'"
                 (string-join (string-split str #\')
                              "'\\''")
                 "'"))

(define (which command)
  "Return the absolute path of COMMAND on `PATH', or #f when it is missing.

@example\n(which guile)\n@result{} /gnu/store/.../bin/guile\n@end example"
  (let* ((process (open-input-pipe (format #f
                                    "command -v ~a 2>/dev/null"
                                    (shell-quote command))))
         (output (get-string-all process)))
    (close-pipe process)
    (let ((path (string-trim-both output)))
      (if (string=? path "") #f path))))

(define (command-exists? command)
  "Return #t when COMMAND can be found on `PATH'."
  (and (which command) #t))

(define* (require-command command #:key (hint #f))
  "Return the path of COMMAND, raising a maak error when it is missing.

Use this at the top of a task that depends on an external program so the user
learns about the missing tool in one line instead of from a failing shell
command.

@example\n(require-command guile #:hint \"Install Guile 3.\")\n@end example"
  (or (which command)
      (raise-maak-error
       (format #f "required command not found on PATH: ~a" command)
       #:hint (or hint
                 (format #f "Install `~a', or add it to PATH." command)))))

(define* (capture command #:key (error? #f))
  "Run COMMAND and return its standard output as a string.

With ERROR? true the command's standard error is captured too.  In dry-run
mode the command is only printed and the empty string is returned.  The exit
status is not checked; use `$' when a failing command must stop the task.

@example\n(string-trim (capture describe-tags-command))\n@end example"
  (cond
    ((dry-run?)
     (unless (quiet?)
       (display (bold-output (format #f "\n[DRY-RUN]: ~a\n" command))))
     "")
    (else
     (let* ((process (open-input-pipe (if error?
                                          (format #f "~a 2>&1" command)
                                          command)))
            (output (get-string-all process)))
       (close-pipe process)
       output))))

;; ====== File and directory helpers ======

(define* (touch file #:key (verbose? #f))
  "Create FILE if it does not exist, or update its timestamp."
  ($ (list (format #f "touch ~a" (shell-quote file)))
     #:verbose? verbose?))

(define* (ln-s target link-name #:key (force? #t) (verbose? #f))
  "Create a symbolic link LINK-NAME pointing at TARGET.

With FORCE? (the default) an existing link is replaced, which is what
`ln -sf' does."
  ($ (list (format #f "ln -s~a ~a ~a"
                   (if force? "f" "")
                   (shell-quote target)
                   (shell-quote link-name)))
     #:verbose? verbose?))

(define* (chmod-file mode file #:key (recursive? #f) (verbose? #f))
  "Change the permissions of FILE to MODE, an octal string such as 755."
  ($ (list (format #f "chmod ~a~a ~a"
                   (if recursive? "-R " "")
                   mode
                   (shell-quote file)))
     #:verbose? verbose?))

(define* (read-file file #:key (default #f))
  "Return the contents of FILE as a string.

When the file does not exist, DEFAULT is returned instead of raising; this
keeps a task from having to guard every optional file with `file-exists?'."
  (if (file-exists? file)
      (call-with-input-file file get-string-all)
      default))

(define* (write-file file content #:key (append? #f) (mkdir-parents? #t))
  "Write CONTENT to FILE, creating its directory first when needed.

With APPEND? #t the content is added to the end of FILE rather than replacing
it."
  (let ((dir (dirname file)))
    (when (and mkdir-parents?
               (not (string=? dir "."))
               (not (file-exists? dir)))
      (mkdir-p dir)))
  (let ((port (open-file file (if append? "a" "w"))))
    (display content port)
    (close-port port))
  file)

(define (with-directory dir thunk)
  "Call THUNK with the process working directory changed to DIR.

The previous directory is restored even when THUNK raises.

@example\n(with-directory build-dir (lambda () (syscall make-command)))\n@end example"
  (let ((previous (getcwd)))
    (dynamic-wind
     (lambda ()
       (chdir dir))
     thunk
     (lambda ()
       (chdir previous)))))

(define (cd dir)
  "Change the process working directory to DIR and return it.

Unlike `with-directory' the change lasts for the rest of the run, which is
what a task that only ever works from one place wants."
  (chdir dir)
  dir)

;; ====== Environment variables ======

(define* (env-get name #:optional (default #f))
  "Return the value of environment variable NAME, or DEFAULT when unset."
  (let ((value (getenv name)))
    (if value value default)))

(define (env-set! name value)
  "Set environment variable NAME to VALUE and return VALUE."
  (setenv name value)
  value)

(define (env-unset! name)
  "Remove environment variable NAME."
  (unsetenv name)
  name)

(define (with-env name value thunk)
  "Call THUNK with environment variable NAME set to VALUE.

The previous value, if any, is restored afterwards."
  (let ((previous (getenv name)))
    (dynamic-wind
     (lambda ()
       (setenv name value))
     thunk
     (lambda ()
       (if previous
           (setenv name previous)
           (unsetenv name))))))

(define (with-env-vars variables thunk)
  "Call THUNK with every NAME and VALUE pair of VARIABLES set.

@example\n(with-env-vars configured-variables (lambda () (syscall make-command)))\n@end example"
  (let loop ((vars variables))
    (if (null? vars)
        (thunk)
        (with-env (caar vars)
                  (cdar vars)
                  (lambda ()
                    (loop (cdr vars)))))))

;; ====== Environment files ======
;;
;; A `.env' file is a list of `NAME=VALUE' lines, and loading one is the
;; smallest possible way to hand a task the configuration it needs without the
;; user having to export anything.  The parser is deliberately small: it reads
;; the assignments and nothing else.  No expansion of `$OTHER', no command
;; substitution, no `source' -- a configuration file that can run code is a
;; different and much larger promise.

(define (env-file-entries path)
  "Return the `(NAME . VALUE)' assignments in the environment file PATH.

Blank lines and lines whose first non-blank character is `#' are skipped, as
are lines without an `='.  A leading `export ' is accepted so a file that is
valid for a POSIX shell is valid here too.  The name and the value are trimmed,
and a value wrapped in matching single or double quotes is unwrapped.

@example
(env-file-entries \".env\")
@result{} ((\"CI\" . \"1\") (\"DATABASE_URL\" . \"postgres://localhost/dev\"))
@end example"
  (if (not (file-exists? path))
      '()
      (let loop ((lines (string-split (call-with-input-file path
                                        get-string-all)
                                      #\newline))
                 (entries '()))
        (if (null? lines)
            (reverse entries)
            (let* ((line (string-trim-both (car lines)))
                   (stripped (if (string-prefix? "export " line)
                                 (string-trim (string-drop line
                                                           (string-length "export ")))
                                 line))
                   (separator (and (not (string=? stripped ""))
                                   (not (string-prefix? "#" stripped))
                                   (string-index stripped #\=))))
              (if (not separator)
                  (loop (cdr lines) entries)
                  (let* ((name (string-trim-both (substring stripped 0 separator)))
                         (raw (string-trim-both (substring stripped
                                                           (+ separator 1))))
                         (value (if (and (> (string-length raw) 1)
                                         (or (and (string-prefix? "\"" raw)
                                                  (string-suffix? "\"" raw))
                                             (and (string-prefix? "'" raw)
                                                  (string-suffix? "'" raw))))
                                    (substring raw 1 (- (string-length raw) 1))
                                    raw)))
                    (if (string=? name "")
                        (loop (cdr lines) entries)
                        (loop (cdr lines)
                              (cons (cons name value) entries))))))))))

(define* (load-env-file path #:key (override? #f))
  "Read the `NAME=VALUE' assignments of PATH into the environment.

Returns the list of variables that were actually set.  A name that is already
in the environment keeps its value unless OVERRIDE? is #t: the environment a
process was started with has the last word over a file next to the code, which
is the precedence `dotenv' established and the one that makes
`CI=1 maak test' behave as asked.  Pass `#:override? #t' when the file is meant
to win.

A missing PATH is not an error -- the file is optional by definition -- and
returns the empty list.

@example
(load-env-file \".env\")
(load-env-file \".env.local\" #:override? #t)
@end example"
  (let loop ((entries (env-file-entries path))
             (loaded '()))
    (if (null? entries)
        (reverse loaded)
        (let* ((name (caar entries))
               (value (cdar entries))
               (already (getenv name)))
          (if (and already (not override?))
              (loop (cdr entries) loaded)
              (begin
                (setenv name value)
                (log-info "  Environment: ~a~a\n"
                          name
                          (if already
                              (format #f " (was ~a)" already)
                              ""))
                (loop (cdr entries)
                      (cons name loaded))))))))


(define (home-dir)
  "Return the current user's home directory, or the empty string when unset."
  (or (getenv "HOME") ""))

(define (tmp-dir)
  "Return the directory for temporary files, honouring `TMPDIR'."
  (or (getenv "TMPDIR") "/tmp"))
