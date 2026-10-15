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

(define-module (maak maak)
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
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak nix)
  #:use-module (maak i18n)
  #:use-module (maak version)
  #:re-export (log-message log-info
                           log-error
                           log-separator
                           bold-output
                           get-module-var
                           delete-file-recursively
                           manifest-shell
                           time-machine
                           time-machine-manifest-shell
                           program-shell
                           time-machine-program-shell
                           mkdir-p
                           cp
                           mv
                           quiet?
                           dry-run?
                           banner?
                           remkdir-p
                           maak-proc?
                           maak-task?
                           $
                           ~
                           syscall
                           cat
                           ;; ====== Errors ======
                           &maak-error
                           make-maak-error                           maak-error? 
                           maak-error-message
                           maak-error-hint
                           raise-maak-error
                           task-error
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
                           dedent-string
                           indent-string
                           text
                           ;; ====== Guix shells and containers ======
                           guix-shell
                           guix-container
                           guix-pack
                           ;; ====== Shell helpers ======
                           environment-assignments
                           decorate-command
                           shell-quote
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
                           write-file
                           ;; ====== Nix helpers ======
                           nix-shell
                           nix-develop
                           nix-build
                           nix-flake-build
                           nix-flake-check
                           nix-run
                           ;; ====== i18n helpers ======
                           i18n-locale-dir
                           i18n-po-file
                           i18n-mo-file
                           i18n-pot-file
                           i18n-source-globs
                           i18n-extract
                           i18n-init
                           i18n-merge
                           i18n-compile
                           i18n-init-all
                           i18n-merge-all
                           i18n-compile-all
                           i18n-refresh
                           i18n-translator
                           i18n-language
                           ;; ====== Version ======
                           maak-version
                           maak-version-string
                           maak-homepage
                           maak-license
                           print-version)
  #:export (resolve-maak-tasks list-tasks run-tasks run-task
                              format-task-doc available-task-names
                              unknown-task-error))

(define* (resolve-maak-tasks #:key (module-name '(maak)))
  "Return the runnable tasks of MODULE-NAME as `(NAME . VARIABLE)' pairs.

When the module declares exports, only those are considered; otherwise every
procedure the module defines counts as a task.  See the export rules in the
README."
  (let* ((mod (resolve-module module-name))
         (public-mod (module-public-interface mod))
         ;; Check if a public interface exists AND contains at least one export.
         (has-exports? (and public-mod
                            (not (null? (module-map (lambda (name var)
                                                      name) public-mod)))))
         ;; If it has exports, search only those. Otherwise, fall back to the whole module.
         (target-mod (if has-exports? public-mod mod))
         (tasks (filter (lambda (x)
                          (not (equal? #f x)))
                        (module-map (lambda (name var)
                                      (cond
                                        ((maak-task? var)
                                         (cons name var))
                                        (else #f))) target-mod))))
    tasks))

(define (task-name<? a b)
  "Order two tasks by name, so a listing or an error message is stable."
  (string<? (symbol->string (car a))
            (symbol->string (car b))))

(define (available-task-names)
  "Return the names of the tasks maak can currently run, sorted."
  (map (lambda (task)
         (symbol->string (car task)))
       (sort (resolve-maak-tasks) task-name<?)))

(define (format-task-doc name doc)
  "Return the lines that describe task NAME, given its docstring DOC.

A docstring that is one paragraph starts inline after the name, and every
continuation line aligns under the first word of the documentation, so a
docstring wrapped over several lines reads as one block:

@example
build: Compile the project.
       The result lands in ./target.
@end example

A docstring with a blank line in it is a set of paragraphs, and running text
would flatten that shape away, so it prints as a block instead: the name on its
own line and every line indented to the same column, with the blank lines left
blank.

Common leading indentation is removed first with `dedent-string', so a
docstring written inside an indented function still reads flush left."
  (let* ((label (string-append (bold-output name) ":"))
         (prefix (string-append name ": "))
         ;; The column the documentation aligns to: the visible length of the
         ;; name and its colon and space.  The ANSI codes `bold-output' adds
         ;; occupy no column, which is why the margin comes from PREFIX rather
         ;; than from LABEL.
         (margin (make-string (string-length prefix) #\space))
         (text (and (string? doc) (dedent-string doc)))
         (lines (if (and text (not (string=? text "")))
                    (string-split text #\newline)
                    '()))
         (indented (lambda (line)
                     ;; A blank line inside the documentation stays blank
                     ;; rather than being padded out to the margin.
                     (if (string=? line "") ""
                         (string-append margin line)))))
    (cond
      ((null? lines)
       (list label))
      ((any blank-line? lines)
       (cons label (map indented lines)))
      (else
       (cons (string-append prefix (car lines))
             (map indented (cdr lines)))))))

(define* (list-tasks #:key (as-is? #f))
  "List documentation for every maak task in the loaded file.

Tasks are sorted by name unless AS-IS? is #t.  Each docstring is laid out so
its lines align under the task name rather than wrapping back to the left
margin; see `format-task-doc'."
  (let* ((raw-tasks (resolve-maak-tasks))
         (tasks (if as-is? raw-tasks (sort raw-tasks task-name<?))))
    (log-separator)
    (for-each (lambda (task)
                (let* ((proc (variable-ref (cdr task)))
                       (doc (procedure-documentation proc))
                       (lines (format-task-doc (symbol->string (car task))
                                               doc)))
                  (newline)
                  (log-message (string-join lines "\n"))))
              tasks)
    (newline)
    (newline)))

(define (unknown-task-error task)
  "Raise a maak error saying TASK is not runnable, listing the tasks that are."
  (let ((available (available-task-names)))
    (raise-maak-error
     (if (equal? 'default task)
         "Could not find a `default' task in this Maak file."
         (format #f "Unknown task: ~a" task))
     #:hint
     (if (null? available)
         "No runnable tasks are defined. Define a function, and export it if the module declares exports."
         (format #f "Available tasks: ~a" (string-join available ", "))))))

(define* (run-tasks tasks
                    #:key (task-args '())
                    (keep-going? #f)
                    (on-failure #f))
  "Run TASKS sequentially, in the order given.

Every task must name a procedure the loaded Maak file makes visible; otherwise
a maak error lists the tasks that do exist.  TASK-ARGS are passed to each task.
Returns the names of the tasks that failed, in the order they were given -- the
empty list when everything succeeded.

With KEEP-GOING? a task that fails stops nothing: the tasks after it still run.
ON-FAILURE is then called with the task name and the exception it raised, as
`(ON-FAILURE TASK KEY VALUE ...)', so the caller can report the failure where
it happened and decide what a run full of them means.  Without KEEP-GOING? the
first failure is raised as it always was, backtrace and all."
  (let loop ((remaining tasks)
             (failed '()))
    (cond
      ((null? remaining) (reverse failed))
      (else
       (let ((task (car remaining)))
         (if (not keep-going?)
             (begin
               (unless (get-module-var task)
                 (unknown-task-error task))
               (run-task task
                         #:task-args task-args)
               (loop (cdr remaining) failed))
             (let ((failure #f))
               (catch #t
                 (lambda ()
                   (unless (get-module-var task)
                     (unknown-task-error task))
                   (run-task task
                             #:task-args task-args))
                 (lambda (key . vals)
                   (set! failure (cons key vals))))
               (if failure
                   (begin
                     (when on-failure
                       ;; The key and its values go on as they arrived, so the
                       ;; callback can tell a maak error from anything else.
                       (apply on-failure task failure))
                     (loop (cdr remaining) (cons task failed)))
                   (loop (cdr remaining) failed)))))))))

(define* (run-task task
                   #:key (task-args '()))
  "Run TASK, a single Maak task, logging its documentation first.

Args:
  task: a symbol naming the task to run.
  task-args: the arguments to pass to the task procedure.

Returns the value of the task procedure.  Raises a maak error when TASK does
not name a runnable task, either because it is missing or because it exists but
is not visible as a task (see the export rules in the README)."
  (let* ((proc (get-module-var task))
         (proc-doc (and (maak-proc? proc) (procedure-documentation proc)))
         (valid-tasks (resolve-maak-tasks)))
    (cond
      ;; Ensure the procedure is valid AND exists in the exported tasks list.
      ((and (maak-proc? proc)
            (assoc task valid-tasks))
       (log-separator)
       (log-info "=> Running Maak task: ~a\n" task)
       (when (and (string? proc-doc)
                  (not (string=? proc-doc "")))
         (log-info "  ~a" proc-doc))
       (newline)
       (apply proc task-args))
      (else (unknown-task-error task)))))
