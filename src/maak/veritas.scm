;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Veritas is the test framework this project and its siblings are written
;; with, and every one of their Maak files spells the same invocation out by
;; hand: four environment variables the runner reads, the library and the test
;; tree on the load path, and one expression that exits with the runner's
;; status.
;;
;; This module is that invocation, in the two shapes a Maak file needs it: the
;; argv `run' takes directly, and the same words with the expression quoted for
;; `manifest-shell' and `time-machine-manifest-shell', which join their
;; arguments into one shell line.  Getting that difference wrong is the quiet
;; bug the quoting is for: an expression with spaces in it becomes three
;; arguments, and guile reports a syntax error instead of running a suite.
;;
;; Nothing here is specific to this repository's suite: MODULE, TYPE and
;; TOP-DIR are the runner's own vocabulary, so a project with integration specs
;; beside its unit specs passes a type of integration.

;;; Code:

(define-module (maak veritas)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (veritas-expression
            veritas-env
            veritas-test-argv
            veritas-shell-command
            veritas-test))

;; The form guile is asked to run.  The exit matters: the runner answers with a
;; status, and without `exit' that status would describe the last expression
;; rather than the suite.
(define veritas-expression
  "(exit ((@ (veritas runner) run-tests)))")

(define (require-name value what)
  "Return VALUE as a non-empty string, or raise naming WHAT."
  (unless (and (string? value) (not (string=? value "")))
    (raise-maak-error
     (format #f "~a takes a non-empty string, got ~s" what value)
     #:hint "Name the value as a string, or leave the option out."))
  value)

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write a list such as ./src and ./test for the load paths."))
    items))

(define* (veritas-env #:key (module "veritas") (top-dir "test")
                      (type "unit") (benchmarks? #f))
  "Return the VERITAS_* assignments the runner reads, as NAME=VALUE strings.

The suite is located by MODULE, the module the runner loads, TOP-DIR, where the
specs live, and TYPE, which directory under TOP-DIR.  BENCHMARKS? turns the
benchmark run on.  They are strings because `run' hands its `#:env' straight to
`env', so a value with a space in it stays one value.

@example
(veritas-env)
@result{} (\"VERITAS_TEST_MODULE=veritas\" \"VERITAS_TEST_TOP_DIR=test\" ...)
@end example"
  (list (format #f "VERITAS_TEST_MODULE=~a" (require-name module "veritas's module"))
        (format #f "VERITAS_TEST_TOP_DIR=~a" (require-name top-dir "veritas's top directory"))
        (format #f "VERITAS_TEST_TYPE=~a" (require-name type "veritas's type"))
        (format #f "VERITAS_RUN_BENCHMARKS=~a" (if benchmarks? "true" "false"))))

(define (veritas-words guile load-paths)
  "Return the interpreter and the load path flags, which both shapes share.

The two shapes differ in one word -- whether the expression is quoted for a
shell -- so everything before it is built once, here."
  (append (list (require-name guile "veritas's guile"))
          (append-map (lambda (path)
                        (list "-L" (require-name path "veritas's load path")))
                      (string-list load-paths "veritas's load paths"))))

(define* (veritas-test-argv #:key (guile "guile")
                            (load-paths '("./src" "./test"))
                            (expression veritas-expression))
  "Return the argv that runs the Veritas suite, without running it.

LOAD-PATHS are the directories guile searches before anything installed, which
is what makes the checkout's own library and specs the ones under test.  GUILE
names the interpreter, so a task can pin one.

The expression is one argv element, unquoted: this list is what `run' starts
directly.  For the words a manifest shell re-reads, use `veritas-shell-command',
which is this list with the expression quoted.

@example
(veritas-test-argv)
@result{} (\"guile\" \"-L\" \"./src\" \"-L\" \"./test\" \"-c\" veritas-expression)
@end example"
  (append (veritas-words guile load-paths)
          (list "-c" (require-name expression "veritas's expression"))))

(define* (veritas-shell-command #:key (guile "guile")
                                (load-paths '("./src" "./test"))
                                (expression veritas-expression))
  "Return the same words with EXPRESSION quoted for a shell.

`manifest-shell' and `time-machine-manifest-shell' join their words into one
command line, so the expression, which has spaces and parentheses, has to
survive as one word.  Use `veritas-test-argv' with `run', which starts guile
without a shell and needs no quoting."
  (append (veritas-words guile load-paths)
          (list "-c" (shell-quote (require-name expression "veritas's expression")))))

(define* (veritas-test #:key (verbose? #f) (guile "guile")
                       (load-paths '("./src" "./test"))
                       (module "veritas") (top-dir "test") (type "unit")
                       (benchmarks? #f) (expression veritas-expression))
  "Run the Veritas suite and fail the task when it does not pass.

This is the one line a Maak file needs for its test task:

@example
(define (test)
  (veritas-test))
@end example

The status the runner answers with is the exit status guile leaves, so a red
suite is a failed task with no shell fallback to remember."
  (run (veritas-test-argv #:guile guile #:load-paths load-paths
                          #:expression expression)
       #:env (veritas-env #:module module #:top-dir top-dir
                          #:type type #:benchmarks? benchmarks?)
       #:verbose? verbose?))
