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
;; Tests for the command line itself: the greeting banner and how to turn it
;; off, the environment files, and what a task that fails looks like from the
;; outside.
;;
;; Everything here goes through `main', the way the wrapper script calls it,
;; with the output ports captured so the tests assert on what a user would read.
;; `main' loads the fixture Maak file, which defines a module named `(maak)';
;; that is what a real run does too, but it does mean these suites must run
;; sequentially rather than as fibers.
;;
;; The fixture paths are relative to the project root, which is where the test
;; runner is started from (see the Tests section of the README).

;;; Code:

(define-module (veritas unit cli)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak main)
  #:export (spec))

(define fixture-file "test/maak/fixtures/cli.scm")
(define fixture-env "test/maak/fixtures/cli.env")

(define (options-for . args)
  "Return the parsed command line ARGS, as `main' would see it."
  (getopt-long (cons "maak" args) maak-option-spec))

(define (run-cli . args)
  "Run the CLI with ARGS and return `(STATUS STDOUT STDERR)'."
  (let ((out (open-output-string))
        (err (open-output-string)))
    (let ((status (parameterize ((current-output-port out)
                                 (current-error-port err))
                    ;; The keyword goes inside the list `apply' hands over,
                    ;; or it would be passed as a positional argument.
                    (apply main (list #:args (cons "maak" args))))))
      (list status (get-output-string out) (get-output-string err)))))

(define (status-of result)
  (list-ref result 0))

(define (stdout-of result)
  (list-ref result 1))

(define (stderr-of result)
  (list-ref result 2))

(define (run-task task . extra)
  "Run TASK of the fixture Maak file, without the banner."
  (apply run-cli (append (list "--file" fixture-file
                               "--no-banner"
                               "--tasks" task)
                         extra)))

(define switch-suite
  (suite "Command line switches"
         #:concurrent? #f
         #:shuffle? #f
         (test "the environment switch understands the usual negatives"
               (assert-false (truthy-string? #f))
               (assert-false (truthy-string? ""))
               (assert-false (truthy-string? "0"))
               (assert-false (truthy-string? "false"))
               (assert-false (truthy-string? "NO"))
               (assert-true (truthy-string? "1"))
               (assert-true (truthy-string? "yes")))
         (test "the banner is on unless something turns it off"
               (assert-true (resolve-banner? (options-for)))
               (assert-false (resolve-banner? (options-for "--no-banner")))
               (assert-true (resolve-banner? (options-for "--banner"))))
         (test "MAAK_NO_BANNER turns the banner off and --banner wins over it"
               (env-set! "MAAK_NO_BANNER" "1")
               (assert-false (resolve-banner? (options-for)))
               (assert-true (resolve-banner? (options-for "--banner")))
               (assert-false (resolve-banner? (options-for "--no-banner")))
               (env-unset! "MAAK_NO_BANNER"))
         (test "environment files come from the flag, the environment, or the file"
               (assert-equal #:expect '("a.env" "b.env")
                             #:got (resolve-env-files
                                    (options-for "--env-file" "a.env:b.env")
                                    fixture-file))
               (assert-equal #:expect '()
                             #:got (resolve-env-files
                                    (options-for "--no-env-file")
                                    fixture-file))
               (env-set! "MAAK_ENV_FILE" "one.env:two.env")
               (assert-equal #:expect '("one.env" "two.env")
                             #:got (resolve-env-files (options-for)
                                                      fixture-file))
               (env-unset! "MAAK_ENV_FILE")
               ;; Nothing given: the `.env' beside the Maak file is used.
               (assert-equal
                #:expect '("test/maak/fixtures/.env")
                #:got (resolve-env-files (options-for) fixture-file)))))

(define banner-suite
  (suite "Banner and listing"
         #:concurrent? #f
         #:shuffle? #f
         (test "the banner greets and names the Maak file"
               (let ((result (run-cli "--file" fixture-file
                                      "--tasks" "ok")))
                 (assert-equal #:expect 0 #:got (status-of result))
                 (assert-contains #:expect (stdout-of result)
                                  #:got "the infinitely extensible")
                 (assert-contains #:expect (stdout-of result)
                                  #:got fixture-file)
                 (assert-contains #:expect (stdout-of result)
                                  #:got "ok-task-ran")))
         (test "--no-banner keeps the task output"
               (let ((result (run-task "ok")))
                 (assert-equal #:expect 0 #:got (status-of result))
                 (assert-false (string-contains (stdout-of result)
                                                "the infinitely extensible"))
                 (assert-false (string-contains (stdout-of result)
                                                "Loading tasks from Maak file"))
                 (assert-contains #:expect (stdout-of result)
                                  #:got "ok-task-ran")))
         (test "MAAK_NO_BANNER is honoured by the run, not only by the switch"
               (env-set! "MAAK_NO_BANNER" "true")
               (let ((result (run-cli "--file" fixture-file
                                      "--tasks" "ok")))
                 (env-unset! "MAAK_NO_BANNER")
                 (assert-false (string-contains (stdout-of result)
                                                "the infinitely extensible"))))
         (test "--list shows every task with its docstring"
               (let ((result (run-cli "--file" fixture-file
                                      "--no-banner" "--list")))
                 (assert-equal #:expect 0 #:got (status-of result))
                 (assert-contains #:expect (stdout-of result)
                                  #:got "ok: Print a line and succeed.")
                 (assert-contains #:expect (stdout-of result)
                                  #:got "boom: Fail the way a task is supposed to fail.")))))

(define failure-suite
  (suite "A task that fails"
         #:concurrent? #f
         #:shuffle? #f
         (test "a task error prints one line with its hint and no backtrace"
               (let ((result (run-task "boom")))
                 (assert-equal #:expect 1 #:got (status-of result))
                 (assert-contains #:expect (stderr-of result)
                                  #:got "maak: error: no artefact for cli-test")
                 (assert-contains #:expect (stderr-of result)
                                  #:got "hint: Run `maak ok' first.")
                 (assert-false (string-contains (stderr-of result)
                                                "Backtrace"))
                 (assert-false (string-contains (stderr-of result)
                                                "stack"))))
         (test "an unknown task lists the tasks that do exist"
               (let ((result (run-task "nope")))
                 (assert-equal #:expect 1 #:got (status-of result))
                 (assert-contains #:expect (stderr-of result)
                                  #:got "Unknown task: nope")
                 (assert-contains #:expect (stderr-of result)
                                  #:got "Available tasks: boom, ok")))))

(define env-file-suite
  (suite "Environment files at the command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "--env-file is read before the tasks run"
               (env-unset! "MAAK_CLI_TAG")
               (let ((result (run-task "report-env" "--env-file" fixture-env)))
                 (assert-equal #:expect 0 #:got (status-of result))
                 (assert-contains #:expect (stdout-of result)
                                  #:got "TAG=from-env-file")))
         (test "MAAK_ENV_FILE is read when no flag is given"
               (env-unset! "MAAK_CLI_TAG")
               (env-set! "MAAK_ENV_FILE" fixture-env)
               (let ((result (run-task "report-env")))
                 (env-unset! "MAAK_ENV_FILE")
                 (assert-contains #:expect (stdout-of result)
                                  #:got "TAG=from-env-file")))
         (test "a .env beside the Maak file is read by default"
               (env-unset! "MAAK_CLI_DEFAULT")
               (let ((result (run-task "report-default")))
                 (assert-contains #:expect (stdout-of result)
                                  #:got "DEFAULT=from-default-env")))
         (test "--no-env-file reads nothing at all"
               (env-unset! "MAAK_CLI_DEFAULT")
               (let ((result (run-task "report-default" "--no-env-file")))
                 (assert-contains #:expect (stdout-of result)
                                  #:got "DEFAULT=unset")))
         (test "the environment beats the file for a name it already has"
               (env-set! "MAAK_CLI_TAG" "from-the-environment")
               (let ((result (run-task "report-env" "--env-file" fixture-env)))
                 (env-unset! "MAAK_CLI_TAG")
                 (assert-contains #:expect (stdout-of result)
                                  #:got "TAG=from-the-environment")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               switch-suite
               banner-suite
               failure-suite
               env-file-suite))
