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
;; off, the environment files, what a task that fails looks like from the
;; outside, and what a run of several tasks does about the ones that fail.
;;
;; Everything here goes through `main', the way the wrapper script calls it,
;; with the output ports captured so the tests assert on what a user would read.
;; `main' loads the fixture Maak file, which defines a module named `(maak)';
;; that is what a real run does too, but it does mean these suites must run
;; sequentially rather than as fibers.
;;
;; Two rules shape how these tests are written, and breaking either one makes a
;; test that cannot fail (see the Tests section of the README):
;;
;; - Every assertion is a case of its own, so it has to be a form of its own.
;;   A case is the value a form evaluates to, so a `let' that wrapped five
;;   assertions would leave only the last of them counted, with the other four
;;   evaluated and their verdicts dropped.  A test here therefore asks for the
;;   run it looks at once per assertion instead of binding one run in a `let';
;;   a run of `main' costs a few milliseconds, which is cheaper than a test that
;;   cannot fail.
;; - An assertion's arguments are eager: they are computed once, while the file
;;   loads, in the order written.  That is what lets a test run the CLI and hand
;;   the result to a check that only compares, and it is why a variable is set
;;   and unset around the assertion rather than inside a hook.
;;
;; The fixture paths are relative to the project root, which is where the test
;; runner is started from (see the Tests section of the README).

;;; Code:

(define-module (veritas unit cli)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak main)
  #:export (spec))

(define fixture-file "test/maak/fixtures/cli.scm")
(define fixture-env "test/maak/fixtures/cli.env")
(define keep-going-fixture "test/maak/fixtures/keep-going.scm")
(define subdir "test/maak/fixtures/subdir")

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

;; `maak: error:' and the other reports are written in colour, and the escapes
;; sit between the words, so an assertion about what a user reads has to look
;; at what a terminal would show.
(define ansi-escapes
  (make-regexp "\x1b\\[[0-9;]*[A-Za-z]"))

(define (plain str)
  "Return STR without the colour escapes a report writes around its words."
  (regexp-substitute/global #f ansi-escapes str 'pre "" 'post))

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

(define (run-cli-fixture file . args)
  "Run the CLI on FILE, without the banner."
  (apply run-cli (append (list "--file" file "--no-banner") args)))

;; ====== The runs the assertions read ======
;;
;; Each of these is called once per assertion that looks at it, and any of them
;; may be called in the middle of a test that sets a variable the run reads.

(define (greeting-run)
  "Run the fixture's `ok' task with the greeting banner on."
  (run-cli "--file" fixture-file "--tasks" "ok"))

(define (bare-run)
  "Run the fixture's `ok' task without the banner."
  (run-task "ok"))

(define (listing-run)
  "List the tasks of the fixture Maak file."
  (run-cli "--file" fixture-file "--no-banner" "--list"))

(define (boom-run)
  "Run the fixture task that fails the way a task is supposed to fail."
  (run-task "boom"))

(define (unknown-run)
  "Run a task the fixture Maak file does not define."
  (run-task "nope"))

(define (env-file-run)
  "Run the fixture task that reports what an environment file set."
  (run-task "report-env" "--env-file" fixture-env))

(define (stopped-run)
  "Run four tasks of the keep-going fixture, the second and third of them failing."
  (run-cli-fixture keep-going-fixture
                   "--tasks"
                   "first-pass,second-fail,third-fail,last-pass"))

(define (kept-going-run)
  "The same four tasks, with `--keep-going'."
  (run-cli-fixture keep-going-fixture
                   "--keep-going"
                   "--tasks"
                   "first-pass,second-fail,third-fail,last-pass"))

(define (ignored-run)
  "A failing task then a passing one, with `--ignore-errors'."
  (run-cli-fixture keep-going-fixture
                   "--ignore-errors"
                   "--tasks"
                   "first-pass,second-fail,last-pass"))

(define (subdir-run)
  "Run the task of the Maak file in SUBDIR, read from the project root.

`main' changes directory for the rest of the process, which is what `make -C'
does; the working directory is put back afterwards so the rest of the run is
unaffected."
  (let ((origin (getcwd)))
    (dynamic-wind
     (lambda () #t)
     (lambda ()
       (run-cli "--directory" subdir "--no-banner" "--tasks" "sub-marker"))
     (lambda () (chdir origin)))))

(define (quiet-subdir-run)
  "The same run, asked to say nothing about the directory it entered."
  (let ((origin (getcwd)))
    (dynamic-wind
     (lambda () #t)
     (lambda ()
       (run-cli "--directory" subdir
                "--no-print-directory"
                "--no-banner"
                "--tasks" "sub-marker"))
     (lambda () (chdir origin)))))

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
               (assert-equal #:expect 0
                             #:got (status-of (greeting-run)))
               (assert-contains #:expect (stdout-of (greeting-run))
                                #:got "the infinitely extensible")
               (assert-contains #:expect (stdout-of (greeting-run))
                                #:got fixture-file)
               (assert-contains #:expect (stdout-of (greeting-run))
                                #:got "ok-task-ran"))
         (test "--no-banner keeps the task output"
               (assert-equal #:expect 0
                             #:got (status-of (bare-run)))
               (assert-false (string-contains (stdout-of (bare-run))
                                              "the infinitely extensible"))
               (assert-false (string-contains (stdout-of (bare-run))
                                              "Loading tasks from Maak file"))
               (assert-contains #:expect (stdout-of (bare-run))
                                #:got "ok-task-ran"))
         (test "MAAK_NO_BANNER is honoured by the run, not only by the switch"
               (env-set! "MAAK_NO_BANNER" "true")
               (assert-equal #:expect 0
                             #:got (status-of (greeting-run)))
               (assert-false (string-contains (stdout-of (greeting-run))
                                              "the infinitely extensible"))
               (env-unset! "MAAK_NO_BANNER"))
         (test "--list shows every task with its docstring"
               (assert-equal #:expect 0
                             #:got (status-of (listing-run)))
               (assert-contains #:expect (stdout-of (listing-run))
                                #:got "ok: Print a line and succeed.")
               (assert-contains #:expect (stdout-of (listing-run))
                                #:got "boom: Fail the way a task is supposed to fail."))))

(define failure-suite
  (suite "A task that fails"
         #:concurrent? #f
         #:shuffle? #f
         (test "a task error prints one line with its hint and no backtrace"
               (assert-equal #:expect 1
                             #:got (status-of (boom-run)))
               (assert-contains #:expect (plain (stderr-of (boom-run)))
                                #:got "maak: error: no artefact for cli-test")
               (assert-contains #:expect (plain (stderr-of (boom-run)))
                                #:got "hint: Run `maak ok' first.")
               (assert-false (string-contains (plain (stderr-of (boom-run)))
                                              "Backtrace"))
               (assert-false (string-contains (plain (stderr-of (boom-run)))
                                              "stack")))
         (test "an unknown task lists the tasks that do exist"
               (assert-equal #:expect 1
                             #:got (status-of (unknown-run)))
               (assert-contains #:expect (plain (stderr-of (unknown-run)))
                                #:got "Unknown task: nope")
               (assert-contains #:expect (plain (stderr-of (unknown-run)))
                                #:got "Available tasks: boom, ok"))))

(define env-file-suite
  (suite "Environment files at the command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "--env-file is read before the tasks run"
               (env-unset! "MAAK_CLI_TAG")
               (assert-equal #:expect 0
                             #:got (status-of (env-file-run)))
               (assert-contains #:expect (stdout-of (env-file-run))
                                #:got "TAG=from-env-file"))
         (test "MAAK_ENV_FILE is read when no flag is given"
               (env-unset! "MAAK_CLI_TAG")
               (env-set! "MAAK_ENV_FILE" fixture-env)
               (assert-contains #:expect (stdout-of (run-task "report-env"))
                                #:got "TAG=from-env-file")
               (env-unset! "MAAK_ENV_FILE"))
         (test "a .env beside the Maak file is read by default"
               (env-unset! "MAAK_CLI_DEFAULT")
               (assert-contains #:expect (stdout-of (run-task "report-default"))
                                #:got "DEFAULT=from-default-env"))
         (test "--no-env-file reads nothing at all"
               (env-unset! "MAAK_CLI_DEFAULT")
               (assert-contains
                #:expect (stdout-of (run-task "report-default" "--no-env-file"))
                #:got "DEFAULT=unset"))
         (test "the environment beats the file for a name it already has"
               (env-set! "MAAK_CLI_TAG" "from-the-environment")
               (assert-contains #:expect (stdout-of (env-file-run))
                                #:got "TAG=from-the-environment")
               (env-unset! "MAAK_CLI_TAG"))))

(define runs-suite
  (suite "Several tasks in one run"
         #:concurrent? #f
         #:shuffle? #f
         (test "a failure stops the tasks after it by default"
               (assert-equal #:expect 1
                             #:got (status-of (stopped-run)))
               (assert-contains #:expect (stdout-of (stopped-run))
                                #:got "first-pass-ran")
               (assert-false (string-contains (stdout-of (stopped-run))
                                              "last-pass-ran"))
               (assert-contains #:expect (plain (stderr-of (stopped-run)))
                                #:got "no artefact for second-fail"))
         (test "--keep-going runs the tasks after a failure and fails at the end"
               (assert-equal #:expect 1
                             #:got (status-of (kept-going-run)))
               (assert-contains #:expect (stdout-of (kept-going-run))
                                #:got "first-pass-ran")
               (assert-contains #:expect (stdout-of (kept-going-run))
                                #:got "last-pass-ran")
               ;; Every failure is reported where it happened, naming the task
               ;; that raised it, and the run says at the end which tasks did
               ;; not succeed.
               (assert-contains #:expect (plain (stderr-of (kept-going-run)))
                                #:got "maak: error (task second-fail):")
               (assert-contains #:expect (plain (stderr-of (kept-going-run)))
                                #:got "no artefact for second-fail")
               (assert-contains #:expect (plain (stderr-of (kept-going-run)))
                                #:got "(task third-fail):")
               (assert-contains #:expect (plain (stderr-of (kept-going-run)))
                                #:got "2 tasks failed: second-fail, third-fail"))
         (test "--ignore-errors keeps going and still exits 0"
               (assert-equal #:expect 0
                             #:got (status-of (ignored-run)))
               (assert-contains #:expect (stdout-of (ignored-run))
                                #:got "last-pass-ran")
               (assert-contains
                #:expect (stdout-of (ignored-run))
                #:got "maak: warning: 1 task failed and --ignore-errors was given: second-fail")
               (assert-contains #:expect (plain (stderr-of (ignored-run)))
                                #:got "no artefact for second-fail"))))

(define directory-suite
  (suite "Changing directory before anything else"
         #:concurrent? #f
         #:shuffle? #f
         (test "--directory reads the Maak file of the directory it entered"
               (assert-equal #:expect 0
                             #:got (status-of (subdir-run)))
               (assert-contains #:expect (stdout-of (subdir-run))
                                #:got "Entering directory")
               (assert-contains #:expect (stdout-of (subdir-run))
                                #:got "subdir-marker-ran"))
         (test "a directory that is not there is an error, not a silent fallback"
               (assert-equal #:expect 1
                             #:got (status-of
                                    (run-cli "--directory" "no/such/directory"
                                             "--no-banner" "--tasks" "ok")))
               (assert-contains                        #:expect (plain
                                  (stderr-of
                                   (run-cli "--directory" "no/such/directory"
                                            "--no-banner" "--tasks" "ok")))
                #:got "no such directory: no/such/directory")
               ;; The failed change of directory left the working directory
               ;; where it was, so nothing was run from the wrong place.
               (assert-true (file-exists? "./maak.scm"))
               (assert-true (file-exists? fixture-file)))
         (test "the directory entered is reported by default"
               (assert-true (resolve-print-directory? (options-for))))
         (test "--no-print-directory leaves the report out"
               (assert-false (resolve-print-directory?
                              (options-for "--no-print-directory"))))
         (test "--quiet asks for silence as well"
               (assert-false (resolve-print-directory? (options-for "--quiet"))))
         (test "--no-print-directory runs the same task without the line"
               (assert-equal #:expect 0
                             #:got (status-of (quiet-subdir-run)))
               (assert-contains #:expect (stdout-of (quiet-subdir-run))
                                #:got "subdir-marker-ran")
               (assert-false (string-contains (stdout-of (quiet-subdir-run))
                                              "Entering directory")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               switch-suite
               banner-suite
               failure-suite
               env-file-suite
               runs-suite
               directory-suite))
