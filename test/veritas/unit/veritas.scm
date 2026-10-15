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
;; Unit tests for `(maak veritas)': reading a captured Veritas report back and
;; finding the tests in it that ran no case at all.
;;
;; The reports here are built line by line rather than captured from a run, so
;; each test says exactly which shape of report it is about.

;;; Code:

(define-module (veritas unit veritas)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak veritas)
  #:export (spec))

(define (line level message)
  "Return the report line the JSON reporter writes for a LEVEL event with MESSAGE."
  (scm->json-string `((level . ,level) (message . ,message))))

(define (report . lines)
  "Return a captured report holding LINES, one to a line."
  (string-join lines "\n"))

(define (names events)
  "Return the names of the tests in the EVENTS, in report order."
  (map veritas-test-name (veritas-report-tests events)))

(define (cases events)
  "Return the number of cases of each test in the EVENTS, in report order."
  (map veritas-test-cases (veritas-report-tests events)))

(define reading-suite
  (suite "Reading a captured report"
         #:concurrent? #f
         #:shuffle? #f
         (test "a report holds one event per reporter line"
               (assert-equal
                #:expect '(("info" . "hi"))
                #:got (veritas-report-events (report "" (line "info" "hi")))))
         (test "the runner's own printing is not an event"
               (assert-equal
                #:expect '()
                #:got (veritas-report-events
                       (report "scanning directories for unit tests..."
                               "found unit tests: (dsl.scm)"
                               "nothing here"
                               "  [test] from the emoji reporter"))))
         (test "a line that starts like JSON but is not JSON is dropped"
               (assert-equal
                #:expect '(("test" . "a real one"))
                #:got (veritas-report-events
                       (report "{" "{\"level\":" "{\"level\":\"test\""
                               (line "test" "a real one")))))
         (test "what is printed after an event on its line is not part of it"
               ;; The reporter ends an event without a newline, so anything
               ;; printed next -- a command's own message, a test's `display'
               ;; -- lands on the end of that line.
               (assert-equal
                #:expect '(("test" . "a test") ("pass" . "got: #f"))
                #:got (veritas-report-events
                       (report (string-append (line "test" "a test")
                                              "mkdir: map '/tmp/x' is aangemaakt")
                               (string-append (line "pass" "got: #f")
                                               "trailing words")))))
         (test "a message holding a brace of its own is read whole"
               (assert-equal
                #:expect '("pass" . "got: {a . b} and [c]")
                #:got (car (veritas-report-events
                            (report (string-append (line "pass" "got: {a . b} and [c]")
                                                   "more output"))))))
         (test "the line naming the file is read, and dropped as a case"
               (assert-equal
                #:expect '(("file" . "dsl.scm"))
                #:got (veritas-report-events
                       (report "running unit tests from file: dsl.scm, entrypoint: spec"))))
         (test "a test is named by the line, not by its JSON quoting"
               (assert-equal
                #:expect '("the \"quoted\" case" "back\\slash")
                #:got (names (veritas-report-events
                              (report (line "test" "the \"quoted\" case")
                                      (line "pass" "got: ok")
                                      (line "test" "back\\slash")
                                      (line "pass" "got: ok"))))))))

(define counting-suite
  (suite "Counting the cases of a test"
         #:concurrent? #f
         #:shuffle? #f
         (test "the lines after a test are its cases"
               (let ((events (veritas-report-events
                              (report (line "test" "adds numbers")
                                      (line "pass" "got: 4")
                                      (line "pass" "got: 5")
                                      (line "test" "asserts nothing")))))
                 (assert-equal #:expect '("adds numbers" "asserts nothing")
                               #:got (names events))
                 (assert-equal #:expect '(2 0) #:got (cases events))))
         (test "a failing or erroring case still counts as a case"
               (let ((events (veritas-report-events
                              (report (line "test" "fails")
                                      (line "fail" "expected 1, got 2")
                                      (line "test" "errors")
                                      (line "error" "task raised an error")))))
                 (assert-equal #:expect '(1 1) #:got (cases events))))
         (test "a suite, an info line and a benchmark are not cases"
               (let ((events (veritas-report-events
                              (report (line "suite" "a suite")
                                      (line "test" "inside")
                                      (line "info" "shuffled with seed 1")
                                      (line "debug" "assertion-results: (#t)")
                                      (line "benchmark" "measures something")
                                      (line "test" "after")))))
                 (assert-equal #:expect '(0 0) #:got (cases events))))
         (test "a test at the very end of the report is not lost"
               (let ((events (veritas-report-events
                              (report (line "test" "last with a case")
                                      (line "pass" "got: ok")
                                      (line "test" "last without one")))))
                 (assert-equal #:expect '("last without a case" "last without one")
                               #:got (names events))
                 (assert-equal #:expect '(1 0) #:got (cases events))))
         (test "every test is filed under the file the runner was running"
               (let ((events (veritas-report-events
                              (report "running unit tests from file: dsl.scm, entrypoint: spec"
                                      (line "test" "first")
                                      (line "pass" "got: ok")
                                      (line "test" "second")
                                      (line "pass" "got: ok")
                                      "running unit tests from file: cli.scm, entrypoint: spec"
                                      (line "test" "third")
                                      (line "pass" "got: ok")))))
                 (assert-equal #:expect '("dsl.scm" "dsl.scm" "cli.scm")
                               #:got (map veritas-test-file
                                           (veritas-report-tests events)))))))

(define findings-suite
  (suite "The tests that asserted nothing"
         #:concurrent? #f
         #:shuffle? #f
         (test "a test with no case of its own is the finding"
               (let* ((events (veritas-report-events
                               (report "running unit tests from file: dsl.scm, entrypoint: spec"
                                       (line "test" "checks something")
                                       (line "pass" "got: ok")
                                       (line "test" "prints instead")
                                       (line "test" "cleans up instead"))))
                      (vacuous (veritas-vacuous-tests events)))
                 (assert-equal #:expect '("prints instead" "cleans up instead")
                               #:got (map veritas-test-name vacuous))
                 (assert-equal #:expect '("dsl.scm" "dsl.scm")
                               #:got (map veritas-test-file vacuous))))
         (test "a report where every test has a case has no finding"
               (assert-equal
                #:expect '()
                #:got (veritas-vacuous-tests
                       (veritas-report-events
                        (report (line "test" "one") (line "pass" "got: 1")
                                (line "test" "two") (line "fail" "got: no"))))))
         (test "the totals are the tests and the cases of the run"
               (assert-equal
                #:expect '((tests . 3) (cases . 3))
                #:got (veritas-report-totals
                       (veritas-report-events
                        (report (line "test" "one") (line "pass" "got: 1")
                                (line "test" "two") (line "pass" "got: 2")
                                (line "pass" "got: 3")
                                (line "test" "three"))))))
         (test "a report of nothing at all has no tests and no cases"
               (assert-equal #:expect '((tests . 0) (cases . 0))
                             #:got (veritas-report-totals
                                    (veritas-report-events "nothing was run")))
               (assert-equal #:expect '()
                             #:got (veritas-report-tests
                                    (veritas-report-events ""))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               reading-suite
               counting-suite
               findings-suite))
