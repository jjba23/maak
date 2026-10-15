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
                               #:got (names events))))
         (test "a test with no line of its own has no cases"
               (let ((events (veritas-report-events
                              (report (line "test" "adds numbers")
                                      (line "pass" "got: 4")
                                      (line "pass" "got: 5")
                                      (line "test" "asserts nothing")))))
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
                 (assert-equal #:expect '("last with a case" "last without one")
                               #:got (names events))))
         (test "a test at the very end of the report keeps its cases"
               (let ((events (veritas-report-events
                              (report (line "test" "last with a case")
                                      (line "pass" "got: ok")
                                      (line "test" "last without one")))))
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
                               #:got (map veritas-test-name vacuous))))
         (test "a finding names the file it was reported under"
               (let* ((events (veritas-report-events
                               (report "running unit tests from file: dsl.scm, entrypoint: spec"
                                       (line "test" "checks something")
                                       (line "pass" "got: ok")
                                       (line "test" "prints instead"))))
                      (vacuous (veritas-vacuous-tests events)))
                 (assert-equal #:expect '("dsl.scm")
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

(define failures-suite
  (suite "Which failures the report reported"
         #:concurrent? #f
         #:shuffle? #f
         (test "a fail line is a failed case of the test it follows"
               (assert-equal
                #:expect '(1 0)
                #:got (map veritas-test-failed-cases
                          (veritas-report-tests
                           (veritas-report-events
                            (report (line "test" "fails")
                                    (line "fail" "expected 1, got 2")
                                    (line "test" "passes")
                                    (line "pass" "got: ok")))))))
         (test "an error line is not a failed case"
               ;; The run logs its own giving up as an error, and that line
               ;; belongs to no test: counting it would let a case that recorded
               ;; no assertion look like one that reported a failure.
               (assert-equal
                #:expect '(0)
                #:got (map veritas-test-failed-cases
                          (veritas-report-tests
                           (veritas-report-events
                            (report (line "test" "quiet")
                                    (line "error"
                                          "aborting due to error in test case...")))))))
         (test "a passing test has no failed case"
               (assert-equal
                #:expect '(0)
                #:got (map veritas-test-failed-cases
                          (veritas-report-tests
                           (veritas-report-events
                            (report (line "test" "one")
                                    (line "pass" "got: 1")))))))))

(define rendering-suite
  (suite "Rendering a report for a person"
         #:concurrent? #f
         #:shuffle? #f
         (test "every event is handed over, in the order the report holds it"
               (let ((written '())
                     (port (open-output-string)))
                 (veritas-report-render
                  (report (line "test" "one")
                          (line "pass" "got: 1")
                          (line "fail" "expected 2, got 1"))
                  #:logger (lambda (level message)
                             (set! written (cons (cons level message) written)))
                  #:port port)
                 (assert-equal
                  #:expect '(("test" . "one")
                             ("pass" . "got: 1")
                             ("fail" . "expected 2, got 1"))
                  #:got (reverse written))))
         (test "the runner's own lines are printed as they stand"
               (let ((port (open-output-string)))
                 (veritas-report-render
                  (report "scanning directories for unit tests..."
                          (line "test" "one"))
                  #:logger (lambda (level message) '())
                  #:port port)
                 (assert-contains
                  #:expect (get-output-string port)
                  #:got "scanning directories for unit tests...")))
         (test "what a test printed after an event is kept"
               (let ((port (open-output-string)))
                 (veritas-report-render
                  (report (string-append (line "pass" "got: #f")
                                         "and the words it printed"))
                  #:logger (lambda (level message) '())
                  #:port port)
                 (assert-contains #:expect (get-output-string port)
                                  #:got "and the words it printed")))
         (test "a level Veritas does not decorate is printed plainly"
               (let ((port (open-output-string)))
                 (parameterize ((current-output-port port))
                   ((veritas-report-logger) "warn" "a level of its own"))
                 (assert-contains #:expect (get-output-string port)
                                  #:got "a level of its own")))))

(define source-suite
  (suite "The assertions a run cannot read"
         #:concurrent? #f
         #:shuffle? #f
         (test "a case of one assertion is read"
               (assert-equal #:expect '()
                             #:got (veritas-source-findings
                                    "(test \"one\" (assert-true #t))")))
         (test "a case whose body holds two assertions drops one"
               (assert-equal
                #:expect '(("two" . 1))
                #:got (veritas-source-findings
                       "(test \"two\" (let ((x 1)) (assert-true x) (assert-true x)))")))
         (test "every assertion before the last one is dropped"
               (assert-equal
                #:expect '(("many" . 2))
                #:got (veritas-source-findings
                       "(test \"many\" (begin (assert-true #t) (assert-true #t) (assert-true #t)))")))
         (test "a case that calls the assertion it builds is read"
               (assert-equal #:expect '()
                             #:got (veritas-source-findings
                                    "(test \"calls\" (lambda (vc) ((assert-true #t) vc)))")))
         (test "an assertion a lambda case builds and never calls is dropped"
               (assert-equal
                #:expect '(("lambda" . 1))
                #:got (veritas-source-findings
                       "(test \"lambda\" (lambda (vc) (assert-true #t)))")))
         (test "both branches of a conditional are followed"
               (assert-equal
                #:expect '()
                #:got (veritas-source-findings
                       "(test \"if\" (if (even? 2) (assert-true #t) (assert-true #f)))")))
         (test "a test nested in a suite is read"
               (assert-equal
                #:expect '(("inner" . 1))
                #:got (veritas-source-findings
                       "(suite \"outer\" (test \"inner\" (let ((x 1)) (assert-true x) (assert-true x))))")))
         (test "a test's flags and hooks are not cases"
               (assert-equal
                #:expect '()
                #:got (veritas-source-findings
                       "(test \"hooked\" #:before (lambda (vc) #t) (assert-true #t))")))
         (test "a source with no test in it has no finding"
               (assert-equal #:expect '()
                             #:got (veritas-source-findings "(define (helper) 1)")))
         (test "a source that cannot be read has no finding"
               (assert-equal #:expect '()
                             #:got (veritas-source-findings "(test \"unclosed\"")))
         (test "a test the source does not name is still a finding"
               (assert-equal
                #:expect '((#f . 1))
                #:got (veritas-source-findings
                       "(test (make-name) (let ((x 1)) (assert-true x) (assert-true x)))")))))

(define (spec-reader files)
  "Return a procedure reading the spec names of FILES from the source they hold."
  (lambda (file)
    (or (assoc-ref files file) "")))

(define unread-suite
  (suite "The unread assertions of a run"
         #:concurrent? #f
         #:shuffle? #f
         (test "the files the report names are the ones a run read"
               (assert-equal
                #:expect '("dsl.scm" "rot.scm")
                #:got (veritas-report-files
                       (veritas-report-events
                        (report "running unit tests from file: dsl.scm, entrypoint: spec"
                                (line "test" "one")
                                "running unit tests from file: rot.scm, entrypoint: spec"
                                (line "test" "two"))))))
         (test "a file the report names twice is listed once"
               (assert-equal
                #:expect '("dsl.scm")
                #:got (veritas-report-files
                       (veritas-report-events
                        (report "running unit tests from file: dsl.scm, entrypoint: spec"
                                (line "test" "one")
                                (line "test" "two"))))))
         (test "a report that names no file lists none"
               (assert-equal
                #:expect '()
                #:got (veritas-report-files
                       (veritas-report-events
                        (report (line "test" "one"))))))
         (test "a dropped assertion is found in the file it is in"
               (assert-equal
                #:expect '(("rot.scm" "two" 1))
                #:got (veritas-unread-findings
                       (veritas-report-events
                        (report "running unit tests from file: rot.scm, entrypoint: spec"
                                (line "test" "two")))
                       (spec-reader
                        '(("rot.scm" .
                           "(test \"two\" (let ((x 1)) (assert-true x) (assert-true x)))"))))))
         (test "every file a run named is read for its own findings"
               (assert-equal
                #:expect '(("dsl.scm" #f 1) ("rot.scm" "two" 1))
                #:got (veritas-unread-findings
                       (veritas-report-events
                        (report "running unit tests from file: dsl.scm, entrypoint: spec"
                                (line "test" "one")
                                "running unit tests from file: rot.scm, entrypoint: spec"
                                (line "test" "two")))
                       (spec-reader
                        '(("dsl.scm" .
                           "(test (make-name) (let ((x 1)) (assert-true x) (assert-true x)))")
                          ("rot.scm" .
                           "(test \"two\" (let ((x 1)) (assert-true x) (assert-true x)))"))))))
         (test "a file the report did not run is not read"
               (let ((asked '()))
                 (veritas-unread-findings
                  (veritas-report-events
                   (report "running unit tests from file: rot.scm, entrypoint: spec"
                           (line "test" "two")))
                  (lambda (file)
                    (set! asked (cons file asked))
                    ""))
                 (assert-equal #:expect '("rot.scm") #:got asked)))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               reading-suite
               counting-suite
               findings-suite
               failures-suite
               rendering-suite
               source-suite
               unread-suite))
