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
;; Reading the report a Veritas run leaves behind.
;;
;; A test whose body evaluates to something that is not an assertion has no
;; cases at all, and Veritas is quite right to run it: there is nothing to fail.
;; The trouble is that such a test is indistinguishable from a passing one in
;; the report, so the suite can rot in silence.  This module answers the one
;; question that tells the two apart -- which tests produced no case of their
;; own? -- by reading the report back.
;;
;; The input is the capture of a run made with Veritas' JSON reporter, which
;; writes one line per event: a `test' line, then one line per case, and so on.
;; A line that is not JSON is ignored, so the runner's own progress lines and
;; whatever a suite prints on the way through are simply skipped.  The one
;; exception is the runner's line naming the file it is about to run, which is
;; read too: it is what lets a finding point at a file rather than only at a
;; name.  The parser is otherwise indifferent to *which* reporter wrote the
;; lines and to what Veritas calls its levels -- anything that is neither a
;; test, a suite, an info line nor a benchmark counts as evidence that the test
;; it follows did run something, so a level added in a later Veritas release
;; reads as a case rather than as silence.
;;
;; One detail of the reporter's output shapes the parser: an event line is
;; written with a newline in front of it and none after it, and the capture is
;; shared with the tests themselves, so whatever prints next -- a command's
;; message, a test's own `display' -- lands on the end of the event's line.
;; A line is therefore read as the JSON object it starts with, cut out at the
;; brace that closes it, and not as JSON to its last character.

;;; Code:

(define-module (maak veritas)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (veritas-report-events
            veritas-report-tests
            veritas-vacuous-tests
            veritas-report-totals
            veritas-test-name
            veritas-test-file
            veritas-test-cases))

;; The levels of the report that are not a case: the structural lines that open
;; a test or a suite, the runner's own chatter, and a benchmark, which measures
;; instead of asserting.
(define case-free-levels
  '("test" "suite" "benchmark" "info" "debug"))

;; The runner prints this before each file it runs, and in a parallel run the
;; parent prints the files in order as it follows them, so the line always
;; arrives ahead of the tests it belongs to.
(define running-file-regexp
  (make-regexp "running .* tests from file: ([^,]*), entrypoint:"))

(define (veritas-file-marker line)
  "Return the test file LINE says the runner is about to run, or #f.

The name is the one the report uses, relative to the directory of the test
type -- `dsl.scm' for what is really `test/veritas/unit/dsl.scm' -- so a caller
that knows the tree is what turns it into a path."
  (let ((match (regexp-exec running-file-regexp line)))
    (and match
         (match:substring match 1))))

(define (json-object-length line)
  "Return the length of the JSON object LINE starts with, or #f when it has none.

The scan keeps track of whether it is inside a string, so a message holding a
brace or an escaped quote of its own does not end the object early, and returns
as soon as the brace closing the outermost object is found.  What follows on
the line belongs to whoever printed it."
  (let loop ((i 0)
             (in-string? #f)
             (escaped? #f)
             (depth 0))
    (if (>= i (string-length line))
        #f
        (let ((c (string-ref line i)))
          (cond
            (escaped? (loop (+ i 1) in-string? #f depth))
            ((char=? c #\\) (loop (+ i 1) in-string? #t depth))
            ((char=? c #\") (loop (+ i 1) (not in-string?) #f depth))
            (in-string? (loop (+ i 1) in-string? #f depth))
            ((or (char=? c #\{) (char=? c #\[)) (loop (+ i 1) #f #f (+ depth 1)))
            ((or (char=? c #\}) (char=? c #\]))
             (if (= depth 1) (+ i 1) (loop (+ i 1) #f #f (- depth 1))))
            (else (loop (+ i 1) in-string? #f depth)))))))

(define (veritas-json-event line)
  "Return LINE as the `(LEVEL . MESSAGE)' event of a JSON reporter, or #f.

A line that is not a JSON object carrying a level is not an event: the capture
holds the runner's plain progress lines and anything the tests printed, and
none of that should count for or against a test.  The object is cut out of the
line first, because the reporter ends its lines without a newline."
  (let ((trimmed (string-trim line)))
    (if (not (string-prefix? "{" trimmed))
        #f
        (let ((length (json-object-length trimmed)))
          (if (not length)
              #f
              (catch #t
                (lambda ()
                  (let* ((object (json-string->scm (substring trimmed
                                                             0 length)))
                         (level (and (list? object)
                                     (assoc-ref object "level")))
                         (message (and (list? object)
                                       (assoc-ref object "message"))))
                    (and level
                         (cons level
                               (if (string? message) message "")))))
                (lambda (key . args) #f)))))))

(define (veritas-report-events text)
  "Return the events of TEXT, the captured report of a Veritas run, in order.

Each event is a `(LEVEL . MESSAGE)' pair: one of the reporter's own levels
(`\"test\"', `\"pass\"', `\"fail\"', and so on), or `\"file\"' with the name of the
file the runner is about to run.  Everything else in TEXT is dropped."
  (filter-map (lambda (line)
                (or (veritas-json-event line)
                    (let ((file (veritas-file-marker line)))
                      (and file (cons "file" file)))))
              (string-split text #\newline)))

(define (veritas-test file name cases)
  "Return one reported test: its FILE, its NAME and how many CASES it ran."
  (list (cons 'file file)
        (cons 'name name)
        (cons 'cases cases)))

(define (veritas-test-name test)
  "Return the name of the reported TEST."
  (assoc-ref test 'name))

(define (veritas-test-file test)
  "Return the file of the reported TEST, as the report names it, or #f."
  (assoc-ref test 'file))

(define (veritas-test-cases test)
  "Return the number of cases the reported TEST ran."
  (assoc-ref test 'cases))

(define (count-case test)
  "Return TEST with one more case to its name."
  (veritas-test (veritas-test-file test)
                (veritas-test-name test)
                (+ 1 (veritas-test-cases test))))

(define (veritas-report-tests events)
  "Return the tests of EVENTS, in the order the report lists them.

Each entry is an alist of `file', `name' and `cases'.  Every event that is not
itself a test, a suite, an info line or a benchmark is counted as a case of the
test it follows, which is what makes a count of zero mean what it says: the
test was announced, and nothing about it was ever reported again."
  (let loop ((events events)
             (file #f)
             (pending #f)
             (finished '()))
    (if (null? events)
        (reverse (if pending (cons pending finished) finished))
        (let* ((event (car events))
               (level (car event))
               (message (cdr event)))
          (cond
            ((string=? level "file")
             (loop (cdr events) message pending finished))
            ((string=? level "test")
             (loop (cdr events)
                   file
                   (veritas-test file message 0)
                   (if pending (cons pending finished) finished)))
            ((member level case-free-levels)
             (loop (cdr events) file pending finished))
            (else
             (loop (cdr events)
                   file
                   (if pending (count-case pending) #f)
                   finished)))))))

(define (veritas-vacuous-tests events)
  "Return the tests of EVENTS that ran no case at all, in report order."
  (filter (lambda (test)
            (zero? (veritas-test-cases test)))
          (veritas-report-tests events)))

(define (veritas-report-totals events)
  "Return the `(tests . N)' and `(cases . N)' of EVENTS as an alist."
  (let ((tests (veritas-report-tests events)))
    (list (cons 'tests (length tests))
          (cons 'cases (apply + (map veritas-test-cases tests))))))
