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
;;
;; One more question is asked of the same run, and it cannot be answered from
;; the report, because Veritas never reports what it could not read: the
;; assertions written *inside* a case that the case does not read.  A case is
;; the value of a form, so a body of several forms gives the run only its last
;; assertion and drops the rest -- they are evaluated, their verdicts are never
;; read, and the suite goes green whatever they would have said.  An assertion
;; that is never called logs nothing, so what the run never read cannot be read
;; back: the question is answered by reading the spec instead of the report.

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
            veritas-test-cases
            veritas-test-failed-cases
            veritas-report-render
            veritas-report-logger
            veritas-report-files
            veritas-source-findings
            veritas-unread-findings))

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

(define (veritas-test file name cases failed)
  "Return one reported test: its FILE, NAME, CASES and failing CASES."
  (list (cons 'file file)
        (cons 'name name)
        (cons 'cases cases)
        (cons 'failed failed)))

(define (veritas-test-name test)
  "Return the name of the reported TEST."
  (assoc-ref test 'name))

(define (veritas-test-file test)
  "Return the file of the reported TEST, as the report names it, or #f."
  (assoc-ref test 'file))

(define (veritas-test-cases test)
  "Return the number of cases the reported TEST ran."
  (assoc-ref test 'cases))

(define (veritas-test-failed-cases test)
  "Return how many of the cases of the reported TEST the report failed.

A failed case is one that reported a failure of its own.  An error line is not
one of them: a task that raised, and the run giving up on a case that recorded
nothing, are both logged at the end of a section rather than as a case."
  (assoc-ref test 'failed))

(define (count-case test)
  "Return TEST with one more case to its name."
  (veritas-test (veritas-test-file test)
                (veritas-test-name test)
                (+ 1 (veritas-test-cases test))
                (veritas-test-failed-cases test)))

(define (count-failed-case test)
  "Return TEST with one more of its cases counted as failed."
  (veritas-test (veritas-test-file test)
                (veritas-test-name test)
                (veritas-test-cases test)
                (+ 1 (veritas-test-failed-cases test))))

(define (veritas-report-tests events)
  "Return the tests of EVENTS, in the order the report lists them.

Each entry is an alist of `file', `name', `cases' and `failed'.  Every event that
is not itself a test, a suite, an info line or a benchmark is counted as a case
of the test it follows, which is what makes a count of zero mean what it says:
the test was announced, and nothing about it was ever reported again.  A `fail'
is counted again as a failed case, so a test whose failure the report never
reported as one of its cases has a count of zero there."
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
                   (veritas-test file message 0 0)
                   (if pending (cons pending finished) finished)))
            ((member level case-free-levels)
             (loop (cdr events) file pending finished))
            (else
             (loop (cdr events)
                   file
                   (if pending
                       (if (string=? level "fail")
                           (count-failed-case (count-case pending))
                           (count-case pending))
                       #f)
                   finished)))))))

;;; ====== The report a person reads ======
;;
;; A run reported as JSON is the run reported once: the reporter that writes
;; the lines a person reads and the reporter that writes data differ only in how
;; they decorate the same events, and Veritas' own logger can decorate them
;; again.  That is what this does: every event is handed to Veritas' reporter
;; and everything else -- the runner's chatter, whatever a test printed -- is
;; printed as it stands, so the report reads back the way it would have looked.
;; Veritas is looked up rather than imported: a project that reads a report need
;; not have the framework installed, and the plain fallback keeps the task
;; working when it is not there.

(define renderable-levels
  '("pass" "fail" "error" "test" "suite" "debug" "info" "assert"))

(define (veritas-dispatch-log)
  "Return Veritas' own `dispatch-log', or #f when Veritas is not on the load path."
  (catch #t
    (lambda () (module-ref (resolve-module '(veritas reporter))
                           'dispatch-log))
    (lambda (key . args) #f)))

(define (veritas-report-logger)
  "Return the procedure that writes one event of a report for a person.

It takes the LEVEL of the event and its MESSAGE, and writes them where the
current output port points.  Veritas' own reporter writes them when it is on the
load path, so the report reads as the reporter would have printed it; without
Veritas, the level and the message are printed plainly instead.  A level that re-
porter has no logging for is printed plainly too, which is what keeps a level a
later Veritas adds from bringing a report down."
  (let ((dispatch (veritas-dispatch-log)))
    (lambda (level message)
      (if (and dispatch (member level renderable-levels))
          (or (catch #t
                (lambda ()
                  (dispatch (list (cons 'reporter 'emoji-reporter))
                            (string->symbol (string-append "log-" level))
                            message)
                  #t)
                (lambda (key . args) #f))
              (format #t "  [~a] ~a" level message))
          (format #t "  [~a] ~a" level message)))))

(define (leading-space-count line)
  "Return how many characters of LINE are whitespace before its first word."
  (let loop ((i 0))
    (if (and (< i (string-length line))
             (char-whitespace? (string-ref line i)))
        (loop (+ i 1))
        i)))

(define (report-event-at text at)
  "Return the event that starts at AT, as `(END LEVEL MESSAGE)', or #f.

END is where the event's JSON object stops in TEXT, so a caller can print what
follows it as what it is -- the output that landed after the event.  An event
starts a line of its own: the reporter writes one with a newline in front of it
and none after it, so a brace in the middle of a line is something a test
printed rather than an event."
  (if (and (> at 0) (not (char=? (string-ref text (- at 1)) #\newline)))
      #f
      (let* ((line-end (or (string-index text #\newline at)
                           (string-length text)))
             (line (substring text at line-end))
             (indent (leading-space-count line))
             (trimmed (string-trim-right (substring line indent)))
             (length (and (string-prefix? "{" trimmed)
                          (json-object-length trimmed))))
        (if (not length)
            #f
            (let ((object (catch #t
                            (lambda ()
                              (json-string->scm (substring trimmed 0 length)))
                            (lambda (key . args) #f))))
              (if (not (and (list? object) (assoc-ref object "level")))
                  #f
                  (let ((message (assoc-ref object "message")))
                    (list (+ at indent length)
                          (assoc-ref object "level")
                          (if (string? message) message "")))))))))

(define (render-text text from to)
  "Print the part of TEXT between FROM and TO, when there is one."
  (if (< from to)
      (display (substring text from to))))

(define* (veritas-report-render text
                                #:key
                                (logger (veritas-report-logger))
                                (port (current-output-port)))
  "Print TEXT, a captured report, the way a reporter would have printed it.

LOGGER writes one event, and defaults to Veritas' own reporter.  Everything
between events is printed as it stands: the runner's progress lines and whatever
a test printed on the way through are part of the report a person reads, and
none of it is an event.  The newline the reporter puts in front of an event is
dropped, because LOGGER writes one of its own."
  (with-output-to-port port
    (lambda ()
      (let loop ((at 0) (from 0))
        (if (>= at (string-length text))
            (render-text text from at)
            (let ((event (report-event-at text at)))
              (if (not event)
                  (loop (+ at 1) from)
                  (let ((before (if (and (> at 0)
                                         (char=? (string-ref text (- at 1))
                                                 #\newline))
                                    (- at 1)
                                    at)))
                    (render-text text from before)
                    (logger (cadr event) (caddr event))
                    (loop (car event) (car event))))))))))

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

(define (veritas-report-files events)
  "Return the spec files EVENTS say the runner ran, without duplicates.

Each name is the one the report uses -- `dsl.scm' -- which is relative to the
directory of the test type.  Only the files the report names are listed, so a
run that was filtered to one file is judged on what it ran."
  (delete-duplicates (filter-map veritas-test-file (veritas-report-tests events))))

;;; ====== The cases a run cannot read ======
;;
;; A case is the value of a form in a test's body, and the run reads one thing
;; from it: the assertion it evaluates to, called with the validation client.
;; A body of several forms therefore gives the run its last assertion only.  The
;; assertions before it are built when the case runs and then dropped, still
;; carrying verdicts nobody will read, which is how a suite comes to check a
;; fraction of what it says it checks while going green.
;;
;; None of the dropped assertions reaches the report -- an assertion that is
;; never called logs nothing, so there was never an event -- and what the run
;; never read cannot be read back.  So the question is answered from the source
;; instead, where an assertion the case neither evaluates to nor calls is
;; visible as what it is.
;; The reading is deliberately generous about what a form evaluates to: a body is
;; followed to its last form and a conditional to every branch, so a case that
;; hands its assertion up through a `let' or an `if' is left alone.  An
;; assertion built inside a lambda is not followed, because the value of a
;; lambda is not what a case is read for: such an assertion is only read when
;; the case calls it, and calling it is what the reading looks for next.

(define (assertion-call? form)
  "Return whether FORM is a call to an assertion procedure.

Every assertion Veritas offers is named after one: `assert', `assert-true',
`assert-equal' and the rest.  A call to something else -- a helper of the
suite's own, say -- is not an assertion here, and stays where it is."
  (and (pair? form)
       (symbol? (car form))
       (string-prefix? "assert" (symbol->string (car form)))))

(define (lambda-form? form)
  "Return whether FORM is a lambda."
  (and (pair? form)
       (memq (car form) '(lambda lambda* case-lambda))))

(define (last-form form)
  "Return the last form of FORM, or #f when it holds none."
  (if (pair? form)
      (car (last-pair form))
      #f))

(define (value-chain form)
  "Return the subforms whose value could be the value of FORM.

This is what says which assertion a case is read for.  A body is followed to its
last form (`let', `begin', `when' and everything else shaped like them), a
conditional to each of its branches, and a lambda to nothing."
  (cond
    ((not (pair? form)) '())
    ((lambda-form? form) '())
    (else
     (let ((head (car form)))
       (cond
         ((eq? head 'if)
          (remove not (list (if (pair? (cddr form)) (caddr form) #f)
                            (if (pair? (cdddr form)) (car (cdddr form)) #f))))
         ((memq head '(cond case))
          (filter-map (lambda (clause)
                        (and (pair? clause) (last-form clause)))
                      (cdr form)))
         (else
          (let ((last (last-form form)))
            (if last (list last) '()))))))))

(define (case-assertions form)
  "Return the assertions a run reads for FORM, when FORM is a case."
  (cond
    ((not (pair? form)) '())
    ((assertion-call? form) (list form))
    (else (append-map case-assertions (value-chain form)))))

(define (assertions-within form)
  "Return every assertion call written in FORM, however deeply it nests."
  (cond
    ((not (pair? form)) '())
    ((assertion-call? form) (list form))
    (else (append (assertions-within (car form))
                  (assertions-in-forms (cdr form))))))

(define (assertions-in-forms forms)
  "Return every assertion call written in FORMS, a list of forms."
  (cond
    ((null? forms) '())
    ((pair? forms) (append (assertions-within (car forms))
                           (assertions-in-forms (cdr forms))))
    (else (assertions-within forms))))

(define (applied-assertions form)
  "Return the assertion calls FORM calls, as `((assert-true #t) vc)' does.

An assertion that is called hands its verdict to whatever called it, so it is
read even when the case does not evaluate to it."
  (cond
    ((not (pair? form)) '())
    ((and (pair? (car form)) (assertion-call? (car form)))
     (cons (car form)
           (append (applied-assertions (car form))
                   (applied-in-forms (cdr form)))))
    (else (append (applied-assertions (car form))
                  (applied-in-forms (cdr form))))))

(define (applied-in-forms forms)
  "Return the assertion calls called anywhere in FORMS, a list of forms."
  (cond
    ((null? forms) '())
    ((pair? forms) (append (applied-assertions (car forms))
                           (applied-in-forms (cdr forms))))
    (else (applied-assertions forms))))

(define (dropped-assertions form)
  "Return the assertion calls written in FORM that a run can never read.

An assertion is read when the case evaluates to it, or when the case calls it:
`((assert-true #t) vc)' puts its verdict into the case's own result.  Everything
else written in the case is built, evaluated and dropped."
  (let ((read (case-assertions form))
        (applied (applied-assertions form)))
    (remove (lambda (call) (or (memq call read) (memq call applied)))
            (assertions-within form))))

(define (case-forms test-form)
  "Return the case forms of TEST-FORM.

The cases are its arguments after the name, minus the `#:keyword value' pairs,
which are the test's flags and hooks rather than cases of their own."
  (let loop ((forms (cddr test-form)) (cases '()))
    (cond
      ((null? forms) (reverse cases))
      ((not (pair? forms)) (reverse cases))
      ((keyword? (car forms))
       (loop (if (pair? (cdr forms)) (cddr forms) '()) cases))
      (else (loop (cdr forms) (cons (car forms) cases))))))

(define (test-forms form)
  "Return every `test' form written in FORM, at any depth."
  (cond
    ((not (pair? form)) '())
    ((eq? (car form) 'test) (list form))
    (else (append (test-forms (car form))
                  (test-forms-in (cdr form))))))

(define (test-forms-in forms)
  "Return every `test' form written in FORMS, a list of forms."
  (cond
    ((null? forms) '())
    ((pair? forms) (append (test-forms (car forms))
                           (test-forms-in (cdr forms))))
    (else (test-forms forms))))

(define (read-forms text)
  "Return every form of TEXT, or #f when it cannot be read as source.

A spec that cannot be read is not reported on here: the runner fails on it long
before this reading would."
  (catch #t
    (lambda ()
      (call-with-input-string text
        (lambda (port)
          (let loop ((form (read port)) (forms '()))
            (if (eof-object? form)
                (reverse forms)
                (loop (read port) (cons form forms)))))))
    (lambda (key . args) #f)))

(define (source-finding test-form)
  "Return the finding TEST-FORM makes, or #f when it makes none.

A finding is the test's name, as far as the source writes it down, and how many
assertions of it the run drops."
  (let ((dropped (append-map dropped-assertions (case-forms test-form))))
    (and (pair? dropped)
         (let ((name (and (pair? (cdr test-form)) (cadr test-form))))
           (cons (if (string? name) name #f)
                 (length dropped))))))

(define (veritas-source-findings text)
  "Return the cases of TEXT, a spec, that write assertions a run cannot read.

Each finding is a pair: the name of the test, when the source writes it as a
string, and the number of assertions written in its cases that the run evaluates
and then drops, because the case neither evaluates to them nor calls them.  A
case of one assertion always reads, however it is written; what this finds is the
second, third and fourth assertion of a body, which look like checks and are not.

@example
(veritas-source-findings spec-source)
@result{} ((\"one\" . 1))
@end example"
  (let ((forms (read-forms text)))
    (if (not forms)
        '()
        (filter-map source-finding (append-map test-forms forms)))))

(define (veritas-unread-findings events read-spec)
  "Return the assertions of a run's specs that no run can read.

EVENTS is the report of a run and READ-SPEC takes the name of a spec file, as
the report writes it, and returns that file's source.  Only the files the report
says the runner ran are read, so a run that was filtered to one file is judged
on what it ran.  Each finding is `(FILE NAME COUNT)': the file the case is in,
the name of its test as the source writes it (or #f when it writes none), and
how many assertions of it the run drops.

@example
(veritas-unread-findings report-events read-file)
@result{} ((\"one.scm\" \"names\" 1))
@end example"
  (append-map (lambda (file)
                (map (lambda (finding)
                       (list file (car finding) (cdr finding)))
                     (veritas-source-findings (read-spec file))))
              (veritas-report-files events)))
