;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Unit tests for `(maak tasks)', the task description: what a Maak file says
;; its tasks are when it is read rather than evaluated.  The fixture is the one
;; the shell test reads too, so a shape that is added to it is described, tested
;; and printed for editors in the same place.
;;
;; The reader is the half of the description that can answer without loading
;; the file, which is why these tests call it directly instead of going through
;; `main': what a run of `maak --list --json' answers is a spec of its own
;; (`test/veritas/unit/json.scm').

;;; Code:

(define-module (veritas unit listing)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak tasks)
  #:export (spec))

(define fixture "contrib/test/fixture/maak.scm")

(define listed (static-tasks fixture))

(define (named name)
  "Return the description of the fixture's task NAME."
  (find (lambda (description)
          (string=? name (task-description-name description)))
        listed))

(define (names)
  "Return the names of the fixture's described tasks, in file order."
  (map task-description-name listed))

(define (line-of-definition signature occurrence)
  "Return the 1-based line of the OCCURRENCE'th line starting with SIGNATURE."
  (let loop ((lines (string-split
                     (call-with-input-file fixture get-string-all)
                     #\newline))
             (number 1)
             (seen 0))
    (cond
      ((null? lines) 0)
      ((string-prefix? signature (car lines))
       (if (= seen (- occurrence 1))
           number
           (loop (cdr lines) (+ number 1) (+ seen 1))))
      (else (loop (cdr lines) (+ number 1) seen)))))

(define (broken-fixture)
  "Write a Maak file that does not read to the end, and return its path."
  (let ((path (format #f "/tmp/maak-listing-~a.scm" (getpid))))
    (call-with-output-file path
      (lambda (port) (display "(define (build \"unterminated\n" port)))
    path))

(define reading-suite
  (suite "Reading a Maak file without evaluating it"
         #:concurrent? #f
         #:shuffle? #f
         (test "the exported tasks are described in file order"
               (assert-equal #:expect '("deploy" "report" "echo" "tidy" "build")
                             #:got (names)))
         (test "an unexported procedure is not a task"
               (assert-false (member "private-helper" (names))))
         (test "a value definition is not a task"
               (assert-false (member "build-directory" (names))))
         (test "the last definition is the one described"
               (assert-equal #:expect "Compile everything."
                             #:got (task-description-doc (named "build"))))
         (test "a task is placed on the line its definition starts"
               (assert-equal #:expect (line-of-definition "(define (build)" 2)
                             #:got (task-description-line (named "build"))))
         (test "a lambda's docstring is read"
               (assert-equal #:expect "Print the report for DAY."
                             #:got (task-description-doc (named "report"))))
         (test "a file that does not read to the end is refused"
               (assert-error (lambda () (static-tasks (broken-fixture)))))))

(define argument-suite
  (suite "The arguments of a task"
         #:concurrent? #f
         #:shuffle? #f
         (test "every kind of argument is read, in order"
               (assert-equal #:expect '(required optional key rest)
                             #:got (map argument-kind
                                        (task-description-arguments
                                         (named "deploy")))))
         (test "an optional argument keeps its default"
               (assert-equal #:expect "\"dev\""
                             #:got (argument-default
                                    (find (lambda (argument)
                                            (string=? "env" (argument-name argument)))
                                          (task-description-arguments
                                           (named "deploy"))))))
         (test "a keyword argument keeps its keyword"
               (assert-equal #:expect "#:jobs"
                             #:got (argument-keyword
                                    (third (task-description-arguments
                                            (named "deploy"))))))
         (test "the prose for an argument comes from the docstring"
               (assert-equal
                #:expect "the host to ship to, as HOST[:PORT]."
                #:got (argument-doc
                       (find (lambda (argument)
                               (string=? "target" (argument-name argument)))
                             (task-description-arguments (named "deploy"))))))
         (test "an argument with no prose has none"
               (assert-equal #:expect ""
                             #:got (argument-doc (car (task-description-arguments
                                                       (named "echo"))))))
         (test "the section ends with the docstring"
               (assert-equal #:expect '()
                             #:got (argument-docs "Compile everything.")))
         (test "prose may wrap over several lines"
               (assert-equal #:expect '(("target" . "one line and another"))
                             #:got (argument-docs
                                    "Ship it.\n\nArgs:\n  target: one line\n    and another\n")))))

(define metadata-suite
  (suite "The metadata a task declares"
         #:concurrent? #f
         #:shuffle? #f
         (test "a task declares what it needs first"
               (assert-equal #:expect '("build")
                             #:got (task-description-depends (named "deploy"))))
         (test "a task declares an example invocation"
               (assert-equal #:expect "maak deploy prod.example.com"
                             #:got (task-description-example (named "deploy"))))
         (test "a task without metadata declares none"
               (assert-equal #:expect #f
                             #:got (task-description-hidden? (named "build"))))
         (test "a task can keep itself out of the listings"
               (assert-true (task-description-hidden? (named "tidy"))))))

(define printing-suite
  (suite "The description, printed"
         #:concurrent? #f
         #:shuffle? #f
         (test "the JSON is one object naming the file"
               (assert-true
                (string-prefix? "{\"version\":1,\"file\":"
                                (tasks-json listed fixture))))
         (test "the JSON carries the root the file sits in"
               (assert-true
                (and (string-contains (tasks-json listed fixture)
                                      "\"root\":\"contrib/test/fixture\"")
                     #t)))
         (test "the JSON leaves a hidden task out"
               (assert-false (string-contains (tasks-json listed fixture)
                                              "\"name\":\"tidy\"")))
         (test "the JSON carries an argument's prose"
               (assert-true
                (and (string-contains
                      (tasks-json listed fixture)
                      "\"doc\":\"the host to ship to, as HOST[:PORT].\"")
                     #t)))
         (test "the JSON carries what a task needs first"
               (assert-true (and (string-contains (tasks-json listed fixture)
                                                  "\"depends\":[\"build\"]")
                                 #t)))
         (test "the names leave a hidden task out"
               (assert-equal #:expect "deploy\nreport\necho\nbuild\n"
                             #:got (task-names-text listed)))
         (test "the preview says what to type for an argument"
               (assert-true
                (and (string-contains
                      (task-preview-text (named "deploy"))
                      "the host to ship to, as HOST[:PORT].")
                     #t)))
         (test "the preview keeps what a task needs first"
               (assert-true
                (and (string-contains
                      (task-preview-text (named "deploy"))
                      "needs: build")
                     #t)))
         (test "the words are one tab separated line per argument"
               (assert-equal
                #:expect
                (string-append "required\ttarget\t\t0\n"
                               "optional\tenv\t\"dev\"\t1\n"
                               "key\tjobs\t1\t1\n"
                               "rest\textra\t\t0\n")
                #:got (task-words-text (named "deploy"))))
         (test "a Zed task list leaves a hidden task out"
               (assert-false (string-contains (zed-tasks-json listed)
                                              "maak: tidy")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               reading-suite
               argument-suite
               metadata-suite
               printing-suite))
