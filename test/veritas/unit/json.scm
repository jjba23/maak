;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `maak --list --json', the listing a machine reads.  What the static
;; reader answers on its own is tested in `listing.scm'; what is tested here is
;; the answer a run gives, which is the loaded module's task set and docstrings
;; completed by the file's lambda lists.
;;
;; A case therefore goes through `main', the way the launcher calls it, with the
;; output ports captured: the fixture Maak file is loaded, so the module `(maak)'
;; is defined, and these suites must run sequentially rather than as fibers.

;;; Code:

(define-module (veritas unit json)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak main)
  #:export (spec))

;; The fixture `listing.scm' reads too, so a shape is described once.
(define fixture "contrib/test/fixture/maak.scm")
(define order-fixture "test/maak/fixtures/order.scm")

(define (run-cli . args)
  "Run the CLI with ARGS and return (STATUS STDOUT STDERR)."
  (let ((out (open-output-string))
        (err (open-output-string)))
    (let ((status (parameterize ((current-output-port out)
                                 (current-error-port err))
                    (apply main (list #:args (cons "maak" args))))))
      (list status (get-output-string out) (get-output-string err)))))

(define ansi-escapes
  (make-regexp "\x1b\\[[0-9;]*[A-Za-z]"))

(define (plain str)
  "Return STR without the colour escapes a report writes around its words."
  (regexp-substitute/global #f ansi-escapes str 'pre "" 'post))

(define (json-run)
  "The task listing of the fixture, as JSON."
  (second (run-cli "--file" fixture "--list" "--json")))

(define (said? text needle)
  "Return whether TEXT holds NEEDLE, as #t or #f rather than as an offset."
  (and (string-contains text needle) #t))

(define listing-suite
  (suite "The listing as JSON"
         #:concurrent? #f
         #:shuffle? #f
         (test "the run succeeds and prints one object"
               (assert-equal #:expect (list 0 #t)
                             #:got (list (first (run-cli "--file" fixture
                                                         "--list" "--json"))
                                         (string-prefix? "{\"version\":1,"
                                                         (json-run)))))
         (test "the object ends where the JSON ends"
               (assert-true (string-suffix? "}\n" (json-run))))
         (test "no greeting is printed in front of it"
               (assert-false (said? (json-run) "Maak ")))
         (test "the file it was read from is named"
               (assert-true (said? (json-run)
                                   (string-append "\"file\":\""
                                                  (canonicalize-path fixture)
                                                  "\""))))
         (test "a task carries the docstring the loader has"
               (assert-true (said? (json-run) "\"doc\":\"Compile everything.\"")))
         (test "a task carries the lambda list the file writes"
               (assert-true (said? (json-run)
                                   "\"name\":\"target\",\"kind\":\"required\"")))
         (test "a task carries the prose for an argument"
               (assert-true (said? (json-run)
                                   "\"doc\":\"the host to ship to, as HOST[:PORT].\"")))
         (test "a task carries what it needs first"
               (assert-true (said? (json-run) "\"depends\":[\"build\"]")))
         (test "a hidden task is left out"
               (assert-false (said? (json-run) "\"name\":\"tidy\"")))
         (test "the tasks come in the file's order"
               (let ((deploy (string-contains (json-run) "\"name\":\"deploy\""))
                     (report (string-contains (json-run) "\"name\":\"report\""))
                     (build (string-contains (json-run) "\"name\":\"build\"")))
                 (assert-equal #:expect #t
                               #:got (and deploy report build
                                          (< deploy report)
                                          (< report build)
                                          #t))))
         (test "a hidden task of another file is described when it is named"
               (assert-false (said? (second (run-cli "--file" order-fixture
                                                     "--list" "--json"))
                                    "\"name\":\"tidy\"")))
         (test "a task the file does not spell out still appears"
               (assert-true (said? (json-run) "\"name\":\"echo\"")))))

(define refusal-suite
  (suite "A listing without --list"
         #:concurrent? #f
         #:shuffle? #f
         (test "--json on its own is refused"
               (assert-equal #:expect (list 1 #t)
                             #:got (let ((result (run-cli "--file" fixture
                                                          "--json")))
                                     (list (first result)
                                           (said? (plain (third result))
                                                  "--json prints the task listing")))))
         (test "the refusal says how to ask"
               (assert-true (said? (plain (third (run-cli "--file" fixture
                                                           "--json")))
                                   "maak --list --json")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               listing-suite
               refusal-suite))
