;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A Maak file for the sift cases in `test/veritas/unit/integration.scm': each
;; task asks `sift-status' for one of sift's three answers and prints the number,
;; so the case can see what the library returned in a process where forking is
;; safe.  `(maak sift)' is asserted in process in `sift.scm'; what only a
;; subprocess can show is that `sift-status' really runs the program, reads its
;; status and passes its output on -- and that is a fork the test runner's own
;; fibers cannot survive.
;;
;; The stand-in is named by a path from the project root, which is where the
;; launcher is started, so no environment variable has to reach the child.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (status-clean
            status-findings
            status-error
            meaning
            gate))

(sift-command "test/maak/fixtures/fake-sift")

(define (status-clean)
  "Print the status sift answered with when the tree is clean."
  (format #t "status=~a~%" (sift-status (sift-argv "clean" '()))))

(define (status-findings)
  "Print the status sift answered with when the tree has findings."
  (format #t "status=~a~%" (sift-status (sift-argv "findings" '()))))

(define (status-error)
  "Print the status sift answered with when it could not read the tree."
  (format #t "status=~a~%" (sift-status (sift-argv "error" '()))))

(define (meaning)
  "Print what sift's status means, for a tree that has findings."
  (format #t "meaning=~a~%"
          (sift-exit-meaning (sift-status (sift-argv "findings" '())))))

(define (gate)
  "Run the formatting gate, which fails this task on a tree with findings."
  (sift-check '()))
