;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture Maak file for the tests of `--keep-going' and `--ignore-errors':
;; two tasks that pass and two that fail, interleaved, so a run of all four
;; shows exactly what a run does about the failures in the middle.  Like the
;; other fixtures it is named `(maak)' on purpose, and lives outside the
;; directory the test runner scans.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (first-pass second-fail third-fail last-pass))

(define (first-pass)
  "Print a line and succeed."
  (display "first-pass-ran\n"))

(define (second-fail)
  "Fail the way a task is supposed to fail."
  (task-error "no artefact for ~a" #:args (list "second-fail")
              #:hint "Run `maak first-pass' first."))

(define (third-fail)
  "Fail with a message of its own."
  (task-error "the third task has nothing to show"))

(define (last-pass)
  "Print a line and succeed."
  (display "last-pass-ran\n"))
