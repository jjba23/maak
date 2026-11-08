;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture Maak file for the tests of the command line's task and argument
;; grammar, which is what decides whether a word names a task or is passed to
;; one.  Its tasks say which of the two happened:
;;
;; - `alpha' and `beta' take no arguments, so a word that reached them as an
;;   argument would fail the run rather than pass quietly;
;; - `echo-args' takes any number of them and prints them, which is how a test
;;   reads back what a run passed on.
;;
;; Like the other fixtures it is named `(maak)' on purpose, and lives outside
;; the directory the test runner scans.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-13)
  #:use-module (maak maak)
  #:export (alpha beta echo-args))

(define (alpha)
  "Print a marker and succeed."
  (display "alpha-ran\n"))

(define (beta)
  "Print a marker and succeed."
  (display "beta-ran\n"))

(define (echo-args . args)
  "Print the arguments the run passed, comma separated."
  (display (format #f "args=~a\n" (string-join args ","))))
