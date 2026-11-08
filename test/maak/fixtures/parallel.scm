;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture Maak file for the tests of `-j/--jobs'.  Its tasks exist to make
;; the two properties of a parallel run observable:
;;
;; - `slow' sleeps before it prints and `fast' prints at once, so a run that
;;   keeps its output in the order the tasks were named prints `slow-ran'
;;   before `fast-ran' even though `fast' finished first;
;; - `boom' prints and then fails, so a failing worker can be told apart from a
;;   successful one in the block it wrote and in the run's exit status.
;;
;; Like the other fixtures it is named `(maak)' on purpose, and lives outside
;; the directory the test runner scans.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (slow fast boom))

(define (slow)
  "Sleep briefly, then print a marker."
  (run (list "sleep" "0.2"))
  (display "slow-ran\n"))

(define (fast)
  "Print a marker at once."
  (display "fast-ran\n"))

(define (boom)
  "Print a marker, then fail."
  (display "about-to-fail\n")
  (task-error "parallel boom"))
