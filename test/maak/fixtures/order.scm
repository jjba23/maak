;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture Maak file for the tests of what a task declares: one task writes a
;; marker file, another reports whether it can see it, and the second one needs
;; the first.  A run therefore says more than "both ran": it says the task that
;; needs the other started after the other had finished.
;;
;; `tidy' is hidden, so a listing can be checked against it.  Like the other
;; fixtures it is named `(maak)' on purpose, and lives outside the directory the
;; test runner scans.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (ice-9 textual-ports)
  #:use-module (maak maak)
  #:export (prepare finish tidy))

(define (marker)
  "Return the file PREPARE writes and FINISH looks for."
  (getenv "MAAK_ORDER_MARKER"))

(define (prepare)
  "Record that it ran, in the file the marker variable names."
  (call-with-output-file (marker)
    (lambda (port) (display "prepared" port))))

(define (finish)
  "Report whether the task it needs has already run."
  (display (if (and (marker) (file-exists? (marker)))
               "finish-saw-prepare\n"
               "finish-saw-nothing\n")))

(task-metadata! finish #:depends '(prepare))

(define (tidy)
  "A task a listing keeps to itself."
  (display "tidy-ran\n"))

(task-metadata! tidy #:hidden? #t)
