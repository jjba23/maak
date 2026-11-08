;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture for the tests of `-C/--directory': the Maak file a run finds in
;; the directory it changes to.  It is the default name, so a run needs no `-f'
;; at all, and its one task says which file it came from.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (sub-marker))

(define (sub-marker)
  "Print the line that says this task came from the subdirectory."
  (display "subdir-marker-ran\n"))
