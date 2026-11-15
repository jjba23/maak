;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture for `maak explain': four tasks whose composition is a small graph,
;; one quoted name that is not a call, and one call with an argument.  It is
;; named `(maak)' like a real Maak file and loaded explicitly with `--file'.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (alpha beta gamma delta))

(define (alpha)
  "Run the two halves."
  (beta)
  '(delta)
  (gamma 1))

(define (beta)
  "The first half."
  #t)

(define (gamma argument)
  "The second half.

Args:
  argument: a number."
  argument)

(define (delta)
  "Call the whole thing again."
  (alpha))
