;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture for the argument resolution tests: a module that declares
;; exports, so only the exported procedures count as tasks.  It deliberately
;; lives outside `test/maak/unit' so the runner does not try to call a `spec'
;; in it.

;;; Code:

(define-module (maak fixtures tasks)
  #:declarative? #t
  #:export (alpha beta))

(define (alpha)
  "The first exported task."
  'alpha)

(define (beta)
  "The second exported task."
  'beta)

(define (hidden)
  "A private helper, not a task."
  'hidden)
