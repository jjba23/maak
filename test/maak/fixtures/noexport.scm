;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture for the open-fallback rule: a module with no `#:export', where
;; every procedure is a task.  Like its sibling it lives outside
;; `test/maak/unit' so the runner does not treat it as a test file.

;;; Code:

(define-module (maak fixtures noexport)
  #:declarative? #t)

(define (one)
  "The first task of an open module."
  1)

(define (two)
  "The second task of an open module."
  2)
