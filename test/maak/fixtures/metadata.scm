;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture for what a task declares about itself: what it needs first,
;; whether a listing should offer it, and how it is invoked.  The tasks return
;; their own name rather than doing any work, so the runner can be called
;; without touching the tree.
;;
;; It deliberately lives outside `test/maak/unit' so the runner does not try to
;; call a `spec' in it.

;;; Code:

(define-module (maak fixtures metadata)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (fmt build test deploy tidy lint broken loop-a loop-b))

(define (fmt)
  "Format the sources."
  'fmt)

(define (build)
  "Compile the project."
  'build)

(define (test)
  "Run the suite."
  'test)

(define (deploy)
  "Ship what was built and tested."
  'deploy)

(define (tidy)
  "Clean up."
  'tidy)

(define (lint)
  "Lint the sources."
  'lint)

(define (broken)
  "Need a task that does not exist."
  'broken)

(define (loop-a)
  "Need the task that needs this one."
  'loop-a)

(define (loop-b)
  "Need the task that needs this one, the other way round."
  'loop-b)

(task-metadata! build #:depends '(fmt) #:example "maak build")
(task-metadata! test #:depends '(build))
(task-metadata! deploy #:depends '(test))
(task-metadata! tidy #:hidden? #t)
(task-metadata! lint #:depends '("fmt"))
(task-metadata! broken #:depends '(no-such-task))
(task-metadata! loop-a #:depends '(loop-b))
(task-metadata! loop-b #:depends '(loop-a))
