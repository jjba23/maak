;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; A fixture for `--plan': one task of every kind of command maak can build,
;; so the plan shows a direct argv, a shell line, a pipeline and a captured
;; command, and the environment and elevation a command was given.  A second
;; task computes without running anything, which is what an empty plan is.
;;
;; It is named `(maak)' like a real Maak file, and lives outside the directory
;; the runner scans, so it is loaded explicitly with `--file'.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (greet only-computes chatty))

(define (greet name)
  "Greet NAME, with one command of every shape.

Args:
  name: who to greet."
  (run (list "echo" name))
  (run (list "ls" "-l")
       #:env '(("CI" . "1"))
       #:sudo? #t)
  ($ '("echo hi > /tmp/maak-plan-fixture"))
  (run-pipeline (list (list "cat" "maak.scm")
                      (list "wc" "-l")))
  (capture "date +%Y"))

(define (only-computes)
  "Do some arithmetic and run nothing."
  (+ 1 2))

(define (chatty)
  "Print first, then run a command."
  (display "noise\n")
  (run (list "true")))
