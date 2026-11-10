;; The fixture `contrib/test/maak-tasks.sh' reads.  It defines one of every
;; shape a task can have, and two things that are not tasks.
;;
;; SPDX-License-Identifier: MIT

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (build deploy report echo))

(define build-directory "build")

(define (build)
  "Compile the project."
  (run (list "make" "build")))

(define* (deploy target #:optional (env "dev") #:key (jobs 1) #:rest extra)
  "Ship the build to the server.

Two lines, so the preview indents both."
  (log-info "~a ~a" target env))

(define report
  (lambda (day)
    "Print the report for DAY."
    (log-info "~a" day)))

(define (echo . words)
  "Echo every word."
  (log-info "~a" words))

;; Not exported, so not a task.
(define (private-helper)
  "Visible only inside this file."
  #t)

;; The last definition is the one maak binds.
(define (build)
  "Compile everything."
  (run (list "make" "all")))
