;; The fixture `contrib/test/maak-tasks.sh' and `test/veritas/unit/listing.scm'
;; read.  It defines one of every shape a task can have, two things that are
;; not tasks, and every piece of metadata a task may declare about itself.
;;
;; SPDX-License-Identifier: MIT

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (build deploy report echo tidy))

(define build-directory "build")

(define (build)
  "Compile the project."
  (run (list "make" "build")))

(define* (deploy target #:optional (env "dev") #:key (jobs 1) #:rest extra)
  "Ship the build to the server.

Two lines, so the preview indents both.

Args:
  target: the host to ship to, as HOST[:PORT].
  env: which environment's .env file to read, when there is one.
  jobs: how many uploads at once; maak cannot pass a keyword."
  (log-info "~a ~a" target env))

(task-metadata! deploy
                #:depends '(build)
                #:example "maak deploy prod.example.com")

(define report
  (lambda (day)
    "Print the report for DAY."
    (log-info "~a" day)))

(define (echo . words)
  "Echo every word."
  (log-info "~a" words))

;; Exported, and hidden: work a project keeps for the tasks that need it.
(define (tidy)
  "Remove the build directory."
  (delete-file-recursively "build"))

(task-metadata! tidy #:hidden? #t)

;; Not exported, so not a task.
(define (private-helper)
  "Visible only inside this file."
  #t)

;; The last definition is the one maak binds.
(define (build)
  "Compile everything."
  (run (list "make" "all")))
