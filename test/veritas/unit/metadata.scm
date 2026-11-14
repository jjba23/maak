;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Unit tests for what a task declares about itself -- what it needs first,
;; whether a listing offers it, and how it is invoked -- and for the order a run
;; puts those declarations in.
;;
;; The declarations are read from a fixture module, resolved the way the runner
;; resolves a Maak file's tasks; the run order is asked for with the same
;; function the sequential and the parallel runner call, so a case here is a
;; statement about what both of them do.
;;
;; The last two cases go through `main' instead, because what a listing prints
;; is only visible from there.  They use a fixture Maak file, which defines the
;; module `(maak)' and so must not run concurrently with anything else that
;; loads a Maak file.  What a forked worker does about a dependency is a
;; question about the process rather than about the call, so that case lives
;; with the others of its kind in `integration.scm'.

;;; Code:

(define-module (veritas unit metadata)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak maak)
  #:use-module (maak main)
  #:export (spec))

(define fixture-module '(maak fixtures metadata))
(define order-fixture "test/maak/fixtures/order.scm")

(define known (resolve-maak-tasks #:module-name fixture-module))

(define (task-proc name)
  "Return the procedure the fixture module defines as NAME."
  (variable-ref (assoc-ref known name)))

(define (caught-message thunk)
  "Return the maak error message THUNK raises, or #f."
  (catch #t
    (lambda ()
      (thunk)
      #f)
    (lambda (key . vals)
      (let ((error (find-maak-error key vals)))
        (and error (maak-error-message error))))))

(define (run-cli . args)
  "Run the CLI with ARGS and return (STATUS STDOUT STDERR)."
  (let ((out (open-output-string))
        (err (open-output-string)))
    (let ((status (parameterize ((current-output-port out)
                                 (current-error-port err))
                    (apply main (list #:args (cons "maak" args))))))
      (list status (get-output-string out) (get-output-string err)))))

(define (listing-run)
  "List the tasks of the ordering fixture."
  (run-cli "--file" order-fixture "--no-banner" "--list"))

(define declaration-suite
  (suite "What a task declares about itself"
         #:concurrent? #f
         #:shuffle? #f
         (test "a task that declares nothing needs nothing"
               (assert-equal #:expect '()
                             #:got (task-depends (task-proc 'fmt))))
         (test "a task declares what it needs first"
               (assert-equal #:expect '(fmt)
                             #:got (task-depends (task-proc 'build))))
         (test "a dependency may be named as a string"
               (assert-equal #:expect '(fmt)
                             #:got (task-depends (task-proc 'lint))))
         (test "a task declares an example invocation"
               (assert-equal #:expect "maak build"
                             #:got (task-example (task-proc 'build))))
         (test "a task with no example has an empty one"
               (assert-equal #:expect ""
                             #:got (task-example (task-proc 'fmt))))
         (test "a task is visible unless it says otherwise"
               (assert-false (task-hidden? (task-proc 'fmt))))
         (test "a task can ask a listing to leave it out"
               (assert-true (task-hidden? (task-proc 'tidy))))
         (test "metadata for something that is not a task is refused"
               (assert-error (lambda () (task-metadata! 42))))
         (test "a dependency that is not a name is refused"
               (assert-error (lambda ()
                               (task-metadata! (task-proc 'tidy)
                                               #:depends '(42)))))))

(define ordering-suite
  (suite "The order a run puts the tasks in"
         #:concurrent? #f
         #:shuffle? #f
         (test "dependencies come before the task that needs them"
               (assert-equal #:expect '(fmt build test deploy)
                             #:got (task-order '(deploy) #:known known)))
         (test "a task named twice is run once"
               (assert-equal #:expect '(fmt build)
                             #:got (task-order '(build build) #:known known)))
         (test "the order the tasks were named is kept"
               (assert-equal #:expect '(fmt build test)
                             #:got (task-order '(test build) #:known known)))
         (test "a run of nothing is no order at all"
               (assert-equal #:expect '()
                             #:got (task-order '() #:known known)))
         (test "a dependency that names no task is an error"
               (assert-error (lambda ()
                               (task-order '(broken) #:known known))))
         (test "a cycle is an error"
               (assert-error (lambda ()
                               (task-order '(loop-a) #:known known))))
         (test "a cycle says which tasks it runs through"
               (assert-equal #:expect "dependency cycle: loop-a -> loop-b -> loop-a"
                             #:got (caught-message
                                    (lambda ()
                                      (task-order '(loop-a) #:known known)))))))

(define listing-suite
  (suite "A listing honours the declarations"
         #:concurrent? #f
         #:shuffle? #f
         (test "the listing leaves a hidden task out"
               (assert-false (string-contains (second (listing-run))
                                              "tidy")))
         (test "the listing says what a task needs first"
               (assert-true (and (string-contains (second (listing-run))
                                                  "needs: prepare")
                                 #t)))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               declaration-suite
               ordering-suite
               listing-suite))
