;; maak

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; maak is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; maak is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with maak.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Unit tests for task resolution and the errors the runner raises: which
;; procedures count as tasks under the export rules, and what the user is told
;; when a task is not found.

;;; Code:

(define-module (veritas unit tasks)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak maak)
  #:use-module (maak dsl)
  #:export (spec))

(define (task-names module-name)
  "Return the names of MODULE-NAME's tasks, sorted."
  (sort (map (lambda (task)
               (symbol->string (car task)))
             (resolve-maak-tasks #:module-name module-name))
        string<?))

(define (caught-message thunk)
  "Return the maak error message THUNK raises."
  (catch #t
    (lambda ()
      (thunk)
      #f)
    (lambda (key . vals)
      (let ((error (find-maak-error key vals)))
        (and error (maak-error-message error))))))

(define (caught-hint thunk)
  "Return the maak error hint THUNK raises."
  (catch #t
    (lambda ()
      (thunk)
      #f)
    (lambda (key . vals)
      (let ((error (find-maak-error key vals)))
        (and error (maak-error-hint error))))))

(define resolution-suite
  (suite "Task resolution"
         #:concurrent? #f
         #:shuffle? #f
         (test "exports restrict the visible tasks"
               (assert-equal #:expect '("alpha" "beta")
                             #:got (task-names '(maak fixtures tasks))))
         (test "non-exported procedures are hidden"
               (assert-false (member "hidden"
                                     (task-names '(maak fixtures tasks)))))
         (test "a module without exports is open season"
               (assert-equal #:expect '("one" "two")
                             #:got (task-names '(maak fixtures noexport))))
         (test "available-task-names returns a list of strings"
               (let ((names (available-task-names)))
                 (assert-true (list? names))
                 (assert-true (every string? names))))
         (test "available-task-names is sorted"
               (let ((names (available-task-names)))
                 (assert-equal #:expect names
                               #:got (sort names string<?))))))

(define error-suite
  (suite "Unknown task errors"
         #:concurrent? #f
         #:shuffle? #f
         (test "an unknown task names itself"
               (assert-equal #:expect "Unknown task: nope"
                             #:got (caught-message
                                    (lambda ()
                                      (unknown-task-error 'nope)))))
         (test "a missing default gets its own message"
               (assert-equal
                #:expect "Could not find a `default' task in this Maak file."
                #:got (caught-message
                       (lambda ()
                         (unknown-task-error 'default)))))
         (test "the error carries a hint"
               (let ((hint (caught-hint (lambda ()
                                          (unknown-task-error 'nope)))))
                 (assert-true (string? hint))
                 (assert-true (positive? (string-length hint)))))
         (test "run-task raises for a task that does not exist"
               (assert-error (lambda ()
                               (run-task 'definitely-missing-task))))
         (test "run-tasks raises for a task that does not exist"
               (assert-error (lambda ()
                               (run-tasks '(definitely-missing-task)))))
         (test "run-tasks of nothing is a no-op"
               (assert-equal #:expect #t
                             #:got (begin (run-tasks '())
                                          #t)))))

(define layout-suite
  (suite "Docstring layout is exported"
         #:concurrent? #f
         #:shuffle? #f
         (test "format-task-doc is available from (maak maak)"
               (assert-true (procedure? format-task-doc))
               (assert-equal #:expect '("t: one line")
                             #:got (format-task-doc "t" "one line")))
         (test "available-task-names is available from (maak maak)"
               (assert-true (procedure? available-task-names)))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               resolution-suite
               error-suite
               layout-suite))
