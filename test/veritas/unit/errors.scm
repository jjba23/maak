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
;; Unit tests for the error reporting in `(maak main)'.
;;
;; The messages are the part of the program a user meets when something goes
;; wrong, so they are pinned here: the real errors below are raised and caught,
;; rather than hand-fed to the formatter, because Guile's argument shapes are
;; exactly what used to be misread.  Reporting goes to the error port, which is
;; captured instead of letting it reach the terminal.

;;; Code:

(define-module (veritas unit errors)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak main)
  #:export (spec))

(define (caught-message thunk)
  "Return the message `(maak main)' would print for a failing THUNK."
  (catch #t thunk
         (lambda (key . vals)
           (exception->message key vals))))

(define (reported thunk)
  "Return everything THUNK writes to the error port."
  (let ((port (open-output-string)))
    (parameterize ((current-error-port port))
      (thunk))
    (get-output-string port)))

;; A case is read for its last form only, so a case holds one assertion and
;; calls for the report it checks.  These build the reports the cases below
;; read: the same one twice, rather than two assertions in one case.

(define (primitive-message)
  "Return the message of a failure inside a primitive."
  (caught-message (lambda () (car 1))))

(define (reported-maak-error message hint)
  "Return what reporting the maak error MESSAGE with HINT writes."
  (reported (lambda ()
              (report-maak-error (make-maak-error message hint)))))

(define (reported-generic-error key arguments)
  "Return what reporting an error of KEY with ARGUMENTS writes."
  (reported (lambda ()
              (report-generic-error key arguments))))

(define (debug-switch-report)
  "Return what reporting an error that is not a maak error writes."
  (reported-generic-error #f (list #f "boom" '() #f)))

(define (missing-module-report)
  "Return what reporting a module Guile could not find writes."
  (reported-generic-error 'misc-error
                          (list #f "no code for module ~S"
                                '((no such module)) #f)))

(define (unbound-variable-report)
  "Return what reporting a variable that is not bound writes."
  (reported-generic-error #f
                          (list #f "Unbound variable: ~S"
                                '(no-such-binding) #f)))

(define message-suite
  (suite "Messages of caught errors"
         #:concurrent? #f
         #:shuffle? #f
         (test "an error message is reported as it stands"
               (assert-equal #:expect "boom"
                             #:got (caught-message (lambda () (error "boom")))))
         (test "an unbound variable is named"
               (assert-equal #:expect "Unbound variable: no-such-binding"
                             #:got (caught-message
                                    (lambda ()
                                      (eval 'no-such-binding
                                            (current-module))))))
         (test "a primitive failure explains itself"
               (assert-contains #:expect (primitive-message)
                                #:got "expecting pair"))
         (test "a primitive failure is not named by the procedure"
               ;; The name of the procedure is the first string Guile
               ;; attaches, and it is not the message.
               (assert-false (string=? (primitive-message) "car")))
         (test "a missing module is named the way Guile names it"
               ;; `resolve-interface' rather than `use-modules': the latter is
               ;; expanded where it stands, so it would fail while compiling
               ;; this file instead of when the test runs.
               (assert-equal #:expect "no code for module (no such module)"
                             #:got (caught-message
                                    (lambda ()
                                      (resolve-interface
                                       '(no such module))))))
         (test "the irritants of a user error are shown"
               (assert-equal #:expect "custom a b"
                             #:got (caught-message
                                    (lambda () (error "custom" 'a 'b)))))
         (test "a template that cannot be filled is shown as it stands"
               (assert-equal #:expect "thrown ~S here"
                             #:got (caught-message
                                    (lambda ()
                                      (throw 'misc-error
                                             "thrown ~S here"
                                             '())))))))

(define reporting-suite
  (suite "Reporting to the user"
         #:concurrent? #f
         #:shuffle? #f
         (test "a maak error is labelled"
               (assert-contains #:expect (reported-maak-error
                                          "Unknown task: nope"
                                          "Available tasks: build, test")
                                #:got "maak: error:"))
         (test "a maak error prints its message"
               (assert-contains #:expect (reported-maak-error
                                          "Unknown task: nope"
                                          "Available tasks: build, test")
                                #:got "Unknown task: nope"))
         (test "a maak error prints its hint"
               (assert-contains #:expect (reported-maak-error
                                          "Unknown task: nope"
                                          "Available tasks: build, test")
                                #:got "hint: Available tasks: build, test"))
         (test "a maak error prints no backtrace"
               (assert-false
                (string-contains (reported-maak-error
                                  "Unknown task: nope"
                                  "Available tasks: build, test")
                                 "Backtrace")))
         (test "a maak error without a hint prints its message"
               ;; The label is colour-escaped, so the message is what is looked
               ;; for rather than the label and the message in one string.
               (let ((output (reported-maak-error "nope" #f)))
                 (assert-contains #:expect output #:got "nope")))
         (test "a maak error without a hint prints no hint"
               (let ((output (reported-maak-error "nope" #f)))
                 (assert-false (string-contains output "hint:"))))
         (test "an unexpected error prints what went wrong"
               (assert-contains #:expect (debug-switch-report) #:got "boom"))
         (test "an unexpected error points at the debug switch"
               (assert-contains #:expect (debug-switch-report)
                                #:got "MAAK_DEBUG=1"))
         (test "an unexpected error prints no backtrace"
               (assert-false (string-contains (debug-switch-report)
                                              "Backtrace")))
         (test "a missing module names the module"
               (assert-contains #:expect (missing-module-report)
                                #:got "no code for module (no such module)"))
         (test "a missing module points at the load path switch"
               (assert-contains #:expect (missing-module-report)
                                #:got "-L/--load-path"))
         (test "a missing module names the load path variable"
               (assert-contains #:expect (missing-module-report)
                                #:got "MAAK_LOAD_PATH"))
         (test "an unbound variable is named"
               (assert-contains #:expect (unbound-variable-report)
                                #:got "no-such-binding"))
         (test "an unbound variable says where it is not bound"
               (assert-contains #:expect (unbound-variable-report)
                                #:got "is not bound here"))
         (test "an unbound variable explains the import rule"
               (assert-contains #:expect (unbound-variable-report)
                                #:got "resolve-module"))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               message-suite
               reporting-suite))
