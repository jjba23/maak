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
               (let ((message (caught-message (lambda () (car 1)))))
                 (assert-contains #:expect message
                                  #:got "expecting pair")
                 ;; The name of the procedure is the first string Guile
                 ;; attaches, and it is not the message.
                 (assert-false (string=? message "car"))))
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
         (test "a maak error prints one line and its hint"
               (let ((output (reported (lambda ()
                                         (report-maak-error
                                          (make-maak-error
                                           "Unknown task: nope"
                                           "Available tasks: build, test"))))))
                 (assert-contains #:expect output #:got "maak: error:")
                 (assert-contains #:expect output
                                  #:got "Unknown task: nope")
                 (assert-contains #:expect output
                                  #:got "hint: Available tasks: build, test")
                 (assert-false (string-contains output "Backtrace"))))
         (test "a maak error without a hint prints only the message"
               (let ((output (reported (lambda ()
                                         (report-maak-error
                                          (make-maak-error "nope" #f))))))
                 (assert-contains #:expect output #:got "maak: error: nope")
                 (assert-false (string-contains output "hint:"))))
         (test "an unexpected error points at the debug switch"
               (let ((output (reported (lambda ()
                                         (report-generic-error
                                          #f (list #f "boom" '() #f))))))
                 (assert-contains #:expect output #:got "boom")
                 (assert-contains #:expect output #:got "MAAK_DEBUG=1")
                 (assert-false (string-contains output "Backtrace"))))
         (test "a missing module points at the load path"
               (let ((output (reported (lambda ()
                                         (report-generic-error
                                          'misc-error
                                          (list #f
                                                "no code for module ~S"
                                                '((no such module))
                                                #f))))))
                 (assert-contains #:expect output
                                  #:got "no code for module (no such module)")
                 (assert-contains #:expect output
                                  #:got "-L/--load-path")
                 (assert-contains #:expect output
                                  #:got "MAAK_LOAD_PATH")))
         (test "an unbound variable explains the import rule"
               (let ((output (reported (lambda ()
                                         (report-generic-error
                                          #f
                                          (list #f
                                                "Unbound variable: ~S"
                                                '(no-such-binding)
                                                #f))))))
                 (assert-contains #:expect output #:got "no-such-binding")
                 (assert-contains #:expect output
                                  #:got "is not bound here")
                 (assert-contains #:expect output
                                  #:got "resolve-module")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               message-suite
               reporting-suite))
