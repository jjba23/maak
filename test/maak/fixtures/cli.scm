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
;; A fixture Maak file for the command line tests.  It is named `(maak)' on
;; purpose: that is the module name a real Maak file uses, so loading it
;; exercises the same path the CLI takes.  It lives outside the directory the
;; test runner scans, and is loaded explicitly with `--file'.
;;
;; The three tasks cover the three things the CLI has to get right: a task that
;; succeeds, a task that fails the way a task should (a message and a hint, no
;; backtrace), and a task that reports what the environment holds.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (maak maak)
  #:export (ok boom report-env report-default))

(define (ok)
  "Print a line and succeed."
  (display "ok-task-ran\n"))

(define (boom)
  "Fail the way a task is supposed to fail."
  (task-error "no artefact for ~a" #:args (list "cli-test")
              #:hint "Run `maak ok' first."))

(define (report-env)
  "Print the value of the variable the environment file sets."
  (display (format #f "TAG=~a\n" (env-get "MAAK_CLI_TAG" "unset"))))

(define (report-default)
  "Print the value of the variable the default .env sets."
  (display (format #f "DEFAULT=~a\n" (env-get "MAAK_CLI_DEFAULT" "unset"))))
