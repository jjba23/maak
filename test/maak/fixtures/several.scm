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
;; A fixture Maak file for the tests of the command line's task and argument
;; grammar, which is what decides whether a word names a task or is passed to
;; one.  Its tasks say which of the two happened:
;;
;; - `alpha' and `beta' take no arguments, so a word that reached them as an
;;   argument would fail the run rather than pass quietly;
;; - `echo-args' takes any number of them and prints them, which is how a test
;;   reads back what a run passed on.
;;
;; Like the other fixtures it is named `(maak)' on purpose, and lives outside
;; the directory the test runner scans.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-13)
  #:use-module (maak maak)
  #:export (alpha beta echo-args))

(define (alpha)
  "Print a marker and succeed."
  (display "alpha-ran\n"))

(define (beta)
  "Print a marker and succeed."
  (display "beta-ran\n"))

(define (echo-args . args)
  "Print the arguments the run passed, comma separated."
  (display (format #f "args=~a\n" (string-join args ","))))
