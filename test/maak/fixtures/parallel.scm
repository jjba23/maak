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
;; A fixture Maak file for the tests of `-j/--jobs'.  Its tasks exist to make
;; the two properties of a parallel run observable:
;;
;; - `slow' sleeps before it prints and `fast' prints at once, so a run that
;;   keeps its output in the order the tasks were named prints `slow-ran'
;;   before `fast-ran' even though `fast' finished first;
;; - `boom' prints and then fails, so a failing worker can be told apart from a
;;   successful one in the block it wrote and in the run's exit status.
;;
;; Like the other fixtures it is named `(maak)' on purpose, and lives outside
;; the directory the test runner scans.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (slow fast boom))

(define (slow)
  "Sleep briefly, then print a marker."
  (run (list "sleep" "0.2"))
  (display "slow-ran\n"))

(define (fast)
  "Print a marker at once."
  (display "fast-ran\n"))

(define (boom)
  "Print a marker, then fail."
  (display "about-to-fail\n")
  (task-error "parallel boom"))
