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
;; A fixture for the open-fallback rule: a module with no `#:export', where
;; every procedure is a task.  Like its sibling it lives outside
;; `test/maak/unit' so the runner does not treat it as a test file.

;;; Code:

(define-module (maak fixtures noexport)
  #:declarative? #t)

(define (one)
  "The first task of an open module."
  1)

(define (two)
  "The second task of an open module."
  2)
