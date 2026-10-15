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
;; A fixture for the argument resolution tests: a module that declares
;; exports, so only the exported procedures count as tasks.  It deliberately
;; lives outside `test/maak/unit' so the runner does not try to call a `spec'
;; in it.

;;; Code:

(define-module (maak fixtures tasks)
  #:declarative? #t
  #:export (alpha beta))

(define (alpha)
  "The first exported task."
  'alpha)

(define (beta)
  "The second exported task."
  'beta)

(define (hidden)
  "A private helper, not a task."
  'hidden)
