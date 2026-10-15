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
;; A fixture for the tests of `-C/--directory': the Maak file a run finds in
;; the directory it changes to.  It is the default name, so a run needs no `-f'
;; at all, and its one task says which file it came from.

;;; Code:

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  #:export (sub-marker))

(define (sub-marker)
  "Print the line that says this task came from the subdirectory."
  (display "subdir-marker-ran\n"))
