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
;; Unit tests for `format-task-doc', the layout `maak --list' uses.  The name
;; is printed through `bold-output', which wraps it in ANSI escapes; the tests
;; strip those before comparing, so they assert on what the user sees rather
;; than on the exact escape codes.

;;; Code:

(define-module (veritas unit docstrings)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak maak)
  #:export (spec))

(define escape-character
  (integer->char 27))

(define (strip-ansi str)
  "Return STR without ANSI escape sequences."
  (let loop ((chars (string->list str))
             (out '())
             (escaping? #f))
    (cond
      ((null? chars) (list->string (reverse out)))
      ((and escaping? (char=? (car chars) #\m))
       (loop (cdr chars) out #f))
      (escaping? (loop (cdr chars) out #t))
      ((char=? (car chars) escape-character)
       (loop (cdr chars) out #t))
      (else (loop (cdr chars) (cons (car chars) out) #f)))))

(define (strip-ansi-list lines)
  (map strip-ansi lines))

(define layout-suite
  (suite "Task docstring layout"
         #:concurrent? #f
         #:shuffle? #f
         (test "a single line stays inline"
               (assert-equal #:expect '("build: Compile the project.")
                             #:got (format-task-doc
                                    "build"
                                    "Compile the project.")))
         (test "two lines align the continuation"
               (assert-equal #:expect '("t: first" "   second")
                             #:got (format-task-doc "t"
                                                    "first\nsecond")))
         (test "one paragraph keeps every line aligned under the name"
               (assert-equal
                #:expect '("t: first" "   second" "   third" "   fourth")
                #:got (format-task-doc
                       "t"
                       "first\nsecond\nthird\nfourth")))
         (test "a paragraph break starts a block under the name"
               (assert-equal
                #:expect '("t:" "   first" "   second" "" "   third")
                #:got (strip-ansi-list (format-task-doc
                                        "t"
                                        "first\nsecond\n\nthird"))))
         (test "a wrapped paragraph aligns with the widest name"
               (assert-equal
                #:expect '("collect-artefacts: Gather the build output."
                          "                   The result is a tarball.")
                #:got (format-task-doc
                       "collect-artefacts"
                       "Gather the build output.\nThe result is a tarball.")))
         (test "an indented multi-line docstring keeps its alignment"
               (assert-equal
                #:expect '("t: first" "   second")
                #:got (format-task-doc
                       "t"
                       "\n     first\n     second\n     ")))
         (test "a block keeps its blank lines blank"
               (assert-equal
                #:expect '("t:" "   a" "" "   b" "" "   c")
                #:got (strip-ansi-list (format-task-doc
                                        "t"
                                        "a\n\nb\n\nc"))))
         (test "a docstring with no documentation prints the name only"
               (assert-equal #:expect '("t:")
                             #:got (strip-ansi-list
                                    (format-task-doc "t" #f)))
               (assert-equal #:expect '("t:")
                             #:got (strip-ansi-list
                                    (format-task-doc "t" "")))
               (assert-equal #:expect '("t:")
                             #:got (strip-ansi-list
                                    (format-task-doc "t" "   "))))
         (test "source indentation is removed before laying out"
               (assert-equal #:expect '("t: first" "   second")
                             #:got (format-task-doc "t"
                                                    "   first\n   second")))
         (test "the margin follows the length of the name"
               (assert-equal #:expect '("longname: a" "          b")
                             #:got (format-task-doc "longname" "a\nb")))
         (test "blank lines inside a block are not padded"
               (assert-equal #:expect '("t:" "   a" "" "   b")
                             #:got (strip-ansi-list
                                    (format-task-doc "t" "a\n\nb"))))
         (test "the name is emphasised with ANSI escapes"
               (assert-equal #:expect #t
                             #:got (let ((label (car (format-task-doc
                                                      "t" #f))))
                                     (and (string-contains label
                                                           (string-append
                                                            (string
                                                             escape-character)
                                                            "[1m"))
                                          #t))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               layout-suite))
