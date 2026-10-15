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
;; Unit tests for `(maak dsl)': the pretty multiline string helpers, the load
;; path facilities and the maak error type.
;;
;; The suites run sequentially (`#:concurrent? #f') because the load path is
;; process-wide state: two tests adding directories at the same time would
;; observe each other's changes.

;;; Code:

(define-module (veritas unit dsl)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (spec))

(define strings-suite
  (suite "Pretty multiline strings"
         #:concurrent? #f
         #:shuffle? #f
         (test "dedent removes the common indentation"
               (assert-equal #:expect "hello"
                             #:got (dedent-string "   hello"))
               (assert-equal #:expect "a\n  b"
                             #:got (dedent-string "  a\n    b")))
         (test "dedent drops surrounding blank lines"
               (assert-equal #:expect "a\nb"
                             #:got (dedent-string "\n  a\n  b\n")))
         (test "dedent keeps interior blank lines"
               (assert-equal #:expect "a\n\nb"
                             #:got (dedent-string "  a\n\n  b")))
         (test "dedent expands tabs"
               (assert-equal #:expect "a\nb"
                             #:got (dedent-string "\ta\n\tb")))
         (test "dedent of a single word is the word"
               (assert-equal #:expect "word"
                             #:got (dedent-string "word")))
         (test "indent adds spaces to every line"
               (assert-equal #:expect "  a\n  b"
                             #:got (indent-string "a\nb" #:by 2)))
         (test "indent can leave the first line flush"
               (assert-equal #:expect "a\n  b"
                             #:got (indent-string "a\nb" #:by 2
                                                  #:first? #f)))
         (test "the text macro joins and dedents its lines"
               (assert-equal #:expect "a\nb"
                             #:got (text "a" "b"))
               (assert-equal #:expect "x\ny"
                             #:got (text "  x" "  y")))
         (test "string-lines splits on newlines"
               (assert-equal #:expect '("a" "b")
                             #:got (string-lines "a\nb")))
         (test "unlines joins with newlines"
               (assert-equal #:expect "a\nb"
                             #:got (unlines '("a" "b"))))
         (test "unwords joins with spaces"
               (assert-equal #:expect "a b"
                             #:got (unwords '("a" "b"))))
         (test "blank-line? recognises whitespace"
               (assert-true (blank-line? ""))
               (assert-true (blank-line? "  \t "))
               (assert-false (blank-line? "x")))
         (test "leading-whitespace-count counts spaces and tabs"
               (assert-equal #:expect 3
                             #:got (leading-whitespace-count "   x"))
               (assert-equal #:expect 2
                             #:got (leading-whitespace-count "\t\tx"))
               (assert-equal #:expect 0
                             #:got (leading-whitespace-count "x")))))

(define load-path-suite
  (suite "Load paths"
         #:concurrent? #f
         #:shuffle? #f
         (test "split-load-path splits on colons"
               (assert-equal #:expect '("a" "b")
                             #:got (split-load-path "a:b")))
         (test "split-load-path drops empty entries"
               (assert-equal #:expect '("a" "b")
                             #:got (split-load-path "a::b:")))
         (test "split-load-path handles the empty cases"
               (assert-equal #:expect '()
                             #:got (split-load-path ""))
               (assert-equal #:expect '()
                             #:got (split-load-path #f)))
         (test "with-load-path adds the directories"
               (assert-true
                (with-load-path '("/tmp/maak-test-load-path")
                  (lambda ()
                    (and (member "/tmp/maak-test-load-path" %load-path)
                         #t)))))
         (test "with-load-path restores the load path"
               (with-load-path '("/tmp/maak-test-restore")
                              (lambda ()
                                'ignored))
               (assert-false (member "/tmp/maak-test-restore" %load-path)))
         (test "with-load-path restores the load path after an error"
               (assert-error
                (lambda ()
                  (with-load-path '("/tmp/maak-test-error")
                    (lambda ()
                      (error "deliberate failure")))))
               (assert-false (member "/tmp/maak-test-error" %load-path)))
         (test "add-load-path! records the directory"
               (assert-equal #:expect "/tmp/maak-test-add"
                             #:got (add-load-path! "/tmp/maak-test-add"))
               (assert-true (and (member "/tmp/maak-test-add" %load-path)
                                 #t))
               (assert-true (and (member "/tmp/maak-test-add"
                                         (maak-load-path))
                                 #t))
               ;; Leave the process as the test found it.
               (set! %load-path (delete "/tmp/maak-test-add" %load-path))
               (maak-load-path (delete "/tmp/maak-test-add"
                                       (maak-load-path))))
         (test "add-load-paths! adds every directory"
               (add-load-paths! '("/tmp/maak-test-many-a"
                                  "/tmp/maak-test-many-b"))
               (assert-true (and (member "/tmp/maak-test-many-a" %load-path)
                                 #t))
               (assert-true (and (member "/tmp/maak-test-many-b" %load-path)
                                 #t))
               (set! %load-path
                     (delete "/tmp/maak-test-many-a"
                             (delete "/tmp/maak-test-many-b" %load-path)))
               (maak-load-path (delete "/tmp/maak-test-many-a"
                                       (delete "/tmp/maak-test-many-b"
                                               (maak-load-path)))))))

(define error-suite
  (suite "Maak errors"
         #:concurrent? #f
         #:shuffle? #f
         (test "make-maak-error builds a typed error"
               (assert-true (maak-error? (make-maak-error "message"
                                                          "hint")))
               (assert-equal #:expect "message"
                             #:got (maak-error-message
                                    (make-maak-error "message" "hint")))
               (assert-equal #:expect "hint"
                             #:got (maak-error-hint
                                    (make-maak-error "message" "hint")))
               (assert-equal #:expect #f
                             #:got (maak-error-hint
                                    (make-maak-error "message" #f))))
         (test "raise-maak-error raises a catchable maak error"
               (assert-equal #:expect "boom"
                             #:got (catch #t
                                     (lambda ()
                                       (raise-maak-error "boom"))
                                     (lambda (key . vals)
                                       (maak-error-message
                                        (find-maak-error key vals))))))
         (test "raise-maak-error carries its hint"
               (assert-equal #:expect "try this"
                             #:got (catch #t
                                     (lambda ()
                                       (raise-maak-error
                                        "boom"
                                        #:hint "try this"))
                                     (lambda (key . vals)
                                       (maak-error-hint
                                        (find-maak-error key vals))))))
         (test "find-maak-error returns #f for a foreign error"
               (assert-equal #:expect #f
                             #:got (catch #t
                                     (lambda ()
                                       (error "plain guile error"))
                                     (lambda (key . vals)
                                       (find-maak-error key vals)))))
         (test "raise-maak-error is catchable with assert-error"
               (assert-error (lambda ()
                               (raise-maak-error "nope"))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               strings-suite
               load-path-suite
               error-suite))
