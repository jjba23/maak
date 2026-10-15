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
;; Unit tests for `(maak shell)': quoting, program lookup, a handful of file
;; helpers that touch a temporary directory, and the environment helpers.
;;
;; The suites run sequentially because they change process-wide state (the
;; working directory and environment variables) as they go.

;;; Code:

(define-module (veritas unit shell)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (spec))

(define missing-command
  "definitely-not-a-real-command-maak-test")

(define (scratch-directory label)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (format #f "~a/maak-test-~a-~a"
                     (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define quoting-suite
  (suite "Shell quoting and lookup"
         #:concurrent? #f
         #:shuffle? #f
         (test "shell-quote wraps in single quotes"
               (assert-equal #:expect "'a b'"
                             #:got (shell-quote "a b"))
               (assert-equal #:expect "''"
                             #:got (shell-quote "")))
         (test "shell-quote escapes embedded single quotes"
               (assert-equal #:expect "'it'\\''s'"
                             #:got (shell-quote "it's")))
         (test "which finds a program that exists"
               (assert-true (string? (which "guile"))))
         (test "which returns #f for a missing program"
               (assert-false (which missing-command)))
         (test "command-exists? mirrors which"
               (assert-true (command-exists? "guile"))
               (assert-false (command-exists? missing-command)))
         (test "require-command returns the path it found"
               (assert-true (string? (require-command "guile"))))
         (test "require-command raises for a missing program"
               (assert-error (lambda ()
                               (require-command missing-command))))))

(define file-suite
  (suite "File helpers"
         #:concurrent? #f
         #:shuffle? #f
         (test "write-file then read-file round-trips"
               (let* ((dir (scratch-directory "files"))
                      (file (string-append dir "/a.txt")))
                 (write-file file "hello")
                 (assert-equal #:expect "hello"
                               #:got (read-file file))
                 (delete-file-recursively dir)))
         (test "write-file creates parent directories"
               (let* ((dir (scratch-directory "parents"))
                      (file (string-append dir "/deep/nested/a.txt")))
                 (write-file file "deep")
                 (assert-equal #:expect "deep"
                               #:got (read-file file))
                 (delete-file-recursively dir)))
         (test "write-file appends when asked"
               (let* ((dir (scratch-directory "append"))
                      (file (string-append dir "/a.txt")))
                 (write-file file "one")
                 (write-file file "two" #:append? #t)
                 (assert-equal #:expect "onetwo"
                               #:got (read-file file))
                 (delete-file-recursively dir)))
         (test "read-file returns its default for a missing file"
               (let ((missing (string-append (scratch-directory "missing")
                                             "/nope.txt")))
                 (assert-equal #:expect "fallback"
                               #:got (read-file missing
                                                #:default "fallback"))
                 (assert-equal #:expect #f
                               #:got (read-file missing))))
         (test "touch creates a file"
               (let* ((dir (scratch-directory "touch"))
                      (file (string-append dir "/touched")))
                 (parameterize ((quiet? #t))
                   (touch file))
                 (assert-true (file-exists? file))
                 (delete-file-recursively dir)))
         (test "with-directory changes and restores the directory"
               (let* ((start (getcwd))
                      (inside (with-directory (tmp-dir)
                                              (lambda ()
                                                (getcwd)))))
                 (assert-true (string? inside))
                 (assert-equal #:expect start
                               #:got (getcwd))))))

(define environment-suite
  (suite "Environment helpers"
         #:concurrent? #f
         #:shuffle? #f
         (test "home-dir and tmp-dir return strings"
               (assert-true (string? (home-dir)))
               (assert-true (string? (tmp-dir))))
         (test "env-set! then env-get round-trips"
               (env-set! "MAAK_TEST_VARIABLE" "value")
               (assert-equal #:expect "value"
                             #:got (env-get "MAAK_TEST_VARIABLE"))
               (env-unset! "MAAK_TEST_VARIABLE"))
         (test "env-unset! removes the variable"
               (env-set! "MAAK_TEST_VARIABLE" "value")
               (env-unset! "MAAK_TEST_VARIABLE")
               (assert-equal #:expect #f
                             #:got (env-get "MAAK_TEST_VARIABLE")))
         (test "env-get falls back to its default"
               (assert-equal #:expect "default"
                             #:got (env-get missing-command "default")))
         (test "with-env sets and restores the variable"
               (assert-equal #:expect "inner"
                             #:got (with-env "MAAK_TEST_WITH"
                                             "inner"
                                             (lambda ()
                                               (env-get "MAAK_TEST_WITH"))))
               (assert-equal #:expect #f
                             #:got (env-get "MAAK_TEST_WITH")))
         (test "with-env restores an existing value"
               (env-set! "MAAK_TEST_WITH" "outer")
               (with-env "MAAK_TEST_WITH" "inner"
                         (lambda ()
                           'ignored))
               (assert-equal #:expect "outer"
                             #:got (env-get "MAAK_TEST_WITH"))
               (env-unset! "MAAK_TEST_WITH"))
         (test "with-env-vars sets every pair"
               (assert-equal #:expect '("1" "2")
                             #:got (with-env-vars
                                    '(("MAAK_TEST_A" . "1")
                                      ("MAAK_TEST_B" . "2"))
                                    (lambda ()
                                      (list (env-get "MAAK_TEST_A")
                                            (env-get "MAAK_TEST_B")))))
               (env-unset! "MAAK_TEST_A")
               (env-unset! "MAAK_TEST_B"))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               quoting-suite
               file-suite
               environment-suite))
