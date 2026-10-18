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

;; A case is read for its last form only, so a case holds one assertion and
;; asks for what it needs: the same path twice, rather than two assertions in
;; one case.

(define (missing-file-path)
  "Return the path of a file that is not there, in a scratch directory."
  (string-append (scratch-directory "missing") "/nope.txt"))

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
         ;; A test case has to evaluate to an assertion, which is a procedure
         ;; of one validation client.  Cleanup is therefore done before the
         ;; assertion is built, never after it: a trailing `(delete-file ...)'
         ;; would leave the case with no assertion at all, and the test would
         ;; pass without checking anything.
         (test "write-file then read-file round-trips"
               (let* ((dir (scratch-directory "files"))
                      (file (string-append dir "/a.txt"))
                      (content (begin
                                 (write-file file "hello")
                                 (read-file file))))
                 (delete-file-recursively dir)
                 (assert-equal #:expect "hello" #:got content)))
         (test "write-file creates parent directories"
               (let* ((dir (scratch-directory "parents"))
                      (file (string-append dir "/deep/nested/a.txt"))
                      (content (begin
                                 (write-file file "deep")
                                 (read-file file))))
                 (delete-file-recursively dir)
                 (assert-equal #:expect "deep" #:got content)))
         (test "write-file appends when asked"
               (let* ((dir (scratch-directory "append"))
                      (file (string-append dir "/a.txt"))
                      (content (begin
                                 (write-file file "one")
                                 (write-file file "two" #:append? #t)
                                 (read-file file))))
                 (delete-file-recursively dir)
                 (assert-equal #:expect "onetwo" #:got content)))
         (test "read-file returns its default for a missing file"
               (assert-equal #:expect "fallback"
                             #:got (read-file (missing-file-path)
                                              #:default "fallback")))
         (test "read-file of a missing file is #f without a default"
               (assert-equal #:expect #f
                             #:got (read-file (missing-file-path))))
         (test "touch creates a file"
               (let* ((dir (scratch-directory "touch"))
                      (file (string-append dir "/touched"))
                      (created? (begin
                                  (parameterize ((quiet? #t))
                                    (touch file))
                                  (file-exists? file))))
                 (delete-file-recursively dir)
                 (assert-true created?)))
         (test "with-directory changes the directory"
               (assert-true (string? (with-directory (tmp-dir)
                                        (lambda ()
                                          (getcwd))))))
         (test "with-directory restores the directory"
               (let ((start (getcwd)))
                 (with-directory (tmp-dir)
                                 (lambda ()
                                   (getcwd)))
                 (assert-equal #:expect start
                               #:got (getcwd))))))

(define capture-suite
  (suite "Capturing output"
         #:concurrent? #f
         #:shuffle? #f
         (test "capture returns the output of a successful command"
               (assert-equal #:expect "fine\n"
                             #:got (parameterize ((quiet? #t))
                                     (capture "echo fine"))))
         (test "capture raises when the command fails"
               (assert-error
                (lambda ()
                  (parameterize ((quiet? #t))
                    (capture "exit 3")))))
         (test "capture keeps partial output only when allowed to fail"
               (assert-equal #:expect "partial\n"
                             #:got (parameterize ((quiet? #t))
                                     (capture "echo partial; exit 3"
                                              #:allow-failure? #t))))
         (test "capture includes standard error when asked"
               (assert-equal #:expect "oops\n"
                             #:got (parameterize ((quiet? #t))
                                     (capture "sh -c 'echo oops >&2'"
                                              #:error? #t))))))

(define spacing-suite
  (suite "File helpers keep paths with spaces whole"
         #:concurrent? #f
         #:shuffle? #f
         (test "mkdir-p creates one directory, not a directory per word"
               (let* ((root (scratch-directory "spaces"))
                      (dir (string-append root "/a b"))
                      (created? (begin
                                  (parameterize ((quiet? #t)) (mkdir-p dir))
                                  (file-exists? dir))))
                 (delete-file-recursively root)
                 (assert-true created?)))
         (test "touch creates a file whose name has a space"
               (let* ((root (scratch-directory "touch-space"))
                      (file (string-append root "/a b.txt"))
                      (created? (begin
                                  (parameterize ((quiet? #t)) (touch file))
                                  (file-exists? file))))
                 (delete-file-recursively root)
                 (assert-true created?)))
         (test "cp copies paths whose names have spaces"
               (let* ((root (scratch-directory "cp-space"))
                      (from (string-append root "/a b.txt"))
                      (to (string-append root "/c d.txt"))
                      (copied? (begin
                                 (write-file from "spaced")
                                 (parameterize ((quiet? #t))
                                   (cp from to #:verbose-copy? #f))
                                 (file-exists? to))))
                 (delete-file-recursively root)
                 (assert-true copied?)))
         (test "mv moves paths whose names have spaces"
               (let* ((root (scratch-directory "mv-space"))
                      (from (string-append root "/a b.txt"))
                      (to (string-append root "/c d.txt"))
                      (moved? (begin
                                (write-file from "moved")
                                (parameterize ((quiet? #t)) (mv from to))
                                (and (file-exists? to)
                                     (not (file-exists? from))))))
                 (delete-file-recursively root)
                 (assert-true moved?)))
         (test "ln-s links a path whose name has a space"
               (let* ((root (scratch-directory "ln-space"))
                      (target (string-append root "/a b.txt"))
                      (link (string-append root "/c d.txt"))
                      (linked? (begin
                                 (write-file target "linked")
                                 (parameterize ((quiet? #t))
                                   (ln-s target link))
                                 (file-exists? link))))
                 (delete-file-recursively root)
                 (assert-true linked?)))
         (test "chmod-file changes the mode of a path with a space"
               (let* ((root (scratch-directory "chmod-space"))
                      (file (string-append root "/a b.sh"))
                      (perms (begin
                               (write-file file "#!/bin/sh\n")
                               (parameterize ((quiet? #t))
                                 (chmod-file "700" file))
                               (logand (stat:perms (stat file)) #o777))))
                 (delete-file-recursively root)
                 (assert-equal #:expect #o700 #:got perms)))
         (test "delete-file-recursively removes a directory with a space"
               (let* ((root (scratch-directory "rm-space"))
                      (dir (string-append root "/a b"))
                      (gone? (begin
                               (parameterize ((quiet? #t)) (mkdir-p dir))
                               (parameterize ((quiet? #t))
                                 (delete-file-recursively dir))
                               (not (file-exists? dir)))))
                 (delete-file-recursively root)
                 (assert-true gone?)))))

(define expansion-suite
  (suite "Shell expansion when asked for"
         #:concurrent? #f
         #:shuffle? #f
         (test "delete-file-recursively expands a wildcard when asked"
               (let* ((root (scratch-directory "expand-rm"))
                      (a (string-append root "/a b.txt"))
                      (b (string-append root "/c.txt"))
                      (keep (string-append root "/keep.log"))
                      (gone? (begin
                               (write-file a "a")
                               (write-file b "b")
                               (write-file keep "c")
                               (parameterize ((quiet? #t))
                                 (delete-file-recursively
                                  (string-append root "/*.txt")
                                  #:expand? #t))
                               (and (not (file-exists? a))
                                    (not (file-exists? b))
                                    (file-exists? keep)))))
                 (delete-file-recursively root)
                 (assert-true gone?)))
         (test "cp expands a wildcard when asked"
               (let* ((root (scratch-directory "expand-cp"))
                      (dest (string-append root "/dest"))
                      (a (string-append root "/a b.txt"))
                      (b (string-append root "/c.txt"))
                      (copied? (begin
                                 (write-file a "a")
                                 (write-file b "b")
                                 (parameterize ((quiet? #t))
                                   (mkdir-p dest)
                                   (cp (string-append root "/*.txt")
                                       dest
                                       #:verbose-copy? #f
                                       #:expand? #t))
                                 (and (file-exists? (string-append dest
                                                                   "/a b.txt"))
                                      (file-exists? (string-append dest
                                                                   "/c.txt"))))))
                 (delete-file-recursively root)
                 (assert-true copied?)))
         (test "chmod-file expands a wildcard when asked"
               (let* ((root (scratch-directory "expand-chmod"))
                      (a (string-append root "/a.txt"))
                      (b (string-append root "/b.txt"))
                      (perms (begin
                               (write-file a "a")
                               (write-file b "b")
                               (parameterize ((quiet? #t))
                                 (chmod-file "600"
                                             (string-append root "/*.txt")
                                             #:expand? #t))
                               (list (logand (stat:perms (stat a)) #o777)
                                     (logand (stat:perms (stat b)) #o777)))))
                 (delete-file-recursively root)
                 (assert-equal #:expect (list #o600 #o600) #:got perms)))))

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

(define env-file-suite
  (suite "Environment files"
         #:concurrent? #f
         #:shuffle? #f
         (test "the assignments are read in the order they are written"
               (assert-equal
                #:expect '(("CI" . "1")
                          ("MAAK_SMOKE" . "hello")
                          ("MAAK_SMOKE_QUOTED" . "quoted value")
                          ("SINGLE" . "single quoted")
                          ("EMPTY" . "")
                          ("DATABASE_URL"
                           . "postgres://localhost/dev?sslmode=disable")
                          ("INDENTED" . "spaced"))
                #:got (env-file-entries "test/maak/fixtures/demo.env")))
         (test "environment files skip lines without an ="
               (assert-false
                (assoc "NO_EQUALS_LINE"
                       (env-file-entries "test/maak/fixtures/demo.env"))))
         (test "environment files skip comments"
               (assert-false
                (assoc "#" (env-file-entries "test/maak/fixtures/demo.env"))))
         (test "a file that does not exist has no assignments"
               (assert-equal #:expect '()
                             #:got (env-file-entries "no/such/file.env")))
         (test "loading sets the names and reports them"
               (env-unset! "MAAK_SMOKE")
               (assert-equal #:expect '("CI" "MAAK_SMOKE" "MAAK_SMOKE_QUOTED"
                                        "SINGLE" "EMPTY" "DATABASE_URL"
                                        "INDENTED")
                             #:got (parameterize ((quiet? #t))
                                     (load-env-file
                                      "test/maak/fixtures/demo.env")))
               (assert-equal #:expect "hello"
                             #:got (env-get "MAAK_SMOKE"))
               (assert-equal #:expect "quoted value"
                             #:got (env-get "MAAK_SMOKE_QUOTED")))
         (test "the environment keeps a name it already has"
               (env-set! "MAAK_SMOKE" "from-the-environment")
               (parameterize ((quiet? #t))
                 (load-env-file "test/maak/fixtures/demo.env"))
               (assert-equal #:expect "from-the-environment"
                             #:got (env-get "MAAK_SMOKE"))
               (env-unset! "MAAK_SMOKE"))
         (test "overriding is possible when asked for"
               (env-set! "MAAK_SMOKE" "from-the-environment")
               (parameterize ((quiet? #t))
                 (load-env-file "test/maak/fixtures/demo.env"
                                #:override? #t))
               (assert-equal #:expect "hello"
                             #:got (env-get "MAAK_SMOKE"))
               (env-unset! "MAAK_SMOKE"))
         (test "loading a file that is not there changes nothing"
               (assert-equal #:expect '()
                             #:got (load-env-file "no/such/file.env")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               quoting-suite
               file-suite
               capture-suite
               spacing-suite
               expansion-suite
               environment-suite
               env-file-suite))
