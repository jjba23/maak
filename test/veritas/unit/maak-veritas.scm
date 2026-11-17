;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak veritas)': the argv the runner takes, the four variables it
;; reads, and the difference between the two shapes the module offers -- the
;; argv `run' starts directly and the one a manifest shell re-reads, where the
;; expression has to survive as a single word.
;;
;; No suite is run here: the invocation is asserted as words, which is exactly
;; what makes a broken one visible before it is a red build that says nothing.

;;; Code:

(define-module (veritas unit maak-veritas)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak veritas)
  #:export (spec))

(define argv-suite
  (suite "The Veritas invocation"
         #:concurrent? #f
         #:shuffle? #f
         (test "the plain invocation puts the checkout first on the load path"
               (assert-equal #:expect '("guile" "-L" "./src" "-L" "./test"
                                        "-c" "(exit ((@ (veritas runner) run-tests)))")
                             #:got (veritas-test-argv)))
         (test "the load paths and the interpreter can be named"
               (assert-equal #:expect (list "guile3" "-L" "src" "-L" "specs"
                                            "-c" veritas-expression)
                             #:got (veritas-test-argv #:guile "guile3"
                                                      #:load-paths '("src" "specs"))))
         (test "one load path is accepted without a list"
               (assert-equal #:expect (list "guile" "-L" "src"
                                            "-c" veritas-expression)
                             #:got (veritas-test-argv #:load-paths "src")))
         (test "the expression can be replaced"
               (assert-equal #:expect '("guile" "-L" "./src" "-L" "./test"
                                        "-c" "(display 1)")
                             #:got (veritas-test-argv #:expression "(display 1)")))
         (test "the shell shape quotes the expression as one word"
               (assert-equal #:expect (shell-quote veritas-expression)
                             #:got (list-ref (veritas-shell-command) 6)))
         (test "an empty module name is refused"
               (assert-error (lambda () (veritas-env #:module ""))))
         (test "load paths that are not strings are refused"
               (assert-error (lambda () (veritas-test-argv #:load-paths '(1)))))))

(define environment-suite
  (suite "The variables the runner reads"
         #:concurrent? #f
         #:shuffle? #f
         (test "the four variables name the module, the tree and the type"
               (assert-equal #:expect '("VERITAS_TEST_MODULE=veritas"
                                        "VERITAS_TEST_TOP_DIR=test"
                                        "VERITAS_TEST_TYPE=unit"
                                        "VERITAS_RUN_BENCHMARKS=false")
                             #:got (veritas-env)))
         (test "a project with integration specs names its own"
               (assert-equal #:expect '("VERITAS_TEST_MODULE=guile-sift"
                                        "VERITAS_TEST_TOP_DIR=specs"
                                        "VERITAS_TEST_TYPE=integration"
                                        "VERITAS_RUN_BENCHMARKS=true")
                             #:got (veritas-env #:module "guile-sift"
                                                #:top-dir "specs"
                                                #:type "integration"
                                                #:benchmarks? #t)))
         (test "the suite is not run in a dry run"
               (parameterize ((dry-run? #t))
                 (assert-equal #:expect #t
                               #:got (veritas-test))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               environment-suite))
