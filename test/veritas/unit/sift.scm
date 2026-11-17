;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak sift)': the pure argv builder, the meaning of the three exit
;; statuses, and the walk that finds the files.  sift itself is not needed.
;;
;; The statuses are the part worth a test rather than a reading.  A gate that
;; treated 1 as a crash and one that ignored it would both pass a casual look,
;; so the decision they share is asserted here for each of the three codes.  The
;; run itself -- that sift is started, its status read and its output passed on
;; -- is asserted through the launcher in `integration.scm', because a fork made
;; inside this runner's fibers is a way to wait forever.

;;; Code:

(define-module (veritas unit sift)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak sift)
  #:export (spec))

(define sandbox-counter 0)

(define (fresh-dir)
  "Return a directory of this run's own, created, for one case."
  (set! sandbox-counter (+ sandbox-counter 1))
  (let ((dir (format #f "~a/maak-sift-~a-~a"
                     (tmp-dir) (getpid) sandbox-counter)))
    (mkdir dir)
    dir))

(define (with-sandbox thunk)
  "Call THUNK with a fresh directory, removing it afterwards."
  (let ((dir (fresh-dir)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk dir))
      (lambda ()
        (when (file-exists? dir)
          (delete-file-recursively dir))))))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))



(define (names paths)
  "Return PATHS without their directory, as strings."
  (map basename paths))

;; A case is read for its last form only, so each one holds a single assertion
;; and asks for what it needs (see docs/development.org).

(define argv-suite
  (suite "The sift command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "the plain form names the subcommand and the file"
               (assert-equal #:expect '("sift" "check" "src")
                             #:got (sift-argv "check" "src")))
         (test "the dialect and the style reach the command line"
               (assert-equal #:expect '("sift" "check" "--dialect" "scheme"
                                        "--style" "community" "src")
                             #:got (sift-argv "check" "src"
                                              #:dialect "scheme"
                                              #:style "community")))
         (test "a lint run can ask for a severity and a format"
               (assert-equal #:expect '("sift" "lint" "--severity" "warning"
                                        "--format" "github" "src" "tests")
                             #:got (sift-argv "lint" '("src" "tests")
                                              #:severity "warning"
                                              #:output-format "github")))
         (test "format writes when asked"
               (assert-equal #:expect '("sift" "format" "--write" "src")
                             #:got (sift-argv "format" "src" #:write? #t)))
         (test "format can print the patch instead"
               (assert-equal #:expect '("sift" "format" "--diff" "src")
                             #:got (sift-argv "format" "src" #:diff? #t)))
         (test "formatting options that take a number are written as words"
               (assert-equal #:expect '("sift" "format" "--line-length" "96"
                                        "--indent" "2" "src")
                             #:got (sift-argv "format" "src"
                                              #:line-length 96 #:indent 2)))
         (test "several filters are repeated rather than joined"
               (assert-equal #:expect '("sift" "lint" "--exclude-checker" "SIFT001"
                                        "--exclude-checker" "SIFT002" "src")
                             #:got (sift-argv "lint" "src"
                                              #:exclude-checkers '("SIFT001" "SIFT002"))))
         (test "named checkers are joined with commas"
               (assert-equal #:expect '("sift" "lint" "--checker"
                                        "unused-module,SIFT012" "src")
                             #:got (sift-argv "lint" "src"
                                              #:checkers '("unused-module" "SIFT012"))))
         (test "arguments this builder does not name come last"
               (assert-equal #:expect '("sift" "lint" "--" "src")
                             #:got (sift-argv "lint" '() #:arguments '("--" "src"))))
         (test "writing and printing at once is refused"
               (assert-error (lambda () (sift-argv "format" "src"
                                                   #:write? #t #:stdout? #t))))
         (test "sorting without writing is refused"
               (assert-error (lambda () (sift-argv "format" "src" #:sort? #t))))
         (test "ci refuses to fix what it judges"
               (assert-error (lambda () (sift-argv "ci" "src" #:fix? #t))))
         (test "an empty subcommand is refused"
               (assert-error (lambda () (sift-argv "" '("src")))))
         (test "files that are not strings are refused"
               (assert-error (lambda () (sift-argv "check" '(1 2)))))))

;; The statuses are read from a pure decision rather than from a stand-in
;; program: a suite that forks while the runner has fibers of its own is a way
;; to wait forever, which is a fact about the runner and not about sift.
(define status-suite
  (suite "Reading sift's three answers"
         #:concurrent? #f
         #:shuffle? #f
         (test "zero is clean"
               (assert-equal #:expect 'clean
                             #:got (sift-exit-meaning 0)))
         (test "one is a finding"
               (assert-equal #:expect 'findings
                             #:got (sift-exit-meaning 1)))
         (test "two is an error, which is not a finding"
               (assert-equal #:expect 'error
                             #:got (sift-exit-meaning 2)))
         (test "a dry run answers clean without running anything"
               (parameterize ((sift-command "definitely-not-a-program") (dry-run? #t))
                 (assert-equal #:expect 'clean
                               #:got (sift-exit-meaning
                                      (sift-status (sift-argv "check" '()))))))))

(define files-suite
  (suite "Finding the files sift reads"
         #:concurrent? #f
         #:shuffle? #f
         (test "the walk finds Lisp files and leaves the rest alone"
               (with-sandbox
                (lambda (dir)
                  (write-text (in-vicinity dir "one.scm") "")
                  (write-text (in-vicinity dir "notes.txt") "")
                  (assert-equal #:expect '("one.scm")
                                #:got (names (sift-files dir))))))
         (test "the walk descends into a subdirectory"
               (with-sandbox
                (lambda (dir)
                  (mkdir (in-vicinity dir "sub"))
                  (write-text (in-vicinity dir "sub/two.el") "")
                  (assert-equal #:expect '("two.el")
                                #:got (names (sift-files dir))))))
         (test "a pruned directory is skipped"
               (with-sandbox
                (lambda (dir)
                  (mkdir (in-vicinity dir "fixtures"))
                  (write-text (in-vicinity dir "fixtures/wrong.scm") "")
                  (write-text (in-vicinity dir "right.scm") "")
                  (assert-equal #:expect '("right.scm")
                                #:got (names (sift-files dir #:exclude '("fixtures")))))))
         (test "the walk answers in sorted order"
               (with-sandbox
                (lambda (dir)
                  (write-text (in-vicinity dir "b.scm") "")
                  (write-text (in-vicinity dir "a.scm") "")
                  (assert-equal #:expect '("a.scm" "b.scm")
                                #:got (names (sift-files dir))))))
         (test "a directory that is not there is an error"
               (assert-error (lambda () (sift-files "/definitely/not/a/directory"))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               status-suite
               files-suite))
