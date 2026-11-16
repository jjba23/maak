;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak elisp)': the batch argv builder, the three checks, the file
;; discovery and the practice readers.  No Emacs is needed: the builder is
;; asserted as a list of words, the commands are read from a dry run, and the
;; practice helpers are pure functions over files this spec writes.

;;; Code:

(define-module (veritas unit elisp)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak elisp)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-elisp-~a-~a" (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (with-scratch label thunk)
  (let ((dir (scratch-directory label)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk dir))
      (lambda ()
        (when (file-exists? dir)
          (parameterize ((quiet? #t))
            (delete-file-recursively dir)))))))

(define good-source
  (string-append
   ";;; foo.el --- A test package -*- lexical-binding: t; -*-\n"
   ";; Version: 1.2.3\n"
   ";; Package-Requires: ((emacs \"27.1\"))\n"
   "\n"
   ";;; Commentary:\n"
   ";; A test.\n"
   "\n"
   ";;; Code:\n"
   "(defun foo () nil)\n"
   "(provide 'foo)\n"
   ";;; foo.el ends here\n"))

(define bad-source
  (string-append
   "(defun bar () t)\n"))

(define (fake-emacs directory)
  "Write an Emacs stand-in in DIRECTORY and return its path."
  (let ((path (in-vicinity directory "emacs")))
    (write-text path
                (string-append
                 "#!/bin/sh\n"
                 "printf '%s\\n' \"29.1\"\n"))
    (chmod path #o755)
    path))

(define argv-suite
  (suite "The Emacs batch command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "the quiet batch invocation is the default"
               (parameterize ((emacs-command "emacs"))
                 (assert-equal #:expect '("emacs" "--batch" "-Q")
                               #:got (emacs-batch-argv))))
         (test "load paths, files, evals, functions and arguments keep their order"
               (parameterize ((emacs-command "emacs"))
                 (assert-equal
                  #:expect '("emacs" "--batch" "-Q"
                             "-L" "lisp" "-l" "test/foo-tests.el"
                             "--eval" "(princ 1)"
                             "-f" "batch-byte-compile"
                             "foo.el")
                  #:got (emacs-batch-argv
                         #:load-paths '("lisp")
                         #:load-files '("test/foo-tests.el")
                         #:evals '("(princ 1)")
                         #:functions '("batch-byte-compile")
                         #:arguments '("foo.el")))))
         (test "a run can keep the site file when asked"
               (parameterize ((emacs-command "emacs"))
                 (assert-equal #:expect '("emacs" "--batch")
                               #:got (emacs-batch-argv #:quick? #f))))
         (test "the program value may carry flags"
               (parameterize ((emacs-command "emacs -nw"))
                 (assert-equal #:expect '("emacs" "-nw" "--batch" "-Q")
                               #:got (emacs-batch-argv))))
         (test "a Lisp string literal escapes quotes and backslashes"
               (assert-equal #:expect "\"a\\\"b\\\\c\""
                             #:got (elisp-quote "a\"b\\c")))))

(define running-suite
  (suite "Running Emacs"
         #:concurrent? #f
         #:shuffle? #f
         (test "a batch run is what a dry run prints"
               (parameterize ((emacs-command "emacs"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (emacs-batch #:evals '("(princ 1)")))
                   (assert-equal
                    #:expect (list #t)
                    #:got (list (and (string-contains
                                      (get-output-string out)
                                      "emacs --batch -Q --eval '(princ 1)'")
                                     #t))))))
         (test "the output query returns what Emacs printed"
               (with-scratch "output"
                (lambda (dir)
                  (parameterize ((emacs-command (fake-emacs dir)))
                    (assert-equal #:expect "29.1\n"
                                  #:got (emacs-capture
                                         #:evals '("(princ emacs-version)")))))))
         (test "the version trims the printed answer"
               (with-scratch "version"
                (lambda (dir)
                  (parameterize ((emacs-command (fake-emacs dir)))
                    (assert-equal #:expect "29.1"
                                  #:got (elisp-version))))))))

(define check-suite
  (suite "The three checks"
         #:concurrent? #f
         #:shuffle? #f
         (test "byte-compiling names the function and the files"
               (parameterize ((emacs-command "emacs"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (elisp-byte-compile '("foo.el") #:load-paths '("lisp")))
                   (assert-equal
                    #:expect (list #t #t)
                    #:got (list (and (string-contains
                                      (get-output-string out)
                                      "-f batch-byte-compile")
                                     #t)
                                (and (string-contains
                                      (get-output-string out)
                                      "foo.el")
                                     #t))))))
         (test "warnings can be made to fail the compile"
               (parameterize ((emacs-command "emacs"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (elisp-byte-compile '("foo.el") #:warnings-as-errors? #t))
                   (assert-true (and (string-contains
                                      (get-output-string out)
                                      "byte-compile-error-on-warn")
                                     #t)))))
         (test "checkdoc runs over the files it was given"
               (parameterize ((emacs-command "emacs"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (elisp-checkdoc '("foo.el")))
                   (assert-true (and (string-contains
                                      (get-output-string out)
                                      "checkdoc-current-buffer")
                                     #t)))))
         (test "ERT loads the test files and runs every test by default"
               (parameterize ((emacs-command "emacs"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (elisp-ert '("foo-tests.el") #:load-paths '(".")))
                   (let ((text (get-output-string out)))
                     (assert-equal
                      #:expect (list #t #t)
                      #:got (list (and (string-contains text "-l foo-tests.el")
                                       #t)
                                  (and (string-contains
                                        text "ert-run-tests-batch-and-exit t")
                                       #t)))))))
         (test "an ERT selector is inserted as the expression it is"
               (parameterize ((emacs-command "emacs"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (elisp-ert '("foo-tests.el") #:selector "(tag smoke)"))
                   (assert-true (and (string-contains
                                      (get-output-string out)
                                      "ert-run-tests-batch-and-exit (tag smoke)")
                                     #t)))))
         (test "an empty file list is refused"
               (assert-error (lambda () (elisp-byte-compile '()))))))

(define practice-suite
  (suite "The practices"
         #:concurrent? #f
         #:shuffle? #f
         (test "the version header is read"
               (with-scratch "version"
                (lambda (dir)
                  (let ((path (in-vicinity dir "foo.el")))
                    (write-text path good-source)
                    (assert-equal #:expect "1.2.3"
                                  #:got (elisp-package-version path))))))
         (test "the requirements header is read as it is written"
               (with-scratch "requires"
                (lambda (dir)
                  (let ((path (in-vicinity dir "foo.el")))
                    (write-text path good-source)
                    (assert-equal #:expect "((emacs \"27.1\"))"
                                  #:got (elisp-package-requires path))))))
         (test "the provided feature is read"
               (with-scratch "provide"
                (lambda (dir)
                  (let ((path (in-vicinity dir "foo.el")))
                    (write-text path good-source)
                    (assert-equal #:expect '("foo")
                                  #:got (elisp-provides path))))))
         (test "lexical binding and the footer are recognised"
               (with-scratch "conventions"
                (lambda (dir)
                  (let ((path (in-vicinity dir "foo.el")))
                    (write-text path good-source)
                    (assert-equal #:expect '(#t #t)
                                  #:got (list (elisp-lexical-binding? path)
                                              (elisp-footer? path)))))))
         (test "a file that keeps the conventions has no problems"
               (with-scratch "good"
                (lambda (dir)
                  (let ((path (in-vicinity dir "foo.el")))
                    (write-text path good-source)
                    (assert-equal #:expect '()
                                  #:got (elisp-practices path))))))
         (test "a bare file names every convention it is missing"
               (with-scratch "bad"
                (lambda (dir)
                  (let ((path (in-vicinity dir "bar.el")))
                    (write-text path bad-source)
                    (let ((problems (elisp-practices path)))
                      (assert-equal #:expect 6
                                    #:got (length problems)))))))
         (test "the files are found recursively and hidden directories are skipped"
               (with-scratch "files"
                (lambda (dir)
                  (let ((lisp (in-vicinity dir "lisp"))
                        (sub (in-vicinity dir "lisp/sub"))
                        (hidden (in-vicinity dir ".cache")))
                    (mkdir-p sub)
                    (mkdir-p hidden)
                    (write-text (in-vicinity lisp "a.el") good-source)
                    (write-text (in-vicinity sub "b.el") bad-source)
                    (write-text (in-vicinity hidden "c.el") bad-source)
                    (assert-equal
                     #:expect (list (in-vicinity lisp "a.el")
                                    (in-vicinity sub "b.el"))
                     #:got (elisp-files lisp))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               running-suite
               check-suite
               practice-suite))
