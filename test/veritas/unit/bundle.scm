;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak bundle)': the package file it generates, the checks it makes
;; before writing a path into one, and the `guix pack' command a dry run prints.
;; A real pack is a Guix build and is not run here; what is tested is the
;; description the build reads, which is the part a maak change can break.
;;
;; The generated file uses Guix's `#~' reader, so it is asserted on as text
;; rather than read back by plain Guile.

;;; Code:

(define-module (veritas unit bundle)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak bundle)
  #:export (spec))

(define (said? text needle)
  (and (string-contains text needle) #t))

(define script
  (bundle-script #:name "demo"
                 #:maak-file "maak.scm"
                 #:packages (list "bash" "coreutils" "git")))

(define script-suite
  (suite "The package a bundle is built from"
         #:concurrent? #f
         #:shuffle? #f
         (test "the package is named after the bundle"
               (assert-true (said? script "(name \"demo\")")))
         (test "the package installs the Maak file it was given"
               (assert-true (said? script
                                   (format #f "(local-file ~s \"maak.scm\")"
                                           (canonicalize-path "maak.scm")))))
         (test "the package installs a launcher"
               (assert-true (said? script "out \"/bin/demo\"")))
         (test "the launcher finds the maak and the Maak file"
               (assert-true (and (said? script "-f ~a/share/demo/maak.scm")
                                 (said? script "file-append maak \"/bin/maak\"")
                                 #t)))
         (test "the launcher puts its own directory on PATH"
               (assert-true (said? script "dirname")))
         (test "the package propagates maak, Guile and the other packages"
               (assert-true (said? script
                                   "(append (list maak guile bash)")))
         (test "the packages are the ones the caller named"
               (assert-true (said? script "\"git\"")))
         (test "a third-party maak package expression is inserted as written"
               (assert-true (said? (bundle-script
                                    #:name "demo"
                                    #:maak-file "maak.scm"
                                    #:maak-package "(begin (load \"/p/guix.scm\") maak)")
                                   "(define maak (begin (load \"/p/guix.scm\") maak))")))))

(define refusal-suite
  (suite "What a bundle refuses"
         #:concurrent? #f
         #:shuffle? #f
         (test "a name with a slash is refused"
               (assert-error (lambda () (bundle-script #:name "a/b"))))
         (test "an empty name is refused"
               (assert-error (lambda () (bundle-script #:name ""))))
         (test "a missing Maak file is refused"
               (assert-error (lambda () (bundle-script #:maak-file "no-such.scm"))))
         (test "a package list that is not a list is refused"
               (assert-error (lambda () (bundle-script #:packages "bash"))))
         (test "a package entry that is not a name is refused"
               (assert-error (lambda () (bundle-script #:packages (list 42)))))
         (test "loading a bundle that is not docker is refused"
               (assert-error (lambda () (bundle #:format "tarball" #:load? #t))))))

(define command-suite
  (suite "The pack a bundle asks for"
         #:concurrent? #f
         #:shuffle? #f
         (test "a tarball bundle asks for a relocatable pack with bin on PATH"
               (let ((out (open-output-string)))
                 (parameterize ((current-output-port out)
                                (dry-run? #t))
                   (bundle #:name "demo" #:maak-file "maak.scm"
                           #:packages (list "bash" "coreutils")))
                 (assert-true
                  (and (said? (get-output-string out) "guix pack --file=")
                       (said? (get-output-string out) "--format=tarball")
                       (said? (get-output-string out) "--relocatable")
                       (said? (get-output-string out) "-S /bin=bin")
                       #t))))
         (test "a bundle returns the file it would write"
               (let ((out (open-output-string)))
                 (parameterize ((current-output-port out)
                                (dry-run? #t))
                   (assert-true
                    (said? (bundle #:name "demo" #:maak-file "maak.scm")
                           "demo-bundle.tar.gz")))))
         (test "a docker bundle is written as an image tarball"
               (let ((out (open-output-string)))
                 (parameterize ((current-output-port out)
                                (dry-run? #t))
                   (assert-true
                    (said? (bundle #:name "demo" #:maak-file "maak.scm"
                                   #:format "docker")
                           "demo-bundle-docker.tar.gz")))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               script-suite
               refusal-suite
               command-suite))
