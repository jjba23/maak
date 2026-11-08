;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The version is written once, in src/maak/version.scm, where the library and
;; `maak --version' read it and where default.nix reads it while it evaluates.
;; Every file that cannot read a Scheme module carries a copy instead: the RPM
;; spec, the manual page, the installation page, the Arch recipe and the
;; Debian changelog.
;;
;; `maak sync-version' rewrites all but the changelog from that file, and this
;; spec is what notices a copy that nobody rewrote.  It found two: the Arch
;; recipe still said 0.12.2 and the changelog 0.8.3, while the program said
;; 0.14.1 -- a recipe that would have built a release two versions old.
;;
;; The copies are read from the checkout the runner was started in, so a run
;; from the project root compares the tree with the version about to be
;; released, which is exactly what the release asks.

;;; Code:

(define-module (veritas unit version)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (maak version)
  #:export (spec))

(define (slurp path)
  "Return the whole of PATH as a string."
  (call-with-input-file path get-string-all))

(define (version-in path pattern)
  "Return the version PATTERN names in PATH, or #f when it names none.

PATTERN's first group is the version, and the first match in the file is the
one read: each of these files has a field for it, so the first match is the
field rather than a number that happens to look like one."
  (let ((found (string-match pattern (slurp path))))
    (and found (match:substring found 1))))

(define version-suite
  (suite "The version's copies"
         #:concurrent? #f
         #:shuffle? #f
         (test "the program writes the version in three numbers"
               (assert-equal #:expect #t
                             #:got (and (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+$"
                                                      maak-version)
                                        #t)))

         (test "the RPM spec carries the released version"
               (assert-equal #:expect maak-version
                             #:got (version-in
                                    "packaging/opensuse/maak.spec"
                                    "Version:[ \t]+([0-9]+\\.[0-9]+\\.[0-9]+)")))

         (test "the manual page carries the released version"
               (assert-equal #:expect maak-version
                             #:got (version-in "man/maak.1"
                                               "maak ([0-9]+\\.[0-9]+\\.[0-9]+)")))

         (test "the installation page carries the released version"
               (assert-equal #:expect maak-version
                             #:got (version-in
                                    "docs/installation.org"
                                    "maak ([0-9]+\\.[0-9]+\\.[0-9]+)")))

         (test "the Arch recipe carries the released version"
               (assert-equal #:expect maak-version
                             #:got (version-in
                                    "packaging/arch/PKGBUILD"
                                    "pkgver=([0-9]+\\.[0-9]+\\.[0-9]+)")))

         (test "the Debian changelog's newest entry carries the released version"
               (assert-equal #:expect maak-version
                             #:got (version-in
                                    "packaging/debian/changelog"
                                    "maak \\(([0-9]+\\.[0-9]+\\.[0-9]+)-")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               version-suite))
