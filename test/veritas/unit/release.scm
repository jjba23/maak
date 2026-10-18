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
;; Unit tests for `(maak release)', the module that keeps the copies of the
;; version in step with `maak-version'.  The first case is the one that matters
;; for a release: it reads this checkout's own spec, manual page and README, so
;; a tree that has been bumped without `maak sync-version' fails here rather
;; than shipping a spec that disagrees with the program.
;;
;; The other cases work on a miniature tree in the temporary directory, which
;; is what lets them show a stale copy being found and rewritten without
;; editing the checkout they are run in.  The miniature files are written out
;; in full rather than generated from `version-sites', so the patterns those
;; sites use are tested against text that looks like the real thing.

;;; Code:

(define-module (veritas unit release)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak version)
  #:use-module (maak release)
  #:export (spec))

(define (release-scratch-directory)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (string-append (or (getenv "TMPDIR") "/tmp")
                            "/maak-test-release-"
                            (number->string (getpid)))))
    (parameterize ((quiet? #t))
      (when (file-exists? dir)
        (delete-file-recursively dir))
      (mkdir-p dir))
    dir))

(define (clean-up dir)
  "Remove the scratch DIR without printing what was removed."
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

(define (write-version-tree root version)
  "Write a miniature of the files that name the version into ROOT.

Every file says VERSION, in the place and the shape the real one does: the
spec's `Version:' field, the manual page's `.TH' header, and the two places the
README quotes -- an RPM artefact name and the sample output of `--version'."
  (parameterize ((quiet? #t))
    (mkdir-p (string-append root "/packaging/opensuse"))
    (mkdir-p (string-append root "/man"))
    (write-file (string-append root "/packaging/opensuse/maak.spec")
                (format #f "Name:           maak~%Version:        ~a~%Release:        0~%"
                        version))
    (write-file (string-append root "/man/maak.1")
                (format #f ".TH MAAK 1 \"2026\" \"maak ~a\" \"User Commands\"~%"
                        version))
    (write-file (string-append root "/README.org")
                (format #f "~/rpmbuild/RPMS/x86_64/maak-~a-0.x86_64.rpm~%maak ~a~%"
                        version version))))

(define (scratch-tree version)
  "Return a scratch directory holding files that all say VERSION."
  (let ((dir (release-scratch-directory)))
    (write-version-tree dir version)
    dir))

(define (named-files mismatches)
  "Return the files NAMED by MISMATCHES, each once, in the order they appear."
  (delete-duplicates (map first mismatches)))

(define release-suite
  (suite "The version's copies"
         #:concurrent? #f
         #:shuffle? #f

         (test "every file that names the version says the one (maak version) has"
               (assert-equal #:expect '()
                             #:got (version-mismatches)))

         (test "the version is written the way default.nix reads it"
               ;; default.nix finds the value with a regular expression over
               ;; the module's source, so a version of another shape would
               ;; evaluate to an error rather than a package.
               (assert-true (and (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+$"
                                               maak-version)
                                 #t)))

         (test "a tree that has fallen behind is reported site by site"
               (let ((dir (scratch-tree "0.0.1")))
                 (dynamic-wind
                   (lambda () #f)
                   (lambda ()
                     ;; Four sites, in three files: the README names the
                     ;; version twice.
                     (assert-equal #:expect (list 4 3)
                                   #:got (list (length (version-mismatches
                                                        #:directory dir))
                                               (length (named-files
                                                        (version-mismatches
                                                         #:directory dir))))))
                   (lambda () (clean-up dir)))))

         (test "sync-version rewrites every file that is behind, and says which"
               (let ((dir (scratch-tree "0.0.1")))
                 (dynamic-wind
                   (lambda () #f)
                   (lambda ()
                     (assert-equal #:expect
                                   (list (list "packaging/opensuse/maak.spec"
                                               "man/maak.1"
                                               "README.org")
                                         '())
                                   #:got (list (sync-version! #:directory dir)
                                               (version-mismatches
                                                #:directory dir))))
                   (lambda () (clean-up dir)))))

         (test "sync-version leaves a tree that is in step alone"
               (let ((dir (scratch-tree maak-version)))
                 (dynamic-wind
                   (lambda () #f)
                   (lambda ()
                     (assert-equal #:expect '()
                                   #:got (sync-version! #:directory dir)))
                   (lambda () (clean-up dir)))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               release-suite))
