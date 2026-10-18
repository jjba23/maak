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
;; The version is written by hand in one place, `maak-version' in `(maak
;; version)'.  Three files of this repository cannot read a Scheme module and
;; so carry a copy of it: the RPM spec, which `rpmbuild' parses before it
;; unpacks the source; the manual page, which is roff; and the README, which
;; quotes both.  This module rewrites those copies from the module, and reports
;; the ones that have fallen behind, so a release is one edit and a command
;; rather than a search of the tree.
;;
;; `default.nix' is deliberately not among them: Nix reads the module while it
;; evaluates the derivation, so there is no copy there to keep in step.
;;
;; The file names are this repository's own, which is why this sits beside the
;; library rather than inside it: a project using maak would name its own.

;;; Code:

(define-module (maak release)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak version)
  #:export (version-sites
            version-mismatches
            sync-version!))

;; Each site is a file, the text in it that names the version, and what that
;; text becomes: the part before the version and the part after it.  A site's
;; text is replaced whole, so everything the file says around it stays exactly
;; as it was -- the column the spec's field is aligned to, the quotes around
;; the manual page's header, and the wording of the README's sample output.
(define version-sites
  '(("packaging/opensuse/maak.spec"
     "Version:[ \t]+[0-9][0-9.]*"
     "Version:        "
     "")
    ("man/maak.1"
     "maak [0-9][0-9.]*"
     "maak "
     "")
    ("README.org"
     "maak-[0-9][0-9.]*-0"
     "maak-"
     "-0")
    ("README.org"
     "maak [0-9][0-9.]*"
     "maak "
     "")))

(define (site-version-text site version)
  "Return what SITE should say when the version is VERSION."
  (string-append (third site) version (fourth site)))

(define (site-files)
  "Return the files the sites name, each once, in the order they appear."
  (fold (lambda (site files)
          (if (member (first site) files)
              files
              (append files (list (first site)))))
        '()
        version-sites))

(define (sites-of file)
  "Return the sites that live in FILE."
  (filter (lambda (site) (string=? (first site) file)) version-sites))

(define (site-path directory file)
  "Return the path of FILE inside DIRECTORY.

A directory of `.' is the working directory itself, and the path of a file in
it is the name the sites use, so a caller in the checkout need not spell it
out."
  (if (string=? directory ".")
      file
      (string-append directory "/" file)))

(define (read-text path)
  "Return the contents of PATH, raising a maak error when it is not there."
  (let ((text (read-file path)))
    (unless text
      (raise-maak-error
       (format #f "no file at ~a" path)
       #:hint
       "Run this from the root of the maak checkout, or name the root with #:directory."))
    text))

(define (site-found path pattern)
  "Return the text PATTERN matches in the file at PATH, or #f when it matches none."
  (let ((match (string-match pattern (read-text path))))
    (and match (match:substring match 0))))

(define* (version-mismatches #:key (directory "."))
  "Return the version sites that do not name `(maak version)'.

Each entry is `(FILE FOUND EXPECTED)': what the file says now, which is #f
when its pattern matches nothing at all, and what the current version would
have it say.  An empty list means every copy is in step.

DIRECTORY is the root the site paths are resolved against, `.' by default."
  (filter-map
   (lambda (site)
     (let* ((expected (site-version-text site maak-version))
            (found (site-found (site-path directory (first site))
                               (second site))))
       (and (not (and found (string=? found expected)))
            (list (first site) found expected))))
   version-sites))

(define* (sync-version! #:key (directory "."))
  "Write `(maak version)' into every file that names it.

Return the files that changed, in the order the sites name them; an empty list
means the tree was already in step.  Only a file that actually differs is
written, so a run that changes nothing leaves the tree alone and says nothing.

DIRECTORY is the root the site paths are resolved against, `.' by default."
  (fold
   (lambda (file changed)
     (let* ((path (site-path directory file))
            (text (read-text path))
            (rewritten
             (fold (lambda (site text)
                     (regexp-substitute/global
                      #f (second site) text 'pre
                      (site-version-text site maak-version) 'post))
                   text
                   (sites-of file))))
       (if (string=? text rewritten)
           changed
           (begin
             (write-file path rewritten)
             (append changed (list file))))))
   '()
   (site-files)))
