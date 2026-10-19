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
;; Tests for `maak sync-version', the task that keeps the copies of the version
;; in step with `maak-version'.  They run the task the way a release does --
;; through the launcher, against a tree named on the command line -- so what is
;; tested is the task as it is wired, not a procedure that happens to hold the
;; same rules.
;;
;; The task deliberately imports nothing from a maak module: it reads the
;; version out of the tree and rewrites the copies with plain Guile, which is
;; what lets it run under an installed maak older than the one the checkout
;; names.  A case here cannot prove that on its own, since the launcher beside
;; the sources always has the current library; what the cases do catch is the
;; regressions that made a library module tempting -- a `use-modules' inside
;; the task body, or a version read from the running library rather than from
;; the tree.
;;
;; Each case works on a tree of its own under the temporary directory, so none
;; of them edits the checkout it is run in.  The first case copies the
;; checkout's own files into such a tree and asks that nothing there changes:
;; the same check the module test made, but read-only, so a tree that has been
;; bumped without the sync fails here instead of being repaired by the test
;; that noticed.  The miniature trees write each file out in full rather than
;; generating it from the site list, so the patterns are tested against text
;; that looks like the real thing.
;;
;; The paths are relative to the project root, which is where the test runner
;; is started from (see the Tests section of the README).

;;; Code:

(define-module (veritas unit release)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (spec))

(define launcher "scripts/maak")

;; The files the task keeps in step.
(define sites
  '("packaging/opensuse/maak.spec"
    "man/maak.1"
    "README.org"))

(define version-source "src/maak/version.scm")

;; ====== A tree to run against ======

(define (scratch-directory name)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (format #f "~a/maak-release-~a-~a" (tmp-dir) (getpid) name)))
    (parameterize ((quiet? #t))
      (when (file-exists? dir)
        (delete-file-recursively dir))
      (mkdir-p dir))
    dir))

(define (at dir relative)
  "Return the path of RELATIVE inside DIR."
  (string-append dir "/" relative))

(define (put dir relative text)
  "Write TEXT to RELATIVE inside DIR, making the directories on the way."
  (parameterize ((quiet? #t))
    (mkdir-p (dirname (at dir relative))))
  (write-file (at dir relative) text))

(define (get dir relative)
  "Return the text of RELATIVE inside DIR."
  (read-file (at dir relative)))

(define (clean-up dir)
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

(define (version-file version)
  "Return a version module that names VERSION, in the shape the task reads."
  (format #f "(define maak-version~%  \"~a\")~%" version))

(define (site-texts version)
  "Return the copies of VERSION as `(FILE . TEXT)', one per site.

The spec's field is padded, the manual page quotes its header and the README
names the version twice, which is what the task has to leave alone."
  (list (cons "packaging/opensuse/maak.spec"
              (format #f "Name:           maak~%Version:        ~a~%Release:        0~%"
                      version))
        (cons "man/maak.1"
              (format #f ".TH MAAK 1 \"2026\" \"maak ~a\" \"User Commands\"~%"
                      version))
        (cons "README.org"
              (format #f "~~/rpmbuild/RPMS/x86_64/maak-~a-0.x86_64.rpm~%maak ~a~%"
                      version version))))

(define (miniature-tree name version)
  "Return a scratch tree whose files all name VERSION."
  (let ((dir (scratch-directory name)))
    (put dir version-source (version-file version))
    (for-each (lambda (site)
                (put dir (car site) (cdr site)))
              (site-texts version))
    dir))

(define (checkout-tree name)
  "Return a scratch copy of this checkout's own version and copies."
  (let ((dir (scratch-directory name)))
    (put dir version-source (read-file version-source))
    (for-each (lambda (site)
                (put dir site (read-file site)))
              sites)
    dir))

(define (copies dir)
  "Return `(FILE . TEXT)' for the sites of DIR, in the order they are listed."
  (map (lambda (site) (cons site (get dir site))) sites))

(define (changed-copies before after)
  "Return the files whose text differs between BEFORE and AFTER."
  (filter-map (lambda (was now)
                (and (not (string=? (cdr was) (cdr now)))
                     (car was)))
              before after))

;; ====== Running the task ======

(define (unique-path name)
  "Return a path of this run's own for NAME, under the temporary directory."
  (format #f "~a/maak-release-run-~a-~a" (tmp-dir) (getpid) name))

(define (without-load-notes text)
  "Return TEXT without the notes Guile prints while it compiles a file.

A launcher started against a cold cache compiles the library and this
checkout's Maak file before it runs anything, and says so on standard error.
Those lines are about loading rather than about the run, so a case that asks
what the run said must not read them."
  (string-join
   (remove (lambda (line) (string-prefix? ";;; " line))
           (string-split text #\newline))
   "\n"))

(define (launcher-run . arguments)
  "Run the launcher with ARGUMENTS and return `(STATUS STDOUT STDERR)'."
  (let* ((err-file (unique-path "stderr"))
         (port (with-error-to-file err-file
                 (lambda ()
                   (apply open-pipe* OPEN_READ launcher arguments))))
         (stdout (get-string-all port))
         (status (status:exit-val (close-pipe port)))
         (stderr (without-load-notes (read-file err-file #:default ""))))
    (delete-file err-file)
    (list status stdout stderr)))

(define (run-sync dir)
  "Run `sync-version' on DIR through the launcher, without the banner."
  (launcher-run "--no-banner" "sync-version" dir))

(define (status-of result)
  (list-ref result 0))

(define (stdout-of result)
  (list-ref result 1))

(define (stderr-of result)
  (list-ref result 2))

(define (said? text needle)
  "Return #t when TEXT contains NEEDLE, and #f otherwise."
  (and (string-contains text needle) #t))

;; ====== The suite ======

(define release-suite
  (suite "Releasing"
         #:concurrent? #f
         #:shuffle? #f

         (test "this checkout's copies say the version its module does"
               ;; Read-only on purpose: the tree is copied aside and the task
               ;; runs there, so a bump that skipped the sync fails the suite
               ;; rather than being repaired by it.  The status is part of the
               ;; assertion, because a run that never started would leave the
               ;; copies untouched and the check would pass without reading
               ;; them.
               (let ((dir (checkout-tree "checkout")))
                 (dynamic-wind
                   (lambda () #f)
                   (lambda ()
                     (let* ((before (copies dir))
                            (result (run-sync dir)))
                       (assert-equal #:expect (list 0 '())
                                     #:got (list (status-of result)
                                                 (changed-copies
                                                  before
                                                  (copies dir))))))
                   (lambda ()
                     (clean-up dir)))))

         (test "a tree that has fallen behind is rewritten, and nothing around it"
               ;; The version in the module is 1.2.3 and the copies still say
               ;; 0.9.0: every one of them is rewritten, with the spec's
               ;; padding, the manual page's quoting and both places the README
               ;; names the version left as they were.
               (let ((dir (miniature-tree "behind" "0.9.0")))
                 (dynamic-wind
                   (lambda () #f)
                   (lambda ()
                     (put dir version-source (version-file "1.2.3"))
                     (assert-equal
                      #:expect
                      (list 0
                            "Name:           maak\nVersion:        1.2.3\nRelease:        0\n"
                            ".TH MAAK 1 \"2026\" \"maak 1.2.3\" \"User Commands\"\n"
                            "~/rpmbuild/RPMS/x86_64/maak-1.2.3-0.x86_64.rpm\nmaak 1.2.3\n")
                      #:got
                      (list (status-of (run-sync dir))
                            (get dir "packaging/opensuse/maak.spec")
                            (get dir "man/maak.1")
                            (get dir "README.org"))))
                   (lambda ()
                     (clean-up dir)))))

         (test "the run names every file it wrote"
               (let ((dir (miniature-tree "named" "0.9.0")))
                 (dynamic-wind
                   (lambda () #f)
                   (lambda ()
                     (put dir version-source (version-file "1.2.3"))
                     (let ((out (stdout-of (run-sync dir))))
                       (assert-equal
                        #:expect (list #t #t #t)
                        #:got (list (said? out "Wrote 1.2.3 into packaging/opensuse/maak.spec.")
                                    (said? out "Wrote 1.2.3 into man/maak.1.")
                                    (said? out "Wrote 1.2.3 into README.org.")))))
                   (lambda ()
                     (clean-up dir)))))

         (test "a tree that is in step is left alone"
               (let ((dir (miniature-tree "in-step" "1.2.3")))
                 (dynamic-wind
                   (lambda () #f)
                   (lambda ()
                     (let ((before (copies dir)))
                       (let ((result (run-sync dir)))
                         (assert-equal
                          #:expect (list 0 '())
                          #:got (list (status-of result)
                                      (changed-copies before (copies dir)))))))
                   (lambda ()
                     (clean-up dir)))))

         (test "a tree with no version module is refused, and says where it looked"
               (let ((dir (scratch-directory "no-version")))
                 (dynamic-wind
                   (lambda () #f)
                   (lambda ()
                     (let* ((result (run-sync dir))
                            (said (stderr-of result)))
                       (assert-equal
                        #:expect (list 1 #t)
                        #:got (list (status-of result)
                                    (said? said "src/maak/version.scm")))))
                   (lambda ()
                     (clean-up dir)))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               release-suite))
