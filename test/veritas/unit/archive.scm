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
;; Unit tests for `(maak archive)'.  The argument builders are checked directly
;; and through dry-run mode, and the procedures are then run against real `tar'
;; and `zip' archives of files written into a temporary directory.  `zip' and
;; `unzip' are part of the test environment in `manifest.scm', so the real
;; tests do not depend on what the host happens to have installed.

;;; Code:

(define-module (veritas unit archive)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak archive)
  #:export (spec))

(define escape-character
  (integer->char 27))

(define (strip-ansi str)
  "Return STR without ANSI escape sequences."
  (let loop ((chars (string->list str))
             (out '())
             (escaping? #f))
    (cond
      ((null? chars) (list->string (reverse out)))
      ((and escaping? (char=? (car chars) #\m))
       (loop (cdr chars) out #f))
      (escaping? (loop (cdr chars) out #t))
      ((char=? (car chars) escape-character)
       (loop (cdr chars) out #t))
      (else (loop (cdr chars) (cons (car chars) out) #f)))))

(define (dry-run-command thunk)
  "Return the command THUNK would run, as printed by dry-run mode."
  (parameterize ((dry-run? #t) (quiet? #f))
    (strip-ansi (with-output-to-string thunk))))

(define (scratch-directory label)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (format #f "~a/maak-test-archive-~a-~a"
                     (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (remove-directory dir)
  "Remove DIR quietly, whatever the tests left in it."
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

(define (listing-of archive)
  "Return the names `tar -tf' prints for ARCHIVE."
  (capture (format #f "tar -tf ~a" (shell-quote archive))))

(define (zip-listing-of archive)
  "Return the text `unzip -l' prints for ARCHIVE."
  (capture (format #f "unzip -l ~a" (shell-quote archive))))

(define arguments-suite
  (suite "Archive argument lists"
         #:concurrent? #f
         #:shuffle? #f
         (test "a .tar.gz name selects gzip"
               (assert-equal
                #:expect '("tar" "-c" "-z" "-f" "p.tar.gz" "src")
                #:got (tar-create-argv "p.tar.gz" (list "src"))))
         (test "a .tar.xz name selects xz"
               (assert-equal
                #:expect '("tar" "-c" "-J" "-f" "p.tar.xz" "src")
                #:got (tar-create-argv "p.tar.xz" (list "src"))))
         (test "a .tar name selects no compression"
               (assert-equal
                #:expect '("tar" "-c" "-f" "p.tar" "src")
                #:got (tar-create-argv "p.tar" (list "src"))))
         (test "a .tar.zst name selects zstd"
               (assert-equal
                #:expect '("tar" "-c" "--zstd" "-f" "p.tar.zst" "src")
                #:got (tar-create-argv "p.tar.zst" (list "src"))))
         (test "the short extension names select their compressor"
               (assert-equal
                #:expect (list '("tar" "-c" "-z" "-f" "p.tgz" "src")
                               '("tar" "-c" "-j" "-f" "p.tbz2" "src")
                               '("tar" "-c" "-J" "-f" "p.txz" "src")
                               '("tar" "-c" "--zstd" "-f" "p.tzst" "src"))
                #:got (list (tar-create-argv "p.tgz" (list "src"))
                            (tar-create-argv "p.tbz2" (list "src"))
                            (tar-create-argv "p.txz" (list "src"))
                            (tar-create-argv "p.tzst" (list "src")))))
         (test "a compression symbol is accepted too"
               (assert-equal #:expect '("-z")
                             #:got (tar-compression-option 'gz)))
         (test "an explicit compression wins over the extension"
               (assert-equal
                #:expect '("tar" "-c" "-j" "-f" "p.tar.gz" "src")
                #:got (tar-create-argv "p.tar.gz" (list "src")
                                       #:compression "bzip2")))
         (test "gzip is accepted by name and by alias"
               (assert-equal #:expect '("-z")
                             #:got (tar-compression-option "gzip")))
         (test "bzip2 is accepted by name and by alias"
               (assert-equal #:expect '("-j")
                             #:got (tar-compression-option "bzip2")))
         (test "xz has a single spelling"
               (assert-equal #:expect '("-J")
                             #:got (tar-compression-option "xz")))
         (test "zstd is accepted by name and by alias"
               (assert-equal #:expect '("--zstd")
                             #:got (tar-compression-option "zst")))
         (test "no compression is the empty flag list"
               (assert-equal #:expect '()
                             #:got (tar-compression-option "none")))
         (test "an unknown compression is refused"
               (assert-error (lambda ()
                               (tar-compression-option "rar"))))
         (test "tar options precede the files"
               (assert-equal
                #:expect '("tar" "-c" "-z" "-v" "-f" "p.tar.gz"
                           "-C" "src" "--exclude=*.o" ".")
                #:got (tar-create-argv "p.tar.gz" (list ".")
                                       #:directory "src"
                                       #:exclude (list "*.o")
                                       #:verbose? #t)))
         (test "tar extraction names the tree and the strip count"
               (assert-equal
                #:expect '("tar" "-x" "-f" "p.tar.gz" "-C" "out"
                           "--strip-components=1")
                #:got (tar-extract-argv "p.tar.gz"
                                        #:directory "out"
                                        #:strip-components 1)))
         (test "tar extraction is verbose when asked"
               (assert-equal
                #:expect '("tar" "-x" "-v" "-f" "p.tar.gz")
                #:got (tar-extract-argv "p.tar.gz" #:verbose? #t)))
         (test "tar extraction refuses a negative strip count"
               (assert-error (lambda ()
                               (tar-extract-argv "p.tar.gz"
                                                 #:strip-components -1))))
         (test "tar listing is a separate argv"
               (assert-equal #:expect '("tar" "-t" "-f" "p.tar.gz")
                             #:got (tar-list-argv "p.tar.gz")))
         (test "zip passes exclude patterns after the files"
               (assert-equal
                #:expect '("zip" "-r" "p.zip" "src" "-x" "*.o" "*.go")
                #:got (zip-create-argv "p.zip" (list "src")
                                       #:exclude (list "*.o" "*.go"))))
         (test "zip can store bare names and be quiet"
               (assert-equal
                #:expect '("zip" "-q" "-r" "-j" "p.zip" "a.txt")
                #:got (zip-create-argv "p.zip" (list "a.txt")
                                       #:junk-paths? #t
                                       #:quiet? #t)))
         (test "zip extraction defaults to overwriting"
               (assert-equal
                #:expect '("unzip" "-o" "p.zip" "-d" "out")
                #:got (zip-extract-argv "p.zip" #:directory "out")))
         (test "zip extraction can keep existing files"
               (assert-equal
                #:expect '("unzip" "-n" "p.zip")
                #:got (zip-extract-argv "p.zip" #:overwrite? #f)))
         (test "zip listing is its own argv"
               (assert-equal #:expect '("unzip" "-l" "p.zip")
                             #:got (zip-list-argv "p.zip")))
         (test "an empty tar name is refused"
               (assert-error (lambda () (tar-create "" (list "a")))))
         (test "an empty zip name is refused"
               (assert-error (lambda () (zip-create "" (list "a")))))
         (test "an empty file list is refused by tar"
               (assert-error (lambda () (tar-create "p.tar" '()))))
         (test "an empty file list is refused by zip"
               (assert-error (lambda () (zip-create "p.zip" '()))))
         (test "a dry run prints the command it would run"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (tar-create "/tmp/maak-test-dry.tar.gz" (list "src"))))
                #:got "tar -c -z -f /tmp/maak-test-dry.tar.gz src"))))

(define tar-suite
  (suite "Real tar archives"
         #:concurrent? #f
         #:shuffle? #f
         (test "a created tarball lists its files"
               (let* ((dir (scratch-directory "tar-create"))
                      (archive (string-append dir "/p.tar.gz"))
                      (names (begin
                               (write-file (string-append dir "/a.txt") "one")
                               (write-file (string-append dir "/b.txt") "two")
                               (tar-create archive (list "a.txt" "b.txt")
                                           #:directory dir)
                               (listing-of archive))))
                 (remove-directory dir)
                 (assert-contains #:expect names #:got "a.txt")))
         (test "an excluded file is not in the tarball"
               (let* ((dir (scratch-directory "tar-exclude"))
                      (archive (string-append dir "/p.tar.gz"))
                      (names (begin
                               (write-file (string-append dir "/a.txt") "one")
                               (write-file (string-append dir "/b.log") "two")
                               (tar-create archive (list ".")
                                           #:directory dir
                                           #:exclude (list "*.log"))
                               (listing-of archive))))
                 (remove-directory dir)
                 (assert-false (string-contains names "b.log"))))
         (test "a tarball can be extracted"
               (let* ((dir (scratch-directory "tar-extract"))
                      (archive (string-append dir "/p.tar.gz"))
                      (out (string-append dir "/out"))
                      (text (begin
                              (write-file (string-append dir "/a.txt") "hello")
                              (tar-create archive (list "a.txt") #:directory dir)
                              (mkdir-p out)
                              (tar-extract archive #:directory out)
                              (read-file (string-append out "/a.txt")))))
                 (remove-directory dir)
                 (assert-equal #:expect "hello" #:got text)))
         (test "a strip count removes the top-level directory"
               (let* ((dir (scratch-directory "tar-strip"))
                      (archive (string-append dir "/p.tar.gz"))
                      (out (string-append dir "/out"))
                      (text (begin
                              (mkdir-p (string-append dir "/p"))
                              (write-file (string-append dir "/p/a.txt") "deep")
                              (tar-create archive (list "p") #:directory dir)
                              (mkdir-p out)
                              (tar-extract archive
                                           #:directory out
                                           #:strip-components 1)
                              (read-file (string-append out "/a.txt")))))
                 (remove-directory dir)
                 (assert-equal #:expect "deep" #:got text)))
         (test "an xz tarball round-trips"
               (let* ((dir (scratch-directory "tar-xz"))
                      (archive (string-append dir "/p.tar.xz"))
                      (names (begin
                               (write-file (string-append dir "/a.txt") "xz")
                               (tar-create archive (list "a.txt") #:directory dir)
                               (listing-of archive))))
                 (remove-directory dir)
                 (assert-contains #:expect names #:got "a.txt")))
         (test "tar-list succeeds on a real archive"
               (let* ((dir (scratch-directory "tar-list"))
                      (archive (string-append dir "/p.tar.gz"))
                      (ok? (begin
                             (write-file (string-append dir "/a.txt") "one")
                             (tar-create archive (list "a.txt") #:directory dir)
                             (parameterize ((quiet? #t))
                               (tar-list archive)))))
                 (remove-directory dir)
                 (assert-true ok?)))))

(define zip-suite
  (suite "Real zip archives"
         #:concurrent? #f
         #:shuffle? #f
         (test "a created zip lists its files"
               (let* ((dir (scratch-directory "zip-create"))
                      (archive (string-append dir "/p.zip"))
                      (names (begin
                               (write-file (string-append dir "/a.txt") "one")
                               (zip-create archive (list "a.txt") #:directory dir)
                               (zip-listing-of archive))))
                 (remove-directory dir)
                 (assert-contains #:expect names #:got "a.txt")))
         (test "a zip can be extracted"
               (let* ((dir (scratch-directory "zip-extract"))
                      (archive (string-append dir "/p.zip"))
                      (out (string-append dir "/out"))
                      (text (begin
                              (write-file (string-append dir "/a.txt") "hello")
                              (zip-create archive (list "a.txt") #:directory dir)
                              (mkdir-p out)
                              (zip-extract archive #:directory out)
                              (read-file (string-append out "/a.txt")))))
                 (remove-directory dir)
                 (assert-equal #:expect "hello" #:got text)))
         (test "an excluded file is not in the zip"
               (let* ((dir (scratch-directory "zip-exclude"))
                      (archive (string-append dir "/p.zip"))
                      (names (begin
                               (write-file (string-append dir "/a.txt") "one")
                               (write-file (string-append dir "/b.log") "two")
                               (zip-create archive (list "a.txt" "b.log")
                                           #:directory dir
                                           #:exclude (list "*.log"))
                               (zip-listing-of archive))))
                 (remove-directory dir)
                 (assert-false (string-contains names "b.log"))))
         (test "a directory tree is added recursively"
               (let* ((dir (scratch-directory "zip-recursive"))
                      (archive (string-append dir "/p.zip"))
                      (names (begin
                               (mkdir-p (string-append dir "/src"))
                               (write-file (string-append dir "/src/a.txt") "one")
                               (zip-create archive (list "src") #:directory dir)
                               (zip-listing-of archive))))
                 (remove-directory dir)
                 (assert-contains #:expect names #:got "src/a.txt")))
         (test "zip-list succeeds on a real archive"
               (let* ((dir (scratch-directory "zip-list"))
                      (archive (string-append dir "/p.zip"))
                      (ok? (begin
                             (write-file (string-append dir "/a.txt") "one")
                             (zip-create archive (list "a.txt") #:directory dir)
                             (parameterize ((quiet? #t))
                               (zip-list archive)))))
                 (remove-directory dir)
                 (assert-true ok?)))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               arguments-suite
               tar-suite
               zip-suite))
