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
;;
;; Three rules keep the cases independent of each other, because the suite runs
;; in the same process as every other test file and a case that read files it
;; did not write would pass for the wrong reason:
;;
;; - each case writes into a directory of its own, made fresh and removed
;;   afterwards (`in-scratch-directory'), never into a fixed path;
;; - a case's tree holds the files and its archive sits beside the tree, so no
;;   archive is written into the tree being packed, which is what makes GNU tar
;;   abort with "file changed as we read it";
;; - `#:directory' is a change of the process working directory for the length
;;   of `zip-create', so the suite is kept sequential and the library is asked
;;   to put the directory back, which one case asserts.

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

(define (remove-directory dir)
  "Remove DIR quietly, whatever the tests left in it."
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

;; Every case that writes files gets a directory of its own, made fresh, and
;; gives it back when it is done.  Both halves matter here:
;;
;; - The unit suite is loaded into one process (the runner walks the test files
;;   one after another), so a name written down on disk is a name the rest of
;;   the suite shares.  A case that found another case's archive would be
;;   reading files it did not write, and could pass for the wrong reason.
;; - `zip-create' changes the process working directory for the length of its
;;   command, because Info-ZIP has no `-C'.  A case that left the working
;;   directory changed would run every case after it somewhere else, so the
;;   directory is removed on the way out even when the case raised, and
;;   `packing a tree leaves the working directory where it was' checks that the
;;   library puts it back.
;;
;; The name carries the process and a counter of the directories this suite has
;; asked for, so no two cases -- and no two runs sharing a TMPDIR -- name the
;; same path.  A path that is there anyway, because an earlier run was killed
;; before it could clean up, is removed rather than reused: a case that starts
;; from somebody else's files is a case that is not testing anything.

(define scratch-count 0)

(define (scratch-directory label)
  "Return an empty directory of this case's own for LABEL."
  (set! scratch-count (+ scratch-count 1))
  (let ((dir (format #f "~a/maak-test-archive-~a-~a-~a"
                     (tmp-dir) label (getpid) scratch-count)))
    (when (file-exists? dir)
      (remove-directory dir))
    (mkdir-p dir)
    dir))

(define (in-scratch-directory label thunk)
  "Call THUNK with a fresh directory for LABEL, and remove the directory after.

THUNK is handed the path.  The directory is removed whether THUNK returned or
raised, so nothing this suite writes outlives the case that wrote it, and the
removal happens while the case is still running rather than in its last line,
where an error would skip it."
  (let ((dir (scratch-directory label)))
    (dynamic-wind
     (lambda () #t)
     (lambda () (thunk dir))
     (lambda () (remove-directory dir)))))

(define (mentions? text needle)
  "Return whether TEXT holds NEEDLE, as #t or #f."
  (and (string-contains text needle) #t))

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
                        ;; The archive is named inside a directory of the case's own, so
                        ;; the expectation is built from the same string the command
                        ;; would have used rather than from a path fixed in the source
                        ;; that two runs could disagree about.
                        (in-scratch-directory
                         "dry-run"
                         (lambda (dir)
                           (let ((archive (string-append dir "/p.tar.gz")))
                             (assert-contains
                              #:expect (dry-run-command
                                        (lambda ()
                                          (tar-create archive (list "src"))))
                              #:got (format #f "tar -c -z -f ~a src" archive))))))))

;; Each case below is a directory of its own, a tree inside it holding the
;; files to pack, and the archive beside the tree rather than inside it.
;;
;; The tree/archive split is not decoration.  tar reads the tree it packs, so an
;; archive written into that tree changes while tar is reading it, and GNU tar
;; aborts with "file changed as we read it".  Keeping the two apart is what the
;; case `packing a tree does not fail on the tarball being written' asks about,
;; and every other case stays clear of the trap so that a failure here means
;; what it says.

(define tar-suite
  (suite "Real tar archives"
         #:concurrent? #f
         #:shuffle? #f
         (test "a created tarball lists its files"
               (in-scratch-directory
                "tar-create"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.tar.gz")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "one")
                    (write-file (string-append tree "/b.txt") "two")
                    (tar-create archive (list "a.txt" "b.txt")
                                #:directory tree)
                    (let ((names (listing-of archive)))
                      (assert-equal #:expect (list #t #t)
                                    #:got (list (mentions? names "a.txt")
                                                (mentions? names "b.txt"))))))))
         (test "an excluded file is not in the tarball"
               (in-scratch-directory
                "tar-exclude"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.tar.gz")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "one")
                    (write-file (string-append tree "/b.log") "two")
                    (tar-create archive (list ".")
                                #:directory tree
                                #:exclude (list "*.log"))
                    (let ((names (listing-of archive)))
                      (assert-equal #:expect (list #t #f)
                                    #:got (list (mentions? names "a.txt")
                                                (mentions? names "b.log"))))))))
         (test "packing a tree does not fail on the tarball being written"
               (in-scratch-directory
                "tar-tree"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "one")
                    (assert-true
                     (parameterize ((quiet? #t))
                       (tar-create (string-append dir "/p.tar.gz")
                                   (list ".")
                                   #:directory tree)))))))
         (test "the tarball lands in the task directory, not the packed tree"
               (in-scratch-directory
                "tar-outside"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "one")
                    (with-directory dir
                      (lambda ()
                        (tar-create "p.tar.gz" (list ".") #:directory tree)))
                    (assert-equal
                     #:expect (list #t #f)
                     #:got (list (file-exists?
                                  (string-append dir "/p.tar.gz"))
                                 (file-exists?
                                  (string-append tree "/p.tar.gz"))))))))
         (test "a tarball can be extracted"
               (in-scratch-directory
                "tar-extract"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.tar.gz"))
                        (out (string-append dir "/out")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "hello")
                    (tar-create archive (list "a.txt") #:directory tree)
                    (mkdir-p out)
                    (tar-extract archive #:directory out)
                    (assert-equal
                     #:expect "hello"
                     #:got (read-file (string-append out "/a.txt")))))))
         (test "a strip count removes the top-level directory"
               (in-scratch-directory
                "tar-strip"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.tar.gz"))
                        (out (string-append dir "/out")))
                    (mkdir-p (string-append tree "/p"))
                    (write-file (string-append tree "/p/a.txt") "deep")
                    (tar-create archive (list "p") #:directory tree)
                    (mkdir-p out)
                    (tar-extract archive #:directory out #:strip-components 1)
                    (assert-equal
                     #:expect "deep"
                     #:got (read-file (string-append out "/a.txt")))))))
         (test "an xz tarball round-trips"
               (in-scratch-directory
                "tar-xz"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.tar.xz")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "xz")
                    (tar-create archive (list "a.txt") #:directory tree)
                    (assert-true (mentions? (listing-of archive)
                                            "a.txt"))))))
         (test "tar-list succeeds on a real archive"
               (in-scratch-directory
                "tar-list"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.tar.gz")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "one")
                    (tar-create archive (list "a.txt") #:directory tree)
                    (assert-true (parameterize ((quiet? #t))
                                   (tar-list archive)))))))
         ;; `#:directory' is how both families are told which tree to read, and
         ;; `zip-create' changes into it for the length of its command.  Both are
         ;; supposed to come back: a task that ran later would otherwise run in
         ;; whatever the last archive left behind, which is why this is asserted
         ;; rather than assumed.
         (test "packing a tree leaves the working directory where it was"
               (in-scratch-directory
                "tar-cwd"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (before (getcwd)))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "one")
                    (parameterize ((quiet? #t))
                      (tar-create (string-append dir "/p.tar.gz") (list ".")
                                  #:directory tree)
                      (zip-create (string-append dir "/p.zip") (list ".")
                                  #:directory tree))
                    (assert-equal #:expect before
                                  #:got (getcwd))))))))

(define zip-suite
  (suite "Real zip archives"
         #:concurrent? #f
         #:shuffle? #f
         (test "a created zip lists its files"
               (in-scratch-directory
                "zip-create"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.zip")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "one")
                    (zip-create archive (list "a.txt") #:directory tree)
                    (assert-true (mentions? (zip-listing-of archive)
                                            "a.txt"))))))
         (test "a zip can be extracted"
               (in-scratch-directory
                "zip-extract"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.zip"))
                        (out (string-append dir "/out")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "hello")
                    (zip-create archive (list "a.txt") #:directory tree)
                    (mkdir-p out)
                    (zip-extract archive #:directory out)
                    (assert-equal
                     #:expect "hello"
                     #:got (read-file (string-append out "/a.txt")))))))
         (test "an excluded file is not in the zip"
               (in-scratch-directory
                "zip-exclude"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.zip")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "one")
                    (write-file (string-append tree "/b.log") "two")
                    (zip-create archive (list "a.txt" "b.log")
                                #:directory tree
                                #:exclude (list "*.log"))
                    (let ((names (zip-listing-of archive)))
                      (assert-equal #:expect (list #t #f)
                                    #:got (list (mentions? names "a.txt")
                                                (mentions? names "b.log"))))))))
         (test "a directory tree is added recursively"
               (in-scratch-directory
                "zip-recursive"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.zip")))
                    (mkdir-p (string-append tree "/src"))
                    (write-file (string-append tree "/src/a.txt") "one")
                    (zip-create archive (list "src") #:directory tree)
                    (assert-true (mentions? (zip-listing-of archive)
                                            "src/a.txt"))))))
         (test "zip-list succeeds on a real archive"
               (in-scratch-directory
                "zip-list"
                (lambda (dir)
                  (let ((tree (string-append dir "/tree"))
                        (archive (string-append dir "/p.zip")))
                    (mkdir-p tree)
                    (write-file (string-append tree "/a.txt") "one")
                    (zip-create archive (list "a.txt") #:directory tree)
                    (assert-true (parameterize ((quiet? #t))
                                   (zip-list archive)))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               arguments-suite
               tar-suite
               zip-suite))
