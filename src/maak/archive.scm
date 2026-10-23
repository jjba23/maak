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
;; The archive helpers a project needs around a release: make a tarball or a
;; zip, unpack one, and see what is inside without unpacking it.  They are
;; thin, argument-list builders over the system tools rather than a second
;; implementation of either format, so a tarball made here is the same tarball
;; `tar' would have made by hand.
;;
;; Two families are exported, and each has a command builder next to the
;; procedure that runs it:
;;
;;   tar-create, tar-extract, tar-list
;;   zip-create, zip-extract, zip-list
;;
;; The `-argv' builders return the argument list without running anything.
;; They are what the procedures use, and they are exported because a task that
;; wants to log, inspect or extend a command should not have to reimplement
;; the flag order that the format requires.
;;
;; Nothing here changes maak's working directory permanently.  `#:directory'
;; names the tree the files are read from, and the archive path is still
;; resolved against the directory the task was started in.
;;
;; One trap is worth knowing about, because tar reports it badly.  Writing the
;; archive inside the tree it is packing means tar reads a file that is still
;; growing, and GNU tar aborts with "file changed as we read it" and exit
;; code 1.  A relative output name is therefore always written in the task
;; directory, and a task that packs "." should not hand tar an absolute output
;; path that points back into that tree.

;;; Code:

(define-module (maak archive)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (tar-create
            tar-extract
            tar-list
            tar-create-argv
            tar-extract-argv
            tar-list-argv
            tar-compression-option
            zip-create
            zip-extract
            zip-list
            zip-create-argv
            zip-extract-argv
            zip-list-argv))

;; ====== Tar ======

(define (tar-compression-option compression)
  "Return the tar flags that select COMPRESSION, as a list.

The names accepted are the ones a release actually uses: @code{none},
@code{gz}, @code{bz2}, @code{xz} and @code{zstd}.  @code{gzip}, @code{bzip2}
and @code{zst} are accepted as aliases, so a caller can spell the compressor
rather than its extension.  A name that is not one of these raises a maak error
instead of producing an archive tar would refuse to read."
  (let ((name (cond
                ((not compression) "none")
                ((symbol? compression) (symbol->string compression))
                ((string? compression) compression)
                (else
                 (raise-maak-error
                  (format #f "tar compression must be a string, got ~s"
                          compression)
                  #:hint "Use one of: none, gz, bz2, xz, zstd.")))))
    (let ((name (string-downcase name)))
      (cond
        ((member name '("none" "")) '())
        ((member name '("gz" "gzip")) (list "-z"))
        ((member name '("bz2" "bzip2")) (list "-j"))
        ((string=? name "xz") (list "-J"))
        ((member name '("zstd" "zst")) (list "--zstd"))
        (else
         (raise-maak-error
          (format #f "unknown tar compression: ~a" name)
          #:hint "Use one of: none, gz, bz2, xz, zstd."))))))

(define (tar-infer-compression output)
  "Return the compression OUTPUT's name asks for, or @code{none}.

Used when @code{#:compression} is left at @code{auto}: a file called
@code{project.tar.xz} is compressed with xz, and a plain @code{project.tar}
with nothing.  The suffix is only a default; naming the compression explicitly
always wins."
  (cond
    ((string-suffix? ".tar.gz" output) "gz")
    ((string-suffix? ".tgz" output) "gz")
    ((string-suffix? ".tar.bz2" output) "bz2")
    ((string-suffix? ".tbz2" output) "bz2")
    ((string-suffix? ".tar.xz" output) "xz")
    ((string-suffix? ".txz" output) "xz")
    ((string-suffix? ".tar.zst" output) "zstd")
    ((string-suffix? ".tzst" output) "zstd")
    (else "none")))

(define* (tar-create-argv output files
                          #:key (directory ".")
                          (compression "auto")
                          (exclude '())
                          (verbose? #f))
  "Return the argv that writes FILES as the tarball OUTPUT.

FILES is a list of paths relative to DIRECTORY.  With COMPRESSION
@code{auto}, the output's extension names the compressor (see
@code{tar-infer-compression}); any other value is passed to
@code{tar-compression-option}.  EXCLUDE is a list of patterns, each passed as
@code{--exclude=PATTERN}.  With VERBOSE? the argv includes @code{-v}, so tar
prints every file it adds."
  (when (or (not (string? output)) (string=? output ""))
    (raise-maak-error
     "tar-create expects the archive file to write"
     #:hint "Write (tar-create \"project-1.0.tar.gz\" (list \"src\" \"README.org\"))."))
  (when (or (not (list? files)) (null? files))
    (raise-maak-error
     "tar-create expects at least one file to put in the archive"
     #:hint "Name the paths to pack, as in (list \"src\" \"README.org\")."))
  (let* ((compression (if (and (string? compression)
                               (string=? compression "auto"))
                          (tar-infer-compression output)
                          compression))
         (flags (tar-compression-option compression)))
    (append (list "tar" "-c")
            flags
            (if verbose? (list "-v") '())
            (list "-f" output)
            (if (and directory (not (string=? directory ".")))
                (list "-C" directory)
                '())
            (map (lambda (pattern)
                   (string-append "--exclude=" pattern))
                 exclude)
            files)))

(define* (tar-create output files
                     #:key (directory ".") (compression "auto")
                     (exclude '()) (verbose? #f) (allow-failure? #f))
  "Write FILES as the tarball OUTPUT.

FILES is a list of paths relative to DIRECTORY, which defaults to the current
directory.  The compressor follows the output's extension unless COMPRESSION
names one, and EXCLUDE is a list of patterns to leave out.

OUTPUT is written in the directory the task started in, not in DIRECTORY, so
packing the current directory does not put the growing tarball into the tree
tar is reading.  Passing an absolute OUTPUT that points inside the packed tree
does, and tar then fails with \"file changed as we read it\".

@example
(tar-create \"maak-1.0.tar.gz\" (list \"src\" \"README.org\" \"COPYING\")
            #:exclude (list \"*.o\" \"*.go\"))
(tar-create \"maak-1.0.tar.xz\" (list \".\") #:compression \"xz\")
@end example"
  (run (tar-create-argv output files
                        #:directory directory
                        #:compression compression
                        #:exclude exclude
                        #:verbose? verbose?)
       #:allow-failure? allow-failure?))

(define* (tar-extract-argv archive
                           #:key (directory #f) (strip-components 0)
                           (verbose? #f))
  "Return the argv that unpacks ARCHIVE.

The compressor is detected from the archive itself, which is why no
compression flag is needed here.  DIRECTORY is where the files are written,
and STRIP-COMPONENTS drops that many leading path components, which is how a
release tarball with a single top-level directory is unpacked into the current
one.
@example
(tar-extract-argv \"project-1.0.tar.gz\" #:directory \"build\" #:strip-components 1)
@end example"
  (when (or (not (string? archive)) (string=? archive ""))
    (raise-maak-error
     "tar-extract expects the archive to unpack"
     #:hint "Write (tar-extract \"project-1.0.tar.gz\")."))
  (when (or (not (integer? strip-components)) (< strip-components 0))
    (raise-maak-error
     (format #f "tar-extract expects #:strip-components to be a non-negative integer, got ~s"
             strip-components)
     #:hint "Use 0 to keep the archive's paths, or 1 to drop its top-level directory."))
  (append (list "tar" "-x")
          (if verbose? (list "-v") '())
          (list "-f" archive)
          (if (and directory (not (string=? directory "")))
              (list "-C" directory)
              '())
          (if (> strip-components 0)
              (list (format #f "--strip-components=~a" strip-components))
              '())))

(define* (tar-extract archive
                      #:key (directory #f) (strip-components 0)
                      (verbose? #f) (allow-failure? #f))
  "Unpack ARCHIVE, into DIRECTORY when one is named.

STRIP-COMPONENTS drops leading path components as the files are written; use 1
for the usual release tarball that wraps everything in one directory.

@example
(tar-extract \"maak-1.0.tar.gz\" #:directory \"build\" #:strip-components 1)
@end example"
  (run (tar-extract-argv archive
                         #:directory directory
                         #:strip-components strip-components
                         #:verbose? verbose?)
       #:allow-failure? allow-failure?))

(define* (tar-list-argv archive #:key (verbose? #f))
  "Return the argv that prints the names of the files in ARCHIVE."
  (when (or (not (string? archive)) (string=? archive ""))
    (raise-maak-error
     "tar-list expects the archive to read"
     #:hint "Write (tar-list \"project-1.0.tar.gz\")."))
  (append (list "tar" "-t")
          (if verbose? (list "-v") '())
          (list "-f" archive)))

(define* (tar-list archive #:key (verbose? #f) (allow-failure? #f))
  "Print the names of the files in ARCHIVE, without unpacking it.

With VERBOSE? tar prints the long listing instead of bare names."
  (run (tar-list-argv archive #:verbose? verbose?)
       #:allow-failure? allow-failure?))

;; ====== Zip ======

(define* (zip-create-argv output files
                          #:key (recursive? #t) (exclude '())
                          (junk-paths? #f) (quiet? #f) (verbose? #f))
  "Return the argv that writes FILES as the zip archive OUTPUT.

The archive is created in the current directory; @code{zip-create} is the one
that changes into @code{#:directory} for the duration of the command, because
Info-ZIP has no @code{-C}-style option to name a tree.  EXCLUDE is a list of
patterns passed as @code{-x}, after the files, which is where zip expects them.
@code{#:junk-paths?} stores bare file names rather than the paths they were
found under."
  (when (or (not (string? output)) (string=? output ""))
    (raise-maak-error
     "zip-create expects the archive file to write"
     #:hint "Write (zip-create \"project-1.0.zip\" (list \"src\" \"README.org\"))."))
  (when (or (not (list? files)) (null? files))
    (raise-maak-error
     "zip-create expects at least one file to put in the archive"
     #:hint "Name the paths to pack, as in (list \"src\" \"README.org\")."))
  (append (list "zip")
          (if quiet? (list "-q") '())
          (if verbose? (list "-v") '())
          (if recursive? (list "-r") '())
          (if junk-paths? (list "-j") '())
          (list output)
          files
          (if (null? exclude)
              '()
              (append (list "-x") exclude))))

(define* (zip-create output files
                     #:key (directory ".") (recursive? #t) (exclude '())
                     (junk-paths? #f) (quiet? #f) (verbose? #f)
                     (allow-failure? #f))
  "Write FILES as the zip archive OUTPUT.

FILES is a list of paths relative to DIRECTORY, which defaults to the current
directory.  The archive itself is written relative to the directory the task
started in, so a `#:directory \"dist\"' does not leave the zip inside dist.

@example
(zip-create \"maak-1.0.zip\" (list \"src\" \"README.org\")
            #:exclude (list \"*.go\" \"*.o\"))
@end example"
  (let* ((change-directory? (and directory
                                  (not (string=? directory "."))))
         ;; The archive is written where the task started, not inside the tree
         ;; it is reading, so a relative name is made absolute before the
         ;; working directory changes.  Without a change it stays as written,
         ;; which is what a dry run should show.
         (output-path (if (and change-directory?
                             (not (string-prefix? "/" output)))
                          (string-append (getcwd) "/" output)
                          output))
         (call (lambda ()
                 (run (zip-create-argv output-path
                                       files
                                       #:recursive? recursive?
                                       #:exclude exclude
                                       #:junk-paths? junk-paths?
                                       #:quiet? quiet?
                                       #:verbose? verbose?)
                      #:allow-failure? allow-failure?))))
    (if change-directory?
        (with-directory directory call)
        (call))))

(define* (zip-extract-argv archive
                           #:key (directory #f) (overwrite? #t)
                           (quiet? #f))
  "Return the argv that unpacks ARCHIVE.

OVERWRITE? is @code{unzip}'s @code{-o}: files already there are replaced
rather than asked about, which is what an unattended task needs.  DIRECTORY is
where the files are written."
  (when (or (not (string? archive)) (string=? archive ""))
    (raise-maak-error
     "zip-extract expects the archive to unpack"
     #:hint "Write (zip-extract \"project-1.0.zip\")."))
  (append (list "unzip")
          (if quiet? (list "-q") '())
          (if overwrite? (list "-o") (list "-n"))
          (list archive)
          (if (and directory (not (string=? directory "")))
              (list "-d" directory)
              '())))

(define* (zip-extract archive
                      #:key (directory #f) (overwrite? #t)
                      (quiet? #f) (allow-failure? #f))
  "Unpack ARCHIVE, into DIRECTORY when one is named.

With OVERWRITE? (the default) existing files are replaced without a prompt; with
it #f they are kept and unzip skips them."
  (run (zip-extract-argv archive
                         #:directory directory
                         #:overwrite? overwrite?
                         #:quiet? quiet?)
       #:allow-failure? allow-failure?))

(define* (zip-list-argv archive #:key (quiet? #f))
  "Return the argv that lists the contents of ARCHIVE."
  (when (or (not (string? archive)) (string=? archive ""))
    (raise-maak-error
     "zip-list expects the archive to read"
     #:hint "Write (zip-list \"project-1.0.zip\")."))
  (append (list "unzip")
          (if quiet? (list "-q") '())
          (list "-l" archive)))

(define* (zip-list archive #:key (quiet? #f) (allow-failure? #f))
  "Print a listing of the files in ARCHIVE, without unpacking it."
  (run (zip-list-argv archive #:quiet? quiet?)
       #:allow-failure? allow-failure?))
