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
;; Unit tests for `(maak guix)'.  The point of the module is that it is loosely
;; coupled: it loads and refuses well when there is no Guix, and asks Guix only
;; when a task calls one of its helpers.  So most of the cases here run in the
;; plain manifest environment, which has no Guix modules and may well have no
;; `guix' program; the two that need the program say so with #:skip?.
;;
;; The queries themselves are not tested here: they build store paths and would
;; need a channel cache and often the network, which a unit suite should not
;; depend on.  What is tested is the answer-guessing around them.

;;; Code:

(define-module (veritas unit guix)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak guix)
  #:export (spec))

(define (guix-scratch-directory)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (string-append (or (getenv "TMPDIR") "/tmp")
                            "/maak-test-guix-"
                            (number->string (getpid)))))
    (parameterize ((quiet? #t))
      (when (file-exists? dir)
        (delete-file-recursively dir))
      (mkdir-p dir))
    dir))

(define (scratch-channels-file dir)
  "Return a channels file that lists no channels, inside DIR.

A pinned lookup never reads a file, so an empty list is enough for the cases
that only check what happens before Guix is asked anything."
  (let ((path (string-append dir "/channels.scm")))
    (write-file path "(list)\n")
    path))

(define (clean-up dir)
  "Remove the scratch DIR without printing what was removed."
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

(define (fake-guix dir body)
  "Write an executable `guix' in DIR that runs BODY, and return its path.

A fake program is how the cases below reach the plumbing around a query -- the
file it writes and the file it leaves behind -- without asking a real Guix
anything, and without needing one installed."
  (let ((path (string-append dir "/guix")))
    (write-file path (string-append "#!/bin/sh\n" body "\n"))
    (parameterize ((quiet? #t))
      (chmod-file "755" path))
    path))

(define (with-path dir thunk)
  "Call THUNK with DIR first on `PATH', restoring the old one afterwards."
  (let ((saved (or (getenv "PATH") "")))
    (dynamic-wind
      (lambda () (setenv "PATH" (string-append dir ":" saved)))
      thunk
      (lambda () (setenv "PATH" saved)))))

(define (query-files)
  "Return the names of the query files of this process now in the temp directory."
  (let ((prefix (format #f "maak-guix-~a-" (getpid))))
    (or (scandir (tmp-dir)
                 (lambda (name) (string-prefix? prefix name)))
        '())))

(define (kept-file hint)
  "Return the file name HINT says a failed query was kept in, or #f.

The hint is written by the module itself, so the test reads the name out of it
rather than guessing what the file is called."
  (let* ((marker "is kept in ")
         (start (and hint (string-contains hint marker))))
    (and start
         (substring hint
                    (+ start (string-length marker))
                    (string-index hint #\; (+ start (string-length marker)))))))

(define (refusal-message thunk)
  "Return the message THUNK raised a maak error with, or #f when it did not.

An error that is not a maak error reports itself as #f, because a test that
asserts on the wording wants the reader-facing sentence, not a backtrace."
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . args)
      (let ((error (find-maak-error key args)))
        (and error (maak-error-message error))))))

(define guix-suite
  (suite "Guix values"
         #:concurrent? #f
         #:shuffle? #f
         (test "guix-available? answers with a boolean"
               (assert-true (and (memq (guix-available?) '(#t #f)) #t)))
         (test "a missing channels file is named in the error"
               (assert-true
                (and (string-contains (refusal-message
                                       (lambda () (guix-channels
                                                   #:file "no-such-channels.scm")))
                                      "no channels file")
                     #t)))
         (test "a package name cannot be empty"
               (let* ((dir (guix-scratch-directory))
                      (file (scratch-channels-file dir))
                      (message (refusal-message
                                (lambda () (guix-derivation "" #:channels file)))))
                 (clean-up dir)
                 (assert-true (and (string? message) #t))))
         (test "a pinned revision has to be a commit"
               (assert-true
                (and (string? (refusal-message
                               (lambda () (guix-derivation-at "" "hello"))))
                     #t)))
         (test "guix-program refuses arguments that are not a list"
               (assert-error (lambda () (guix-program "hello" "nope"))))
         (test "a query without Guix says how to get it"
               (let* ((dir (guix-scratch-directory))
                      (file (scratch-channels-file dir))
                      (saved (or (getenv "PATH") ""))
                      (message (dynamic-wind
                                 (lambda () (setenv "PATH" ""))
                                 (lambda ()
                                   (refusal-message
                                    (lambda () (guix-derivation "hello"
                                                                #:channels file))))
                                 (lambda () (setenv "PATH" saved)))))
                 (clean-up dir)
                 (assert-true (and (string? message)
                                   (string-contains message
                                                    "Guix is not available")
                                   #t))))
         (test "guix-channels reads the pins of a channels file"
               #:skip? (or (not (guix-available?))
                           (not (file-exists? "channels.scm")))
               (assert-equal #:expect 40
                             #:got (string-length
                                    (cdr (assoc "guix"
                                                (guix-channels
                                                 #:file "channels.scm"))))))
         (test "a query removes the file it ran"
               (let* ((dir (guix-scratch-directory))
                      (before (length (query-files)))
                      (answer #f))
                 (fake-guix dir "echo '(maak/ok \"/gnu/store/fake-hello\")'")
                 (with-path dir
                            (lambda ()
                              (set! answer
                                    (guix-build-at
                                     "0fb918d61609f3aadc2d8ecc212926b3bdfabb4c"
                                     "hello"))))
                 (let ((after (length (query-files))))
                   (clean-up dir)
                   ;; One assertion, because only the last form of a `let' body
                   ;; reaches the runner: two calls here would drop the first.
                   (assert-true (and (equal? answer "/gnu/store/fake-hello")
                                     (= before after))))))
         (test "a failed query keeps the file it ran, and names it"
               (let* ((dir (guix-scratch-directory))
                      (hint #f))
                 (fake-guix dir "exit 1")
                 (with-path dir
                            (lambda ()
                              (set! hint
                                    (catch #t
                                      (lambda ()
                                        (guix-build-at
                                         "0fb918d61609f3aadc2d8ecc212926b3bdfabb4c"
                                         "hello")
                                        #f)
                                      (lambda (key . args)
                                        (let ((error (find-maak-error key args)))
                                          (and error (maak-error-hint error))))))))
                 (let* ((path (kept-file hint))
                        (contents (and path (read-file path))))
                   (when (and path (file-exists? path))
                     (delete-file path))
                   (clean-up dir)
                   (assert-true (and (string? contents)
                                     (string-contains contents "0fb918d")
                                     #t)))))
         (test "a dry run prints the query instead of asking Guix"
               #:skip? (not (guix-available?))
               (let* ((dir (guix-scratch-directory))
                      (file (scratch-channels-file dir))
                      (printed (parameterize ((dry-run? #t) (quiet? #f))
                                 (with-output-to-string
                                   (lambda ()
                                     (guix-derivation "hello" #:channels file))))))
                 (clean-up dir)
                 (assert-true (and (string-contains printed "guix repl --") #t))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               guix-suite))
