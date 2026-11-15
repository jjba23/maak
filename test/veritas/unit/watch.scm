;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `--watch`: how a tree is described, what counts as a change, and
;; which files a watch follows.  The loop itself is exercised by hand and by
;; the CLI; what a unit test can pin down is the comparison it refuses to get
;; wrong -- a task that writes into the tree must not start itself again, which
;; is why the snapshot is the thing the loop takes after a run.
;;
;; A case that needs files makes a directory of its own and removes it in a
;; `dynamic-wind', so the assertion is still the value the case evaluates to
;; (see docs/development.org).

;;; Code:

(define-module (veritas unit watch)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak watch)
  #:export (spec))

(define sandbox-counter 0)

(define (fresh-dir)
  "Return a directory of this run's own, created, for one case."
  (set! sandbox-counter (+ sandbox-counter 1))
  (let ((dir (format #f "~a/maak-watch-~a-~a"
                     (tmp-dir) (getpid) sandbox-counter)))
    (mkdir dir)
    dir))

(define (with-sandbox thunk)
  "Call THUNK with a fresh directory, removing it afterwards."
  (let ((dir (fresh-dir)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk dir))
      (lambda ()
        (when (file-exists? dir)
          (delete-file-recursively dir))))))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (names paths)
  "Return PATHS without their directory, as strings, sorted."
  (sort (map basename paths) string<?))

(define snapshot-suite
  (suite "A tree as a snapshot"
         #:concurrent? #f
         #:shuffle? #f
         (test "a snapshot names every file under the tree"
               (with-sandbox
                (lambda (dir)
                  (write-text (in-vicinity dir "one.scm") "one")
                  (assert-equal #:expect '("one.scm")
                                #:got (names (watch-files dir))))))
         (test "a changed file changes the snapshot"
               (with-sandbox
                (lambda (dir)
                  (let ((path (in-vicinity dir "one.scm")))
                    (write-text path "one")
                    (let ((before (watch-snapshot (watch-files dir))))
                      (write-text path "much longer")
                      (assert-true (watch-changed?
                                    before
                                    (watch-snapshot (watch-files dir)))))))))
         (test "a new file changes the snapshot"
               (with-sandbox
                (lambda (dir)
                  (let ((before (watch-snapshot (watch-files dir))))
                    (write-text (in-vicinity dir "new.scm") "new")
                    (assert-true (watch-changed?
                                  before
                                  (watch-snapshot (watch-files dir))))))))
         (test "a removed file changes the snapshot"
               (with-sandbox
                (lambda (dir)
                  (let ((path (in-vicinity dir "gone.scm")))
                    (write-text path "gone")
                    (let ((before (watch-snapshot (watch-files dir))))
                      (delete-file path)
                      (assert-true (watch-changed?
                                    before
                                    (watch-snapshot (watch-files dir)))))))))
         (test "the same tree is the same snapshot"
               (with-sandbox
                (lambda (dir)
                  (write-text (in-vicinity dir "one.scm") "one")
                  (assert-false (watch-changed?
                                 (watch-snapshot (watch-files dir))
                                 (watch-snapshot (watch-files dir)))))))
         (test "a watch skips a repository's internals"
               (with-sandbox
                (lambda (dir)
                  (mkdir (in-vicinity dir ".git"))
                  (write-text (in-vicinity (in-vicinity dir ".git") "config") "x")
                  (write-text (in-vicinity dir "source.scm") "x")
                  (assert-equal #:expect '("source.scm")
                                #:got (names (watch-files dir))))))
         (test "a watch of this checkout follows its Maak file"
               (assert-true (and (member (string-append (getcwd) "/maak.scm")
                                        (watch-files "."))
                                 #t)))))

(define (caught thunk)
  "Return `\"raised\"' when THUNK raises, or `\"returned\"' when it does not.

The answer is computed eagerly, so it can be an assertion's argument: an
assertion object is checked later, by which time a `dynamic-wind' around it has
already restored the environment."
  (catch #t
    (lambda () (thunk) "returned")
    (lambda (key . vals) "raised")))

(define interval-suite
  (suite "The watch interval"
         #:concurrent? #f
         #:shuffle? #f
         (test "the interval has a default"
               (dynamic-wind
                 (lambda () (env-unset! "MAAK_WATCH_INTERVAL"))
                 (lambda () (assert-equal #:expect 1 #:got (watch-interval)))
                 (lambda () (env-unset! "MAAK_WATCH_INTERVAL"))))
         (test "the interval reads MAAK_WATCH_INTERVAL"
               (dynamic-wind
                 (lambda () (env-set! "MAAK_WATCH_INTERVAL" "2.5"))
                 (lambda () (assert-equal #:expect 2.5 #:got (watch-interval)))
                 (lambda () (env-unset! "MAAK_WATCH_INTERVAL"))))
         (test "a value that is not a number is refused"
               (dynamic-wind
                 (lambda () (env-set! "MAAK_WATCH_INTERVAL" "soon"))
                 (lambda () (assert-equal #:expect "raised"
                                          #:got (caught watch-interval)))
                 (lambda () (env-unset! "MAAK_WATCH_INTERVAL"))))
         (test "a value that is not positive is refused"
               (dynamic-wind
                 (lambda () (env-set! "MAAK_WATCH_INTERVAL" "0"))
                 (lambda () (assert-equal #:expect "raised"
                                          #:got (caught watch-interval)))
                 (lambda () (env-unset! "MAAK_WATCH_INTERVAL"))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               snapshot-suite
               interval-suite))
