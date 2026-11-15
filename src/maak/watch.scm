;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `maak --watch TASK' re-runs the task when the tree changes.  The repository's
;; own `dev' and `hot-reload' tasks were the hand-rolled version of this, and
;; needed watchexec; this is the same idea in maak itself, with no dependency
;; beyond a filesystem and a clock.
;;
;; Watching is a snapshot comparison: the tree is described as a sorted list of
;; file, modification time and size, and a change is a different list.  A git
;; checkout is asked for the files it tracks and for the untracked ones it does
;; not ignore (`git ls-files --cached --others --exclude-standard'), which is
;; the set a person means by "the tree"; a directory that is not a checkout is
;; walked, skipping the directories that are never a source file.  Neither
;; answer follows a symbolic link, so a loop in the tree cannot hang the watch.
;;
;; The snapshot is taken after each run rather than before the next poll, so a
;; task that writes into the tree -- a build directory, a log, a generated
;; file -- does not start itself again.  A change that lands while a run is in
;; progress is therefore part of that run's baseline; the trade is deliberate,
;; because the alternative is a task that rebuilds forever.

;;; Code:

(define-module (maak watch)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (watch-files watch-snapshot watch-changed? watch-tree watch-interval))

;; Directories whose contents are never a source file worth rebuilding for.
(define ignored-directories
  '(".git" ".hg" ".svn" ".jj" "node_modules" ".cache" ".direnv"
    ".venv" "__pycache__"))

(define (ignored-entry? name)
  (member name ignored-directories))

(define (regular-file? path)
  (catch #t
    (lambda () (eq? 'regular (stat:type (lstat path))))
    (lambda args #f)))

(define (directory? path)
  (catch #t
    (lambda () (eq? 'directory (stat:type (lstat path))))
    (lambda args #f)))

(define (walk-files root)
  "Return every regular file under ROOT, skipping the ignored directories."
  (let descend ((directory root))
    (append-map (lambda (name)
                  (let ((path (in-vicinity directory name)))
                    (cond
                      ((ignored-entry? name) '())
                      ((directory? path) (descend path))
                      ((regular-file? path) (list path))
                      (else '()))))
                (or (scandir directory
                             (lambda (name)
                               (not (member name '("." "..")))))
                    '()))))

(define (git-files root)
  "Return the files git reports for ROOT, or #f when git cannot answer.

The list is the tracked files plus the untracked ones git does not ignore,
which is what `.gitignore' already says the tree is.  A directory that is not a
checkout has no `.git', so it is not asked."
  (let* ((port (catch #t
                 (lambda ()
                   (open-pipe* OPEN_READ "git" "-C" root
                               "ls-files" "-z" "--cached" "--others"
                               "--exclude-standard"))
                 (lambda args #f)))
         (output (and port (get-string-all port)))
         (status (and port (close-pipe port))))
    (and status
         (eq? 0 (status:exit-val status))
         (map (lambda (relative) (in-vicinity root relative))
              (filter (lambda (relative) (not (string=? relative "")))
                      (string-split (or output "") #\nul))))))

(define (watch-files root)
  "Return the files a watch of ROOT follows, as absolute paths.

A git checkout is read with `git ls-files'; anything else is walked.  A git
command that fails -- a repository whose index is locked, say -- falls back to
the walk rather than watching nothing."
  (let ((root (canonicalize-path root)))
    (or (and (file-exists? (in-vicinity root ".git")) (git-files root))
        (walk-files root))))

(define (file-state path)
  "Return `(MTIME . SIZE)' of PATH, or #f when it is gone."
  (catch #t
    (lambda ()
      (let ((info (lstat path)))
        (and (eq? 'regular (stat:type info))
             (cons (stat:mtime info) (stat:size info)))))
    (lambda args #f)))

(define (watch-snapshot files)
  "Return FILES as a sorted list of `(PATH MTIME . SIZE)', for comparison.

A file that disappeared between the listing and the stat is left out: it is a
change, and the next snapshot will not mention it either."
  (sort (filter-map (lambda (path)
                      (let ((state (file-state path)))
                        (and state (cons path state))))
                    files)
        (lambda (a b) (string<? (car a) (car b)))))

(define (watch-changed? before after)
  "Return #t when the two snapshots describe a different tree."
  (not (equal? before after)))

(define (wait seconds)
  "Wait SECONDS, which may be fractional.

Guile's `sleep' only takes a whole number of seconds, so the fraction goes to
`usleep'; a watch interval of half a second is a reasonable thing to ask for
and should not be a type error."
  (let* ((whole (inexact->exact (floor seconds)))
         (fraction (- seconds (floor seconds))))
    (when (> whole 0)
      (sleep whole))
    (when (> fraction 0)
      (usleep (inexact->exact (round (* fraction 1000000)))))))

(define* (watch-tree root thunk #:key (interval 1))
  "Run THUNK now, then again whenever the tree under ROOT changes.

INTERVAL is how often the tree is looked at, in seconds.  The run happens
immediately, so `--watch' starts with the same run the same command would do
without it; the snapshot is taken after the run, so what the task itself wrote
is the baseline rather than a change.  THUNK is expected to report its own
failures: the watch has nowhere to return a status to, and a failing run must
not stop it."
  (let ((root (canonicalize-path root)))
    (log-info "maak: watching ~a every ~a s for changes; Ctrl-C stops.\n"
              root interval)
    (thunk)
    (force-output)
    (let loop ((baseline (watch-snapshot (watch-files root))))
      (wait interval)
      (let ((current (watch-snapshot (watch-files root))))
        (if (watch-changed? baseline current)
            (begin
              (log-info "\nmaak: change detected, running again\n")
              (thunk)
              ;; The run's own output is buffered like any other, and a watch
              ;; is exactly the program that would otherwise hold it until the
              ;; next change.
              (force-output)
              ;; What the run wrote is the new baseline, so a task that writes
              ;; into the tree does not start itself again.
              (loop (watch-snapshot (watch-files root))))
            (loop baseline))))))

(define (watch-interval)
  "Return the watch interval in seconds.

`MAAK_WATCH_INTERVAL' sets it; the default is one second.  A value that is not
a positive number is a maak error rather than a silent fallback, so a typo does
not turn into a watch that never notices anything."
  (let ((given (getenv "MAAK_WATCH_INTERVAL")))
    (if (not given)
        1
        (let ((seconds (string->number given)))
          (unless (and seconds (> seconds 0))
            (raise-maak-error
             (format #f "MAAK_WATCH_INTERVAL must be a positive number of seconds, got ~s"
                     given)
             #:hint "Set it to a number such as 1, or unset it for the default."))
          seconds))))
