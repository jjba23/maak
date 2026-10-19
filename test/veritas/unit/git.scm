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
;; Unit tests for `(maak git)'.  The command-building half is checked through
;; dry-run mode, which is the same output `maak --dry-run` shows a user.  The
;; repository half runs real git against throw-away repositories under the
;; temporary directory, so the wrappers are tested as a task uses them.
;;
;; The suites run sequentially because several tests change the process working
;; directory while a repository is built.

;;; Code:

(define-module (veritas unit git)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak git)
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
  (parameterize ((dry-run? #t)
                 (quiet? #f)
                 (elevation-command "sudo"))
    (strip-ansi (with-output-to-string thunk))))

(define (scratch-directory label)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (format #f "~a/maak-test-git-~a-~a"
                     (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (remove-directory dir)
  "Remove DIR quietly, whatever the tests left in it."
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

(define (in dir thunk)
  "Call THUNK with the working directory set to DIR."
  (with-directory dir thunk))

(define (init-repo! dir)
  "Make DIR a git repository whose commits have an identity, and return it."
  (git-init "-q" "-b" "trunk" dir)
  (git (list "config" "user.name" "Maak Test") #:directory dir)
  (git (list "config" "user.email" "maak@example.org") #:directory dir)
  ;; A user's global configuration may sign commits and tags by default; a
  ;; throw-away repository has no key to sign them with.
  (git (list "config" "commit.gpgsign" "false") #:directory dir)
  (git (list "config" "tag.gpgsign" "false") #:directory dir)
  dir)

(define (add-and-commit! dir message)
  "Stage everything in DIR and commit it with MESSAGE."
  (in dir (lambda ()
            (git-add)
            (git-commit message))))

(define argv-suite
  (suite "Git argument lists"
         #:concurrent? #f
         #:shuffle? #f
         (test "git-argv names the checkout with -C"
               (assert-equal #:expect '("git" "-C" "/tmp/repo" "status")
                             #:got (git-argv (list "status")
                                             #:directory "/tmp/repo")))
         (test "git-argv defaults to the current directory"
               (assert-equal #:expect '("git" "-C" "." "status")
                             #:got (git-argv (list "status"))))
         (test "a clone puts the destination last"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (git-clone "https://example.org/p.git"
                                       #:branch "main"
                                       #:depth 1
                                       #:directory "vendor/p")))
                #:got "git clone --branch main --depth 1 https://example.org/p.git vendor/p"))
         (test "a bare clone asks for --bare"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (git-clone "../other" #:bare? #t)))
                #:got "git clone --bare ../other"))
         (test "switching can create the branch"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (git-switch "feature" #:create? #t)))
                #:got "git -C . switch -c feature"))
         (test "a commit keeps its message as one argument"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (git-commit "fix: it's done")))
                #:got "git -C . commit --all -m"))
         (test "a commit can amend an empty tree"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (git-commit "again" #:amend? #t #:allow-empty? #t)))
                #:got "commit --all --amend --allow-empty -m"))
         (test "an annotated tag passes its message"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (git-tag "v1.0" #:message "release one")))
                #:got "git -C . tag --annotate -m"))
         (test "log has its usual default shape"
               (assert-contains
                #:expect (dry-run-command (lambda () (git-log)))
                #:got "git -C . log --oneline --graph --decorate -n 20"))
         (test "log passes explicit arguments through"
               (assert-contains
                #:expect (dry-run-command
                          (lambda () (git-log "-5" "--all")))
                #:got "git -C . log --oneline --graph --decorate -5 --all"))
         (test "the subcommand wrappers reach git"
               (assert-equal
                #:expect '("git" "-C" "." "rebase" "--interactive" "HEAD~2")
                #:got (git-argv (list "rebase" "--interactive" "HEAD~2"))))
         (test "a remote is added with its own subcommand"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (git-remote "add" "origin" "url")))
                #:got "git -C . remote add origin url"))
         (test "an archive names format, prefix and output"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (git-archive-zip "/tmp/p.zip"
                                             #:prefix "p/"
                                             #:revision "v1.0")))
                #:got "git -C . archive --format=zip --prefix=p/ -o /tmp/p.zip v1.0"))
         (test "an empty commit message is refused"
               (assert-error (lambda () (git-commit ""))))
         (test "an empty archive name is refused"
               (assert-error (lambda () (git-archive ""))))
         (test "an empty branch name is refused"
               (assert-error (lambda () (git-switch ""))))
         (test "an empty clone URL is refused"
               (assert-error (lambda () (git-clone ""))))))

(define repository-suite
  (suite "Git repositories"
         #:concurrent? #f
         #:shuffle? #f
         (test "git-init creates a repository"
               (let* ((dir (scratch-directory "init"))
                      (created? (begin
                                  (init-repo! dir)
                                  (file-exists? (string-append dir "/.git")))))
                 (remove-directory dir)
                 (assert-true created?)))
         (test "git-add stages an untracked file"
               (let* ((dir (scratch-directory "add"))
                      (status (begin
                                (init-repo! dir)
                                (write-file (string-append dir "/a.txt") "one")
                                (in dir (lambda () (git-add "a.txt")))
                                (git-output (list "status" "--porcelain")
                                            #:directory dir))))
                 (remove-directory dir)
                 (assert-equal #:expect "A  a.txt" #:got (string-trim-both status))))
         (test "git-commit records the tree, and the branch reads back"
               (let* ((dir (scratch-directory "commit"))
                      (branch (begin
                                (init-repo! dir)
                                (write-file (string-append dir "/a.txt") "one")
                                (add-and-commit! dir "first")
                                (git-current-branch #:directory dir))))
                 (remove-directory dir)
                 (assert-equal #:expect "trunk" #:got branch)))
         (test "git-current-revision is a full hash"
               (let* ((dir (scratch-directory "revision"))
                      (revision (begin
                                  (init-repo! dir)
                                  (write-file (string-append dir "/a.txt") "one")
                                  (add-and-commit! dir "first")
                                  (git-current-revision #:directory dir))))
                 (remove-directory dir)
                 (assert-equal #:expect 40 #:got (string-length revision))))
         (test "git-dirty? notices a modified file"
               (let* ((dir (scratch-directory "dirty"))
                      (dirty? (begin
                                (init-repo! dir)
                                (write-file (string-append dir "/a.txt") "one")
                                (add-and-commit! dir "first")
                                (write-file (string-append dir "/a.txt") "two")
                                (git-dirty? #:directory dir))))
                 (remove-directory dir)
                 (assert-true dirty?)))
         (test "git-dirty? is false after a commit"
               (let* ((dir (scratch-directory "clean"))
                      (dirty? (begin
                                (init-repo! dir)
                                (write-file (string-append dir "/a.txt") "one")
                                (add-and-commit! dir "first")
                                (git-dirty? #:directory dir))))
                 (remove-directory dir)
                 (assert-false dirty?)))
         (test "git-tag and git-tags agree"
               (let* ((dir (scratch-directory "tag"))
                      (tags (begin
                              (init-repo! dir)
                              (write-file (string-append dir "/a.txt") "one")
                              (add-and-commit! dir "first")
                              (in dir (lambda ()
                                        (git-tag "v1" #:message "one")))
                              (git-tags #:directory dir))))
                 (remove-directory dir)
                 (assert-equal #:expect '("v1") #:got tags)))
         (test "git-describe-tag finds the release tag"
               (let* ((dir (scratch-directory "describe"))
                      (tag (begin
                             (init-repo! dir)
                             (write-file (string-append dir "/a.txt") "one")
                             (add-and-commit! dir "first")
                             (in dir (lambda () (git-tag "v2")))
                             (git-describe-tag #:directory dir))))
                 (remove-directory dir)
                 (assert-equal #:expect "v2" #:got tag)))
         (test "a branch can be created and switched to"
               (let* ((dir (scratch-directory "switch"))
                      (branch (begin
                                (init-repo! dir)
                                (write-file (string-append dir "/a.txt") "one")
                                (add-and-commit! dir "first")
                                (in dir (lambda ()
                                          (git-branch "feature")
                                          (git-switch "feature")))
                                (git-current-branch #:directory dir))))
                 (remove-directory dir)
                 (assert-equal #:expect "feature" #:got branch)))
         (test "a detached HEAD answers with its revision"
               (let* ((dir (scratch-directory "detached"))
                      (branch (begin
                                (init-repo! dir)
                                (write-file (string-append dir "/a.txt") "one")
                                (add-and-commit! dir "first")
                                (in dir (lambda () (git-checkout "--detach")))
                                (git-current-branch #:directory dir))))
                 (remove-directory dir)
                 (assert-equal #:expect 40 #:got (string-length branch))))
         (test "a remote can be added and read back"
               (let* ((dir (scratch-directory "remote"))
                      (url (begin
                             (init-repo! dir)
                             (in dir (lambda ()
                                       (git-remote "add" "origin"
                                                   "https://example.org/p.git")))
                             (git-remote-url "origin" #:directory dir))))
                 (remove-directory dir)
                 (assert-equal #:expect "https://example.org/p.git" #:got url)))
         (test "git-remotes lists what was added"
               (let* ((dir (scratch-directory "remotes"))
                      (names (begin
                               (init-repo! dir)
                               (in dir (lambda ()
                                         (git-remote "add" "origin" "url")))
                               (git-remotes #:directory dir))))
                 (remove-directory dir)
                 (assert-equal #:expect '("origin") #:got names)))
         (test "git-diff reports a modified file when failures are allowed"
               (let* ((dir (scratch-directory "diff"))
                      (changed? (begin
                                  (init-repo! dir)
                                  (write-file (string-append dir "/a.txt") "one")
                                  (add-and-commit! dir "first")
                                  (write-file (string-append dir "/a.txt") "two")
                                  (git (list "diff" "--quiet")
                                       #:directory dir
                                       #:allow-failure? #t))))
                 (remove-directory dir)
                 (assert-equal #:expect #f #:got changed?)))
         (test "a stash can be made and popped"
               (let* ((dir (scratch-directory "stash"))
                      (dirty? (begin
                                (init-repo! dir)
                                (write-file (string-append dir "/a.txt") "one")
                                (add-and-commit! dir "first")
                                (write-file (string-append dir "/a.txt") "two")
                                (in dir (lambda ()
                                          (git-stash)
                                          (git-stash "pop")))
                                (git-dirty? #:directory dir))))
                 (remove-directory dir)
                 (assert-true dirty?)))
         (test "git-clean removes an untracked file"
               (let* ((dir (scratch-directory "clean-file"))
                      (path (string-append dir "/junk.txt"))
                      (gone? (begin
                               (init-repo! dir)
                               (write-file (string-append dir "/a.txt") "one")
                               (add-and-commit! dir "first")
                               (write-file path "junk")
                               (in dir (lambda () (git-clean "-f")))
                               (not (file-exists? path)))))
                 (remove-directory dir)
                 (assert-true gone?)))
         (test "git-output reads a value rather than printing it"
               (let* ((dir (scratch-directory "output"))
                      (branch (begin
                                (init-repo! dir)
                                (write-file (string-append dir "/a.txt") "one")
                                (add-and-commit! dir "first")
                                (string-trim-both
                                 (git-output (list "rev-parse" "--abbrev-ref" "HEAD")
                                             #:directory dir)))))
                 (remove-directory dir)
                 (assert-equal #:expect "trunk" #:got branch)))
         (test "a failed command raises unless failure is allowed"
               (let* ((dir (scratch-directory "failure"))
                      (allowed (begin
                                 (init-repo! dir)
                                 (git (list "rev-parse" "--verify" "nope")
                                      #:directory dir
                                      #:allow-failure? #t))))
                 (remove-directory dir)
                 (assert-equal #:expect #f #:got allowed)))
         (test "git-archive-tar writes the tracked files"
               (let* ((dir (scratch-directory "archive-tar"))
                      (archive (string-append (scratch-directory "archive-out")
                                              "/p.tar.gz"))
                      (listing (begin
                                 (init-repo! dir)
                                 (write-file (string-append dir "/a.txt") "one")
                                 (add-and-commit! dir "first")
                                 (git-archive-tar archive
                                                  #:prefix "p/"
                                                  #:directory dir)
                                 (capture (format #f "tar -tf ~a"
                                                  (shell-quote archive))))))
                 (remove-directory dir)
                 (remove-directory (dirname archive))
                 (assert-contains #:expect listing #:got "p/a.txt")))
         (test "git-archive-zip writes the tracked files"
               (let* ((dir (scratch-directory "archive-zip"))
                      (archive (string-append (scratch-directory "archive-zip-out")
                                              "/p.zip"))
                      (listing (begin
                                 (init-repo! dir)
                                 (write-file (string-append dir "/a.txt") "one")
                                 (add-and-commit! dir "first")
                                 (git-archive-zip archive
                                                  #:prefix "p/"
                                                  #:directory dir)
                                 (capture (format #f "unzip -l ~a"
                                                  (shell-quote archive))))))
                 (remove-directory dir)
                 (remove-directory (dirname archive))
                 (assert-contains #:expect listing #:got "p/a.txt")))
         (test "git-clone copies a repository"
               (let* ((source (scratch-directory "clone-source"))
                      (parent (scratch-directory "clone-parent"))
                      (target (string-append parent "/target"))
                      (copied? (begin
                                 (init-repo! source)
                                 (write-file (string-append source "/a.txt") "one")
                                 (add-and-commit! source "first")
                                 (git-clone source #:directory target)
                                 (file-exists? (string-append target "/a.txt")))))
                 (remove-directory source)
                 (remove-directory parent)
                 (assert-true copied?)))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               repository-suite))
