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
;; The git commands a project's Maak file usually wants, and the source
;; archives it usually ships, as procedures rather than tasks.  A Maak file
;; decides which of them to expose, and under which names:
;;
;;   (define-module (maak)
;;     #:use-module (maak git))
;;
;;   (define (commit message)
;;     "Commit every tracked change."
;;     (git-commit message))
;;
;; Every command goes through `run', which hands the arguments to git as a
;; list: a commit message with spaces or quotes reaches git as one argument,
;; and no shell splits it again.  Nothing here assumes a project layout -- the
;; archive helpers take the file name and the prefix, and `git' itself takes
;; `#:directory' -- so the same procedures serve a checkout here and one
;; elsewhere.
;;
;; `git-output' is the reading counterpart of `git': it runs the same command
;; and returns what git printed, which is what a task needs to make a decision
;; (the current branch, the tags, whether the tree is dirty) rather than merely
;; to show the user the output.
;;
;; A command that fails stops the task with git's own message and a maak error
;; naming the command, the way any other `run' does.

;;; Code:

(define-module (maak git)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (git
            git-argv
            git-output
            git-init
            git-clone
            git-status
            git-add
            git-commit
            git-tag
            git-branch
            git-switch
            git-checkout
            git-fetch
            git-pull
            git-push
            git-log
            git-diff
            git-show
            git-merge
            git-rebase
            git-reset
            git-restore
            git-stash
            git-clean
            git-cherry-pick
            git-revert
            git-remote
            git-submodule
            git-config
            git-rev-parse
            git-describe
            git-current-branch
            git-current-revision
            git-describe-tag
            git-remote-url
            git-tags
            git-remotes
            git-dirty?
            git-archive
            git-archive-tar
            git-archive-zip))

;; ====== The runner ======

(define* (git-argv arguments #:key (directory "."))
  "Return the argv that runs git with ARGUMENTS in DIRECTORY.

The checkout is named with @code{-C} rather than by changing maak's working
directory, so a task can read another checkout without disturbing its own.
Exposed because the argument list is the interface every wrapper shares: a test
can assert it, and a caller can log or extend it without running git."
  (append (list "git" "-C" directory) arguments))

(define* (git arguments #:key (directory ".") (allow-failure? #f) (verbose? #f))
  "Run git with ARGUMENTS, a list of strings, in DIRECTORY.

This is what every wrapper below is made of, and the way to reach a command
that has no wrapper -- @code{git fetch}, @code{git reset}, a subcommand added
tomorrow.  Nothing is handed to a shell, so a value with spaces in it stays one
argument.

@example
(git (list \"fetch\" \"--all\"))
(git (list \"log\" \"--oneline\" \"-3\") #:directory \"/path/to/checkout\")
@end example

A command that exits non-zero raises a maak error naming it, unless
ALLOW-FAILURE? is #t, in which case the call answers #f."
  (run (git-argv arguments #:directory directory)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (git-output arguments #:key (directory ".") (allow-failure? #f))
  "Run git with ARGUMENTS in DIRECTORY and return what it printed.

This is the reading half of @code{git}: the output is a string the task can
trim, split and compare, not a line on the terminal.  A command that exits
non-zero raises, unless ALLOW-FAILURE? is #t, in which case the partial output
is returned -- the same contract as @code{capture}.  In dry-run mode nothing
runs, the command is printed, and the empty string comes back.

@example
(string-trim (git-output (list \"rev-parse\" \"--abbrev-ref\" \"HEAD\")))
@end example"
  (capture (string-join (map shell-quote
                             (git-argv arguments #:directory directory))
                        " ")
           #:allow-failure? allow-failure?))

;; ====== The commands ======
;;
;; Each wrapper runs in the current directory, which is what a task wants: maak
;; runs a task in the project it was started in.  A caller that means another
;; checkout uses `git' itself with `#:directory'.

(define (git-init . arguments)
  "Initialise a git repository here, with any arguments git init takes."
  (git (cons "init" arguments)))

(define* (git-clone url #:key (directory #f) (branch #f) (depth #f) (bare? #f)
                    (verbose? #f))
  "Clone URL into DIRECTORY, with BRANCH, DEPTH or BARE? when given.

@code{git clone} is the one command that cannot be pointed at a checkout with
@code{-C}, because the checkout it makes is the destination; the destination is
therefore the final argument rather than a decoration.  Without DIRECTORY git
names the directory after the repository, exactly as it does on the command
line.

@example
(git-clone \"https://example.org/project.git\" #:depth 1)
(git-clone \"../other\" #:directory \"vendor/other\" #:branch \"trunk\")
@end example"
  (when (or (not (string? url)) (string=? url ""))
    (raise-maak-error
     "git-clone expects the URL of the repository to clone"
     #:hint "Write (git-clone \"https://example.org/project.git\")."))
  (run (append (list "git" "clone")
               (if branch (list "--branch" branch) '())
               (if depth (list "--depth" (format #f "~a" depth)) '())
               (if bare? (list "--bare") '())
               (list url)
               (if directory (list directory) '()))
       #:verbose? verbose?))

(define (git-status)
  "Show the current branch and a short status of the working tree."
  (git (list "status" "--short" "--branch")))

(define (git-add . paths)
  "Stage PATHS, or every change in the tree when none is named."
  (git (if (null? paths)
           (list "add" "--all")
           (append (list "add" "--") paths))))

(define* (git-commit message #:key (all? #t) (amend? #f) (allow-empty? #f)
                     (sign? #f))
  "Commit with MESSAGE, every tracked change by default.

ALL? is the @code{-a} that stages modified and deleted tracked files without a
separate @code{git add}; it is #t for the common case of committing the working
tree.  AMEND? replaces the last commit, ALLOW-EMPTY? makes a commit with no
changes, and SIGN? signs it with the configured GPG key.

@example
(git-commit \"fix: keep the config when it is missing\")
(git-commit \"docs: typo\" #:all? #f)     ;; commit only what is staged
@end example"
  (when (or (not (string? message)) (string=? message ""))
    (raise-maak-error
     "git-commit expects a commit message"
     #:hint "Write (git-commit \"feat: add the report\")."))
  (git (append (list "commit")
               (if all? (list "--all") '())
               (if amend? (list "--amend") '())
               (if allow-empty? (list "--allow-empty") '())
               (if sign? (list "--gpg-sign") '())
               (list "-m" message))))

(define* (git-tag name #:key (message #f) (force? #f))
  "Tag the current commit as NAME, without an annotation unless MESSAGE is given.

An annotated tag is what @code{git push --follow-tags} and most release
tooling expects, so passing MESSAGE is the usual choice for a release; a
lightweight tag is the default because it changes nothing in the history.

@example
(git-tag \"v1.2.0\" #:message \"maak 1.2.0\")
@end example"
  (when (or (not (string? name)) (string=? name ""))
    (raise-maak-error
     "git-tag expects a tag name"
     #:hint "Write (git-tag \"v1.2.0\")."))
  (git (append (list "tag")
               (if force? (list "--force") '())
               (if message (list "--annotate" "-m" message) '())
               (list name))))

(define (git-branch . names)
  "List the branches, or create the branches NAMES name."
  (git (append (list "branch") names)))

(define* (git-switch branch #:key (create? #f) (force? #f))
  "Switch to BRANCH, creating it with CREATE? or discarding local changes with
FORCE? when asked.

@code{git switch} is the modern spelling of the checkout half of
@code{git checkout}: it changes branches and nothing else, which is why it is
the default here."
  (when (or (not (string? branch)) (string=? branch ""))
    (raise-maak-error
     "git-switch expects a branch name"
     #:hint "Write (git-switch \"trunk\")."))
  (git (append (list "switch")
               (if create? (list "-c") '())
               (if force? (list "--discard-changes") '())
               (list branch))))

(define (git-checkout . arguments)
  "Run @code{git checkout} with ARGUMENTS.

For a branch, @code{git-switch} says what it means; this wrapper is for the
rest of @code{git checkout} -- restoring paths, detached commits, @code{--orphan}."
  (git (cons "checkout" arguments)))

(define (git-fetch . arguments)
  "Fetch from the remote, with any arguments git fetch takes."
  (git (cons "fetch" arguments)))

(define (git-pull . arguments)
  "Pull from the upstream branch, with any arguments git pull takes."
  (git (cons "pull" arguments)))

(define (git-push . arguments)
  "Push the current branch, with any arguments git push takes.

A bare push does not send tags that are not annotated; @code{(git-push
\"--tags\")} sends them too."
  (git (cons "push" arguments)))

(define (git-log . arguments)
  "Show the history as a decorated one-line graph, twenty commits by default."
  (git (append (list "log" "--oneline" "--graph" "--decorate")
               (if (null? arguments) (list "-n" "20") arguments))))

(define (git-diff . arguments)
  "Show the changes between the working tree and the index, or as ARGUMENTS say.

Use @code{--cached} to compare the index with the last commit, or name two
revisions to compare them with each other."
  (git (cons "diff" arguments)))

(define (git-show . arguments)
  "Show a commit, tag or other object, with any arguments git show takes."
  (git (cons "show" arguments)))

(define (git-merge . arguments)
  "Merge with any arguments git merge takes."
  (git (cons "merge" arguments)))

(define (git-rebase . arguments)
  "Rebase with any arguments git rebase takes."
  (git (cons "rebase" arguments)))

(define (git-reset . arguments)
  "Reset with any arguments git reset takes.

@code{--soft} and @code{--mixed} keep the changes; @code{--hard} throws them
away, which is what makes this wrapper worth naming explicitly in a task."
  (git (cons "reset" arguments)))

(define (git-restore . arguments)
  "Restore paths with any arguments git restore takes."
  (git (cons "restore" arguments)))

(define (git-stash . arguments)
  "Stash with any arguments git stash takes.

The bare call saves the working tree; pass @code{pop} to bring it back, or
@code{list} to see what is saved."
  (git (cons "stash" arguments)))

(define (git-clean . arguments)
  "Remove untracked files with any arguments git clean takes.

@code{git clean} does nothing without @code{-f}; pass it, and @code{-d} for
directories, because there is no safer default that would still be useful."
  (git (cons "clean" arguments)))

(define (git-cherry-pick . arguments)
  "Cherry-pick with any arguments git cherry-pick takes."
  (git (cons "cherry-pick" arguments)))

(define (git-revert . arguments)
  "Revert with any arguments git revert takes."
  (git (cons "revert" arguments)))

(define (git-remote . arguments)
  "Manage remotes with any arguments git remote takes.

@example
(git-remote (list \"add\" \"origin\" \"https://example.org/project.git\"))
(git-remote (list \"-v\"))
@end example"
  (git (cons "remote" arguments)))

(define (git-submodule . arguments)
  "Manage submodules with any arguments git submodule takes."
  (git (cons "submodule" arguments)))

(define (git-config . arguments)
  "Read or write configuration with any arguments git config takes."
  (git (cons "config" arguments)))

(define (git-rev-parse . arguments)
  "Ask git about revisions, with any arguments git rev-parse takes."
  (git (cons "rev-parse" arguments)))

(define (git-describe . arguments)
  "Describe a commit, with any arguments git describe takes."
  (git (cons "describe" arguments)))

;; ====== Reading the checkout ======
;;
;; The wrappers above print; these answer.  Each is a small query over
;; `git-output', trimmed where git adds a newline and returned as a string or a
;; boolean, so a task can branch on the checkout rather than parse a log line.

(define* (git-current-branch #:key (directory "."))
  "Return the branch the checkout in DIRECTORY is on.

A detached HEAD answers with the revision rather than the word HEAD, so a
task that tags a release from CI still gets something it can record."
  (let ((branch (string-trim-both
                 (git-output (list "rev-parse" "--abbrev-ref" "HEAD")
                             #:directory directory))))
    (if (string=? branch "HEAD")
        (git-current-revision #:directory directory)
        branch)))

(define* (git-current-revision #:key (directory "."))
  "Return the full hash of the commit checked out in DIRECTORY."
  (string-trim-both
   (git-output (list "rev-parse" "HEAD") #:directory directory)))

(define* (git-describe-tag #:key (directory "."))
  "Return the most recent tag reachable from HEAD in DIRECTORY.

@example
(git-describe-tag)   ;; => v0.10.0
@end example"
  (string-trim-both
   (git-output (list "describe" "--tags" "--abbrev=0")
               #:directory directory)))

(define* (git-remote-url name #:key (directory "."))
  "Return the URL configured for remote NAME in DIRECTORY."
  (string-trim-both
   (git-output (list "remote" "get-url" name) #:directory directory)))

(define* (git-tags #:key (directory "."))
  "Return the names of the tags in DIRECTORY, sorted."
  (sort (filter (lambda (line) (not (string=? line "")))
                (string-split (git-output (list "tag" "--list")
                                          #:directory directory)
                              #\newline))
        string<?))

(define* (git-remotes #:key (directory "."))
  "Return the names of the remotes configured in DIRECTORY, sorted."
  (sort (filter (lambda (line) (not (string=? line "")))
                (string-split (git-output (list "remote")
                                          #:directory directory)
                              #\newline))
        string<?))

(define* (git-dirty? #:key (directory "."))
  "Return #t when the checkout in DIRECTORY has anything uncommitted.

Both staged and unstaged changes count, as do untracked files: this is the
question @code{git status --porcelain} answers, rather than the narrower one
@code{git diff --quiet} answers."
  (not (string=? ""
                 (string-trim-both
                  (git-output (list "status" "--porcelain")
                              #:directory directory)))))

;; ====== Source archives ======
;;
;; `git archive' packs a commit rather than a directory: the files git tracks
;; in it, without `.git' and without anything untracked or ignored.  That is
;; what a source release wants, and it is why the revision defaults to HEAD
;; rather than to the working tree.

(define* (git-archive output #:key (format "tar.gz") (prefix "")
                      (revision "HEAD") (directory "."))
  "Write REVISION as OUTPUT with git archive, under PREFIX, in FORMAT.

FORMAT is one git archive understands, @code{tar.gz} and @code{zip} being the
usual ones.  PREFIX is the directory the archive unpacks into, and it carries
its own trailing slash.  DIRECTORY is the checkout to read from.

@example
(git-archive \"maak-1.0.tar.gz\" #:prefix \"maak-1.0/\")
@end example"
  (when (or (not (string? output)) (string=? output ""))
    (raise-maak-error
     "git-archive expects the archive file to write"
     #:hint "Write (git-archive \"project-1.0.tar.gz\" #:prefix \"project-1.0/\")."))
  (git (list "archive"
             (string-append "--format=" format)
             (string-append "--prefix=" prefix)
             "-o" output
             revision)
       #:directory directory))

(define* (git-archive-tar output #:key (prefix "") (revision "HEAD")
                          (directory "."))
  "Write REVISION as the gzipped tarball OUTPUT, under PREFIX."
  (git-archive output
               #:format "tar.gz"
               #:prefix prefix
               #:revision revision
               #:directory directory))

(define* (git-archive-zip output #:key (prefix "") (revision "HEAD")
                          (directory "."))
  "Write REVISION as the zip OUTPUT, under PREFIX."
  (git-archive output
               #:format "zip"
               #:prefix prefix
               #:revision revision
               #:directory directory))
