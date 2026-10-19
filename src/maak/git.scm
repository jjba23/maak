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
;; @example
;; (define-module (maak)
;;   #:use-module (maak git))
;;
;; (define (commit message)
;;   "Commit every tracked change."
;;   (git-commit message))
;; @end example
;;
;; Every command goes through `run', which hands the arguments to git as a
;; list: a commit message with spaces or quotes reaches git as one argument,
;; and no shell splits it again.  Nothing here assumes a project layout -- the
;; archive helpers take the file name and the prefix, and `git' itself takes
;; `#:directory' -- so the same procedures serve a checkout here and one
;; elsewhere.
;;
;; A command that fails stops the task with git's own message and a maak error
;; naming the command, the way any other `run' does.

;;; Code:

(define-module (maak git)
  #:declarative? #t
  #:use-module (maak dsl)
  #:export (git
            git-init
            git-status
            git-add
            git-commit
            git-tag
            git-branch
            git-switch
            git-pull
            git-push
            git-log
            git-archive
            git-archive-tar
            git-archive-zip))

;; ====== The runner ======

(define* (git arguments #:key (directory ".") (allow-failure? #f))
  "Run git with ARGUMENTS, a list of strings, in DIRECTORY.

This is what every wrapper below is made of, and the way to reach a command
that has no wrapper -- `git-fetch', `git-reset', a subcommand added tomorrow.
Nothing is handed to a shell, so a value with spaces in it stays one argument.

@example
(git (list \"fetch\" \"--all\"))
(git (list \"log\" \"--oneline\" \"-3\") #:directory \"/path/to/checkout\")
@end example

A command that exits non-zero raises a maak error naming it, unless
ALLOW-FAILURE? is #t, in which case the call answers #f."
  (apply run
         (list (append (list "git" "-C" directory) arguments)
               #:allow-failure? allow-failure?)))

;; ====== The commands ======
;;
;; Each wrapper runs in the current directory, which is what a task wants: maak
;; runs a task in the project it was started in.  A caller that means another
;; checkout uses `git' itself with `#:directory'.

(define (git-init . arguments)
  "Initialise a git repository here, with any arguments git init takes."
  (git (cons "init" arguments)))

(define (git-status)
  "Show the current branch and a short status of the working tree."
  (git (list "status" "--short" "--branch")))

(define (git-add . paths)
  "Stage PATHS, or every change in the tree when none is named."
  (git (if (null? paths)
           (list "add" "--all")
           (append (list "add" "--") paths))))

(define (git-commit message)
  "Commit every tracked change with MESSAGE."
  (git (list "commit" "-am" message)))

(define (git-tag name)
  "Tag the current commit as NAME, without an annotation."
  (git (list "tag" name)))

(define (git-branch . names)
  "List the branches, or create the branches NAMES name."
  (git (append (list "branch") names)))

(define (git-switch branch)
  "Switch to BRANCH, which must already exist."
  (git (list "switch" branch)))

(define (git-pull . arguments)
  "Pull from the upstream branch, with any arguments git pull takes."
  (git (cons "pull" arguments)))

(define (git-push . arguments)
  "Push the current branch, with any arguments git push takes.

A bare push does not send tags that are not annotated; `(git-push \"--tags\")'
sends them too."
  (git (cons "push" arguments)))

(define (git-log . arguments)
  "Show the history as a decorated one-line graph, twenty commits by default."
  (git (append (list "log" "--oneline" "--graph" "--decorate")
               (if (null? arguments) (list "-n" "20") arguments))))

;; ====== Source archives ======
;;
;; `git archive' packs a commit rather than a directory: the files git tracks
;; in it, without `.git' and without anything untracked or ignored.  That is
;; what a source release wants, and it is why the revision defaults to HEAD
;; rather than to the working tree.

(define* (git-archive output #:key (format "tar.gz") (prefix "")
                      (revision "HEAD") (directory "."))
  "Write REVISION as OUTPUT with git archive, under PREFIX, in FORMAT.

FORMAT is one git archive understands, `tar.gz' and `zip' being the usual
ones.  PREFIX is the directory the archive unpacks into, and it carries its
own trailing slash.  DIRECTORY is the checkout to read from.

@example
(git-archive \"maak-1.0.tar.gz\" #:prefix \"maak-1.0/\")
@end example"
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
