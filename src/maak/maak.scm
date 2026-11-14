;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (maak maak)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 getopt-long)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak nix)
  #:use-module (maak guix)
  #:use-module (maak i18n)
  #:use-module (maak version)
  #:use-module (maak git)
  #:use-module (maak archive)
  #:use-module (maak container)
  #:use-module (maak niri)
  #:use-module (maak net)
  #:re-export (log-message log-info
                           log-error
                           log-separator
                           bold-output
                           get-module-var
                           delete-file-recursively
                           rm-rf
                           manifest-shell
                           time-machine
                           time-machine-manifest-shell
                           program-shell
                           time-machine-program-shell
                           mkdir-p
                           cp
                           mv
                           quiet?
                           dry-run?
                           banner?
                           remkdir-p
                           maak-proc?
                           maak-task?
                           $
                           ~
                           syscall
                           cat
                           ;; ====== Programs and background jobs ======
                           run
                           run-pipeline
                           stage
                           stage?
                           pipeline-result?
                           pipeline-ok?
                           pipeline-outputs
                           pipeline-errors
                           job?
                           job-pid
                           job-command
                           job-done?
                           job-exit-code
                           wait-job

                           ;; ====== Errors ======
                           &maak-error
                           make-maak-error                           maak-error? 
                           maak-error-message
                           maak-error-hint
                           raise-maak-error
                           task-error
                           ;; ====== Load paths ======
                           maak-load-path
                           add-load-path!
                           add-load-paths!
                           with-load-path
                           split-load-path
                           environment-load-paths
                           ;; ====== Pretty strings ======
                           string-lines
                           unlines
                           unwords
                           dedent-string
                           indent-string
                           text
                           ;; ====== Guix shells and containers ======
                           guix-shell
                           guix-container
                           guix-pack
                           ;; ====== Guix values ======
                           guix-available?
                           guix-channels
                           guix-derivation
                           guix-build
                           guix-program
                           guix-derivation-at
                           guix-build-at
                           guix-program-at
                           ;; ====== Shell helpers ======
                           environment-assignments
                           decorate-command
                           elevation-command
                           detect-elevation-command
                           command-on-path?
                           shell-quote
                           which
                           command-exists?
                           require-command
                           capture
                           touch
                           ln-s
                           chmod-file
                           with-directory
                           cd
                           env-get
                           env-set!
                           env-unset!
                           with-env
                           with-env-vars
                           env-file-entries
                           load-env-file
                           home-dir
                           tmp-dir
                           read-file
                           write-file
                           ;; ====== Nix helpers ======
                           nix-shell
                           nix-develop
                           nix-build
                           nix-flake-build
                           nix-flake-check
                           nix-run
                           ;; ====== i18n helpers ======
                           i18n-locale-dir
                           i18n-po-file
                           i18n-mo-file
                           i18n-pot-file
                           i18n-source-globs
                           i18n-extract
                           i18n-init
                           i18n-merge
                           i18n-compile
                           i18n-init-all
                           i18n-merge-all
                           i18n-compile-all
                           i18n-refresh
                           i18n-translator
                           i18n-language
                           ;; ====== Git ======
                           git
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
                           git-archive-zip
                           ;; ====== Archives ======
                           tar-create
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
                           zip-list-argv
                           ;; ====== Containers ======
                           container-engine
                           container-compose-command
                           detect-container-engine
                           detect-compose-command
                           container-available?
                           compose-available?
                           container-argv
                           container
                           container-output
                           container-run-argv
                           container-run
                           container-build-argv
                           container-build
                           container-ps-argv
                           container-ps
                           container-images-argv
                           container-images
                           container-stop-argv
                           container-stop
                           container-rm-argv
                           container-rm
                           container-exec-argv
                           container-exec
                           container-logs-argv
                           container-logs
                           container-pull-argv
                           container-pull
                           container-push-argv
                           container-push
                           container-tag-argv
                           container-tag
                           container-inspect-argv
                           container-inspect
                           compose-argv
                           run-compose
                           compose-output
                           compose-up-argv
                           compose-up
                           compose-down-argv
                           compose-down
                           compose-build-argv
                           compose-build
                           compose-logs-argv
                           compose-logs
                           compose-ps-argv
                           compose-ps
                           compose-exec-argv
                           compose-exec
                           compose-run-argv
                           compose-run
                           compose-pull-argv
                           compose-pull
                           compose-restart-argv
                           compose-restart
                           compose-config-argv
                           compose-config
                           ;; ====== niri ======
                           niri
                           niri-available?
                           niri-argv
                           niri-capture
                           niri-action
                           niri-action-argv
                           niri-version
                           niri-outputs
                           niri-workspaces
                           niri-windows
                           niri-layers
                           niri-keyboard-layouts
                           niri-focused-output
                           niri-focused-window
                           niri-pick-window
                           niri-pick-color
                           niri-overview-state
                           niri-casts
                           niri-request-error
                           niri-focus-workspace
                           niri-focus-workspace-previous
                           niri-focus-column
                           niri-focus-monitor
                           niri-focus-window
                           niri-focus-window-previous
                           niri-close-window
                           niri-fullscreen-window
                           niri-toggle-windowed-fullscreen
                           niri-toggle-overview
                           niri-spawn
                           niri-spawn-sh
                           niri-quit
                           niri-screenshot
                           niri-screenshot-screen
                           niri-screenshot-window
                           niri-power-off-monitors
                           niri-power-on-monitors
                           niri-move-window-to-workspace
                           niri-move-column-to-workspace
                           niri-move-window-to-monitor
                           niri-move-column-to-monitor
                           niri-set-window-width
                           niri-set-column-width
                           niri-set-workspace-name
                           niri-unset-workspace-name
                           niri-output
                           niri-output-argv
                           niri-output-on
                           niri-output-off
                           niri-output-mode
                           niri-output-scale
                           niri-output-transform
                           niri-output-position
                           ;; ====== Downloads ======
                           fetch-command
                           detect-fetch-command
                           fetch-available?
                           curl
                           curl-argv
                           curl-output
                           curl-get-argv
                           curl-get
                           curl-text-argv
                           curl-text
                           curl-head-argv
                           curl-head
                           curl-status-argv
                           curl-status
                           curl-post-argv
                           curl-post
                           curl-download-argv
                           curl-download
                           curl-upload-argv
                           curl-upload
                           wget
                           wget-argv
                           wget-output
                           wget-download-argv
                           wget-download
                           wget-mirror-argv
                           wget-mirror
                           wget-text-argv
                           wget-text
                           download
                           ;; ====== Version ======
                           maak-version
                           maak-version-string
                           maak-homepage
                           maak-license
                           print-version)
  #:export (resolve-maak-tasks list-tasks run-tasks run-tasks-parallel
                              run-task
                              format-task-doc available-task-names
                              unknown-task-error
                              task-metadata! task-meta task-depends
                              task-hidden? task-example task-order))

(define* (resolve-maak-tasks #:key (module-name '(maak)))
  "Return the runnable tasks of MODULE-NAME as `(NAME . VARIABLE)' pairs.

When the module declares exports, only those are considered; otherwise every
procedure the module defines counts as a task.  See the export rules in the
Tasks page of the documentation, docs/tasks.org."
  (let* ((mod (resolve-module module-name))
         (public-mod (module-public-interface mod))
         ;; Check if a public interface exists AND contains at least one export.
         (has-exports? (and public-mod
                            (not (null? (module-map (lambda (name var)
                                                      name) public-mod)))))
         ;; If it has exports, search only those. Otherwise, fall back to the whole module.
         (target-mod (if has-exports? public-mod mod))
         (tasks (filter (lambda (x)
                          (not (equal? #f x)))
                        (module-map (lambda (name var)
                                      (cond
                                        ((maak-task? var)
                                         (cons name var))
                                        (else #f))) target-mod))))
    tasks))

(define (task-name<? a b)
  "Order two tasks by name, so a listing or an error message is stable."
  (string<? (symbol->string (car a))
            (symbol->string (car b))))

(define (available-task-names)
  "Return the names of the tasks maak can currently run, sorted."
  (map (lambda (task)
         (symbol->string (car task)))
       (sort (resolve-maak-tasks) task-name<?)))

;; ====== What a task declares about itself ======
;;
;; A docstring says what a task does.  Three things about a task are not prose,
;; and they arrive as an ordinary call after the definition:
;;
;;   (define (deploy) "Ship it." (run (list "rsync" ...)))
;;   (task-metadata! deploy
;;                   #:depends '(build test)
;;                   #:example "maak deploy prod.example.com")
;;
;; The vocabulary is three fields -- what the task needs first, whether a
;; listing should offer it, and how one is invoked -- and it is deliberately
;; closed.  A vocabulary is a decision about what a task is; one that grows a
;; field for every need stops being one.  A fourth field is a change to this
;; file and an argument in the guide's Tasks page, not a key a project adds.
;;
;; The metadata lives on the procedure, which is what makes it a property of the
;; task rather than of its name: it dies with the task it describes, so a run
;; that loads a different Maak file cannot inherit it.

(define-record-type <task-meta>
  (make-task-meta depends hidden? example)
  task-meta?
  (depends task-meta-depends)
  (hidden? task-meta-hidden?)
  (example task-meta-example))

(define (task-depends-names depends)
  "Return DEPENDS as the task names it declares, as symbols."
  (unless (list? depends)
    (raise-maak-error
     (format #f "task-metadata!: #:depends takes a list of task names, got ~s"
             depends)
     #:hint "Write #:depends '(build fmt), naming the tasks this one needs first."))
  (map (lambda (name)
         (cond
           ((symbol? name) name)
           ((string? name) (string->symbol name))
           (else
            (raise-maak-error
             (format #f "task-metadata!: ~s does not name a task" name)
             #:hint "A dependency is a task name, written as a symbol or a string."))))
       depends))

(define* (task-metadata! task #:key (depends '()) (hidden? #f) (example #f))
  "Record what TASK declares about itself.

TASK is the procedure the task is defined as, so this call goes after the
definition, where the name is bound:

@example
(define (deploy) \"Ship it.\" (run (list \"rsync\" ...)))
(task-metadata! deploy
                #:depends '(build test)
                #:example \"maak deploy prod.example.com\")
@end example

DEPENDS names the tasks that must have finished before this one starts, as a
list of symbols or strings; a dependency says *when* a task may run, never
whether it does, because maak has no notion of up to date.  HIDDEN? keeps the
task out of the listings -- the human one and the machine-readable one an editor
reads -- while still letting it be run by name and needed by another task.
EXAMPLE is an invocation to show a reader, prose rather than a value.

The call returns TASK, so a file may write it inline.  A declaration that cannot
be honoured -- a dependency that is not a name, an example that is not a string,
metadata for something that is not a task -- is a maak error where it is
written, rather than a task that runs in the wrong order later."
  (unless (procedure? task)
    (raise-maak-error
     (format #f "task-metadata! expects the procedure a task is defined as, got ~s"
             task)
     #:hint "Write (task-metadata! deploy #:depends '(build)) after the definition of `deploy'."))
  (when (and example (not (string? example)))
    (raise-maak-error
     (format #f "task-metadata!: #:example takes the invocation as a string, got ~s"
             example)
     #:hint "Write #:example \"maak deploy prod.example.com\"."))
  (set-procedure-property!
   task 'maak-task-meta
   (make-task-meta (task-depends-names depends) (and hidden? #t) (and example example)))
  task)

(define (task-meta task)
  "Return what TASK declares about itself, or #f when it declares nothing."
  (and (procedure? task)
       (procedure-property task 'maak-task-meta)))

(define (task-depends task)
  "Return the names of the tasks TASK needs to have finished first, in order."
  (let ((meta (task-meta task)))
    (if meta (task-meta-depends meta) '())))

(define (task-hidden? task)
  "Return #t when TASK asks the listings to leave it out."
  (let ((meta (task-meta task)))
    (and meta (task-meta-hidden? meta) #t)))

(define (task-example task)
  "Return the invocation TASK holds up as an example, or the empty string."
  (let ((meta (task-meta task)))
    (if (and meta (task-meta-example meta)) (task-meta-example meta) "")))

;; ====== The order a run puts the tasks in ======
;;
;; A dependency orders a run and nothing else.  Nothing here asks whether a task
;; is up to date, and nothing is remembered between runs, which is what keeps
;; the ordering from becoming a cache by the back door: every task named, and
;; every task a named one needs, runs.

(define* (task-order tasks #:key (known (resolve-maak-tasks)))
  "Return TASKS as the list to run, with each task's dependencies before it.

A task named twice appears once, and a dependency that two tasks share runs once
before the first of them.  The order the tasks were given is kept otherwise, so
a run of several tasks still reads in the order it was written.

KNOWN is the `(NAME . VARIABLE)' alist the order is resolved against,
`resolve-maak-tasks' by default.  A dependency that names no task of the file,
and a dependency cycle, are maak errors: both leave a run that cannot honour
what the file says, and a silently shortened run is the kind of thing that only
shows up as a deploy that shipped half."
  (define (dependencies-of task)
    (let ((entry (assoc task known)))
      (unless entry
        (unknown-task-error task))
      (task-depends (variable-ref (cdr entry)))))
  (define (visit task seen path)
    ;; Return (ORDER . SEEN) for TASK and everything it needs, dependencies
    ;; first and each of them once.  PATH is the chain being visited, which is
    ;; how a cycle is told from a diamond.
    (cond
      ((memq task seen) (cons '() seen))
      ((memq task path)
       (raise-maak-error
        (format #f "dependency cycle: ~a"
                (string-join (map symbol->string (reverse (cons task path)))
                             " -> "))
        #:hint "Remove one of the dependencies; a task may not need itself, directly or through other tasks."))
      (else
       (let walk ((remaining (dependencies-of task)) (order '()) (seen seen))
         (if (null? remaining)
             (cons (append order (list task)) (cons task seen))
             (let ((sub (visit (car remaining) seen (cons task path))))
               (walk (cdr remaining) (append order (car sub)) (cdr sub))))))))
  (let walk ((remaining tasks) (order '()) (seen '()))
    (if (null? remaining)
        order
        (let ((sub (visit (car remaining) seen '())))
          (walk (cdr remaining) (append order (car sub)) (cdr sub))))))

(define* (format-task-doc name doc #:key (needs '()))
  "Return the lines that describe task NAME, given its docstring DOC.

A docstring that is one paragraph starts inline after the name, and every
continuation line aligns under the first word of the documentation, so a
docstring wrapped over several lines reads as one block:

@example
build: Compile the project.
       The result lands in ./target.
@end example

A docstring with a blank line in it is a set of paragraphs, and running text
would flatten that shape away, so it prints as a block instead: the name on its
own line and every line indented to the same column, with the blank lines left
blank.

Common leading indentation is removed first with `dedent-string', so a
docstring written inside an indented function still reads flush left.

NEEDS names the tasks this one requires to have finished first, and is printed
under the documentation as a line of its own, aligned to the same margin; a task
that needs nothing prints nothing extra."
  (let* ((label (string-append (bold-output name) ":"))
         (prefix (string-append name ": "))
         ;; The column the documentation aligns to: the visible length of the
         ;; name and its colon and space.  The ANSI codes `bold-output' adds
         ;; occupy no column, which is why the margin comes from PREFIX rather
         ;; than from LABEL.
         (margin (make-string (string-length prefix) #\space))
         (text (and (string? doc) (dedent-string doc)))
         (lines (if (and text (not (string=? text "")))
                    (string-split text #\newline)
                    '()))
         (indented (lambda (line)
                     ;; A blank line inside the documentation stays blank
                     ;; rather than being padded out to the margin.
                     (if (string=? line "") ""
                         (string-append margin line))))
         (needed (if (null? needs)
                     '()
                     (list (string-append margin "needs: "
                                          (string-join needs ", "))))))
    (append
     (cond
       ((null? lines)
        (list label))
       ((any blank-line? lines)
        (cons label (map indented lines)))
       (else
        (cons (string-append prefix (car lines))
              (map indented (cdr lines)))))
     needed)))

(define* (list-tasks #:key (as-is? #f))
  "List documentation for every maak task in the loaded file.

Tasks are sorted by name unless AS-IS? is #t.  Each docstring is laid out so
its lines align under the task name rather than wrapping back to the left
margin; see `format-task-doc'.  A task that needs others to have finished first
says so under its documentation, and a task that asked to be left out of the
listings with `task-metadata!' is left out."
  (let* ((raw-tasks (resolve-maak-tasks))
         (tasks (if as-is? raw-tasks (sort raw-tasks task-name<?))))
    (log-separator)
    (for-each (lambda (task)
                (let ((proc (variable-ref (cdr task))))
                  (unless (task-hidden? proc)
                    (newline)
                    (log-message
                     (string-join
                      (format-task-doc (symbol->string (car task))
                                       (procedure-documentation proc)
                                       #:needs (map symbol->string
                                                    (task-depends proc)))
                      "\n")))))
              tasks)
    (newline)
    (newline)))

(define (unknown-task-error task)
  "Raise a maak error saying TASK is not runnable, listing the tasks that are."
  (let ((available (available-task-names)))
    (raise-maak-error
     (if (equal? 'default task)
         "Could not find a `default' task in this Maak file."
         (format #f "Unknown task: ~a" task))
     #:hint
     (if (null? available)
         "No runnable tasks are defined. Define a function, and export it if the module declares exports."
         (format #f "Available tasks: ~a" (string-join available ", "))))))

;; ====== The words a task was given ======
;;
;; A run names its tasks and passes the words that follow them on as arguments,
;; so a run that meant to name two tasks and wrote one of them as a word after
;; the other is not an error about tasks at all: the first task is handed the
;; second task's name.  Whether that is what happened is something only the
;; loaded Maak file can answer, so it is asked -- and only when a task has
;; already failed for being handed words it does not take.

(define (arguments-naming-tasks task-args tasks)
  "Return the words of TASK-ARGS that name one of TASKS, in the order given.

TASKS is the alist `resolve-maak-tasks' returns, so this asks the loaded Maak
file rather than a list of names: a word that merely looks like a task name is
not one, and a word that is one is answered for by the file itself."
  (filter (lambda (arg)
            (and (string? arg)
                 (assoc (string->symbol arg) tasks)))
          task-args))

(define (words-are-tasks-hint task named)
  "Return the hint for TASK, handed the task names in NAMED as arguments.

A task handed the name of another task has almost always been given a run that
wanted two tasks and wrote them the way a shell command takes words, without
the `--' that introduces a task list.  The hint therefore spells out the run
that was probably meant, in both of the spellings the command line accepts."
  (let* ((quoted (string-join (map (lambda (name) (format #f "`~a'" name))
                                   named)
                              ", "))
         (names (cons (symbol->string task) named))
         (as-list (string-join names ","))
         (as-words (string-join names " ")))
    (format #f
            "~a ~a a task of this Maak file, so ~a received ~a as an argument.  To run several tasks, name them in one comma separated list (`maak ~a') or one word each after `--' (`maak -- ~a'); a task's arguments are the words that follow its name."
            quoted
            (if (null? (cdr named)) "names" "name")
            task
            (if (null? (cdr named)) "it" "them")
            as-list
            as-words)))

(define (run-task-procedure task proc task-args tasks)
  "Run PROC for TASK with TASK-ARGS, naming any of them that are tasks.

A task handed more arguments than it takes fails with Guile's complaint about
arity, which says nothing about where the words came from and leaves a run that
meant to name two tasks reading like a broken task.  When one of the words is a
task of the same Maak file, the failure is raised again as a maak error whose
hint says how to write the run that was meant.  Everything else -- a task that
fails for its own reasons, a failure with no task names among its arguments --
is passed on exactly as it arrived."
  (catch #t
    (lambda () (apply proc task-args))
    (lambda (key . vals)
      (let ((named (and (eq? key 'wrong-number-of-args)
                        (arguments-naming-tasks task-args tasks))))
        (if (pair? named)
            (raise-maak-error (exception->message key vals)
                              #:hint (words-are-tasks-hint task named))
            (apply throw key vals))))))

(define* (run-tasks tasks
                    #:key (task-args '())
                    (keep-going? #f)
                    (on-failure #f))
  "Run TASKS sequentially, in the order given.

The tasks run in the order `task-order' settles: a task that declares
dependencies with `task-metadata!' runs after them, and a task named twice runs
once.  Every task must name a procedure the loaded Maak file makes visible;
otherwise a maak error lists the tasks that do exist.  TASK-ARGS are passed to
each task.  Returns the names of the tasks that failed, in the order they were
run -- the empty list when everything succeeded.

With KEEP-GOING? a task that fails stops nothing: the tasks after it still run.
ON-FAILURE is then called with the task name and the exception it raised, as
`(ON-FAILURE TASK KEY VALUE ...)', so the caller can report the failure where
it happened and decide what a run full of them means.  Without KEEP-GOING? the
first failure is raised as it always was, backtrace and all."
  (let loop ((remaining (task-order tasks))
             (failed '()))
    (cond
      ((null? remaining) (reverse failed))
      (else
       (let ((task (car remaining)))
         (if (not keep-going?)
             (begin
               (unless (get-module-var task)
                 (unknown-task-error task))
               (run-task task
                         #:task-args task-args)
               (loop (cdr remaining) failed))
             (let ((failure #f))
               (catch #t
                 (lambda ()
                   (unless (get-module-var task)
                     (unknown-task-error task))
                   (run-task task
                             #:task-args task-args))
                 (lambda (key . vals)
                   (set! failure (cons key vals))))
               (if failure
                   (begin
                     (when on-failure
                       ;; The key and its values go on as they arrived, so the
                       ;; callback can tell a maak error from anything else.
                       (apply on-failure task failure))
                     (loop (cdr remaining) (cons task failed)))
                   (loop (cdr remaining) failed)))))))))

(define* (run-task task
                   #:key (task-args '()))
  "Run TASK, a single Maak task, logging its documentation first.

Args:
  task: a symbol naming the task to run.
  task-args: the arguments to pass to the task procedure.

Returns the value of the task procedure.  Raises a maak error when TASK does
not name a runnable task, either because it is missing or because it exists but
is not visible as a task (see the export rules in the Tasks page,
docs/tasks.org)."
  (let* ((proc (get-module-var task))
         (proc-doc (and (maak-proc? proc) (procedure-documentation proc)))
         (valid-tasks (resolve-maak-tasks)))
    (cond
      ;; Ensure the procedure is valid AND exists in the exported tasks list.
      ((and (maak-proc? proc)
            (assoc task valid-tasks))
       (log-separator)
       (log-info "=> Running Maak task: ~a\n" task)
       (when (and (string? proc-doc)
                  (not (string=? proc-doc "")))
         (log-info "  ~a" proc-doc))
       (newline)
       (run-task-procedure task proc task-args valid-tasks))
      (else (unknown-task-error task)))))

;; ====== Several tasks at once ======
;;
;; A run is sequential by default, which keeps the output in order and every
;; failure where it happened.  `run-tasks-parallel' is the opt-in: each task
;; gets a forked worker of its own and writes to a log file, and the parent
;; prints the logs in the order the tasks were named.  Forking rather than
;; threading is what makes the output separable: a worker's Scheme writes and
;; the output of every command it runs share one file, so no two tasks can
;; interleave their lines.

(define (parallel-task-log-path index)
  "Return a log file of this run's own for the task at INDEX."
  (format #f "~a/maak-task-~a-~a.log" (tmp-dir) (getpid) index))

(define (report-parallel-task-failure task key vals)
  "Print the failure of TASK from a forked worker, into that worker's log."
  (let ((error (find-maak-error key vals)))
    (if error
        (begin
          (format (current-error-port)
                  "\nmaak: error (task ~a): ~a\n"
                  task
                  (maak-error-message error))
          (when (maak-error-hint error)
            (format (current-error-port)
                    "        hint: ~a\n"
                    (maak-error-hint error))))
        (format (current-error-port)
                "\nmaak: error (task ~a): ~a\n"
                task
                (exception->message key vals)))))

(define* (fork-task-worker task task-args log-path)
  "Fork a worker that runs TASK, with its output going to LOG-PATH.

Returns the worker's pid in the parent; the child never returns.  Its exit
status is 0 when the task succeeded and 1 when it raised, so the parent can
report the failure next to the output that explains it."
  ;; Flush the parent's buffers first: the child inherits the ports and would
  ;; otherwise write the same text into its log.
  (force-output (current-output-port))
  (force-output (current-error-port))
  (let ((pid (primitive-fork)))
    (if (zero? pid)
        (let ((log (open-file log-path "w")))
          (dup2 (port->fdes log) 1)
          (dup2 (port->fdes log) 2)
          (parameterize ((current-output-port log)
                         (current-error-port log))
            (let ((code (catch #t
                          (lambda ()
                            (run-task task #:task-args task-args)
                            0)
                          (lambda (key . vals)
                            (report-parallel-task-failure task key vals)
                            1))))
              (force-output log)
              (close-port log)
              (primitive-exit code))))
        pid)))

(define (display-task-log path)
  "Print the captured output of a parallel task and remove its log file."
  (when (file-exists? path)
    (let ((text (call-with-input-file path get-string-all)))
      (unless (string=? text "")
        (display text)
        ;; Blocks sit next to each other, so one that does not end in a newline
        ;; gets one rather than joining the next task's first line.
        (unless (char=? (string-ref text (- (string-length text) 1))
                        #\newline)
          (newline)))
      (delete-file path))))

(define* (run-tasks-parallel tasks
                             #:key (task-args '()) (jobs 0))
  "Run TASKS concurrently and print their output in the order they were given.

TASKS is a list of symbols; TASK-ARGS are passed to each, as `run-tasks' does.
JOBS limits how many run at once: a positive number runs that many, and 0 (the
default) starts them all.

Each task runs in a forked worker, so a task sees the Maak file as it was
loaded and cannot change the environment of the tasks beside it.  A worker's
standard output and standard error, including the output of every command it
runs, go to a log of its own; the logs are printed as blocks, in the order
`task-order' settled, as soon as the next task in that order has finished.  That
is what keeps a parallel run readable: no two tasks interleave their lines.

A worker whose task declared dependencies starts only once they have finished,
so a run of `-j' still puts a task after the ones it needs; the tasks beside it
are unaffected, and a failure does not stop the run -- every task that can start
does.  A task named twice is started once, because `task-order' names it once.

Returns the names of the tasks that failed, in the order they were run.  A task
that cannot be found, or a dependency that names none, is an error before any
worker starts, so a mistyped name cannot leave half a run behind."
  (let* ((valid (resolve-maak-tasks))
         (order (task-order tasks #:known valid))
         (pending (let loop ((remaining order) (index 0) (acc '()))
                    (if (null? remaining)
                        (reverse acc)
                        (loop (cdr remaining)
                              (+ index 1)
                              (cons (list index
                                          (car remaining)
                                          (task-depends
                                           (variable-ref
                                            (cdr (assoc (car remaining) valid)))))
                                    acc)))))
         (count (length pending))
         (limit (if (and (exact-integer? jobs) (>= jobs 1))
                    jobs
                    count))
         (paths (make-vector count #f))
         (statuses (make-vector count #f))
         (running '())
         (done '())
         (next 0)
         (failed '())
         (startable? (lambda (entry)
                       ;; An entry is (INDEX TASK DEPENDENCIES); it may start
                       ;; once every task it needs has finished, whatever that
                       ;; task's exit status was.
                       (every (lambda (needed) (memq needed done))
                              (third entry))))
         (start! (lambda ()
                   (let start ()
                     (when (< (length running) limit)
                       (let ((ready (find startable? pending)))
                         (when ready
                           (let* ((index (first ready))
                                  (path (parallel-task-log-path index)))
                             (vector-set! paths index path)
                             (set! pending (delq ready pending))
                             (set! running
                                   (cons (cons (fork-task-worker (second ready)
                                                                 task-args
                                                                 path)
                                               index)
                                         running))
                             (start)))))))))
    (start!)
    (let wait ()
      (when (or (pair? pending) (pair? running))
        (let* ((result (waitpid -1 0))
               (pid (car result))
               (status (cdr result))
               (entry (assoc pid running)))
          (when entry
            (set! running (delete entry running))
            (let ((index (cdr entry)))
              (vector-set! statuses index status)
              ;; A finished task is what lets the ones needing it start.
              (set! done (cons (list-ref order index) done)))))
        ;; Print every block that is next in line and has finished.
        (let flush ()
          (when (and (< next count) (vector-ref statuses next))
            (let ((code (status:exit-val (vector-ref statuses next))))
              (unless (and code (zero? code))
                (set! failed (cons (list-ref order next) failed))))
            (display-task-log (vector-ref paths next))
            (set! next (+ next 1))
            (flush)))
        (start!)
        (wait)))
    (reverse failed)))
