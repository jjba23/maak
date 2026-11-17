;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; sift (https://github.com/jjba23/sift) formats, lints and reads Scheme, Emacs
;; Lisp and Common Lisp, and keeps its three jobs apart: `format' prints or
;; writes, `lint' reports, `check' asks the formatting question without touching
;; the tree, and `ci' asks both at once.  Its exit status says which job
;; answered:
;;
;;   0  nothing to report
;;   1  findings, or a file `format' would change
;;   2  a usage error, an unreadable file, or a file that cannot be parsed
;;
;; That 1 is what makes this module more than a `run'.  A wrapper that treated
;; it as a crash would report a lint finding as a broken pipeline, and one that
;; ignored it would let a gate pass on a dirty tree, so the status is read here
;; and the gates name which of the two happened.
;;
;; `sift-argv' is a pure builder: a test asserts the command line without sift
;; installed, `--dry-run' prints it and `--plan' records it.  The program is a
;; parameter, honoured from MAAK_SIFT then SIFT, and it may carry a wrapper or a
;; path to a checkout's own `bin/sift', which is how a project lints itself with
;; the sift it is developing.

;;; Code:

(define-module (maak sift)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak watch)
  #:export (sift-command
            sift-files
            sift-argv
            sift
            sift-status
            sift-exit-meaning
            sift-format
            sift-check
            sift-lint
            sift-ci))

;; ====== The program ======

(define (env-value names fallback)
  "Return the first non-empty value among environment NAMES, or FALLBACK."
  (let loop ((names names))
    (if (null? names)
        fallback
        (let ((value (getenv (car names))))
          (if (and value (not (string=? value "")))
              value
              (loop (cdr names)))))))

(define sift-command
  (make-parameter (env-value '("MAAK_SIFT" "SIFT") "sift")))

(define (program-words command)
  "Return COMMAND as the words of a program invocation, or raise.

A value may be a path or carry a wrapper, so it is split the way an unquoted
shell word is split; an empty value is a maak error rather than a command with
no program."
  (let ((words (and (string? command)
                    (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "sift must name a program, got ~s" command)
       #:hint "Set MAAK_SIFT, or pass sift-command as a string."))
    words))

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write '(\"src\" \"tests\") for several files or directories."))
    items))

(define (option-value value what)
  "Return VALUE as the string an option takes, accepting a number too."
  (cond
    ((number? value) (number->string value))
    ((and (string? value) (not (string=? value ""))) value)
    (else
     (raise-maak-error
      (format #f "~a takes a non-empty string or a number, got ~s" what value)
      #:hint "Name the value as a string, or leave the option out."))))

;; ====== The sift command line ======

(define* (sift-argv subcommand files
                    #:key
                    (program (sift-command))
                    (arguments '())
                    (dialect #f)
                    (style #f)
                    (brackets #f)
                    (blank-lines #f)
                    (line-length #f)
                    (indent #f)
                    (comment-align? #f)
                    (sort? #f)
                    (write? #f)
                    (check? #f)
                    (diff? #f)
                    (fix? #f)
                    (stdin? #f)
                    (stdout? #f)
                    (label #f)
                    (severity #f)
                    (output-format #f)
                    (checkers '())
                    (exclude-checkers '())
                    (file-filters '())
                    (config #f)
                    (trust-config? #f)
                    (watch? #f)
                    (quiet? #f)
                    (verbose? #f)
                    (force? #f)
                    (no-color? #f))
  "Return the argv that runs sift SUBCOMMAND over FILES, without running it.

FILES is a string or a list of strings, and a directory means every Lisp file
below it, which is sift's own reading of an operand.  ARGUMENTS are appended
after the files as they are, so an option this builder does not name still
reaches sift.

The combinations sift refuses are refused here as well, before anything runs:
`--write' with `--stdout', `--sort' without `--write', and `ci' with `--fix'.

@example
(sift-argv \"check\" \"src\" #:dialect \"scheme\" #:style \"community\")
@result{} (\"sift\" \"check\" \"--dialect\" \"scheme\" \"--style\" \"community\" \"src\")

(sift-argv \"lint\" '(\"src\" \"tests\") #:severity \"warning\"
           #:output-format \"github\")
@end example"
  (unless (and (string? subcommand) (not (string=? subcommand "")))
    (raise-maak-error
     (format #f "sift needs a subcommand, got ~s" subcommand)
     #:hint "Write (sift-ci \"src\"), or (sift \"lint\" \"src\")."))
  (let ((files (string-list files "sift"))
        (arguments (string-list arguments "sift")))
    (when (and write? stdout?)
      (raise-maak-error
       "sift: --write and --stdout ask for two destinations"
       #:hint "Write the files with #:write? #t, or print them with #:stdout? #t."))
    (when (and sort? (not write?))
      (raise-maak-error
       "sift: --sort needs --write"
       #:hint "Add #:write? #t, or drop #:sort?; sorting to standard output would reorder what is printed only."))
    (when (and fix? (string=? subcommand "ci"))
      (raise-maak-error
       "sift: ci does not take --fix"
       #:hint "Run (sift \"fix\" files) and then (sift-ci files), so the pipeline judges the tree it fixed."))
    (append (program-words program)
            (list subcommand)
            (if dialect (list "--dialect" (option-value dialect "sift's dialect")) '())
            (if style (list "--style" (option-value style "sift's style")) '())
            (if brackets (list "--brackets" (option-value brackets "sift's brackets")) '())
            (if blank-lines (list "--blank-lines" (option-value blank-lines "sift's blank-lines")) '())
            (if line-length (list "--line-length" (option-value line-length "sift's line length")) '())
            (if indent (list "--indent" (option-value indent "sift's indent")) '())
            (if comment-align? '("--comment-align") '())
            (if sort? '("--sort") '())
            (if write? '("--write") '())
            (if check? '("--check") '())
            (if diff? '("--diff") '())
            (if fix? '("--fix") '())
            (if stdin? '("--stdin") '())
            (if stdout? '("--stdout") '())
            (if label (list "--label" (option-value label "sift's label")) '())
            (if severity (list "--severity" (option-value severity "sift's severity")) '())
            (if output-format (list "--format" (option-value output-format "sift's output format")) '())
            (if (pair? checkers)
                (list "--checker" (string-join (string-list checkers "sift") ","))
                '())
            (append-map (lambda (name) (list "--exclude-checker" name))
                        (string-list exclude-checkers "sift"))
            (append-map (lambda (glob) (list "--file-filter" glob))
                        (string-list file-filters "sift"))
            (if config (list "--config" (option-value config "sift's config file")) '())
            (if trust-config? '("--trust-config") '())
            (if watch? '("--watch") '())
            (if quiet? '("--quiet") '())
            (if verbose? '("--verbose") '())
            (if force? '("--force") '())
            (if no-color? '("--no-color") '())
            files
            arguments)))

;; ====== Running ======

(define* (sift subcommand files #:key (arguments '()) (verbose? #f))
  "Run sift SUBCOMMAND over FILES, raising when it exits non-zero.

This is the short form: `(sift \"lint\" \"src\")'.  Use `sift-argv' for the
structured options, and one of the gates below when a finding of status 1
should be a task failure with an explanation rather than a bare non-zero exit.

@example
(sift \"explain\" '() #:arguments '(\"SIFT012\"))
@end example"
  (run (sift-argv subcommand files #:arguments arguments) #:verbose? verbose?))

(define* (sift-status argv #:key (verbose? #f))
  "Run the sift ARGV and return its exit status: 0, 1 or 2.

sift's output goes to the terminal as it is written, because the findings are
the answer and capturing them would leave the user with a status to look up.
A dry run runs nothing and answers 0 after recording the command in the plan,
which is what keeps a gate safe to try before it is trusted."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     "sift-status expects a non-empty list of strings, starting with the program"
     #:hint "Build it with sift-argv, which returns exactly that."))
  (when (dry-run?)
    (run argv #:verbose? verbose?))
  (if (dry-run?)
      0
      ;; `run' answers whether the program succeeded and this asks how it
      ;; failed, so the fork is made by `open-pipe*' and the status read from
      ;; `close-pipe'.  Both are the pair `which' already uses, and both keep the
      ;; fork in C: a child forked from a process with fibers of its own would
      ;; inherit their locks, and a `catch' around `execlp' in that child is a
      ;; way to wait forever.  What sift printed is written back out, because
      ;; the findings are the answer.
      (let* ((port (apply open-pipe* OPEN_READ argv))
             (output (get-string-all port))
             (status (close-pipe port)))
        (display output)
        (force-output)
        (or (status:exit-val status) 1))))

(define (sift-exit-meaning code)
  "Return what sift's exit CODE means: `clean', `findings' or `error'.

Zero is `clean'.  One is `findings', which is what the tree answered badly.
Anything else is `error', which for sift is a usage error, an unreadable file or
a file that cannot be parsed.  The decision lives in one place so a gate, a
caller reading a status and this module's test all agree on it."
  (cond
    ((= code 0) 'clean)
    ((= code 1) 'findings)
    (else 'error)))

(define* (sift-gate argv what #:key (verbose? #f))
  "Run the sift ARGV and raise unless it exits 0, naming what a failure means.

WHAT names the job for the message -- \"check\", \"lint\", \"ci\" -- because a
task that fails should say which question the tree answered badly."
  (let ((code (sift-status argv #:verbose? verbose?)))
    (case (sift-exit-meaning code)
      ((clean) #t)
      ((findings)
       (raise-maak-error
        (format #f "sift ~a has something to report" what)
        #:hint (if (string=? what "check")
                   "sift format would change these files; run (sift-format files #:write? #t)."
                   "Read the findings above; (sift \"fix\" files) applies the fixes sift has.")))
      (else
       (raise-maak-error
        (format #f "sift ~a could not read the files (exit code ~a)" what code)
        #:hint "A status of 2 is a usage error, an unreadable file, or a file that cannot be parsed.")))))

;; ====== The jobs ======

(define* (sift-format files
                      #:key (write? #f) (check? #f) (diff? #f)
                      (style #f) (dialect #f) (arguments '()) (verbose? #f))
  "Format FILES, rewriting them with WRITE? and printing them otherwise.

This does not fail on a reformatted file, because reformatting is the job.
With CHECK? nothing is written and a file that would change is a failure, and
with DIFF? the patch is printed instead: the two shapes a CI job wants, and
neither of them touches the tree."
  (sift-gate (sift-argv "format" files
                        #:write? write? #:check? check? #:diff? diff?
                        #:style style #:dialect dialect
                        #:arguments arguments)
             "format" #:verbose? verbose?))

(define* (sift-check files
                     #:key (dialect #f) (style #f) (arguments '()) (verbose? #f))
  "Ask whether FILES are formatted, failing the task when one is not.

This is `sift check': it prints the patch it would make and exits 1, and that
1 becomes a maak error here, so a task named `check' fails the pipeline without
a shell `|| exit 1' to remember."
  (sift-gate (sift-argv "check" files
                        #:dialect dialect #:style style
                        #:arguments arguments)
             "check" #:verbose? verbose?))

(define* (sift-lint files
                    #:key (severity #f) (output-format #f) (arguments '()) (verbose? #f))
  "Lint FILES, failing the task when a rule at SEVERITY or above fires.

SEVERITY is `error', `warning' or `info' -- the smallest one to report, `info'
unless given.  OUTPUT-FORMAT is `human', `json', `null', `github' or `sarif',
and it changes what is printed rather than what is found."
  (sift-gate (sift-argv "lint" files
                        #:severity severity #:output-format output-format
                        #:arguments arguments)
             "lint" #:verbose? verbose?))

(define* (sift-ci files
                  #:key (dialect #f) (style #f) (severity #f)
                  (output-format #f) (arguments '()) (verbose? #f))
  "Ask the formatting question and the lint question in one pass.

This is the pipeline's command: `sift ci' writes nothing, reports both kinds of
finding, and exits 1 if either answers badly.  OUTPUT-FORMAT `github' turns
each finding into a workflow annotation, and `sarif' writes one document for a
code-scanning service."
  (sift-gate (sift-argv "ci" files
                        #:dialect dialect #:style style #:severity severity
                        #:output-format output-format
                        #:arguments arguments)
             "ci" #:verbose? verbose?))

;; The subcommands this module does not wrap -- `doctor', `explain',
;; `list-checkers', `fix', `version' -- are one line each through `sift', which
;; is the door for everything sift does that no job here is named after:
;; (sift "explain" '() #:arguments '("SIFT012")).

;; ====== Finding the files ======

(define sift-extensions
  '(".scm" ".ss" ".sls" ".sld" ".el" ".lisp" ".cl" ".asd"))

(define* (sift-files directory
                     #:key (extensions sift-extensions) (exclude '()))
  "Return the Lisp files under DIRECTORY, sorted, skipping EXCLUDE directories.

sift walks a directory operand itself, so this is for the list a task means
rather than a different walk: a project whose fixtures are deliberately
unformatted passes `#:exclude '(\"fixtures\")', which is the `find ... -not
-path' the Maak files spell out today, and a project with a second dialect in a
corner passes `#:extensions'.

EXCLUDE names directories to prune at any depth; the directories that never
hold source (`node_modules`, `.git` and the like) are pruned already, by the
same walk and the same list `(maak watch)' uses.

@example
(sift-ci (sift-files \"src\"))
(sift-check (append (sift-files \"src\")
                    (sift-files \"tests\" #:exclude '(\"fixtures\"))))
@end example"
  (unless (directory? directory)
    (raise-maak-error
     (format #f "sift-files: ~a is not a directory" directory)
     #:hint "Name a directory of the checkout, or build the list by hand."))
  (sort (walk-files directory #:extensions extensions #:exclude exclude)
        string<?))
