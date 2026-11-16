;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Emacs Lisp helpers: the batch commands an Emacs package's automation reaches
;; for, and the conventions a package is expected to keep.
;;
;; `emacs-batch-argv' is the pure builder and `emacs-batch' the runner, so a
;; command is printed by `--dry-run' and recorded by `--plan'.  The higher level
;; `elisp-byte-compile', `elisp-checkdoc' and `elisp-ert' are the three things a
;; package's CI does; they run Emacs with `-Q', so the result does not depend on
;; the machine's init file.
;;
;; The practice helpers read the file rather than run Emacs: a package's version
;; is the `;; Version:' header, its feature is the `(provide ...)' form, and
;; `elisp-practices' answers with the conventions a file is missing.  That makes
;; them cheap enough to call in a test and precise enough to gate a release.
;;
;; The program is a parameter, honoured from MAAK_EMACS then EMACS; a value may
;; carry flags the way a shell would read them, as in
;; `MAAK_EMACS="emacs -nw"'.

;;; Code:

(define-module (maak elisp)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (emacs-command
            emacs-batch-argv
            emacs-batch
            emacs-output
            emacs-capture
            elisp-quote
            elisp-version
            elisp-files
            elisp-byte-compile
            elisp-checkdoc
            elisp-ert
            elisp-header
            elisp-package-version
            elisp-package-requires
            elisp-provides
            elisp-feature
            elisp-lexical-binding?
            elisp-footer?
            elisp-practices
            elisp-practices-ok?))

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

(define emacs-command
  (make-parameter (env-value '("MAAK_EMACS" "EMACS") "emacs")))

(define (program-words command what)
  "Return COMMAND as the words of a program invocation, or raise."
  (let ((words (and (string? command)
                    (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "~a must name a program, got ~s" what command)
       #:hint "Set MAAK_EMACS, or pass the program as a string."))
    words))

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write '(\"test/foo-tests.el\") for the files."))
    items))

;; ====== The Emacs batch command line ======

(define* (emacs-batch-argv #:key
                           (quick? #t)
                           (load-paths '())
                           (load-files '())
                           (evals '())
                           (functions '())
                           (arguments '()))
  "Return the argv that runs Emacs in batch, without running it.

QUICK? adds `-Q', which keeps the site file and the init file out of the run;
it is on by default because a package's CI should not depend on the machine's
configuration.  LOAD-PATHS become `-L' words and LOAD-FILES `-l' words; EVALS
become `--eval' and FUNCTIONS `-f'.  ARGUMENTS are appended last, where a
function such as `batch-byte-compile' reads the files it was given.

@example
(emacs-batch-argv #:load-paths '(\".\") #:evals '(\"(princ emacs-version)\"))
@result{} (\"emacs\" \"--batch\" \"-Q\" \"-L\" \".\" \"--eval\" \"(princ emacs-version)\")
@end example"
  (let ((load-paths (string-list load-paths "emacs-batch"))
        (load-files (string-list load-files "emacs-batch"))
        (evals (string-list evals "emacs-batch"))
        (functions (string-list functions "emacs-batch"))
        (arguments (string-list arguments "emacs-batch")))
    (append
     (program-words (emacs-command) "Emacs")
     (list "--batch")
     (if quick? (list "-Q") '())
     (append-map (lambda (path) (list "-L" path)) load-paths)
     (append-map (lambda (file) (list "-l" file)) load-files)
     (append-map (lambda (expression) (list "--eval" expression)) evals)
     (append-map (lambda (function) (list "-f" function)) functions)
     arguments)))

(define* (emacs-batch #:rest options)
  "Run Emacs in batch, forwarding OPTIONS to `emacs-batch-argv'.

@example
(emacs-batch #:functions '(\"batch-byte-compile\")
             #:arguments '(\"foo.el\"))
@end example"
  (run (apply emacs-batch-argv options)))

(define* (emacs-output argv #:key (allow-failure? #f))
  "Run the Emacs batch ARGV and return its standard output.

A query runs even in a dry run, because it reads the project rather than
changing it.  An expression that prints with `princ' is what puts a value on
this output; `message' writes to standard error."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     "emacs-output expects a non-empty list of strings"
     #:hint "Build it with emacs-batch-argv, which returns exactly that."))
  (let* ((line (argv->string argv))
         (port (open-input-pipe line))
         (output (get-string-all port))
         (status (close-pipe port))
         (code (status:exit-val status)))
    (cond
     ((and code (zero? code)) output)
     (allow-failure? #f)
     ((not code)
      (raise-maak-error
       (format #f "emacs was terminated by a signal: ~a" line)
       #:hint "Run the command above by hand to read Emacs' own message."))
     (else
      (raise-maak-error
       (format #f "emacs failed (exit code ~a): ~a" code line)
       #:hint "Run the command above to read Emacs' own message.")))))

(define* (emacs-capture #:rest options)
  "Run Emacs in batch with OPTIONS and return its standard output.

This is the keyword form of `emacs-output':
`(emacs-capture #:evals '(\"(princ emacs-version)\"))'."
  (emacs-output (apply emacs-batch-argv options)))

(define (elisp-quote text)
  "Return TEXT as an Emacs Lisp string literal.

The quotes and backslashes are escaped, so a selector or a path reaches the
`--eval' expression as data rather than as more Lisp."
  (unless (string? text)
    (raise-maak-error
     (format #f "an Emacs Lisp string needs a string, got ~s" text)
     #:hint "Write the selector as a string, for example \"t\"."))
  (call-with-output-string
    (lambda (port)
      (write-char #\" port)
      (string-for-each (lambda (char)
                         (when (memv char '(#\" #\\))
                           (write-char #\\ port))
                         (write-char char port))
                       text)
      (write-char #\" port))))

(define (elisp-version)
  "Return the Emacs version the configured program reports."
  (string-trim-both
   (emacs-capture #:evals '("(princ emacs-version)"))))

;; ====== The files, and the three checks ======

(define (file-is-directory? path)
  (catch #t
    (lambda () (eq? 'directory (stat:type (stat path))))
    (lambda args #f)))

(define (elisp-files-under directory)
  "Return the .el files under DIRECTORY, recursively."
  (let loop ((entries (scandir directory
                               (lambda (name)
                                 (not (member name '("." ".."))))))
             (found '()))
    (if (null? entries)
        found
        (let* ((name (car entries))
               (path (in-vicinity directory name)))
          (cond
           ((string-prefix? "." name)
            (loop (cdr entries) found))
           ((file-is-directory? path)
            (loop (cdr entries) (append (elisp-files-under path) found)))
           ((string-suffix? ".el" name)
            (loop (cdr entries) (cons path found)))
           (else (loop (cdr entries) found)))))))

(define* (elisp-files #:optional (directory "."))
  "Return every `.el' file under DIRECTORY, recursively, sorted.

Hidden directories and their contents are skipped, which leaves a checkout's
`.git' and an editor's cache out of the list."
  (unless (file-exists? directory)
    (raise-maak-error
     (format #f "no such file or directory: ~a" directory)
     #:hint "Name the directory that holds the Emacs Lisp sources."))
  (if (file-is-directory? directory)
      (sort (elisp-files-under directory) string<?)
      (list directory)))

(define* (elisp-byte-compile files
                             #:key (load-paths '()) (warnings-as-errors? #f))
  "Byte-compile FILES in batch, with LOAD-PATHS on the load path.

With WARNINGS-AS-ERRORS? a warning from the compiler fails the command, which is
what a CI gate wants; the default reports the warnings and succeeds."
  (let ((files (string-list files "elisp-byte-compile")))
    (when (null? files)
      (raise-maak-error
       "elisp-byte-compile needs at least one file"
       #:hint "Write (elisp-byte-compile (elisp-files \"lisp\"))."))
    (emacs-batch #:quick? #t
                 #:load-paths load-paths
                 #:evals (if warnings-as-errors?
                             '("(setq byte-compile-error-on-warn t)")
                             '())
                 #:functions '("batch-byte-compile")
                 #:arguments files)))

(define* (elisp-checkdoc files #:key (load-paths '()))
  "Run `checkdoc' on FILES in batch, with LOAD-PATHS on the load path.

checkdoc reports; it does not fail the command.  A gate that has to fail is
`elisp-byte-compile' with `#:warnings-as-errors? #t'."
  (let ((files (string-list files "elisp-checkdoc")))
    (when (null? files)
      (raise-maak-error
       "elisp-checkdoc needs at least one file"
       #:hint "Write (elisp-checkdoc (elisp-files \"lisp\"))."))
    (emacs-batch
     #:quick? #t
     #:load-paths load-paths
     #:evals (list
              (string-append
               "(progn (require 'checkdoc)"
               " (setq enable-local-variables nil)"
               " (dolist (file command-line-args-left)"
               " (with-current-buffer (find-file-noselect file)"
               " (checkdoc-current-buffer t))))"))
     #:arguments files)))

(define* (elisp-ert test-files #:key (load-paths '()) (selector "t"))
  "Load TEST-FILES and run the ERT tests matching SELECTOR in batch.

SELECTOR is ERT's selector as an Emacs Lisp expression, `t` by default, so every
test runs.  A string selector is written quoted for Lisp -- `\"\\\"foo\\\"\"` --
and a tag selector as the form it is, `\"(tag smoke)\"`.  The expression is
inserted as written, which is safe because it comes from the Maak file rather
than from a report.  Emacs exits non-zero when a test fails."
  (let ((files (string-list test-files "elisp-ert")))
    (when (null? files)
      (raise-maak-error
       "elisp-ert needs at least one test file"
       #:hint "Write (elisp-ert (elisp-files \"test\"))."))
    (unless (and (string? selector) (not (string=? selector "")))
      (raise-maak-error
       (format #f "elisp-ert takes the selector as a string, got ~s" selector)
       #:hint "Write #:selector \"t\", or #:selector \"(tag smoke)\"."))
    (emacs-batch
     #:quick? #t
     #:load-paths load-paths
     #:load-files files
     #:evals (list
              (format #f "(progn (require 'ert) (ert-run-tests-batch-and-exit ~a))"
                      selector)))))

;; ====== The practices ======

(define (slurp path)
  "Return the whole of PATH, or raise when it cannot be read."
  (unless (file-exists? path)
    (raise-maak-error
     (format #f "no such file: ~a" path)
     #:hint "Name an Emacs Lisp source file."))
  (call-with-input-file path get-string-all))

(define (elisp-header path field)
  "Return the value of the `;; FIELD:' header in PATH, or #f.

This is how a package's version and requirements are written in the file
itself, above the commentary."
  (let ((prefix (string-append ";; " field ":")))
    (let loop ((lines (string-split (slurp path) #\newline)))
      (cond
       ((null? lines) #f)
       ((string-prefix? prefix (string-trim-both (car lines)))
        (let ((line (string-trim-both (car lines))))
          (string-trim-both (substring line (string-length prefix)))))
       (else (loop (cdr lines)))))))

(define (elisp-package-version path)
  "Return the `;; Version:' header of PATH, or #f."
  (elisp-header path "Version"))

(define (elisp-package-requires path)
  "Return the `;; Package-Requires:' header of PATH as it is written, or #f."
  (elisp-header path "Package-Requires"))

(define (elisp-provides path)
  "Return the features PATH provides, as written by `(provide ...)'."
  (map (lambda (found) (match:substring found 1))
       (list-matches "provide[ \t]+['\"]?([A-Za-z0-9_+./-]+)"
                     (slurp path))))

(define (elisp-feature path)
  "Return the first feature PATH provides, or #f."
  (let ((features (elisp-provides path)))
    (if (pair? features) (car features) #f)))

(define (elisp-lexical-binding? path)
  "Return #t when PATH sets lexical-binding on its first line."
  (let ((first-line (car (string-split (slurp path) #\newline))))
    (and (string-match "lexical-binding:[ \t]*t" first-line) #t)))

(define (elisp-footer? path)
  "Return #t when PATH carries its `;;; FILE ends here' footer."
  (let ((expected (format #f ";;; ~a ends here" (basename path))))
    (any (lambda (line) (string=? (string-trim-both line) expected))
         (string-split (slurp path) #\newline))))

(define (elisp-practices path)
  "Return the conventions PATH is missing, as a list of sentences.

The list is empty when the file keeps them all.  The checks are the parts of
the Emacs Lisp packaging conventions that a reader can see without running
anything: lexical binding, the version header, the commentary and code
sections, the feature it provides, and the footer."
  (let ((text (slurp path)))
    (filter-map identity
                (list
                 (if (elisp-lexical-binding? path)
                     #f
                     "the first line does not set lexical-binding")
                 (if (elisp-package-version path)
                     #f
                     "there is no `;; Version:' header")
                 (if (string-contains text ";;; Commentary:")
                     #f
                     "there is no `;;; Commentary:' section")
                 (if (string-contains text ";;; Code:")
                     #f
                     "there is no `;;; Code:' section")
                 (if (pair? (elisp-provides path))
                     #f
                     "there is no `(provide ...)' form")
                 (if (elisp-footer? path)
                     #f
                     "there is no `;;; FILE ends here' footer")))))

(define (elisp-practices-ok? path)
  "Return #t when PATH keeps every convention `elisp-practices' checks."
  (null? (elisp-practices path)))
