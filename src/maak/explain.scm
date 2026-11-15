;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; `maak explain TASK' answers the question a run leaves implicit: where the
;; task sits in the composition, how the environment it reads was put together,
;; and which environment file had the last word.  Nothing here runs a task.
;;
;; The composition is read from the Maak file rather than observed: every call
;; to a task name inside another task's body is an edge, so the graph is what
;; the file says, not a trace of one run.  A call hidden behind `apply', a
;; higher-order procedure or a macro is not seen; the page that documents
;; `explain' says so.
;;
;; The environment is reported from the same files a run would read, in the
;; same order, and against the process environment the run started with.  The
;; rule is `load-env-file''s: the process wins, and otherwise the first file
;; that sets a name wins because later files do not override.  The report says
;; which file that was and what the ones after it tried to say.

;;; Code:

(define-module (maak explain)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (environment-alist environment-report explain-text))

(define (environment-alist environment-strings)
  "Return ENVIRONMENT-STRINGS as an alist of `(NAME . VALUE)'.

ENVIRONMENT-STRINGS is what `(environment)' returns: one `NAME=VALUE' string
per variable.  A string without an `=' is skipped rather than guessed at."
  (filter-map (lambda (entry)
                (let ((separator (string-index entry #\=)))
                  (and separator
                       (cons (substring entry 0 separator)
                             (substring entry (+ separator 1))))))
              environment-strings))

(define (environment-report files base-env)
  "Return how the environment variables FILES mention would be resolved.

FILES are the environment files a run would read, in order; BASE-ENV is the
process environment before any of them.  Each answer is
`(NAME VALUE SOURCE IGNORED)', where SOURCE is `\"process environment\"' or the
file that won, and IGNORED is a list of `(FILE . VALUE)' the other files said.

The precedence is `load-env-file''s: the process environment wins, and among
files the first one to set a name wins, because later files do not override.
A variable no file mentions is not reported at all, so the answer never dumps
an unrelated environment."
  (define (collect remaining chosen mentions)
    ;; CHOSEN is ((NAME VALUE SOURCE) ...) in first-seen order, and MENTIONS is
    ;; ((NAME FILE VALUE) ...) in file order, winner and losers alike.
    (if (null? remaining)
        (cons chosen mentions)
        (let ((file (car remaining)))
          (let entries ((remaining-entries (env-file-entries file))
                        (chosen chosen)
                        (mentions mentions))
            (if (null? remaining-entries)
                (collect (cdr remaining) chosen mentions)
                (let* ((entry (car remaining-entries))
                       (name (car entry))
                       (value (cdr entry))
                       (process (assoc name base-env)))
                  (entries (cdr remaining-entries)
                           (cond
                             ((assoc name chosen) chosen)
                             (process
                              (append chosen
                                      (list (list name (cdr process)
                                                  "process environment"))))
                             (else
                              (append chosen (list (list name value file)))))
                           (cons (list name file value) mentions))))))))
  (let* ((collected (collect files '() '()))
         (chosen (car collected))
         (mentions (cdr collected)))
    (map (lambda (entry)
           (let ((name (car entry))
                 (value (cadr entry))
                 (source (caddr entry)))
             (list name value source
                   (filter-map (lambda (mention)
                                 (and (string=? (car mention) name)
                                      ;; The file that won is the source, not an
                                      ;; ignored mention of the same name.
                                      (not (and (not (string=? source "process environment"))
                                                (string=? (cadr mention) source)))
                                      ;; A file that says the same thing as the
                                      ;; winner did not lose anything.
                                      (not (string=? (caddr mention) value))
                                      (cons (cadr mention) (caddr mention))))
                               mentions))))
         chosen)))

(define (indent-lines lines)
  "Return LINES with two spaces in front of every non-empty one."
  (map (lambda (line)
         (if (string=? line "") "" (string-append "  " line)))
       lines))

(define (doc-body-lines doc)
  "Return DOC without its first line, dedented; the empty list when there is none."
  (let* ((text (and (string? doc) (dedent-string doc)))
         (lines (if (and text (not (string=? text "")))
                    (string-split text #\newline)
                    '())))
    (map string-trim-right (if (null? lines) '() (cdr lines)))))

(define (first-doc-line doc)
  "Return the first line of DOC, or the empty string."
  (if (and doc (not (string=? doc "")))
      (car (string-split (dedent-string doc) #\newline))
      ""))

(define (environment-report-line entry)
  "Return one environment variable from a report ENTRY as a line."
  (let* ((name (car entry))
         (value (cadr entry))
         (source (caddr entry))
         (ignored (cadddr entry))
         (origin (if (string=? source "process environment")
                     "from the process environment"
                     (format #f "from ~a" source)))
         (others (filter-map (lambda (mention)
                               (let ((file (car mention))
                                     (other (cdr mention)))
                                 (and (not (string=? other value))
                                      (format #f "~a said ~a" file other))))
                             ignored)))
    (format #f "  ~a=~a (~a~a)"
            name value origin
            (if (null? others)
                ""
                (format #f "; ~a" (string-join others ", "))))))

(define (environment-lines files report)
  "Return the environment section of the explanation."
  (append
   (list (if (null? files)
             "environment files: none (no --env-file, MAAK_ENV_FILE or .env)"
             "environment files, in order (the process environment wins; otherwise the first file that sets a name wins):"))
   (map (lambda (file)
          (format #f "  ~a~a" file
                  (if (file-exists? file) "" " (missing)")))
        files)
   (if (null? report)
       (list "environment: no variable this task reads is set by a file")
       (cons "environment:"
             (map environment-report-line report)))))

(define* (explain-text #:key task file doc needs example calls called-by
                       env-files env-report load-paths elevation)
  "Return the explanation of TASK as the text a person reads.

TASK is its name as a string, FILE the Maak file, DOC its docstring, NEEDS the
dependencies it declared and EXAMPLE the invocation it declared.  CALLS and
CALLED-BY are the names from the static call graph, ENV-FILES and ENV-REPORT
are what `environment-report' answered, LOAD-PATHS the resolved load path and
ELEVATION the privilege escalation command in use."
  (let* ((summary (first-doc-line doc))
         (body (doc-body-lines doc))
         (names (lambda (items)
                  (if (null? items)
                      "—"
                      (string-join (map (lambda (item)
                                          (if (symbol? item)
                                              (symbol->string item)
                                              item))
                                        items)
                                   ", ")))))
    (string-append
     (format #f "maak explain: ~a\n" task)
     (format #f "file: ~a\n\n" file)
     (format #f "~a~a\n" task
             (if (string=? summary "") "" (format #f " — ~a" summary)))
     (if (null? body)
         ""
         (string-append
          (string-join (indent-lines body) "\n")
          "\n"))
     (format #f "needs: ~a\n" (names needs))
     (format #f "example: ~a\n" (if (string=? example "") "—" example))
     (format #f "calls (from the file): ~a\n" (names calls))
     (format #f "called by (from the file): ~a\n\n" (names called-by))
     (format #f "load paths:\n~a\n\n"
             (if (null? load-paths)
                 "  (none)"
                 (string-join (map (lambda (path) (format #f "  ~a" path))
                                   load-paths)
                              "\n")))
     (format #f "elevation: ~a\n\n" elevation)
     (string-join (environment-lines env-files env-report) "\n")
     "\n")))
