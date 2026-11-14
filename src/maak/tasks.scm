;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The task description: what a Maak file says its tasks are, read from the file
;; rather than from the loaded module, and printed in the shapes a consumer
;; other than a person can read.
;;
;; Reading is what makes it possible.  A file is a Guile module, and loading it
;; runs whatever its top level does and needs every module it imports; an editor
;; asks about a file somebody is halfway through writing, on a machine that may
;; not have that project's Guile libraries.  So a task is recognised the way the
;; language spells one -- a top-level `define', `define*', or a name bound to a
;; `lambda' or `lambda*' -- the `#:export' list decides which are visible, the
;; docstring is the documentation, and the lambda list is the argument grammar.
;;
;; What the file says beyond the lambda list is a function call, not new syntax:
;; `task-metadata!' declares what a task needs first, whether a listing should
;; offer it and how it is invoked, and this module reads that call the way it
;; reads a definition.  `(maak maak)' evaluates the same call and is what the
;; runner obeys, so the two answers are compared rather than assumed:
;; `contrib/test/maak-tasks.sh' runs both readers over the same file and
;; requires the same bytes.
;;
;;; Code:

(define-module (maak tasks)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (maak-file-in
            read-forms
            static-tasks
            make-task-description
            task-description?
            task-description-name
            task-description-doc
            task-description-arguments
            task-description-line
            task-description-depends
            task-description-hidden?
            task-description-example
            make-argument
            argument?
            argument-name
            argument-kind
            argument-default
            argument-has-default?
            argument-keyword
            argument-doc
            argument-docs
            argument-signature
            tasks-json
            task-names-text
            tasks-fzf-text
            task-preview-text
            task-words-text
            zed-tasks-json))

;; Line numbers come from the reader rather than from a second scan of the text,
;; which is what lets an editor open the file where a task is written.
(read-enable 'positions)

;;; JSON
;;
;; Written by hand: Guile's `(json)' module is not in every build, and the
;; strings here are names, docstrings and Scheme expressions.

(define (json-string text)
  (call-with-output-string
    (lambda (port)
      (write-char #\" port)
      (string-for-each
       (lambda (char)
         (case char
           ((#\") (display "\\\"" port))
           ((#\\) (display "\\\\" port))
           ((#\newline) (display "\\n" port))
           ((#\return) (display "\\r" port))
           ((#\tab) (display "\\t" port))
           (else
            (if (< (char->integer char) 32)
                (format port "\\u~4,'0x" (char->integer char))
                (write-char char port)))))
       text)
      (write-char #\" port))))

(define (json-object pairs)
  (string-append "{"
                 (string-join
                  (map (lambda (pair)
                         (string-append (json-string (car pair)) ":" (cdr pair)))
                       pairs)
                  ",")
                 "}"))

(define (json-array items)
  (string-append "[" (string-join items ",") "]"))

;;; The description a task carries

;; One argument of a task, as the file writes it in the lambda list.  NAME is
;; the word the task binds, KIND is one of required, optional, key or rest,
;; DEFAULT is the default expression as it is written, KEYWORD is the `#:name'
;; a key argument is passed under (empty otherwise), and DOC is the prose the
;; docstring gives it (empty when it gives none).
(define-record-type <argument>
  (make-argument name kind default has-default? keyword doc)
  argument?
  (name argument-name)
  (kind argument-kind)
  (default argument-default)
  (has-default? argument-has-default?)
  (keyword argument-keyword)
  (doc argument-doc))

;; One task, as its Maak file describes it.  NAME is the task's name as a
;; string, DOC its docstring, ARGUMENTS the list of <argument>s the lambda list
;; takes, LINE the line its definition starts on, and DEPENDS, HIDDEN? and
;; EXAMPLE what `task-metadata!' declared about it.
(define-record-type <task-description>
  (make-task-description name doc arguments line depends hidden? example)
  task-description?
  (name task-description-name)
  (doc task-description-doc)
  (arguments task-description-arguments)
  (line task-description-line)
  (depends task-description-depends)
  (hidden? task-description-hidden?)
  (example task-description-example))

;;; Reading the Maak file

(define (maak-file-in directory)
  "Return the path of the maak.scm in DIRECTORY or its parents, or #f."
  (let loop ((dir directory))
    (let ((candidate (in-vicinity dir "maak.scm")))
      (cond ((file-exists? candidate) candidate)
            (else
             (let ((parent (dirname dir)))
               (and (not (string=? parent dir)) (loop parent))))))))

(define (read-forms path)
  "Return the top-level forms of PATH, in file order.

The file is read, not evaluated.  A file that cannot be read to the end is an
error rather than the tasks before the mistake: maak would refuse to load it
too, and the short list would then overwrite a good one wherever it is written
out, which is the one thing a stale edition is good for."
  (call-with-input-file path
    (lambda (port)
      (let loop ((forms '()))
        (catch #t
          (lambda ()
            (let ((form (read port)))
              (if (eof-object? form) (reverse forms) (loop (cons form forms)))))
          (lambda (key . args)
            (raise-maak-error
             (format #f "~a does not read to the end, so its tasks are unknown" path)
             #:hint "Fix the file, and the tasks before the mistake are read again.")))))))

(define (form-line form)
  "Return the line FORM starts on, or 0 when the reader did not say.

The reader counts lines from zero; an editor counts them from one."
  (if (pair? form)
      (1+ (or (assq-ref (source-properties form) 'line) 0))
      0))

(define (docstring body)
  "Return the docstring at the head of BODY, or the empty string."
  (if (and (pair? body) (string? (car body))) (car body) ""))

;;; The prose for an argument
;;
;; A docstring says what a task does; a task taking arguments is also asked
;; what to type for them, and that answer belongs in the same place rather than
;; in a second format beside it.  The convention is the one the library's own
;; docstrings already keep: a line reading `Args:' starts the section, and every
;; line under it names one argument and gives its prose.
;;
;;   Args:
;;     target: the host to ship to, as HOST[:PORT].
;;     env: which environment file to read.
;;
;; A line that names no argument continues the one above it, so prose may wrap,
;; and a blank line ends the section.

(define (argument-name-line line)
  "Return (NAME . PROSE) when LINE names an argument, else #f."
  (let ((colon (string-index line #\:)))
    (and colon
         (let ((name (string-trim (substring line 0 colon)))
               (prose (string-trim (substring line (+ colon 1)))))
           (and (not (string=? name ""))
                (not (string=? prose ""))
                (string-every (lambda (char)
                                (or (char-alphabetic? char)
                                    (char-numeric? char)
                                    (memv char '(#\- #\? #\! #\# #\* #\_))))
                              name)
                (cons name prose))))))

(define (argument-docs doc)
  "Return the argument prose DOC carries, as (NAME . PROSE) pairs, in order.

The section is introduced by a line reading `Args:' and runs to a blank line or
the end of the docstring; see the note above for the shape of an entry."
  (if (not (string? doc))
      '()
      (let section ((lines (string-split (dedent-string doc) #\newline)))
        (cond
          ((null? lines) '())
          ((string-prefix? "Args:" (string-trim (car lines)))
           (let entries ((remaining (cdr lines)) (found '()) (previous #f))
             (cond
               ((or (null? remaining)
                    (string=? (string-trim (car remaining)) ""))
                (reverse found))
               (else
                (let* ((line (string-trim (car remaining)))
                       (pair (argument-name-line line)))
                  (cond
                    (pair (entries (cdr remaining) (cons pair found) pair))
                    ((and previous (pair? found))
                     (entries (cdr remaining)
                              (cons (cons (car previous)
                                          (string-append (cdr previous) " " line))
                                    (cdr found))
                              previous))
                    (else (entries (cdr remaining) found previous))))))))
          (else (section (cdr lines)))))))

;;; Arguments
;;
;; Every form Guile accepts in a lambda list is understood: a bare name, a
;; `(name default)' pair, `#:optional', `#:key', `#:rest', a dotted tail, and
;; `((#:keyword name) default)'.

(define (keyword-name keyword)
  (string-append "#:" (symbol->string (keyword->symbol keyword))))

(define (argument name kind keyword default has-default?)
  (make-argument name kind
                 (if has-default? (object->string default) "")
                 has-default?
                 (or keyword "")
                 ""))

(define (argument-of item kind)
  "Return ITEM as an argument of KIND, or #f when it is not a name."
  (cond
   ((symbol? item)
    (argument (symbol->string item) kind
              (and (eq? kind 'key) (string-append "#:" (symbol->string item)))
              #f #f))
   ((pair? item)
    (let* ((head (car item))
           (has-default? (pair? (cdr item)))
           (default (and has-default? (cadr item)))
           (explicit (and (pair? head) (keyword? (car head)) (car head)))
           (name (cond ((symbol? head) (symbol->string head))
                       ((and (pair? head) (symbol? (cadr head)))
                        (symbol->string (cadr head)))
                       (else #f))))
      (and name
           (argument name kind
                     (cond (explicit (keyword-name explicit))
                           ((eq? kind 'key) (string-append "#:" name))
                           (else #f))
                     default has-default?))))
   (else #f)))

(define (lambda-list-arguments params)
  "Return the arguments of the lambda list PARAMS, in order."
  (let walk ((rest params) (section 'required) (found '()))
    (cond
     ((null? rest) (reverse found))
     ((pair? rest)
      (let ((item (car rest)))
        (cond
         ((eq? item #:optional) (walk (cdr rest) 'optional found))
         ((eq? item #:key) (walk (cdr rest) 'key found))
         ((eq? item #:rest) (walk (cdr rest) 'rest found))
         ;; `#:allow-other-keys' and any other keyword are flags, not names.
         ((keyword? item) (walk (cdr rest) section found))
         (else
          (walk (cdr rest) section
                (let ((found-argument (argument-of item section)))
                  (if found-argument
                      (cons found-argument found)
                      found)))))))
     ;; A dotted tail, `(define (echo . words) ...)'.
     ((symbol? rest)
      (reverse (cons (argument (symbol->string rest) 'rest #f #f #f) found)))
     (else (reverse found)))))

(define (with-argument-docs arguments docs)
  "Return ARGUMENTS with the prose DOCS holds for each of them."
  (map (lambda (found-argument)
         (make-argument (argument-name found-argument)
                        (argument-kind found-argument)
                        (argument-default found-argument)
                        (argument-has-default? found-argument)
                        (argument-keyword found-argument)
                        (or (assoc-ref docs (argument-name found-argument)) "")))
       arguments))

;;; The metadata a task declares
;;
;; A task says what it needs and how it is invoked with a call of its own, after
;; the definition:
;;
;;   (task-metadata! deploy
;;                   #:depends '(build)
;;                   #:example "maak deploy prod.example.com")
;;
;; Reading it is reading one form, and evaluating it -- which `(maak maak)'
;; does -- is what the runner obeys.  The vocabulary is exactly those three
;; fields: a fourth is a decision about what a task is, and this file is not the
;; place to keep one.

(define (metadata-plist form)
  "Return (NAME . PLIST) when FORM declares a task's metadata, else #f."
  (and (pair? form)
       (eq? (car form) 'task-metadata!)
       (pair? (cdr form))
       (symbol? (cadr form))
       (cons (cadr form) (cddr form))))

(define (metadata-value plist key default)
  "Return the value PLIST gives KEY, or DEFAULT."
  (let ((found (and (list? plist) (memq key plist))))
    (if (and found (pair? (cdr found))) (cadr found) default)))

(define (declared-names value)
  "Return the task names VALUE declares, as strings.

A declaration is a quoted list of names, `#:depends '(build fmt)', which is the
only spelling a reader can follow without evaluating it.  Anything else --
an expression a call builds, say -- is left to the loader, whose answer is the
one that decides what a run does."
  (let ((names (and (pair? value)
                    (eq? (car value) 'quote)
                    (pair? (cdr value))
                    (cadr value))))
    (if (list? names)
        (filter-map (lambda (item)
                      (cond ((symbol? item) (symbol->string item))
                            ((string? item) item)
                            (else #f)))
                    names)
        '())))

;;; Tasks

(define (task-of form)
  "Return (NAME DOC ARGUMENTS LINE) when FORM defines a task, else #f.

Only a procedure is a task, spelled either as the shorthand
`(define (name arg ...) body)' -- or `define*' -- or as a name bound to a
`lambda' or `lambda*'.  A curried definition `(define ((f a) b) ...)' and a
value definition define no task."
  (and (pair? form)
       (memq (car form) '(define define*))
       (let ((parts (cdr form)))
         (cond
          ((and (pair? parts) (pair? (car parts)) (symbol? (car (car parts))))
           (let ((signature (car parts)))
             (list (symbol->string (car signature))
                   (docstring (cdr parts))
                   (lambda-list-arguments (cdr signature))
                   (form-line form))))
          ((and (pair? parts)
                (symbol? (car parts))
                (pair? (cdr parts))
                (pair? (cadr parts))
                (memq (car (cadr parts)) '(lambda lambda*))
                (pair? (cdr (cadr parts)))
                (list? (cadr (cadr parts))))
           (let ((procedure (cadr parts)))
             (list (symbol->string (car parts))
                   (docstring (cddr procedure))
                   (lambda-list-arguments (cadr procedure))
                   (form-line form))))
          (else #f)))))

(define (module-exports forms)
  "Return the names the module clause of FORMS exports, or #f when it exports none."
  (let ((module (find (lambda (form)
                        (and (pair? form) (eq? (car form) 'define-module)))
                      forms)))
    (and module
         (let walk ((clauses (cdr module)) (exports '()) (exported? #f))
           (cond
            ((null? clauses) (and exported? exports))
            ((and (eq? (car clauses) #:export) (pair? (cdr clauses)))
             (walk (cddr clauses)
                   (append exports
                           (filter-map (lambda (item)
                                         (and (symbol? item) (symbol->string item)))
                                       (let ((names (cadr clauses)))
                                         (if (list? names) names '()))))
                   #t))
            (else (walk (cdr clauses) exports exported?)))))))

(define (description-of task plist)
  "Return TASK as a <task-description>, with the metadata PLIST declares.

TASK is (NAME DOC ARGUMENTS LINE) as `task-of' reads it."
  (let ((example (metadata-value plist #:example #f)))
    (make-task-description
     (first task)
     (second task)
     (with-argument-docs (third task) (argument-docs (second task)))
     (fourth task)
     (declared-names (metadata-value plist #:depends '()))
     (and (metadata-value plist #:hidden? #f) #t)
     (if (string? example) example ""))))

(define (static-tasks path)
  "Return the tasks PATH defines, in file order, as <task-description>s.

When the module declares exports, only those are described; a module that
exports nothing describes every procedure it defines, which is the rule the
runner applies to the loaded module (see the Tasks page, docs/tasks.org).

A hidden task is described too: it is left out of what a listing prints, but
naming it is still how it is run.  A name defined more than once is described
where its last definition stands, which is the one maak binds."
  (let* ((forms (read-forms path))
         (exports (module-exports forms))
         (metadata (filter-map metadata-plist forms))
         (raw (let loop ((remaining forms) (tasks '()))
                (if (null? remaining)
                    (reverse tasks)
                    (let ((task (task-of (car remaining))))
                      (loop (cdr remaining)
                            (if task
                                (cons task
                                      (remove (lambda (other)
                                                (string=? (car other) (car task)))
                                              tasks))
                                tasks))))))
         (tasks (if (and exports (pair? exports))
                    (filter (lambda (task) (member (car task) exports)) raw)
                    raw)))
    ;; `task-of' keeps the name as a string, and a metadata form is keyed by the
    ;; symbol the file wrote, so the lookup is made on the name it was read as.
    (map (lambda (task)
           (description-of task (assoc-ref metadata (string->symbol (car task)))))
         tasks)))

;;; Printing

(define (visible-tasks tasks)
  "Return TASKS without the ones that ask to be left out of a listing."
  (remove task-description-hidden? tasks))

(define (argument-signature found-argument)
  "Return the command-line spelling of FOUND-ARGUMENT."
  (let ((name (argument-name found-argument))
        (kind (argument-kind found-argument))
        (default (argument-default found-argument))
        (has-default? (argument-has-default? found-argument))
        (keyword (argument-keyword found-argument)))
    (case kind
      ((required) name)
      ((optional) (if has-default?
                      (format #f "[~a=~a]" name default)
                      (format #f "[~a]" name)))
      ((key) (if has-default? (format #f "~a=~a" keyword default) keyword))
      ((rest) (format #f "...~a" name))
      (else name))))

(define (task-signature task)
  "Return the arguments of TASK as one line, the way a command line spells them."
  (string-join (map argument-signature (task-description-arguments task)) " "))

(define (argument-line found-argument)
  "Return one line describing FOUND-ARGUMENT, for `--preview' and the pickers."
  (let ((kind (argument-kind found-argument))
        (default (argument-default found-argument))
        (has-default? (argument-has-default? found-argument))
        (doc (argument-doc found-argument)))
    (string-append
     (format #f "  ~a ~a"
             (let ((keyword (argument-keyword found-argument)))
               (if (string=? keyword "")
                   (argument-name found-argument)
                   keyword))
             (case kind
               ((required) "required")
               ((optional) (if has-default? "optional" "optional, left out when blank"))
               ((key) (if has-default? "keyword" "keyword, which maak cannot pass"))
               ((rest) "rest")
               (else "argument")))
     (if has-default? (format #f " -- default: ~a" default) "")
     (if (string=? doc "") "" (format #f "; ~a" doc)))))

(define (first-line text)
  "Return TEXT up to its first newline."
  (substring text 0 (or (string-index text #\newline) (string-length text))))

(define (docstring-body doc)
  "Return DOC without the argument section a line reading `Args:' introduces.

The arguments are described line by line under the documentation anyway, so a
preview that showed the section as well would say everything twice."
  (let loop ((lines (string-split doc #\newline)) (kept '()))
    (if (null? lines)
        (string-trim-right (string-join (reverse kept) "\n"))
        (if (string-prefix? "Args:" (string-trim (car lines)))
            (string-trim-right (string-join (reverse kept) "\n"))
            (loop (cdr lines) (cons (car lines) kept))))))

(define (task-names-text tasks)
  "Return the names of TASKS, one per line, in file order."
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (task)
                  (display (task-description-name task) port)
                  (newline port))
                (visible-tasks tasks)))))

(define (tasks-fzf-text tasks)
  "Return TASKS as name, signature and first docstring line, one per line."
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (task)
                  (format port "~a\t~a\t~a~%"
                          (task-description-name task)
                          (task-signature task)
                          (first-line (task-description-doc task))))
                (visible-tasks tasks)))))

(define (task-preview-text task)
  "Return the description of TASK, as a picker's preview pane shows it."
  (let ((doc (docstring-body (task-description-doc task)))
        (signature (task-signature task))
        (needs (task-description-depends task))
        (example (task-description-example task))
        (arguments (task-description-arguments task)))
    (string-append
     (task-description-name task)
     (if (string=? signature "") "" (string-append " " signature)) "\n\n"
     (if (string=? doc "")
         ""
         (string-append
          (string-join (map (lambda (line) (string-append "  " line))
                            (string-split doc #\newline))
                       "\n")
          "\n\n"))
     (if (null? needs) ""
         (format #f "  needs: ~a\n" (string-join needs ", ")))
     (if (string=? example "") ""
         (format #f "  example: ~a\n" example))
     (if (null? arguments) ""
         (string-append (string-join (map argument-line arguments) "\n") "\n")))))

(define (task-words-text task)
  "Return TASK's arguments, one tab separated line each, as a picker reads them.

The line is the kind, the name, the default and whether there is one.  The
prose is not part of it: a picker that prompts for an argument shows the name
and the default, and one that wants to say more reads the JSON."
  (call-with-output-string
    (lambda (port)
      (for-each (lambda (found-argument)
                  (format port "~a\t~a\t~a\t~a~%"
                          (argument-kind found-argument)
                          (argument-name found-argument)
                          (argument-default found-argument)
                          (if (argument-has-default? found-argument) 1 0)))
                (task-description-arguments task)))))

(define (argument-json found-argument)
  (json-object
   (list (cons "name" (json-string (argument-name found-argument)))
         (cons "kind" (json-string (symbol->string (argument-kind found-argument))))
         (cons "default" (json-string (argument-default found-argument)))
         (cons "hasDefault" (if (argument-has-default? found-argument) "true" "false"))
         (cons "keyword" (json-string (argument-keyword found-argument)))
         (cons "doc" (json-string (argument-doc found-argument))))))

(define (task-json task)
  (json-object
   (list (cons "name" (json-string (task-description-name task)))
         (cons "doc" (json-string (task-description-doc task)))
         (cons "line" (number->string (task-description-line task)))
         (cons "depends"
               (json-array (map json-string (task-description-depends task))))
         (cons "example" (json-string (task-description-example task)))
         (cons "arguments"
               (json-array (map argument-json (task-description-arguments task)))))))

(define (zed-watch-task)
  "Return the Zed task that keeps a generated list current.

The generated tasks are a snapshot: they are stale the moment the Maak file
changes, and Zed has no hook that could ask again.  `maak-zed watch' rewrites
the task file when the Maak file changes, and Zed reloads that file, so
starting this task once is what keeps the spawn list current while the editor
is open."
  (json-object
   (list (cons "label" (json-string "maak: watch tasks"))
         (cons "command" (json-string "maak-zed"))
         (cons "args" (json-array (list (json-string "watch"))))
         (cons "cwd" (json-string "$ZED_WORKTREE_ROOT"))
         (cons "tags" (json-array (list (json-string "maak"))))
         ;; A watch runs until it is stopped, so it wants a terminal of its
         ;; own instead of the one the last task left behind.
         (cons "use_new_terminal" "true"))))

(define (zed-tasks-json tasks)
  "Return TASKS as the .zed/tasks.json Zed reads.

Zed has no discovery hook: it reads a static tasks file, so this is written by
`maak-tasks --zed', and kept current by `maak-zed watch', which is the last
entry in the list."
  (json-array
   (append
    (map (lambda (task)
           (let ((summary (first-line (task-description-doc task))))
             (json-object
              (list (cons "label"
                          (json-string
                           (if (string=? summary "")
                               (format #f "maak: ~a" (task-description-name task))
                               (format #f "maak: ~a — ~a"
                                       (task-description-name task) summary))))
                    (cons "command" (json-string "maak"))
                    (cons "args"
                          (json-array (list (json-string (task-description-name task)))))
                    (cons "cwd" (json-string "$ZED_WORKTREE_ROOT"))
                    (cons "tags" (json-array (list (json-string "maak"))))))))
         (visible-tasks tasks))
    (list (zed-watch-task)))))

(define (tasks-json tasks path)
  "Return TASKS as one JSON object, naming the file they were read from.

The object is versioned: `version' is 1 for this shape, so a consumer can tell
a description it understands from one written after it.
"
  (json-object
   (list (cons "version" "1")
         (cons "file" (json-string path))
         (cons "root" (json-string (dirname path)))
         (cons "tasks" (json-array (map task-json (visible-tasks tasks)))))))
