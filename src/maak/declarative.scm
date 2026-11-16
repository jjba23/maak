;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Task files in JSON, TOML and YAML: the same tasks a Maak file defines, for a
;; project that wants a list and none of the language.
;;
;; A data file describes less than Scheme does, and the difference is the point.
;; A task is a name, a docstring, one or more commands, and the handful of
;; declarations `task-metadata!' already carries.  There are no arguments, no
;; conditions and no loops; a project that needs those writes the Maak file,
;; where the whole language is available.
;;
;; Reading a data file is reading data and then building the definitions maak
;; runs.  Nothing in the file becomes source text: the commands are strings in a
;; list, the environment is an alist, and the form that is evaluated is built
;; here from validated values.  A string command is a shell line, because the
;; file said so; a list is a program and its arguments, run without a shell.
;;
;; The task order is the file's order, because the parsers keep the order of a
;; mapping.  A dependency is a name another task in the same file must have
;; finished before, as `#:depends' is in a Maak file.

;;; Code:

(define-module (maak declarative)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak formats)
  #:use-module (maak tasks)
  #:export (declarative-file?
            declarative-task-files
            read-task-file
            load-task-file
            task-file-descriptions
            task-file-call-graph
            task-file-task?
            task-file-task-name
            task-file-task-doc
            task-file-task-depends
            task-file-task-hidden?
            task-file-task-example))

;; A task as a data file describes it.  COMMANDS is the list of commands in the
;; order they run, each `(shell . LINE)' or `(argv . (PROGRAM ARGUMENT ...))'.
;; ENV is an alist of string pairs, DIRECTORY the directory to run in or #f,
;; and the rest are what they are in a Maak file.
(define-record-type <task-file-task>
  (make-task-file-task name doc commands depends env directory hidden? example
                       allow-failure?)
  task-file-task?
  (name task-file-task-name)
  (doc task-file-task-doc)
  (commands task-file-task-commands)
  (depends task-file-task-depends)
  (env task-file-task-env)
  (directory task-file-task-directory)
  (hidden? task-file-task-hidden?)
  (example task-file-task-example)
  (allow-failure? task-file-task-allow-failure?))

(define (declarative-file? path)
  "Return #t when PATH is a task file in a format maak reads."
  (and (data-file-format path) #t))

(define declarative-task-files
  '("maak.yaml" "maak.yml" "maak.toml" "maak.json"))

;; ====== Errors ======

(define* (task-file-error path message #:key (hint #f))
  "Raise a maak error about PATH, naming the file the message is about."
  (raise-maak-error (format #f "~a: ~a" path message) #:hint hint))

(define (name-error path name message)
  (task-file-error path (format #f "task ~a: ~a" name message)
                   #:hint "Check the task in the file; the schema is on docs/task-files.org."))

;; ====== Reading the schema ======

(define (string-value path name key value)
  "Return VALUE after checking it is a string."
  (unless (string? value)
    (name-error path name (format #f "~a must be a string, got ~s" key value)))
  value)

(define (boolean-value path name key value)
  "Return VALUE after checking it is a boolean."
  (unless (boolean? value)
    (name-error path name (format #f "~a must be true or false, got ~s" key value)))
  value)

(define (string-list path name key value)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (name-error path name
                  (format #f "~a must be a list of strings, got ~s" key value)))
    items))

(define (command-entry path name value)
  "Return VALUE as one command: `(shell . LINE)' or `(argv . ARGV)'."
  (cond
   ((string? value) (cons 'shell value))
   ((and (list? value) (pair? value) (every string? value))
    (cons 'argv value))
   (else
    (name-error path name
                (format #f
                        "a command is a shell line or a list of strings, got ~s"
                        value)))))

(define (commands-of path name spec)
  "Return the commands SPEC describes, refusing none and refusing both keys."
  (let ((command (assoc "command" spec))
        (commands (assoc "commands" spec)))
    (when (and command commands)
      (name-error path name "command and commands cannot both be given"))
    (cond
     (command (list (command-entry path name (cdr command))))
     (commands
      (let ((value (cdr commands)))
        (unless (and (list? value) (pair? value))
          (name-error path name "commands must be a non-empty list"))
        (map (lambda (entry) (command-entry path name entry)) value)))
     (else
      (name-error path name "a task needs a command or a commands list")))))

(define (env-of path name spec)
  "Return the environment SPEC describes, as an alist of string pairs."
  (let ((value (assoc-ref spec "env")))
    (cond
     ((not value) '())
     ((not (data-mapping? value))
      (name-error path name "env must be a mapping of names to values"))
     (else
      (map (lambda (entry)
             (let ((key (car entry))
                   (value (cdr entry)))
               (unless (string? value)
                 (name-error path name
                             (format #f "env ~a must be a string, got ~s"
                                     key value)))
               (cons key value)))
           value)))))

(define (task-name path value)
  "Return VALUE after checking it is a name a command line can spell."
  (unless (and (string? value)
               (not (string=? value ""))
               (or (char-alphabetic? (string-ref value 0))
                   (char=? (string-ref value 0) #\_))
               (string-every (lambda (char)
                               (or (char-alphabetic? char)
                                   (char-numeric? char)
                                   (memv char '(#\- #\_))))
                             value))
    (task-file-error
     path
     (format #f "~s is not a task name" value)
     #:hint "A task name starts with a letter or an underscore and holds only letters, digits, `-' and `_'."))
  value)

(define (task-of-spec path name spec)
  "Return the task NAME and SPEC describe."
  (unless (data-mapping? spec)
    (name-error path name (format #f "a task must be a mapping, got ~s" spec)))
  (let* ((allowed '("name" "doc" "command" "commands" "depends" "env"
                    "directory" "hidden" "example" "allow-failure"))
         (unknown (remove (lambda (key) (member key allowed)) (data-keys spec))))
    (unless (null? unknown)
      (name-error path name
                  (format #f "unknown ~a ~a"
                          (if (null? (cdr unknown)) "key" "keys")
                          (string-join unknown ", "))))
    (let ((declared-name (assoc-ref spec "name")))
      (when (and declared-name (not (string=? declared-name name)))
        (name-error path name
                    (format #f "the mapping key and the name ~s disagree"
                            declared-name))))
    (let ((doc (assoc-ref spec "doc"))
          (depends (assoc-ref spec "depends"))
          (directory (assoc-ref spec "directory"))
          (hidden (assoc-ref spec "hidden"))
          (example (assoc-ref spec "example"))
          (allow-failure (assoc-ref spec "allow-failure")))
      (make-task-file-task
       (task-name path name)
       (if doc (string-value path name "doc" doc) "")
       (commands-of path name spec)
       (if depends (string-list path name "depends" depends) '())
       (env-of path name spec)
       (if directory (string-value path name "directory" directory) #f)
       (if hidden (boolean-value path name "hidden" hidden) #f)
       (if example (string-value path name "example" example) "")
       (if allow-failure
           (boolean-value path name "allow-failure" allow-failure)
           #f)))))

(define (tasks-of-value path value)
  "Return the tasks the `tasks' VALUE describes, in file order."
  (cond
   ((data-mapping? value)
    (map (lambda (entry)
           (task-of-spec path (car entry) (cdr entry)))
         value))
   ((and (list? value) (every data-mapping? value))
    (map (lambda (spec)
           (let ((name (assoc-ref spec "name")))
             (unless name
               (task-file-error path "a task in the list has no name"))
             (task-of-spec path name spec)))
         value))
   (else
    (task-file-error
     path
     "tasks must be a mapping of names to task tables, or a list of task tables"
     #:hint "Write `tasks: { build: { command: [cargo, build] } }' in YAML, or a table per task in TOML."))))

(define (read-task-file path)
  "Return the tasks PATH describes, as `<task-file-task>' values in file order.

The schema is closed: an unknown key is an error rather than something ignored,
because a task file that says something maak does not read is a task file whose
author believes something false."
  (let ((data (read-data-file path)))
    (unless (data-mapping? data)
      (task-file-error path "a task file must be a mapping at the top level"))
    (let* ((version (assoc-ref data "version"))
           (allowed '("version" "tasks"))
           (unknown (remove (lambda (key) (member key allowed)) (data-keys data))))
      (unless (null? unknown)
        (task-file-error path
                         (format #f "unknown top-level ~a ~a"
                                 (if (null? (cdr unknown)) "key" "keys")
                                 (string-join unknown ", "))))
      (when (and version (not (equal? version 1)))
        (task-file-error path
                         (format #f "version ~s is not one maak reads" version)
                         #:hint "Leave `version' out, or write `version: 1'.")))
    (let ((tasks (assoc-ref data "tasks")))
      (unless tasks
        (task-file-error path "a task file needs a tasks table"))
      (let ((described (tasks-of-value path tasks)))
        (let ((duplicates
               (filter (lambda (name)
                         (> (length (filter (lambda (other)
                                              (string=? name (task-file-task-name other)))
                                            described))
                            1))
                       (map task-file-task-name described))))
          (when (pair? duplicates)
            (task-file-error path
                             (format #f "task ~a is defined twice"
                                     (car duplicates)))))
        described))))

;; ====== Building the tasks maak runs ======

(define (command-form task entry)
  "Return the form that runs one command of TASK."
  (let ((env (task-file-task-env task))
        (allow-failure? (task-file-task-allow-failure? task)))
    (case (car entry)
      ((shell)
       `($ (list ,(cdr entry))
           #:env (quote ,env)
           #:allow-failure? ,allow-failure?))
      (else
       `(run (quote ,(cdr entry))
             #:env (quote ,env)
             #:allow-failure? ,allow-failure?)))))

(define (body-form task)
  "Return the form that is the body of TASK."
  (let* ((forms (map (lambda (entry) (command-form task entry))
                     (task-file-task-commands task)))
         (body (if (null? (cdr forms))
                   (car forms)
                   (cons 'begin forms))))
    (if (task-file-task-directory task)
        `(with-directory ,(task-file-task-directory task)
                         (lambda () ,body))
        body)))

(define (task-forms task)
  "Return the forms that define and describe TASK."
  (let* ((name (string->symbol (task-file-task-name task)))
         (definition `(define (,name) ,(task-file-task-doc task)
                        ,(body-form task)))
         (depends (task-file-task-depends task))
         (hidden? (task-file-task-hidden? task))
         (example (task-file-task-example task)))
    (if (or (pair? depends) hidden? (not (string=? example "")))
        (list definition
              `(task-metadata! ,name
                               #:depends (quote ,(map string->symbol depends))
                               #:hidden? ,hidden?
                               #:example ,example))
        (list definition))))

(define (load-task-file path)
  "Define PATH's tasks in module `(maak)' and return their names, in file order.

The definitions are built from the data, not read as text: a command is a list
of strings that `run' executes without a shell, or one string that `$' hands to
the shell because the file asked for a shell line."
  (let ((tasks (read-task-file path)))
    (let ((module (resolve-module '(maak) #:ensure #t)))
      (module-use! module (resolve-module '(maak maak)))
      (for-each (lambda (task)
                  (for-each (lambda (form) (eval form module))
                            (task-forms task)))
                tasks))
    (map task-file-task-name tasks)))

;; ====== What a listing and an explanation read ======

(define (task-file-descriptions path)
  "Return PATH's tasks as `<task-description>' values, in file order."
  (map (lambda (task)
         (make-task-description
          (task-file-task-name task)
          (task-file-task-doc task)
          '()
          0
          (task-file-task-depends task)
          (task-file-task-hidden? task)
          (task-file-task-example task)))
       (read-task-file path)))

(define (task-file-call-graph path)
  "Return the calls PATH declares, as the alist `static-call-graph' returns.

A data file has no body to read, so the only edge is a `depends' entry.
"
  (map (lambda (task)
         (cons (string->symbol (task-file-task-name task))
               (map string->symbol (task-file-task-depends task))))
       (read-task-file path)))
