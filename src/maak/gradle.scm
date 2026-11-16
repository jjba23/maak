;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Gradle helpers: the tasks a JVM project runs, as argv a task can inspect.
;;
;; As with Maven and Cargo, the builder is pure and the runner feeds it to
;; `run', so `--dry-run' prints the invocation and `--plan' records it.  The
;; daemon, the build cache and the parallel flag are opt-in keywords because
;; they are decisions about a machine rather than about the project.
;;
;; The program is a parameter, honoured from MAAK_GRADLE then GRADLE, and a
;; checkout's `gradlew' is what `gradle-wrapper-command' answers, because that is
;; the Gradle version the project pins.  A task opts in with
;; `(parameterize ((gradle-command (gradle-wrapper-command))) ...)'.

;;; Code:

(define-module (maak gradle)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (gradle-command
            gradle-wrapper?
            gradle-wrapper-command
            gradle-argv
            gradle
            gradle-output
            gradle-assemble
            gradle-build
            gradle-check
            gradle-test
            gradle-clean
            gradle-run
            gradle-tasks
            gradle-dependencies
            gradle-projects
            gradle-publish))

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

(define gradle-command
  (make-parameter (env-value '("MAAK_GRADLE" "GRADLE") "gradle")))

(define (program-words command what)
  "Return COMMAND as the words of a program invocation, or raise."
  (let ((words (and (string? command)
                    (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "~a must name a program, got ~s" what command)
       #:hint "Set MAAK_GRADLE, or pass the program as a string."))
    words))

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write '(\"clean\" \"build\") for the tasks."))
    items))

(define (executable-file? path)
  (and (file-exists? path) (access? path X_OK)))

(define* (gradle-wrapper? #:optional (directory "."))
  "Return #t when DIRECTORY holds an executable Gradle wrapper."
  (executable-file? (in-vicinity directory "gradlew")))

(define* (gradle-wrapper-command #:optional (directory "."))
  "Return the Gradle command to use in DIRECTORY.

A checkout with an executable `gradlew' answers with it, because that is the
Gradle version the project pins; otherwise the answer is `gradle-command'."
  (if (gradle-wrapper? directory)
      (if (string=? directory ".")
          "./gradlew"
          (in-vicinity directory "gradlew"))
      (gradle-command)))

;; ====== The command line ======

(define (property-words properties)
  "Return PROPERTIES as `-P' words.

An entry is a string such as `\"version=1.0\"', or a pair such as
`(\"version\" . \"1.0\")', which is how an alist already stores it."
  (unless (list? properties)
    (raise-maak-error
     (format #f "gradle's #:properties takes a list, got ~s" properties)
     #:hint "Write '((\"version\" . \"1.0\")) or '(\"version=1.0\")."))
  (map (lambda (entry)
         (cond
          ((pair? entry) (format #f "-P~a=~a" (car entry) (cdr entry)))
          ((string? entry) (string-append "-P" entry))
          (else
           (raise-maak-error
            (format #f "~s is not a Gradle property" entry)
            #:hint "Write a \"name=value\" string or a (name . value) pair."))))
       properties))

(define (choice what value allowed)
  "Return VALUE after checking it is one of ALLOWED."
  (unless (member value allowed)
    (raise-maak-error
     (format #f "gradle's ~a takes ~a, got ~s"
             what (string-join allowed ", ") value)
     #:hint (format #f "Write #:~a \"~a\"." what (car allowed))))
  value)

(define* (gradle-argv tasks
                      #:key
                      (arguments '())
                      (project-dir #f)
                      (build-file #f)
                      (properties '())
                      (exclude-tasks '())
                      (offline? #f)
                      (refresh-dependencies? #f)
                      (no-daemon? #f)
                      (parallel? #f)
                      (build-cache? #f)
                      (rerun-tasks? #f)
                      (continue? #f)
                      (stacktrace? #f)
                      (full-stacktrace? #f)
                      (warning-mode #f)
                      (console #f)
                      (quiet? #f)
                      (info? #f)
                      (debug? #f)
                      (max-workers #f))
  "Return the argv that runs the Gradle TASKS, without running it.

TASKS is a task name or a list of them, as Gradle spells them
(`:module:task` included).  PROJECT-DIR and BUILD-FILE choose the build;
PROPERTIES become `-P' words; EXCLUDE-TASKS become `-x' words.  The daemon,
cache, parallel and rerun flags are keywords, because they decide how a machine
runs the build.  ARGUMENTS are appended as they are.

@example
(gradle-argv '(\"clean\" \"build\")
             #:project-dir \"service\"
             #:properties '((\"version\" . \"1.2.3\"))
             #:no-daemon? #t)
@result{} (\"gradle\" \"--project-dir\" \"service\" \"-Pversion=1.2.3\"
           \"--no-daemon\" \"clean\" \"build\")
@end example"
  (let ((tasks (string-list tasks "gradle"))
        (arguments (string-list arguments "gradle"))
        (properties (property-words properties))
        (excludes (string-list exclude-tasks "gradle")))
    (when (null? tasks)
      (raise-maak-error
       "gradle needs at least one task"
       #:hint "Write (gradle-build), or (gradle \"test\" #:arguments '(\"--tests\" \"Foo\"))."))
    (when (and max-workers
               (not (and (exact-integer? max-workers) (> max-workers 0))))
      (raise-maak-error
       (format #f "gradle takes a positive worker count, got ~s" max-workers)
       #:hint "Write #:max-workers 4, or omit it for Gradle's default."))
    (append
     (program-words (gradle-command) "gradle")
     (if project-dir (list "--project-dir" project-dir) '())
     (if build-file (list "--build-file" build-file) '())
     properties
     (append-map (lambda (task) (list "-x" task)) excludes)
     (if offline? (list "--offline") '())
     (if refresh-dependencies? (list "--refresh-dependencies") '())
     (if no-daemon? (list "--no-daemon") '())
     (if parallel? (list "--parallel") '())
     (if build-cache? (list "--build-cache") '())
     (if rerun-tasks? (list "--rerun-tasks") '())
     (if continue? (list "--continue") '())
     (if stacktrace? (list "--stacktrace") '())
     (if full-stacktrace? (list "--full-stacktrace") '())
     (if warning-mode
         (list "--warning-mode"
               (choice "warning-mode" warning-mode
                       '("all" "fail" "summary" "none")))
         '())
     (if console
         (list "--console"
               (choice "console" console '("plain" "auto" "rich" "verbose")))
         '())
     (if quiet? (list "-q") '())
     (if info? (list "--info") '())
     (if debug? (list "-d") '())
     (if max-workers (list "--max-workers" (number->string max-workers)) '())
     tasks
     arguments)))

(define* (gradle tasks #:rest options)
  "Run the Gradle TASKS, forwarding OPTIONS to `gradle-argv'.

@example
(gradle \"test\" #:arguments '(\"--tests\" \"*Integration*\"))
@end example"
  (run (apply gradle-argv tasks options)))

(define* (gradle-output argv #:key (allow-failure? #f))
  "Run the Gradle ARGV and return its standard output.

A query runs even in a dry run, because it reads the project rather than
changing it."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     "gradle-output expects a non-empty list of strings"
     #:hint "Build it with gradle-argv, which returns exactly that."))
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
       (format #f "gradle was terminated by a signal: ~a" line)
       #:hint "Run the command above by hand to read Gradle's own message."))
     (else
      (raise-maak-error
       (format #f "gradle failed (exit code ~a): ~a" code line)
       #:hint "Run the command above to read Gradle's own message.")))))

;; ====== The common tasks ======

(define (gradle-assemble . arguments)
  "Assemble the project's artefacts, passing ARGUMENTS."
  (gradle "assemble" #:arguments arguments))

(define (gradle-build . arguments)
  "Run the full build, passing ARGUMENTS."
  (gradle "build" #:arguments arguments))

(define (gradle-check . arguments)
  "Run the checks, passing ARGUMENTS."
  (gradle "check" #:arguments arguments))

(define (gradle-test . arguments)
  "Run the tests, passing ARGUMENTS."
  (gradle "test" #:arguments arguments))

(define (gradle-clean . arguments)
  "Remove the build output, passing ARGUMENTS."
  (gradle "clean" #:arguments arguments))

(define (gradle-run . arguments)
  "Run the application task, passing ARGUMENTS."
  (gradle "run" #:arguments arguments))

(define (gradle-tasks . arguments)
  "List the project's tasks, passing ARGUMENTS."
  (gradle "tasks" #:arguments arguments))

(define (gradle-dependencies . arguments)
  "Print the dependency report, passing ARGUMENTS."
  (gradle "dependencies" #:arguments arguments))

(define (gradle-projects . arguments)
  "List the project's modules, passing ARGUMENTS."
  (gradle "projects" #:arguments arguments))

(define (gradle-publish . arguments)
  "Publish the artefacts, passing ARGUMENTS."
  (gradle "publish" #:arguments arguments))
