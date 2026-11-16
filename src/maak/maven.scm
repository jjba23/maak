;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Maven helpers: the lifecycle a Java project runs, as argv a task can inspect.
;;
;; There is no build engine here.  `maven-argv' is a pure builder and `maven'
;; feeds its list to `run', so `--dry-run' prints the invocation and `--plan'
;; records it.  An option this builder does not name travels as an argument,
;; which is how a plugin goal maak has never heard of is still a Maven goal.
;;
;; The program is a parameter, honoured from MAAK_MAVEN then MAVEN, and a value
;; may carry flags the way a shell would read them.  `maven-wrapper-command'
;; answers with `./mvnw' when a checkout has one, which is the version of Maven
;; the project itself pins; a task opts in with
;; `(parameterize ((maven-command (maven-wrapper-command))) ...)'.

;;; Code:

(define-module (maak maven)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (maven-command
            maven-wrapper?
            maven-wrapper-command
            maven-argv
            maven
            maven-output
            maven-clean
            maven-validate
            maven-compile
            maven-test
            maven-package
            maven-verify
            maven-install
            maven-deploy
            maven-site
            maven-dependency-tree
            maven-effective-pom))

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

(define maven-command
  (make-parameter (env-value '("MAAK_MAVEN" "MAVEN") "mvn")))

(define (program-words command what)
  "Return COMMAND as the words of a program invocation, or raise."
  (let ((words (and (string? command)
                    (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "~a must name a program, got ~s" what command)
       #:hint "Set MAAK_MAVEN, or pass the program as a string."))
    words))

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write '(\"clean\" \"package\") for the goals."))
    items))

(define (executable-file? path)
  (and (file-exists? path) (access? path X_OK)))

(define* (maven-wrapper? #:optional (directory "."))
  "Return #t when DIRECTORY holds an executable Maven wrapper."
  (executable-file? (in-vicinity directory "mvnw")))

(define* (maven-wrapper-command #:optional (directory "."))
  "Return the Maven command to use in DIRECTORY.

A checkout with an executable `mvnw' answers with it, because that is the Maven
version the project pins; otherwise the answer is `maven-command'."
  (if (maven-wrapper? directory)
      (if (string=? directory ".")
          "./mvnw"
          (in-vicinity directory "mvnw"))
      (maven-command)))

;; ====== The command line ======

(define (property-words properties)
  "Return PROPERTIES as `-D' words.

An entry is a string such as `\"skipTests=true\"', or a pair such as
`(\"skipTests\" . \"true\")', which is how an alist already stores it."
  (unless (list? properties)
    (raise-maak-error
     (format #f "maven's #:properties takes a list, got ~s" properties)
     #:hint "Write '((\"skipTests\" . \"true\")) or '(\"skipTests=true\")."))
  (map (lambda (entry)
         (cond
          ((pair? entry) (format #f "-D~a=~a" (car entry) (cdr entry)))
          ((string? entry) (string-append "-D" entry))
          (else
           (raise-maak-error
            (format #f "~s is not a Maven property" entry)
            #:hint "Write a \"name=value\" string or a (name . value) pair."))))
       properties))

(define (threads-value threads)
  "Return THREADS as the word Maven's `-T' takes, or #f."
  (cond
   ((not threads) #f)
   ((exact-integer? threads)
    (if (> threads 0)
        (number->string threads)
        (raise-maak-error
         (format #f "maven's #:threads takes a positive number, got ~s" threads)
         #:hint "Write #:threads 4, or #:threads \"1C\".")))
   ((and (string? threads) (not (string=? threads ""))) threads)
   (else
    (raise-maak-error
     (format #f "maven's #:threads takes a number or a string, got ~s" threads)
     #:hint "Write #:threads 4, or #:threads \"1C\"."))))

(define* (maven-argv goals
                     #:key
                     (arguments '())
                     (file #f)
                     (settings #f)
                     (profiles '())
                     (properties '())
                     (offline? #f)
                     (update-snapshots? #f)
                     (quiet? #f)
                     (errors? #f)
                     (debug? #f)
                     (threads #f)
                     (batch-mode? #f)
                     (no-transfer-progress? #f)
                     (fail-at-end? #f)
                     (also-make? #f)
                     (also-make-dependents? #f)
                     (projects '())
                     (resume-from #f))
  "Return the argv that runs the Maven GOALS, without running it.

GOALS is a goal or phase name, or a list of them.  FILE names a POM or a project
directory with `-f'; SETTINGS a settings file with `-s'.  PROFILES and PROJECTS
become `-P' and `-pl' lists.  PROPERTIES become `-D' words.  THREADS is a
positive number or a string such as `\"1C\"'.  ARGUMENTS are appended as they
are, so a plugin option this builder does not name still reaches Maven.

@example
(maven-argv '(\"clean\" \"package\")
            #:properties '((\"skipTests\" . \"true\"))
            #:profiles '(\"release\"))
@result{} (\"mvn\" \"-P\" \"release\" \"-DskipTests=true\" \"clean\" \"package\")
@end example"
  (let ((goals (string-list goals "maven"))
        (arguments (string-list arguments "maven"))
        (profiles (string-list profiles "maven"))
        (projects (string-list projects "maven"))
        (threads (threads-value threads)))
    (when (null? goals)
      (raise-maak-error
       "maven needs at least one goal"
       #:hint "Write (maven-package), or (maven \"verify\" #:arguments '(\"-DskipTests\"))."))
    (append
     (program-words (maven-command) "maven")
     (if file (list "-f" file) '())
     (if settings (list "-s" settings) '())
     (if offline? (list "-o") '())
     (if update-snapshots? (list "-U") '())
     (if quiet? (list "-q") '())
     (if errors? (list "-e") '())
     (if debug? (list "-X") '())
     (if batch-mode? (list "-B") '())
     (if no-transfer-progress? (list "--no-transfer-progress") '())
     (if threads (list "-T" threads) '())
     (if fail-at-end? (list "-fae") '())
     (if also-make? (list "-am") '())
     (if also-make-dependents? (list "-amd") '())
     (if (pair? profiles) (list "-P" (string-join profiles ",")) '())
     (if (pair? projects) (list "-pl" (string-join projects ",")) '())
     (if resume-from (list "-rf" resume-from) '())
     (property-words properties)
     goals
     arguments)))

(define* (maven goals #:rest options)
  "Run the Maven GOALS, forwarding OPTIONS to `maven-argv'.

@example
(maven \"package\" #:arguments '(\"-DskipTests\"))
@end example"
  (run (apply maven-argv goals options)))

(define* (maven-output argv #:key (allow-failure? #f))
  "Run the Maven ARGV and return its standard output.

A query runs even in a dry run, because it reads the project rather than
changing it; `maven help:evaluate' is the usual one."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     "maven-output expects a non-empty list of strings"
     #:hint "Build it with maven-argv, which returns exactly that."))
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
       (format #f "maven was terminated by a signal: ~a" line)
       #:hint "Run the command above by hand to read Maven's own message."))
     (else
      (raise-maak-error
       (format #f "maven failed (exit code ~a): ~a" code line)
       #:hint "Run the command above to read Maven's own message.")))))

;; ====== The lifecycle ======

(define (maven-clean . arguments)
  "Remove the build output with `mvn clean', passing ARGUMENTS."
  (maven "clean" #:arguments arguments))

(define (maven-validate . arguments)
  "Validate the project with `mvn validate', passing ARGUMENTS."
  (maven "validate" #:arguments arguments))

(define (maven-compile . arguments)
  "Compile the sources with `mvn compile', passing ARGUMENTS."
  (maven "compile" #:arguments arguments))

(define (maven-test . arguments)
  "Run the tests with `mvn test', passing ARGUMENTS."
  (maven "test" #:arguments arguments))

(define (maven-package . arguments)
  "Package the artefact with `mvn package', passing ARGUMENTS."
  (maven "package" #:arguments arguments))

(define (maven-verify . arguments)
  "Run the verification phase with `mvn verify', passing ARGUMENTS."
  (maven "verify" #:arguments arguments))

(define (maven-install . arguments)
  "Install the artefact into the local repository, passing ARGUMENTS."
  (maven "install" #:arguments arguments))

(define (maven-deploy . arguments)
  "Deploy the artefact with `mvn deploy', passing ARGUMENTS."
  (maven "deploy" #:arguments arguments))

(define (maven-site . arguments)
  "Build the project site with `mvn site', passing ARGUMENTS."
  (maven "site" #:arguments arguments))

(define (maven-dependency-tree . arguments)
  "Print the dependency tree with `mvn dependency:tree', passing ARGUMENTS."
  (maven "dependency:tree" #:arguments arguments))

(define (maven-effective-pom . arguments)
  "Print the effective POM with `mvn help:effective-pom', passing ARGUMENTS."
  (maven "help:effective-pom" #:arguments arguments))
