;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The plan a task describes: the commands it would run, without running them.
;; `--plan' executes a task with dry-run semantics and the recorder in
;; `(maak dsl)' collects each command the task builds, so the answer describes
;; what the task's own Scheme decided rather than what a reader of its text
;; could guess.
;;
;; The rendering lives here, apart from the recording: a step is data --
;; task, kind, argv, environment, directory, elevation -- and this module
;; prints it for a person or as JSON for a machine.  `--plan --json' is the
;; versioned shape a CI job archives and an editor diffs.

;;; Code:

(define-module (maak plan)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak tasks)
  #:export (plan-text plan-json))

(define (step-name step)
  "Return the task STEP belongs to, or `maak' when nothing named one."
  (let ((task (plan-step-task step)))
    (if task (symbol->string task) "maak")))

(define (step-command step)
  "Return STEP as the one line a person reads."
  (case (plan-step-kind step)
    ((run) (argv->string (plan-step-argv step)))
    ((shell) (car (plan-step-argv step)))
    ((pipeline) (string-join (map argv->string (plan-step-argv step)) " | "))
    (else "")))

(define (step-detail-lines step)
  "Return the labelled detail lines of STEP, one per thing it declares."
  (let* ((assignments (environment-assignments (plan-step-env step)))
         (elevation (and (plan-step-sudo? step) (elevation-command)))
         (directory (plan-step-directory step)))
    (append
     (if directory (list (format #f "cwd: ~a" directory)) '())
     (if (null? assignments) '()
         (list (format #f "env: ~a" (string-join assignments " "))))
     (if (and elevation (not (string=? elevation "")))
         (list (format #f "elevate: ~a" elevation))
         '())
     (if (plan-step-dev-null? step) (list "output: /dev/null") '())
     (if (plan-step-async? step) (list "background: yes") '()))))

(define (plan-text steps)
  "Return STEPS as the plan a person reads.

Each step is numbered, named with the task that built it, and followed by its
command; the details that are not visible in the command line -- the working
directory, the environment it was given, the escalation, a discarded output and
a background start -- are printed underneath it.  A task that builds no command
says so rather than printing an empty plan."
  (string-append
   "maak plan\n"
   (if (null? steps)
       "  No commands: the task only computes.\n"
       (string-join
        (map (lambda (indexed)
               (let ((index (car indexed))
                     (step (cdr indexed)))
                 (string-join
                  (cons (format #f "  [~a] ~a  ~a"
                                index (step-name step) (step-command step))
                        (map (lambda (line) (format #f "              ~a" line))
                             (step-detail-lines step)))
                  "\n")))
             (map cons (iota (length steps) 1) steps))
        "\n"))))

(define (step-json step)
  "Return STEP as one JSON object."
  (let ((kind (plan-step-kind step)))
    (json-object
     (list (cons "task" (json-string (step-name step)))
           (cons "kind" (json-string (symbol->string kind)))
           (cons "argv"
                 (if (eq? kind 'pipeline)
                     (json-array
                      (map (lambda (argv)
                             (json-array (map json-string argv)))
                           (plan-step-argv step)))
                     (json-array (map json-string (plan-step-argv step)))))
           (cons "cwd" (json-string (or (plan-step-directory step) "")))
           (cons "env"
                 (json-array
                  (map json-string
                       (environment-assignments (plan-step-env step)))))
           (cons "elevation"
                 (if (plan-step-sudo? step)
                     (json-string (elevation-command))
                     "null"))
           (cons "devNull" (if (plan-step-dev-null? step) "true" "false"))
           (cons "async" (if (plan-step-async? step) "true" "false"))))))

(define* (plan-json steps #:optional (root (getcwd)))
  "Return STEPS as one versioned JSON object, run from ROOT.

The object is `{\"version\":1,\"root\":...,\"plan\":[...]}', and each step
names its task, its kind (`run', `shell' or `pipeline'), its argv (a list of
strings, a list of lists for a pipeline, or the one line of a shell command),
its working directory, the environment assignments it was given, its
escalation, whether its output is discarded and whether it would be started in
the background."
  (json-object
   (list (cons "version" "1")
         (cons "root" (json-string root))
         (cons "plan" (json-array (map step-json steps))))))
