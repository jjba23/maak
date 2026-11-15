;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `--plan': the typed plan a task describes when it is run with
;; dry-run semantics.  The fixture has one task with one command of every shape
;; -- a direct argv, a shell line, a pipeline and a captured command -- plus a
;; task that runs nothing and one that prints before it runs.
;;
;; The plan is asked for through `main', the way the launcher calls it, so the
;; cases assert on what a person and a machine read.  The JSON is parsed with
;; Guile's own JSON reader, which the test environment provides: a plan that is
;; not valid JSON is a failure here rather than in whatever consumes it.
;;
;; Loading the fixture defines the module `(maak)', so these suites run
;; sequentially rather than as fibers.

;;; Code:

(define-module (veritas unit plan)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak main)
  #:export (spec))

(define fixture "test/maak/fixtures/plan.scm")

(define (run-cli . args)
  "Run the CLI with ARGS and return (STATUS STDOUT STDERR)."
  (let ((out (open-output-string))
        (err (open-output-string)))
    (let ((status (parameterize ((current-output-port out)
                                 (current-error-port err))
                    (apply main (list #:args (cons "maak" args))))))
      (list status (get-output-string out) (get-output-string err)))))

(define (plan-run . args)
  "Plan the fixture's GREET task, with ARGS after the task name."
  (apply run-cli (append (list "--file" fixture "--no-banner"
                               "--plan" "--tasks" "greet" "--" "world")
                         args)))

(define (plan-json . args)
  "Return the parsed JSON of a plan of the fixture's GREET task."
  (json->scm
   (open-input-string
    (second (apply run-cli (append (list "--file" fixture "--no-banner"
                                         "--plan" "--json"
                                         "--tasks" "greet" "--" "world")
                                   args))))))

(define (said? text needle)
  (and (string-contains text needle) #t))

(define ansi-escapes
  (make-regexp "\x1b\\[[0-9;]*[A-Za-z]"))

(define (plain str)
  "Return STR without the colour escapes a report writes around its words."
  (regexp-substitute/global #f ansi-escapes str 'pre "" 'post))

(define text-suite
  (suite "A plan for a person"
         #:concurrent? #f
         #:shuffle? #f
         (test "a direct command is named by its argv"
               (assert-true (said? (second (plan-run)) "echo world")))
         (test "a shell command is named as the shell reads it"
               (assert-true (said? (second (plan-run))
                                   "echo hi > /tmp/maak-plan-fixture")))
         (test "a pipeline is named stage by stage"
               (assert-true (said? (second (plan-run))
                                   "cat maak.scm | wc -l")))
         (test "a command says where it would run"
               (assert-true (said? (second (plan-run)) "cwd: ")))
         (test "a command says what environment it was given"
               (assert-true (said? (second (plan-run)) "env: CI=1")))
         (test "a command says it would be elevated"
               (assert-true (and (string-match "elevate: (sudo|doas)"
                                               (second (plan-run)))
                                 #t)))
         (test "a captured command is in the plan too"
               (assert-true (said? (second (plan-run)) "date +%Y")))
         (test "a task that runs nothing says so"
               (assert-true (said? (second (run-cli "--file" fixture
                                                    "--no-banner"
                                                    "--plan"
                                                    "--tasks" "only-computes"))
                                   "No commands")))
         (test "planning does not run the command"
               (assert-false (file-exists? "/tmp/maak-plan-fixture")))
         (test "the plan can be printed as an option beside the task list"
               (assert-equal #:expect 1
                             #:got (first (run-cli "--file" fixture
                                                   "--no-banner"
                                                   "--list" "--plan"
                                                   "--tasks" "greet"))))))

(define (plan-steps)
  "Return the parsed JSON plan's steps as a list."
  (vector->list (assoc-ref (plan-json) "plan")))

(define (plan-step index)
  "Return step INDEX of the parsed JSON plan."
  (list-ref (plan-steps) index))

(define json-suite
  (suite "A plan for a machine"
         #:concurrent? #f
         #:shuffle? #f
         (test "the JSON plan is versioned"
               (assert-equal #:expect 1
                             #:got (assoc-ref (plan-json) "version")))
         (test "the JSON plan names every command"
               (assert-equal #:expect 5
                             #:got (length (plan-steps))))
         (test "a run step carries its kind and argv"
               (assert-equal #:expect (list "run" '("echo" "world"))
                             #:got (list (assoc-ref (plan-step 0) "kind")
                                         (vector->list (assoc-ref (plan-step 0) "argv")))))
         (test "a shell step carries its line"
               (assert-equal #:expect "shell"
                             #:got (assoc-ref (plan-step 2) "kind")))
         (test "a pipeline step carries each stage"
               (assert-equal #:expect '("cat" "maak.scm")
                             #:got (vector->list
                                    (vector-ref (assoc-ref (plan-step 3) "argv")
                                                0))))
         (test "a step carries its environment and elevation"
               (assert-equal #:expect (list '("CI=1") #t)
                             #:got (list (vector->list
                                          (assoc-ref (plan-step 1) "env"))
                                         (and (member (assoc-ref (plan-step 1)
                                                                 "elevation")
                                                      '("sudo" "doas"))
                                              #t))))
         (test "a step carries the directory it would run in"
               (assert-true (string? (assoc-ref (plan-step 0) "cwd"))))
         (test "a task's own printing does not corrupt the JSON"
               (assert-equal #:expect 1
                             #:got (assoc-ref
                                    (json->scm
                                     (open-input-string
                                      (second (run-cli "--file" fixture
                                                       "--no-banner"
                                                       "--plan" "--json"
                                                       "--tasks" "chatty"))))
                                    "version")))
         (test "a task's own printing still reaches the reader"
                              (assert-true (said? (third (run-cli "--file" fixture
                                                                  "--no-banner"
                                                                  "--plan" "--json"
                                                                  "--tasks" "chatty"))
                                                  "noise")))))

(define refusal-suite
  (suite "What --plan refuses"
         #:concurrent? #f
         #:shuffle? #f
         (test "a plan and a listing are different questions"
               (assert-true (said? (plain (third (run-cli "--file" fixture
                                                          "--no-banner"
                                                          "--list" "--plan")))
                                   "--list is a listing")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               text-suite
               json-suite
               refusal-suite))
