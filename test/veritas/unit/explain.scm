;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `maak explain' and the two readings it renders: the static call
;; graph the file writes, and the environment a run would assemble from the
;; process and the environment files.  The graph is asked of the project's own
;; Maak file and of a fixture with a small, deliberate shape; the environment
;; report is given files of its own, so the precedence is asserted rather than
;; assumed.
;;
;; The CLI cases go through `main' with the fixture, which defines the module
;; `(maak)', so the suites run sequentially rather than as fibers.

;;; Code:

(define-module (veritas unit explain)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak explain)
  #:use-module (maak tasks)
  #:use-module (maak main)
  #:export (spec))

(define fixture "test/maak/fixtures/explain.scm")
(define project "maak.scm")

(define (run-cli . args)
  "Run the CLI with ARGS and return (STATUS STDOUT STDERR)."
  (let ((out (open-output-string))
        (err (open-output-string)))
    (let ((status (parameterize ((current-output-port out)
                                 (current-error-port err))
                    (apply main (list #:args (cons "maak" args))))))
      (list status (get-output-string out) (get-output-string err)))))

(define (said? text needle)
  (and (string-contains text needle) #t))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define sandbox-counter 0)

(define (with-sandbox thunk)
  "Call THUNK with a fresh directory, removing it afterwards."
  (set! sandbox-counter (+ sandbox-counter 1))
  (let ((dir (format #f "~a/maak-explain-~a-~a"
                     (tmp-dir) (getpid) sandbox-counter)))
    (mkdir dir)
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk dir))
      (lambda ()
        (when (file-exists? dir)
          (delete-file-recursively dir))))))

(define graph-suite
  (suite "The call graph the file writes"
         #:concurrent? #f
         #:shuffle? #f
         (test "a task that calls another is an edge"
               (assert-equal #:expect '(beta gamma)
                             #:got (assoc-ref (static-call-graph fixture)
                                              'alpha)))
         (test "a quoted name is not a call"
               (assert-false (memq 'delta
                                   (assoc-ref (static-call-graph fixture)
                                              'alpha))))
         (test "a task that calls nothing has no edges"
               (assert-equal #:expect '()
                             #:got (assoc-ref (static-call-graph fixture)
                                              'beta)))
         (test "the graph keeps the file's task order"
               (assert-equal #:expect '(alpha beta gamma delta)
                             #:got (map car (static-call-graph fixture))))
         (test "the project's own graph is read"
               (assert-equal #:expect '(docs)
                             #:got (assoc-ref (static-call-graph project)
                                              'deploy)))
         (test "a helper task's callers are in the graph"
               (assert-equal #:expect '(dev hr)
                             #:got (let ((graph (static-call-graph project)))
                                     (sort (filter-map
                                            (lambda (entry)
                                              (and (memq 'hot-reload (cdr entry))
                                                   (car entry)))
                                            graph)
                                           (lambda (a b)
                                             (string<? (symbol->string a)
                                                       (symbol->string b)))))))))

(define report-suite
  (suite "The environment a run would assemble"
         #:concurrent? #f
         #:shuffle? #f
         (test "the process environment wins over a file"
               (with-sandbox
                (lambda (dir)
                  (let ((file (in-vicinity dir "a.env")))
                    (write-text file "CI=0\n")
                    (assert-equal #:expect (list (list "CI" "1" "process environment"
                                                       (list (cons file "0"))))
                                  #:got (environment-report (list file)
                                                           '(("CI" . "1"))))))))
         (test "the first file that sets a name wins"
               (with-sandbox
                (lambda (dir)
                  (let ((first-file (in-vicinity dir "a.env"))
                        (second-file (in-vicinity dir "b.env")))
                    (write-text first-file "FOO=bar\n")
                    (write-text second-file "FOO=baz\n")
                    (assert-equal #:expect (list (list "FOO" "bar" first-file
                                                       (list (cons second-file "baz"))))
                                  #:got (environment-report
                                         (list first-file second-file)
                                         '()))))))
         (test "a later file that agrees is not reported as a conflict"
               (with-sandbox
                (lambda (dir)
                  (let ((first-file (in-vicinity dir "a.env"))
                        (second-file (in-vicinity dir "b.env")))
                    (write-text first-file "FOO=bar\n")
                    (write-text second-file "FOO=bar\n")
                    (assert-equal #:expect '()
                                  #:got (cadddr (car (environment-report
                                                      (list first-file second-file)
                                                      '()))))))))
         (test "a variable no file mentions is not reported"
               (with-sandbox
                (lambda (dir)
                  (let ((file (in-vicinity dir "a.env")))
                    (write-text file "FOO=bar\n")
                    (assert-equal #:expect '("FOO")
                                  #:got (map car (environment-report
                                                  (list file)
                                                  '(("SECRET" . "x")))))))))))

(define cli-suite
  (suite "What explain prints"
         #:concurrent? #f
         #:shuffle? #f
         (test "explain names the task and its file"
               (let ((result (run-cli "--file" fixture "--no-banner"
                                      "--explain" "--tasks" "alpha")))
                 (assert-equal #:expect (list 0 #t #t)
                               #:got (list (first result)
                                           (said? (second result) "maak explain: alpha")
                                           (said? (second result) fixture)))))
         (test "explain prints what the task calls"
               (assert-true (said? (second (run-cli "--file" fixture
                                                    "--no-banner"
                                                    "--explain" "--tasks" "alpha"))
                                   "calls (from the file): beta, gamma")))
         (test "explain prints what calls the task"
               (assert-true (said? (second (run-cli "--file" fixture
                                                    "--no-banner"
                                                    "--explain" "--tasks" "alpha"))
                                   "called by (from the file): delta")))
         (test "explain prints the load path and the escalation"
               (let ((result (second (run-cli "--file" fixture "--no-banner"
                                              "--explain" "--tasks" "beta"))))
                 (assert-true (and (said? result "load paths:")
                                   (said? result "elevation:")
                                   #t))))
         (test "explain says which environment file won"
               (with-sandbox
                (lambda (dir)
                  (let ((first-file (in-vicinity dir "a.env"))
                        (second-file (in-vicinity dir "b.env")))
                    (write-text first-file "FOO=bar\n")
                    (write-text second-file "FOO=baz\n")
                    (dynamic-wind
                      (lambda () (env-set! "UNSET_FOR_EXPLAIN" "1"))
                      (lambda ()
                        (assert-true
                         (said? (second (run-cli "--file" fixture "--no-banner"
                                                 "-e" (string-append
                                                       first-file ":" second-file)
                                                 "--explain" "--tasks" "beta"))
                                (format #f "FOO=bar (from ~a" first-file))))
                      (lambda () (env-unset! "UNSET_FOR_EXPLAIN")))))))
         (test "explain refuses a run with no task"
               (assert-true (said? (third (run-cli "--file" fixture
                                                    "--no-banner"
                                                    "--explain"))
                                   "explain needs a task")))
         (test "explain refuses two tasks"
               (assert-true (said? (third (run-cli "--file" fixture
                                                    "--no-banner"
                                                    "--explain" "--tasks" "alpha,beta"))
                                   "explain takes one task")))
         (test "explain refuses a task's arguments"
               (assert-true (said? (third (run-cli "--file" fixture
                                                    "--no-banner"
                                                    "--explain" "--tasks" "alpha" "one"))
                                   "does not pass its arguments")))
         (test "explain refuses an unknown task"
               (assert-true (said? (third (run-cli "--file" fixture
                                                    "--no-banner"
                                                    "--explain" "--tasks" "missing"))
                                   "Unknown task")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               graph-suite
               report-suite
               cli-suite))
