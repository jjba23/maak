;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak declarative)': the task schema read from JSON, TOML and
;; YAML, the definitions built from it, and the listing and call graph the rest
;; of maak asks for.
;;
;; A schema error is caught here rather than with `assert-error', because an
;; assertion is a task the runner executes after the test form returns and the
;; scratch directory is removed by then.  The loaded module is cleared after
;; every case so one file's tasks cannot answer for the next file's.
;;
;; Nothing runs a command: the cases that check what a task runs ask for the
;; plan a dry run prints.

;;; Code:

(define-module (veritas unit declarative)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak maak)
  #:use-module (maak declarative)
  #:use-module (maak tasks)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-declarative-~a-~a"
                     (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (clear-maak-module!)
  "Remove the tasks a loaded task file defined, so the next case starts empty."
  (let ((module (resolve-module '(maak) #:ensure #f)))
    (when module
      (module-clear! module)
      (let ((interface (module-public-interface module)))
        (when interface (module-clear! interface))))))

(define (with-scratch label thunk)
  (let ((dir (scratch-directory label)))
    (dynamic-wind
      (lambda () (clear-maak-module!))
      (lambda () (thunk dir))
      (lambda ()
        (clear-maak-module!)
        (when (file-exists? dir)
          (parameterize ((quiet? #t))
            (delete-file-recursively dir)))))))

(define (error-of thunk)
  "Return the message THUNK raises, or #f when it returns."
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . values)
      (and (maak-error? (car values))
           (maak-error-message (car values))))))

(define (task-named tasks name)
  (find (lambda (task) (string=? name (task-file-task-name task))) tasks))

(define schema-suite
  (suite "The schema"
         #:concurrent? #f
         #:shuffle? #f
         (test "a YAML mapping describes tasks in file order"
               (with-scratch "mapping"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text
                     path
                     (string-append
                      "version: 1\n"
                      "tasks:\n"
                      "  build:\n"
                      "    doc: Build the crate.\n"
                      "    command: [cargo, build, --release]\n"
                      "  test:\n"
                      "    depends: [build]\n"
                      "    commands:\n"
                      "      - [cargo, test]\n"
                      "      - echo done\n"))
                    (let ((tasks (read-task-file path)))
                      (assert-equal
                       #:expect '(("build" "Build the crate." () #f)
                                  ("test" "" ("build") #f))
                       #:got (map (lambda (task)
                                    (list (task-file-task-name task)
                                          (task-file-task-doc task)
                                          (task-file-task-depends task)
                                          (task-file-task-hidden? task)))
                                  tasks)))))))
         (test "a list of tasks is read too"
               (with-scratch "list"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text
                     path
                     "tasks:\n  - name: build\n    command: [cargo, build]\n  - name: test\n    command: cargo test\n")
                    (assert-equal #:expect '("build" "test")
                                  #:got (map task-file-task-name
                                             (read-task-file path)))))))
         (test "TOML describes the same tasks"
               (with-scratch "toml"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.toml")))
                    (write-text
                     path
                     (string-append
                      "version = 1\n"
                      "[tasks.build]\n"
                      "doc = \"Build.\"\n"
                      "command = [\"cargo\", \"build\"]\n"
                      "[tasks.test]\n"
                      "depends = [\"build\"]\n"
                      "commands = [[\"cargo\", \"test\"], \"echo done\"]\n"))
                    (assert-equal #:expect '(("build" "Build." ())
                                             ("test" "" ("build")))
                                  #:got (map (lambda (task)
                                               (list (task-file-task-name task)
                                                     (task-file-task-doc task)
                                                     (task-file-task-depends task)))
                                             (read-task-file path)))))))
         (test "JSON describes the same tasks"
               (with-scratch "json"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.json")))
                    (write-text
                     path
                     "{\"version\": 1, \"tasks\": {\"build\": {\"command\": [\"cargo\", \"build\"]}}}")
                    (assert-equal #:expect '("build")
                                  #:got (map task-file-task-name
                                             (read-task-file path)))))))
         (test "an unknown task key is refused"
               (with-scratch "unknown"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text path
                                "tasks:\n  build:\n    command: [cargo]\n    frobnicate: 1\n")
                    (let ((message (error-of (lambda () (read-task-file path)))))
                      (assert-true (and message
                                        (string-contains message "unknown key")
                                        #t)))))))
         (test "a task with no command is refused"
               (with-scratch "no-command"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text path "tasks:\n  build:\n    doc: nothing\n")
                    (let ((message (error-of (lambda () (read-task-file path)))))
                      (assert-true (and message
                                        (string-contains message "command")
                                        #t)))))))
         (test "a name that is not a task name is refused"
               (with-scratch "bad-name"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text path "tasks:\n  \"not a name\":\n    command: [cargo]\n")
                    (let ((message (error-of (lambda () (read-task-file path)))))
                      (assert-true (and message
                                        (string-contains message "not a task name")
                                        #t)))))))
         (test "a duplicate name in a list is refused"
               (with-scratch "duplicate"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text path
                                "tasks:\n  - name: build\n    command: [a]\n  - name: build\n    command: [b]\n")
                    (let ((message (error-of (lambda () (read-task-file path)))))
                      (assert-true (and message
                                        (string-contains message "defined twice")
                                        #t)))))))))

(define running-suite
  (suite "Running what the file describes"
         #:concurrent? #f
         #:shuffle? #f
         (test "loading a file defines its tasks with their metadata"
               (with-scratch "load"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text
                     path
                     (string-append
                      "tasks:\n"
                      "  build:\n"
                      "    doc: Build it.\n"
                      "    command: [cargo, build]\n"
                      "  test:\n"
                      "    depends: [build]\n"
                      "    hidden: true\n"
                      "    command: cargo test\n"))
                    (load-task-file path)
                    (let ((build (get-module-var 'build))
                          (test (get-module-var 'test)))
                      (assert-equal
                       #:expect (list "Build it." '("build") 1 0)
                       #:got (list (procedure-documentation build)
                                   (map symbol->string (task-depends test))
                                   (if (task-hidden? test) 1 0)
                                   (if (procedure? build) 0 1))))))))
         (test "a dry run prints the commands the file describes"
               (with-scratch "dry-run"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text
                     path
                     (string-append
                      "tasks:\n"
                      "  build:\n"
                      "    command: [cargo, build]\n"
                      "  test:\n"
                      "    command: cargo test\n"))
                    (load-task-file path)
                    (let ((out (open-output-string)))
                      (parameterize ((current-output-port out)
                                     (dry-run? #t)
                                     (quiet? #f))
                        (run-task 'test))
                      (let ((text (get-output-string out)))
                        (assert-equal
                         #:expect (list #t)
                         #:got (list (and (string-contains text "cargo test")
                                          #t)))))))))
         (test "the descriptions and the call graph name the file's tasks"
               (with-scratch "descriptions"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text
                     path
                     "tasks:\n  build:\n    command: [cargo, build]\n  test:\n    depends: [build]\n    command: cargo test\n")
                    (let ((descriptions (task-file-descriptions path)))
                      (assert-equal
                       #:expect '(("build" ()) ("test" ("build")))
                       #:got (map (lambda (description)
                                    (list (task-description-name description)
                                          (task-description-depends description)))
                                  descriptions)))))))
         (test "the call graph points from a task to what it depends on"
               (with-scratch "graph"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.yaml")))
                    (write-text
                     path
                     "tasks:\n  build:\n    command: [cargo, build]\n  test:\n    depends: [build]\n    command: cargo test\n")
                    (assert-equal #:expect '((build) (test build))
                                  #:got (task-file-call-graph path))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               schema-suite
               running-suite))
