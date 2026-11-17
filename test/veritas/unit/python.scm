;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak python)': the uv argv builder, the wrappers that feed it to
;; `run', the pyproject readers and the convention tasks.  No uv and no Python
;; are needed: a builder is asserted as a list of words, and a pyproject is a
;; file this spec writes, so what the readers answer is asserted against text
;; the test controls rather than against a toolchain.
;;
;; The suites run sequentially because they change process-wide state: the
;; program parameters and a few scratch directories.

;;; Code:

(define-module (veritas unit python)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak formats)
  #:use-module (maak python)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-python-~a-~a" (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (with-scratch label thunk)
  (let ((dir (scratch-directory label)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk dir))
      (lambda ()
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

(define (dry-run-output thunk)
  "Return what THUNK prints in a dry run."
  (let ((out (open-output-string)))
    (parameterize ((current-output-port out)
                   (dry-run? #t)
                   (quiet? #f))
      (thunk))
    (get-output-string out)))

(define (fake-program directory name text)
  "Write a stand-in program NAME in DIRECTORY and return its path."
  (let ((path (in-vicinity directory name)))
    (write-text path (string-append "#!/bin/sh\n" text))
    (chmod path #o755)
    path))

(define pyproject
  "[project]
name = \"example-app\"
version = \"1.2.3\"
description = \"An example project\"
requires-python = \">=3.11\"
dependencies = [
  \"requests>=2\",
  \"rich\",
]

[project.scripts]
example = \"example.cli:main\"

[build-system]
requires = [\"hatchling\"]
build-backend = \"hatchling.build\"

[[tool.uv.index]]
name = \"internal\"
url = \"https://example.invalid/simple\"
")

(define (with-pyproject label text thunk)
  "Run THUNK with `pyproject-file' pointing at TEXT, in a scratch directory."
  (with-scratch label
   (lambda (dir)
     (let ((path (in-vicinity dir "pyproject.toml")))
       (write-text path text)
       (parameterize ((pyproject-file path))
         (thunk))))))

(define argv-suite
  (suite "The uv command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "a plain subcommand is the program and its name"
               (parameterize ((uv-command "uv"))
                 (assert-equal #:expect '("uv" "sync")
                               #:got (uv-argv "sync"))))
         (test "global options come before the subcommand"
               (parameterize ((uv-command "uv"))
                 (assert-equal
                  #:expect '("uv" "--project" "app" "--cache-dir" "/tmp/cache"
                             "--offline" "-v" "-v" "--color" "never" "sync")
                  #:got (uv-argv "sync"
                                 #:project "app"
                                 #:cache-dir "/tmp/cache"
                                 #:offline? #t
                                 #:verbose 2
                                 #:color "never"))))
         (test "the command's own options follow the subcommand"
               (parameterize ((uv-command "uv"))
                 (assert-equal
                  #:expect '("uv" "run" "--frozen" "--no-sync"
                             "--env-file" ".env" "--python" "3.12"
                             "pytest" "-q")
                  #:got (uv-argv "run"
                                 #:frozen? #t
                                 #:no-sync? #t
                                 #:env-file ".env"
                                 #:python "3.12"
                                 #:arguments '("pytest" "-q")))))
         (test "a program value may carry a wrapper"
               (parameterize ((uv-command "uv --preview"))
                 (assert-equal #:expect '("uv" "--preview" "sync")
                               #:got (uv-argv "sync"))))
         (test "a subcommand that is not a name is refused"
               (assert-error (lambda () (uv-argv #f))))
         (test "a colour uv does not know is refused"
               (assert-error (lambda () (uv-argv "sync" #:color "rainbow"))))
         (test "a verbosity that is not a count is refused"
               (assert-error (lambda () (uv-argv "sync" #:verbose 0))))))

(define run-suite
  (suite "Running uv"
         #:concurrent? #f
         #:shuffle? #f
         (test "a wrapper passes its arguments after the subcommand"
               (parameterize ((uv-command "uv"))
                 (assert-true (and (string-contains
                                    (dry-run-output
                                     (lambda () (uv-sync "--extra")))
                                    "uv sync --extra")
                                   #t))))
         (test "uvx runs a tool without installing it"
               (parameterize ((uvx-command "uvx"))
                 (assert-equal #:expect '("uvx" "ruff" "check")
                               #:got (uvx-argv "ruff" "check"))))))

(define pyproject-suite
  (suite "The pyproject file"
         #:concurrent? #f
         #:shuffle? #f
         (test "the project's name is read from the [project] table"
               (with-pyproject "name" pyproject
                (lambda () (assert-equal #:expect "example-app"
                                          #:got (python-project-name)))))
         (test "the project's version is read from the [project] table"
               (with-pyproject "version" pyproject
                (lambda () (assert-equal #:expect "1.2.3"
                                          #:got (python-project-version)))))
         (test "the interpreter the project asks for is read whole"
               (with-pyproject "requires" pyproject
                (lambda () (assert-equal #:expect ">=3.11"
                                          #:got (python-requires-python)))))
         (test "the console scripts are the keys of [project.scripts]"
               (with-pyproject "scripts" pyproject
                (lambda () (assert-equal #:expect '("example")
                                          #:got (python-project-scripts)))))
         (test "the build backend is read from the [build-system] table"
               (with-pyproject "backend" pyproject
                (lambda () (assert-equal #:expect "hatchling.build"
                                          #:got (python-build-backend)))))
         (test "the whole file is readable beside the project table"
               (with-pyproject "whole" pyproject
                (lambda ()
                  (assert-equal
                   #:expect '(("uv" ("index"
                                     (("name" . "internal")
                                      ("url" . "https://example.invalid/simple")))))
                   #:got (data-ref (pyproject-data) "tool" '())))))
         (test "a version the backend computes is refused by name"
               (with-scratch "dynamic"
                (lambda (dir)
                  (let ((path (in-vicinity dir "pyproject.toml")))
                    (write-text path
                                "[project]\nname = \"scm-app\"\ndynamic = [\"version\"]\n")
                    (parameterize ((pyproject-file path))
                      (let ((message (error-of python-project-version)))
                        (assert-true (and message
                                          (string-contains message "dynamic")
                                          #t))))))))
         (test "a file with no [project] table is refused"
               (with-scratch "noproject"
                (lambda (dir)
                  (let ((path (in-vicinity dir "pyproject.toml")))
                    (write-text path "[build-system]\nrequires = []\n")
                    (parameterize ((pyproject-file path))
                      (let ((message (error-of python-project-name)))
                        (assert-true (and message
                                          (string-contains message "[project]")
                                          #t))))))))
         (test "a missing file is refused by name"
               (with-scratch "missing"
                (lambda (dir)
                  (parameterize ((pyproject-file
                                  (in-vicinity dir "pyproject.toml")))
                    (let ((message (error-of pyproject-data)))
                      (assert-true (and message
                                        (string-contains message
                                                         "pyproject.toml")
                                        #t)))))))))

(define interpreter-suite
  (suite "The Python interpreter"
         #:concurrent? #f
         #:shuffle? #f
         (test "a module is run with -m"
               (parameterize ((python-command "python3"))
                 (assert-equal #:expect '("python3" "-m" "pytest" "-q")
                               #:got (python-argv #:module "pytest"
                                                  #:arguments '("-q")))))
         (test "an isolated run says so before the arguments"
               (parameterize ((python-command "python3"))
                 (assert-equal #:expect '("python3" "-I" "-c" "print(1)")
                               #:got (python-argv #:isolated? #t
                                                  #:arguments '("-c" "print(1)")))))
         (test "a program value may carry a wrapper"
               (parameterize ((python-command "uv run python"))
                 (assert-equal #:expect '("uv" "run" "python" "-V")
                               #:got (python-argv #:arguments '("-V")))))
         (test "a module is started with the words it is given"
               (parameterize ((python-command "python3"))
                 (assert-true (and (string-contains
                                    (dry-run-output
                                     (lambda () (python-module "pytest" "-q")))
                                    "python3 -m pytest -q")
                                   #t))))))

(define query-suite
  (suite "Asking uv and the interpreter"
         #:concurrent? #f
         #:shuffle? #f
         (test "the version is the line the program prints"
               (with-scratch "uv-version"
                (lambda (dir)
                  (parameterize ((uv-command
                                  (fake-program dir "uv" "echo 'uv 0.7.3'\n")))
                    (assert-equal #:expect "uv 0.7.3"
                                  #:got (uv-version))))))
         (test "a failing query is refused by the command it ran"
               (with-scratch "uv-failure"
                (lambda (dir)
                  (parameterize ((uv-command
                                  (fake-program dir "uv" "exit 2\n")))
                    (let ((message (error-of uv-version)))
                      (assert-true (and message
                                        (string-contains message "exit code 2")
                                        #t)))))))
         (test "the interpreter's version is the line it prints"
               (with-scratch "python-version"
                (lambda (dir)
                  (parameterize ((python-command
                                  (fake-program dir "python3"
                                                "echo 'Python 3.13.0'\n")))
                    (assert-equal #:expect "Python 3.13.0"
                                  #:got (python-version))))))))

(define task-suite
  (suite "What a Python task runs"
         #:concurrent? #f
         #:shuffle? #f
         (test "the tests run through uv"
               (parameterize ((uv-command "uv"))
                 (assert-true (and (string-contains
                                    (dry-run-output (lambda () (python-test)))
                                    "uv run pytest")
                                   #t))))
         (test "the linter checks the project by default"
               (parameterize ((uv-command "uv"))
                 (assert-true (and (string-contains
                                    (dry-run-output (lambda () (python-lint)))
                                    "uv run ruff check .")
                                   #t))))
         (test "the paths a task names replace the default"
               (parameterize ((uv-command "uv"))
                 (assert-true (and (string-contains
                                    (dry-run-output
                                     (lambda () (python-lint "src")))
                                    "uv run ruff check src")
                                   #t))))
         (test "the type check runs over the project"
               (parameterize ((uv-command "uv"))
                 (assert-true (and (string-contains
                                    (dry-run-output (lambda () (python-type-check)))
                                    "uv run mypy .")
                                   #t))))
         (test "a check runs the linter, the type check and the tests"
               (parameterize ((uv-command "uv"))
                 (let ((output (dry-run-output (lambda () (python-check)))))
                   (assert-equal
                    #:expect '(#t #t #t #t)
                    #:got (list (and (string-contains output "ruff check") #t)
                                (and (string-contains output "mypy") #t)
                                (and (string-contains output "pytest") #t)
                                (< (string-contains output "ruff check")
                                   (string-contains output "pytest")))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               run-suite
               pyproject-suite
               interpreter-suite
               query-suite
               task-suite))
