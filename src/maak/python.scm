;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Python helpers: uv, the interpreter, the `pyproject.toml' a project already
;; has, and the four checks a Python CI runs.
;;
;; There is no second environment manager here.  uv owns the lock file, the
;; virtual environment and the interpreter it downloads; a maak task runs the
;; command and reads the result.  `uv-argv' is a pure builder, so a test asserts
;; the command line without uv installed, and `uv' feeds that list to `run', so
;; `--dry-run' prints it and `--plan' records it.  An option this builder does
;; not name travels as an argument, which is how a subcommand maak has never
;; heard of is still a uv command.
;;
;; The pyproject readers answer the two questions a release task asks -- what is
;; this package called, and what version is it -- from the file itself, because
;; they are answered with no uv, no network and no virtual environment.  They
;; read the file with `(maak formats)', so a file the reader understands is a
;; file they answer for, and anything else is a maak error naming the line.
;;
;; The programs are parameters, honoured from MAAK_UV then UV, MAAK_UVX then
;; UVX, and MAAK_PYTHON then PYTHON.  A value may carry a wrapper
;; (`"uv --preview"'), split into words the way an unquoted shell word is, so a
;; task can pin an option for one command.

;;; Code:

(define-module (maak python)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak formats)
  #:export (uv-command
            uvx-command
            python-command
            uv-argv
            uv
            uv-output
            uv-version
            uvx-argv
            uvx
            uv-init
            uv-sync
            uv-lock
            uv-run
            uv-add
            uv-remove
            uv-tree
            uv-export
            uv-build
            uv-publish
            uv-venv
            uv-pip
            uv-python
            python-argv
            python
            python-output
            python-version
            python-module
            pyproject-file
            pyproject-data
            python-project
            python-project-name
            python-project-version
            python-requires-python
            python-project-scripts
            python-build-backend
            python-test
            python-lint
            python-format
            python-type-check
            python-check))

;; ====== The programs ======

(define (env-value names fallback)
  "Return the first non-empty value among environment NAMES, or FALLBACK."
  (let loop ((names names))
    (if (null? names)
        fallback
        (let ((value (getenv (car names))))
          (if (and value (not (string=? value "")))
              value
              (loop (cdr names)))))))

(define uv-command
  (make-parameter (env-value '("MAAK_UV" "UV") "uv")))

(define uvx-command
  (make-parameter (env-value '("MAAK_UVX" "UVX") "uvx")))

(define python-command
  (make-parameter (env-value '("MAAK_PYTHON" "PYTHON") "python3")))

(define (program-words command what)
  "Return COMMAND as the words of a program invocation, or raise.

A value may carry flags, so it is split the way an unquoted shell word is
split; an empty value is a maak error rather than a command with no program."
  (let ((words (and (string? command)
                    (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "~a must name a program, got ~s" what command)
       #:hint "Set MAAK_UV, MAAK_UVX or MAAK_PYTHON, or pass the program as a string."))
    words))

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write '(\"--frozen\" \"--offline\") for the words."))
    items))

(define (require-string value what)
  (unless (and (string? value) (not (string=? value "")))
    (raise-maak-error
     (format #f "~a takes a non-empty string, got ~s" what value)
     #:hint "Name the value as a string, or leave the option out."))
  value)

;; ====== The uv command line ======

(define* (uv-argv subcommand
                  #:key
                  (arguments '())
                  (project #f)
                  (directory #f)
                  (cache-dir #f)
                  (offline? #f)
                  (no-cache? #f)
                  (no-progress? #f)
                  (refresh? #f)
                  (verbose #f)
                  (quiet? #f)
                  (color #f)
                  (frozen? #f)
                  (locked? #f)
                  (no-sync? #f)
                  (env-file #f)
                  (python #f))
  "Return the argv that runs uv SUBCOMMAND, without running it.

The options uv documents for the program as a whole -- PROJECT, DIRECTORY,
CACHE-DIR, OFFLINE?, NO-CACHE?, NO-PROGRESS?, REFRESH?, the verbosity and
COLOR -- come before SUBCOMMAND.  The ones uv documents for the commands that
read the project -- FROZEN?, LOCKED?, NO-SYNC?, ENV-FILE and PYTHON -- come
after it, which is where uv accepts them.  ARGUMENTS are appended as they are,
so an option this builder does not name, or the words `uv run' passes to the
program, still reach uv.

VERBOSE is #t for one `-v' or a positive number for that many.

@example
(uv-argv \"sync\" #:frozen? #t #:python \"3.12\")
@result{} (\"uv\" \"sync\" \"--frozen\" \"--python\" \"3.12\")

(uv-argv \"run\" #:arguments '(\"pytest\" \"-q\"))
@result{} (\"uv\" \"run\" \"pytest\" \"-q\")
@end example"
  (unless (and (string? subcommand) (not (string=? subcommand "")))
    (raise-maak-error
     (format #f "uv needs a subcommand, got ~s" subcommand)
     #:hint "Write (uv-sync), or (uv \"sync\" #:arguments '(\"--extra\" \"dev\"))."))
  (unless (or (not verbose)
              (eq? verbose #t)
              (and (exact-integer? verbose) (> verbose 0)))
    (raise-maak-error
     (format #f "uv's #:verbose takes #t or a positive number, got ~s" verbose)
     #:hint "Write #:verbose #t for one -v, or #:verbose 2 for two."))
  (when (and color (not (member color '("auto" "always" "never"))))
    (raise-maak-error
     (format #f "uv's #:color takes auto, always or never, got ~s" color)
     #:hint "Write #:color \"never\" to keep a log readable."))
  (append
   (program-words (uv-command) "uv")
   (if project (list "--project" project) '())
   (if directory (list "--directory" directory) '())
   (if cache-dir (list "--cache-dir" cache-dir) '())
   (if offline? (list "--offline") '())
   (if no-cache? (list "--no-cache") '())
   (if no-progress? (list "--no-progress") '())
   (if refresh? (list "--refresh") '())
   (cond ((not verbose) '())
         ((eq? verbose #t) (list "-v"))
         (else (make-list verbose "-v")))
   (if quiet? (list "--quiet") '())
   (if color (list "--color" color) '())
   (list subcommand)
   (if frozen? (list "--frozen") '())
   (if locked? (list "--locked") '())
   (if no-sync? (list "--no-sync") '())
   (if env-file (list "--env-file" env-file) '())
   (if python (list "--python" python) '())
   (string-list arguments "uv")))

(define* (uv subcommand #:rest options)
  "Run uv SUBCOMMAND, forwarding OPTIONS to `uv-argv'.

@example
(uv \"run\" #:arguments '(\"pytest\" \"-q\"))
@end example"
  (run (apply uv-argv subcommand options)))

(define* (uv-output argv #:key (allow-failure? #f))
  "Run the uv ARGV and return its standard output as a string.

A query runs even in a dry run: it reads the project rather than changing it.  A
failure raises a maak error naming the command, or comes back as #f when the
caller asked for the exit status instead."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     "uv-output expects a non-empty list of strings"
     #:hint "Build it with uv-argv, which returns exactly that."))
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
       (format #f "uv was terminated by a signal: ~a" line)
       #:hint "Run the command above by hand to read uv's own message."))
     (else
      (raise-maak-error
       (format #f "uv failed (exit code ~a): ~a" code line)
       #:hint "Run the command above to read uv's own message.")))))

(define (uv-version)
  "Return the version line the installed uv prints, trimmed."
  (string-trim-both (uv-output (uv-argv "--version"))))

;; ====== uvx ======

(define (uvx-argv . arguments)
  "Return the argv that runs uvx with ARGUMENTS."
  (append (program-words (uvx-command) "uvx") arguments))

(define (uvx . arguments)
  "Run a tool installed in a temporary environment, as `uvx ruff' does."
  (run (apply uvx-argv arguments)))

;; ====== The commands ======
;;
;; A wrapper is one line so that a task reads like the command it runs.  Any
;; option the wrapper does not name travels as an argument; the structured
;; builder is `uv-argv'.

(define (uv-init . arguments)
  "Create a new project with `uv init', passing ARGUMENTS."
  (uv "init" #:arguments arguments))

(define (uv-sync . arguments)
  "Synchronise the environment with the lock file, passing ARGUMENTS."
  (uv "sync" #:arguments arguments))

(define (uv-lock . arguments)
  "Update the lock file, passing ARGUMENTS."
  (uv "lock" #:arguments arguments))

(define (uv-run . arguments)
  "Run a command in the project's environment, passing ARGUMENTS.

The words after `uv run' are the program and its own arguments, so
`(uv-run \"pytest\" \"-q\")' runs pytest quietly.

@example
(uv-run \"pytest\" \"-k\" \"release\")
(uv-run \"python\" \"-c\" \"import example; print(example.__version__)\")
@end example"
  (uv "run" #:arguments arguments))

(define (uv-add . arguments)
  "Add a dependency to the project, passing ARGUMENTS."
  (uv "add" #:arguments arguments))

(define (uv-remove . arguments)
  "Remove a dependency from the project, passing ARGUMENTS."
  (uv "remove" #:arguments arguments))

(define (uv-tree . arguments)
  "Print the dependency tree, passing ARGUMENTS."
  (uv "tree" #:arguments arguments))

(define (uv-export . arguments)
  "Export the lock file to another format, passing ARGUMENTS."
  (uv "export" #:arguments arguments))

(define (uv-build . arguments)
  "Build the source and wheel distributions into `dist/', passing ARGUMENTS."
  (uv "build" #:arguments arguments))

(define (uv-publish . arguments)
  "Publish the built distributions, passing ARGUMENTS.

A dry run of the release is `(uv-publish \"--dry-run\")': uv's own flag is what
makes it one, while maak's dry run prints the command instead."
  (uv "publish" #:arguments arguments))

(define (uv-venv . arguments)
  "Create the virtual environment, passing ARGUMENTS."
  (uv "venv" #:arguments arguments))

(define (uv-pip . arguments)
  "Run a `uv pip' command, passing ARGUMENTS."
  (uv "pip" #:arguments arguments))

(define (uv-python . arguments)
  "Manage the interpreters uv installs, passing ARGUMENTS."
  (uv "python" #:arguments arguments))

;; ====== The interpreter ======

(define* (python-argv #:key (arguments '()) (module #f) (isolated? #f))
  "Return the argv that runs the Python interpreter, without running it.

MODULE runs the module of that name with `-m', which is how a runner that is
installed but not on `PATH' is meant to be started.  ISOLATED? adds `-I', so
the interpreter ignores `PYTHONPATH' and the user's site directory -- what a
task wants when it reads a value rather than a local experiment.
ARGUMENTS are the words the interpreter or the module is given.

@example
(python-argv #:module \"pytest\" #:arguments '(\"-q\"))
@result{} (\"python3\" \"-m\" \"pytest\" \"-q\")
@end example"
  (append (program-words (python-command) "python")
          (if isolated? (list "-I") '())
          (if module (list "-m" module) '())
          (string-list arguments "python")))

(define* (python #:rest options)
  "Run the Python interpreter, forwarding OPTIONS to `python-argv'."
  (run (apply python-argv options)))

(define* (python-output argv #:key (allow-failure? #f))
  "Run the interpreter ARGV and return its standard output as a string.

See `uv-output' for what a query does in a dry run and on failure."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     "python-output expects a non-empty list of strings"
     #:hint "Build it with python-argv, which returns exactly that."))
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
       (format #f "python was terminated by a signal: ~a" line)
       #:hint "Run the command above by hand to read its own message."))
     (else
      (raise-maak-error
       (format #f "python failed (exit code ~a): ~a" code line)
       #:hint "Run the command above to read its own message.")))))

(define (python-version)
  "Return the version line the installed interpreter prints, trimmed."
  (string-trim-both (python-output (python-argv #:arguments '("--version")))))

(define (python-module module . arguments)
  "Run MODULE with the interpreter, passing ARGUMENTS."
  (python #:module (require-string module "python-module")
          #:arguments arguments))

;; ====== The pyproject file ======
;;
;; The file is read with `(maak formats)', so the reader's documented subset is
;; the contract: a `[project]' table, its sub-tables, and whatever tables a tool
;; adds beside them.  A value that is not there is a maak error naming what is
;; missing, because a release task that guessed the version would publish
;; something else.

(define pyproject-file
  (make-parameter "pyproject.toml"))

(define* (pyproject-data #:optional (path (pyproject-file)))
  "Return PATH read as TOML, a string-keyed alist, in file order.

The file is read even in a dry run, like pkg-config: it describes the project
rather than changing it, and a plan that left out the version a publish would
use would be a plan of a different release."
  (unless (file-exists? path)
    (raise-maak-error
     (format #f "no ~a in this directory" path)
     #:hint "Run the task at the project's root, or point `pyproject-file' at the file."))
  (toml-read-string (call-with-input-file path get-string-all)
                    #:source path))

(define* (python-project #:optional (path (pyproject-file)))
  "Return the `[project]' table of PATH, or raise when there is none."
  (let ((project (data-ref (pyproject-data path) "project" 'missing)))
    (unless (data-mapping? project)
      (raise-maak-error
       (format #f "~a has no [project] table" path)
       #:hint "A pyproject that only configures tools is not a package; name the table, or read the file with pyproject-data."))
    project))

(define (project-field project field)
  "Return FIELD of the PROJECT table, or raise naming what is missing."
  (let ((value (data-ref project field 'missing)))
    (if (eq? value 'missing)
        (raise-maak-error
         (format #f "[project] has no ~a" field)
         #:hint (format #f "Write ~a = \"...\" in the [project] table." field))
        value)))

(define* (python-project-name #:optional (path (pyproject-file)))
  "Return the name the `[project]' table gives the package."
  (project-field (python-project path) "name"))

(define* (python-project-version #:optional (path (pyproject-file)))
  "Return the version the `[project]' table gives the package.

A project that lists \"version\" in `dynamic' has no version in the file: it is
computed by the build backend, and this raises rather than guessing at what the
backend would answer."
  (let* ((project (python-project path))
         (dynamic (data-ref project "dynamic" '())))
    (when (and (list? dynamic) (member "version" dynamic))
      (raise-maak-error
       (format #f "~a leaves the version to the build backend" path)
       #:hint "The [project] table lists \"version\" in dynamic, so read it with `uv version' in the task that needs it."))
    (project-field project "version")))

(define* (python-requires-python #:optional (path (pyproject-file)))
  "Return the interpreter constraint of the project, or #f when it sets none."
  (data-ref (python-project path) "requires-python" #f))

(define* (python-project-scripts #:optional (path (pyproject-file)))
  "Return the names of the console scripts the project defines."
  (let ((scripts (data-ref (python-project path) "scripts" '())))
    (if (data-mapping? scripts) (data-keys scripts) '())))

(define* (python-build-backend #:optional (path (pyproject-file)))
  "Return the build backend of the project, or #f when it names none."
  (data-ref (data-ref (pyproject-data path) "build-system" '())
            "build-backend"
            #f))

;; ====== The checks ======
;;
;; The four a Python CI runs, in the environment the project already declares.
;; `uv run' is what makes them one line: it puts the project's virtual
;; environment on the path, so a tool the project lists as a development
;; dependency is the one that runs, at the version the lock file pinned.  A
;; project that uses other tools writes the `uv-run' call itself.

(define (python-test . arguments)
  "Run the tests with pytest, passing ARGUMENTS to it.

@example
(python-test)
(python-test \"-k\" \"release\" \"-x\")
@end example"
  (apply uv-run "pytest" arguments))

(define (python-lint . arguments)
  "Lint with ruff, over ARGUMENTS or over the whole project.

The default is the project directory, so a check task reads `(python-lint)';
the words a caller gives replace it rather than adding to it, because a path and
a default path are two answers to the same question."
  (apply uv-run "ruff" "check" (if (null? arguments) '(".") arguments)))

(define (python-format . arguments)
  "Format with ruff, over ARGUMENTS or over the whole project."
  (apply uv-run "ruff" "format" (if (null? arguments) '(".") arguments)))

(define (python-type-check . arguments)
  "Type-check with mypy, over ARGUMENTS or over the whole project."
  (apply uv-run "mypy" (if (null? arguments) '(".") arguments)))

(define (python-check)
  "Run the lint, the type check and the tests, in that order.

The order is the cheap failures first: a lint error is a second, a failing test
is a minute.  Each of the three is its own task as well, so a run that wants one
of them names it."
  (python-lint)
  (python-type-check)
  (python-test))
