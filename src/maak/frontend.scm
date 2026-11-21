;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Frontend helpers: npm, pnpm and yarn behind one set of procedures, the
;; package.json a project already has, and the scripts a web build runs.
;;
;; The three managers agree on what a project is and disagree on how to spell
;; half the verbs: `npm ci' against `pnpm install --frozen-lockfile' against
;; `yarn install --immutable', `npm install' against `pnpm add', `npm exec --'
;; against `pnpm exec'.  That disagreement is the whole reason this module
;; exists.  A task says what it means -- install from the lock file, add a
;; dependency, run a script -- and the manager the checkout pins supplies the
;; words.
;;
;; Which manager that is comes from the lock file, because the lock file is
;; written by whichever tool the project actually ran, and is not something a
;; checkout carries by accident.  `MAAK_PACKAGE_MANAGER` overrides it, and
;; `package-manager` is a parameter, so a task can pin one for a single call.
;; The value is a program rather than a name, so `"corepack pnpm"` is allowed
;; the way `"cargo +nightly"` is: the last word is the manager, and the words
;; before it are run first.
;;
;; There is no build engine here.  The scripts in package.json are the build's
;; own description, and `frontend-run' runs one; a task that wants vite, webpack
;; or tsc by name uses `frontend-exec', which is the manager's own way of
;; running a binary from `node_modules'.

;;; Code:

(define-module (maak frontend)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak formats)
  #:export (package-manager
            detect-package-manager
            package-manager-kind
            frontend-argv
            frontend
            frontend-output
            frontend-install
            frontend-ci
            frontend-add
            frontend-remove
            frontend-run
            frontend-exec
            frontend-publish
            frontend-build
            frontend-test
            frontend-lint
            frontend-format
            frontend-check
            package-json-file
            package-json
            package-name
            package-version
            package-scripts
            package-dependencies
            package-dev-dependencies))

;; ====== Which manager ======

(define (env-value names fallback)
  "Return the first non-empty value among environment NAMES, or FALLBACK."
  (let loop ((names names))
    (if (null? names)
      fallback
      (let ((value (getenv (car names))))
        (if (and value (not (string=? value "")))
          value
          (loop (cdr names)))))))

(define (lockfile-manager directory)
  "Return the program a lock file in DIRECTORY names, or #f when it names one."
  (cond [(file-exists? (in-vicinity directory "package-lock.json")) "npm"]
        [(file-exists? (in-vicinity directory "pnpm-lock.yaml")) "pnpm"]
        [(file-exists? (in-vicinity directory "yarn.lock")) "yarn"]
        [else #f]))

(define* (detect-package-manager #:key (directory "."))
  "Return the package manager DIRECTORY pins, named as its program is.

The lock file is the answer because it is the file the manager itself wrote:
`package-lock.json' is npm's, `pnpm-lock.yaml' is pnpm's and `yarn.lock' is
yarn's.  A checkout with none of them answers `npm', the manager every machine
with Node already has.

DIRECTORY is the project's root, the current directory by default, so the
function can be tested without changing where the process runs."
  (or (lockfile-manager directory) "npm"))

(define package-manager
  (make-parameter (env-value '("MAAK_PACKAGE_MANAGER")
                             (detect-package-manager))))

(define (program-words command)
  "Return COMMAND as the words of a program invocation, or raise."
  (let ((words (and (string? command)
                 (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "package-manager must name a program, got ~s" command)
       #:hint "Set MAAK_PACKAGE_MANAGER to npm, pnpm or yarn, or to a command such as \"corepack pnpm\"."))
    words))

(define* (package-manager-kind #:optional (manager (package-manager)))
  "Return the package manager MANAGER runs, as one of `npm', `pnpm' or `yarn'.

MANAGER is the program, so a wrapper is read from its last word:
`\"corepack pnpm\"' is pnpm.  A name this module has no spelling for is a maak
error rather than a guess, because the verbs differ and a wrong one would
install something the project did not ask for."
  (let* ((words (program-words manager))
         (program (basename (car (last-pair words))))
         (kind (string->symbol program)))
    (unless (memq kind '(npm pnpm yarn))
      (raise-maak-error
       (format #f "~a is not one of npm, pnpm or yarn" program)
       #:hint "Set MAAK_PACKAGE_MANAGER to npm, pnpm or yarn, or to a command ending in one."))
    kind))

(define (yarn-berry? directory)
  "Return #t when DIRECTORY holds a Yarn 2 or later configuration.

Yarn 1 and Yarn 2 spell the frozen install and the publish differently, and the
configuration file is what tells them apart: a project with `.yarnrc.yml' is
running a Yarn that reads it."
  (file-exists? (in-vicinity directory ".yarnrc.yml")))

;; ====== The verbs ======

(define (ci-words kind directory)
  "Return the words that install KIND's dependencies exactly from the lock file."
  (case kind
    [(npm) (list "ci")]
    [(pnpm) (list "install" "--frozen-lockfile")]
    [else (if (yarn-berry? directory)
            (list "install" "--immutable")
            (list "install" "--frozen-lockfile"))]))

(define (add-words kind)
  "Return the words that add a dependency with KIND."
  (case kind
    [(npm) (list "install")]
    [else (list "add")]))

(define (remove-words kind)
  "Return the words that remove a dependency with KIND."
  (case kind
    [(npm) (list "uninstall")]
    [(pnpm) (list "remove")]
    [else (list "remove")]))

(define (exec-words kind)
  "Return the words that run a binary from `node_modules' with KIND."
  (case kind
    [(npm) (list "exec" "--")]
    [else (list "exec")]))

(define (publish-words kind directory)
  "Return the words that publish the package with KIND."
  (case kind
    [(yarn) (if (yarn-berry? directory) (list "npm" "publish") (list "publish"))]
    [else (list "publish")]))

(define (directory-words kind directory)
  "Return the words that point KIND at DIRECTORY, the way KIND spells them."
  (case kind
    [(npm) (list "--prefix" directory)]
    [(pnpm) (list "--dir" directory)]
    [else (list "--cwd" directory)]))

;; ====== The command line ======

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write '(\"run\" \"build\") for the words."))
    items))

(define* (frontend-argv words #:key (arguments '()) (directory #f))
  "Return the argv that runs WORDS with the project's package manager.

WORDS is the command and its words -- `'(\"run\" \"build\")' for a script, or
`'(\"install\")'.  The manager's program comes first, then the flag that points
it at DIRECTORY when one is given, then WORDS, then ARGUMENTS as they are, so an
option this module does not name still reaches the manager.

@example
(frontend-argv '(\"run\" \"build\"))
@result{} (\"npm\" \"run\" \"build\")

(frontend-argv '(\"install\") #:directory \"web\")
@result{} (\"npm\" \"--prefix\" \"web\" \"install\")
@end example"
  (let* ((manager (package-manager))
         (kind (package-manager-kind manager))
         (words (string-list words "frontend")))
    (unless (pair? words)
      (raise-maak-error
       "frontend needs at least one word to run"
       #:hint "Write (frontend '(\"run\" \"build\")), or name a script with (frontend-run \"build\")."))
    (append (program-words manager)
            (if directory (directory-words kind directory) '())
            words
            (string-list arguments "frontend"))))

(define* (frontend words #:rest options)
  "Run the package manager with WORDS, forwarding OPTIONS to `frontend-argv'.

@example
(frontend '(\"run\" \"build\"))
@end example"
  (run (apply frontend-argv words options)))

(define* (frontend-output argv #:key (allow-failure? #f))
  "Run the frontend ARGV and return its standard output as a string.

A query runs even in a dry run: it reads the project rather than changing it --
`npm pkg get version' is the usual one.  A failure raises a maak error naming
the command, or comes back as #f when the caller asked for the exit status."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     "frontend-output expects a non-empty list of strings"
     #:hint "Build it with frontend-argv, which returns exactly that."))
  (let* ((line (argv->string argv))
         (port (open-input-pipe line))
         (output (get-string-all port))
         (status (close-pipe port))
         (code (status:exit-val status)))
    (cond
      [(and code (zero? code)) output]
      [allow-failure? #f]
      [(not code)
       (raise-maak-error
        (format #f "the package manager was terminated by a signal: ~a" line)
        #:hint "Run the command above by hand to read its own message.")]
      [else
        (raise-maak-error
         (format #f "the package manager failed (exit code ~a): ~a" code line)
         #:hint "Run the command above to read its own message.")])))

;; ====== The commands ======
;;
;; Each wrapper is one line over `frontend', which is what puts the manager's
;; spelling in one place: the wrapper says what the task means, and the table
;; above says how these three tools spell it.

(define (frontend-install . arguments)
  "Install the project's dependencies, passing ARGUMENTS to the manager."
  (frontend '("install") #:arguments arguments))

(define (frontend-ci . arguments)
  "Install the dependencies exactly as the lock file records them.

This is what a CI run wants and what a developer's `frontend-install' must not
be: it refuses to update the lock file, so a build that would have changed it
fails instead.  The words are the manager's own -- `npm ci',
`pnpm install --frozen-lockfile', `yarn install --immutable' (or
`--frozen-lockfile' on Yarn 1)."
  (let* ((directory ".")
         (kind (package-manager-kind)))
    (frontend (ci-words kind directory) #:arguments arguments)))

(define* (frontend-add packages #:key (dev? #f) (arguments '()))
  "Add PACKAGES as dependencies, from the registry or the file system.

PACKAGES is one name or a list of them.  DEV? asks for a development dependency,
which npm and pnpm spell `--save-dev' and yarn spells `--dev'.  ARGUMENTS are
the manager's own words, appended last.

@example
(frontend-add \"react\")
(frontend-add '(\"vitest\" \"@testing-library/react\") #:dev? #t)
@end example"
  (let ((kind (package-manager-kind)))
    (frontend (append (add-words kind)
                      (string-list packages "frontend-add")
                      (if dev? (list (if (eq? kind 'yarn) "--dev" "--save-dev"))
                        '()))
              #:arguments arguments)))

(define* (frontend-remove packages #:key (arguments '()))
  "Remove PACKAGES from the dependencies.

PACKAGES is one name or a list of them; ARGUMENTS are the manager's own words."
  (let ((kind (package-manager-kind)))
    (frontend (append (remove-words kind)
                      (string-list packages "frontend-remove"))
              #:arguments arguments)))

(define (frontend-run script . arguments)
  "Run the package.json SCRIPT, passing ARGUMENTS to it.

`run' is the one verb all three managers spell the same way, so this is the
wrapper a task reads best with.  The words after SCRIPT go to the script, so a
task that forwards a flag writes them after it:

@example
(frontend-run \"build\")
(frontend-run \"test\" \"--\" \"--watch\")
@end example"
  (frontend (list "run" script) #:arguments arguments))

(define (frontend-exec command . arguments)
  "Run COMMAND from `node_modules/.bin', passing ARGUMENTS to it.

This is how a task runs a tool the project installed without knowing whether the
manager needs `--' between its own words and the tool's."
  (let ((kind (package-manager-kind)))
    (frontend (append (exec-words kind) (list command))
              #:arguments arguments)))

(define (frontend-publish . arguments)
  "Publish the package, passing ARGUMENTS to the manager.

A dry run of the release is `(frontend-publish \"--dry-run\")': npm's own flag is
what makes it one, while maak's dry run prints the command instead."
  (let ((kind (package-manager-kind)))
    (frontend (publish-words kind ".") #:arguments arguments)))

;; ====== The scripts a project has ======

(define (frontend-build . arguments)
  "Run the `build' script, passing ARGUMENTS to it."
  (apply frontend-run "build" arguments))

(define (frontend-test . arguments)
  "Run the `test' script, passing ARGUMENTS to it."
  (apply frontend-run "test" arguments))

(define (frontend-lint . arguments)
  "Run the `lint' script, passing ARGUMENTS to it."
  (apply frontend-run "lint" arguments))

(define (frontend-format . arguments)
  "Run the `format' script, passing ARGUMENTS to it."
  (apply frontend-run "format" arguments))

(define (frontend-check)
  "Run the `lint', `test' and `build' scripts, in that order.

The three are the scripts a web build usually has; a project that names them
otherwise writes the three `frontend-run' calls itself.  The order is the cheap
failure first, so a lint error stops the run before the bundle is built."
  (frontend-lint)
  (frontend-test)
  (frontend-build))

;; ====== The package.json ======

(define package-json-file
  (make-parameter "package.json"))

(define* (package-json #:optional (path (package-json-file)))
  "Return PATH read as JSON, a string-keyed alist, in file order.

The file is read even in a dry run, like pkg-config: it describes the project
rather than changing it, and the version a publish would use is in it."
  (unless (file-exists? path)
    (raise-maak-error
     (format #f "no ~a in this directory" path)
     #:hint "Run the task at the project's root, or point `package-json-file' at the file."))
  (let ((data (json-read-string (call-with-input-file path get-string-all)
                                #:source path)))
    (unless (data-mapping? data)
      (raise-maak-error
       (format #f "~a is not a JSON object" path)
       #:hint "A package.json is one object holding the package's fields."))
    data))

(define (package-field field)
  "Return FIELD of the project's package.json, or raise naming what is missing."
  (let ((value (data-ref (package-json) field 'missing)))
    (unless (string? value)
      (raise-maak-error
       (format #f "package.json has no ~a" field)
       #:hint (format #f "Write \"~a\": \"...\" in package.json." field)))
    value))

(define (package-name)
  "Return the name package.json gives the package."
  (package-field "name"))

(define (package-version)
  "Return the version package.json gives the package."
  (package-field "version"))

(define (package-table key)
  "Return KEY of package.json as an alist, or the empty list when it has none."
  (let ((value (data-ref (package-json) key '())))
    (if (data-mapping? value) value '())))

(define (package-scripts)
  "Return the names of the scripts package.json defines, in file order."
  (data-keys (package-table "scripts")))

(define (package-dependencies)
  "Return the dependencies package.json declares, with the ranges it gives them."
  (package-table "dependencies"))

(define (package-dev-dependencies)
  "Return the development dependencies package.json declares."
  (package-table "devDependencies"))
