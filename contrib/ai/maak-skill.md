# maak skill

maak is a command runner whose task file is a Guile Scheme module. There is no
DSL: a task is a procedure, its docstring is its documentation, and its lambda
list is its argument grammar. `maak --list` prints the tasks, `maak TASK` runs
one, and the words after it become the task's arguments.

This file is what to read before writing or editing a `maak.scm`. It is the
short version; the guide it summarises is in `docs/`, starting at
`docs/index.org`.

## A minimal Maak file

```scheme
(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak))

(define (build)
  "Compile the project into ./build."
  (mkdir-p "build")
  (run (list "cargo" "build" "--release")))
```

The module must be named `(maak)`, and `(maak maak)` is the one import that puts
`run`, `mkdir-p`, `task-error`, `~` and the rest of the toolkit in scope.

## The rules a Maak file is reviewed against

1. **A task is a top-level procedure.** `(define (name) ...)` and
   `(define* (name a #:optional b) ...)` are both tasks. A value is not a task.
2. **The docstring is the documentation.** `maak --list` prints it, so the first
   line must work as a summary on its own.
3. **Arguments are described under an `Args:` line** — one line per argument,
   `name: prose`, a wrapped line continues the one above, and a blank line ends
   the section. `--list --json` pairs that prose with the argument it describes.
4. **`#:export` decides what is runnable.** With no exports every procedure is a
   task; with an export list only the exported ones are, and the rest become
   private helpers.
5. **Arguments arrive as strings**, because they come from a command line.
   Convert with `string->number` and friends where they enter the task. maak
   passes strings only, so a `#:key` argument can never be given from the
   command line.
6. **A command is argv, never a shell string.**
   `(run (list "git" "commit" "-m" message))` keeps a path with a space in it as
   one argument. `$` is the escape hatch for pipes, redirections, globs, `$(...)`
   and `VAR=value` prefixes.
7. **A failure is a sentence and a hint**:
   `(task-error "no artefact for ~a" #:args (list version) #:hint "Run `maak build' first.")`
   prints `maak: error: ...` plus `hint: ...`, exits non-zero and keeps the
   backtrace behind `--debug`. Do not use `error` for a user's mistake.
8. **`task-metadata!` is the only place a task declares things**: `#:depends`
   (ordering), `#:hidden?` (keep it out of listings) and `#:example`. Put the
   call after the definition:
   `(task-metadata! deploy #:depends '(build) #:example "maak deploy example.org")`.
9. **Do not shadow what you call.** A task named `run` cannot use `run`, and a
   task named `list` shadows Guile's. Task names like `build`, `test`, `fmt`,
   `lint`, `deploy`, `docs` are fine.
10. **The build is the program.** Loops, `let`, `when`, imports and helper
    procedures are ordinary Scheme; there is no new syntax to reach for.

## Ask maak, do not guess

| Question | Command |
| --- | --- |
| What tasks does the file define, and what does each do? | `maak --list` |
| The same as versioned JSON, with arguments and dependencies | `maak --list --json` |
| What commands would this task run, without running any? | `maak --plan TASK` |
| The same plan as versioned JSON | `maak --plan --json TASK` |
| What does a task call, and where does its environment come from? | `maak explain TASK` |
| What does the file say while it is still half-written? | `contrib/bin/maak-tasks --preview TASK` |
| Which options exist at all? | `maak --help` |

`--list` *loads* the file, so a syntax error or a bad import is reported there,
with the line. `--list --json`, `--plan --json` and
`--plan` describe what maak found; quote them rather than describing maak from
memory.

## The vocabulary you may call

Everything below is exported by `(maak maak)`, the single import a Maak file
needs. Each group has a page of the guide with the exact keywords:

- Shell: `run` (argv, plus `#:env`, `#:sudo?`, `#:dev-null?`, `#:allow-failure?`,
  `#:async?`, `#:expand?`), `$`, `syscall`, `capture`, `run-pipeline`, `stage`.
- Background jobs: `#:async?` returns a job; `job-pid`, `job-done?`, `wait-job`,
  `job-exit-code`.
- Files: `mkdir-p`, `remkdir-p`, `cp`, `mv`, `cat`, `touch`, `ln-s`,
  `chmod-file`, `delete-file-recursively` / `rm-rf`, `read-file`, `write-file`.
- Environment and directories: `with-directory`, `cd`, `env-get`, `env-set!`,
  `with-env`, `load-env-file`, `home-dir`, `tmp-dir`, `which`,
  `command-exists?`, `require-command`.
- Task registry: `list-tasks`, `available-task-names`, `run-task`, `run-tasks`.
- Strings: `~` (format to a string), `text`, `unlines`, `unwords`,
  `dedent-string`, `indent-string`.
- External worlds, each in its own module that `(maak maak)` re-exports:
  `guix-*`, `nix-*`, `cargo-*`, `uv-*`/`python-*`, `frontend-*`, `maven-*`,
  `gradle-*`, `elisp-*`, `git`/`git-*`, `tar-create`/`zip-create` and the rest of
  `(maak archive)`, `container-*`, `niri-*`, `download`/`net-*`, `i18n-*`,
  `sift-*`, `veritas-test`.

`docs/builtins.org` is the page for the first six groups, and each external
world has its own page under `docs/` — read the page before inventing a name.

## Guile you will actually write

```scheme
(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak))

(define* (deploy host #:optional (env "dev"))
  "Ship the build to HOST.

Args:
  host: the target, as HOST[:PORT].
  env: which environment file to read; dev by default."
  (let* ((pages '("index" "about"))
         (artefact (~ "build/~a.tar.gz" env)))
    (when (file-exists? artefact)
      (run (list "scp" artefact (~ "~a:/srv/" host))))
    (for-each (lambda (page)
                (run (list "rst2html" page)))
              pages)))
```

- `define*` with `#:optional`, `#:key`, and `. rest` for the argument shapes a
  task can have; `define` for none.
- `let` / `let*` for values, `when` / `unless` / `if` / `cond` for branches,
  `for-each` and `map` over lists.
- `string-append`, `string=?`, `substring`, `string-suffix?`, `string->number`,
  `number->string`; `~` is `(format #f …)` when a string is what you want.
- Imports go in the module header, as `#:use-module`. `(maak maak)` brings the
  toolkit on top of the Guile that is always in scope; a task that wants more —
  `(ice-9 regex)`, `(srfi srfi-1)` for `filter` and `last`, `(json)` — imports it
  there.
- Anything that touches the filesystem or the clock belongs in the task, not in
  a helper that a test would have to mock.

## Before calling the work done

1. `maak --list` — the file loads, the task is in the list, the docstring reads
   right.
2. `maak -n TASK ARGS` — the dry run prints every command instead of running it.
   Check the argv: quoting and argument order are visible here and nowhere else.
3. `maak --plan TASK ARGS` — the same commands as a typed plan, with the
   directory, environment and elevation each would get.
4. Only then the real run. If it fails with `maak: error:` and a hint, the
   message is the task's own and the hint says what to do; `--debug` adds the
   Guile backtrace when the failure is in the code.

A dry run executes the task's Scheme, so a check like `(file-exists? …)` sees the
real tree and a task that needs an earlier task's output still fails there. That
is expected, not a bug in maak: `#:depends` orders a real run, and a dry run has
nothing to order.

## Out of scope

- No file targets, no timestamps, no up-to-date skipping: `#:depends` says
  *when* a task may run, never whether it should.
- No Windows, no daemon, no caching layer.
- No private format: a Maak file is a Guile module, and a reader that has the
  file but not maak still has a Scheme program to read.
