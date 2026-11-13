# Vision

maak exists because a build tool should not be a second language. Its task file is
a Guile module, its tasks are procedures, and its command line is a runner that
happens to speak Scheme rather than a language that happens to run commands. The
direction is to make that single decision pay off everywhere it can — in the
editor, in CI, in the container, on a pinned Guix — without ever adding the DSL
that the project was written to avoid.

## Where maak is today

The program lives in `src/maak/`, about eight thousand lines across thirteen
modules: `maak.scm` for tasks and their listing, `main.scm` for the command line,
`dsl.scm` for strings, load paths and the error type, and one module per external
world — `shell.scm`, `git.scm`, `archive.scm`, `guix.scm`, `nix.scm`,
`container.scm`, `net.scm`, `niri.scm`, `i18n.scm`, `version.scm`.

What works, concretely: a Maak file is loaded, its `maak-task?` variables are
collected from the module's public interface, and each one is runnable by name
with the words after it bound to its lambda list. `run` and `run-pipeline` take
argv lists, never a shell string; a pipeline reports every failing stage, gives a
stage its own environment or elevation, and can keep what each stage wrote. The
command line carries Make's flags (`-f`, `-C`, `-n`, `-k`, `-i`, `-j`, `-L`, `-e`)
plus `--list`, `--tasks`, `--debug` and the banner controls. Parallel runs fork one
worker per task and print each task's output as a block, in the order named. Seven
editor integrations in `contrib/` read the task list through
`contrib/bin/maak-tasks`, which parses the file without evaluating it and prints
JSON. The suite runs under Veritas in `test/veritas/unit`, with a page of the
guide for each feature area.

Three decisions constrain everything that follows.

**There is no DSL.** `maak-task?` marks a procedure; the docstring is the
description; the lambda list is the argument grammar. Every future task feature
has to be expressible as metadata attached to a variable or as a convention over
the lambda list — there is no grammar to extend, and that is the point.

**There are no file targets and no timestamps.** Make decides whether to run a
recipe by comparing mtimes; maak runs what it was asked and lets the task decide
what counts as up to date, in Scheme, where the condition may be a hash, a row,
or a network check. Any future caching has to arrive as an explicit, opt-in
concept rather than as the default semantics.

**The runtime is POSIX and Guile.** `-j` forks; pipelines use `pipe` and `dup2`;
jobs are pids. There is no Windows story, and no plan to acquire one. The upside
is that the process model is honest and cheap; the cost is that portability work
would be a rewrite, not a port.

## The vision

1. **One task description, many consumers.** A Maak file is read by maak, by
   `maak.el`, by seven other editors, by shell completions and by CI, and all of
   them agree because they consume the same machine-readable description emitted
   by maak itself, versioned, with a schema. Today each consumer reimplements
   part of the parse.

2. **Tasks carry structure, not just a docstring.** A task can declare what it
   needs and what it produces as data — so `-j` can order rather than merely
   fan out, `--plan` can show a graph, and an editor can grey out what cannot
   run yet. Ordering without caching: dependencies say *when*, never *whether*.

3. **The plan is inspectable before anything runs.** Dry run stops being a
   transcript of quoted shell commands and becomes a typed plan — task, argv,
   resolved environment, working directory, elevation, container image — that a
   human reads, a machine diffs and a CI job archives.

4. **Reproducibility is a value, not a subprocess.** The `guix-build` and
   `guix-program` helpers already read a pinned Guix as values. The direction is
   that every external world maak wraps eventually offers that shape: a
   resolved, inspectable object first, a process second.

5. **The task list is a protocol between project and editor.** Editors do not
   parse human output; they speak to a stable description of the project's
   automation, and `maak.el` leads because it is the canonical front end.

## Features that are in scope and not built yet

- **A machine-readable task listing.** `contrib/bin/maak-tasks` exists precisely
  because `maak --list` prints for humans only. The hard part is not JSON; it is
  making a static reader and the real loader agree, character for character, on a
  file that is valid Scheme but only partly evaluable — macros, conditionals,
  tasks defined through `use-modules`.
- **Task metadata beyond the docstring.** `maak-task?` is already a distinct
  predicate rather than `procedure?`, so the seam exists. The hard part is a
  vocabulary small enough to be non-negotiable: dependencies, a hidden flag, an
  example invocation, nothing else.
- **Argument descriptions editors can prompt for.** The lambda list and argument
  kinds are already read statically; what is missing is the prose for what to
  type, and a rule for where it lives without a second docstring format.
- **Dependency-aware `-j`.** Parallel runs fan out across the tasks named on the
  command line and print in name order. The hard part is a scheduler and a cycle
  check that do not introduce a cache by the back door.
- **A completion generator.** `scripts/maak-completion.bash`, `.zsh` and `.fish`
  are maintained by hand and read the Maak file themselves. The hard part is
  emitting all three from one description without making the scripts slower than
  the static text they replace.
- **A hermetic mode for CI.** A scrub of the ambient environment and a pinned
  `PATH`, so a task that leaks a dependency on the developer's machine fails
  where it should — without contradicting `.env` loading or the parameterised
  `dry-run?`/`quiet?` semantics.

## New features that would be worth having

- **`maak --plan`**, emitting the structure described above, alongside `--list`.
  This is speculative: no code anticipates it beyond the fact that `run` already
  centralises argv construction.
- **`maak --watch TASK`**, re-running a named task when the tree changes. The
  repository's own `dev` and `hot-reload` tasks are a hand-rolled version of
  this, and generalising them would remove a per-project chore.
- **A pinned-tool resolver** — the `reports/guix-pinned-tool-probe.scm` and its
  siblings are probes writing towards this: a task that says "I need `jq` from
  this Guix commit" and gets an argv prefix, not a shell.
- **`maak explain TASK`**, printing which task called which, how the environment
  was assembled, and which `.env` file won. The composition is already a call
  graph; nothing renders it.
- **A self-contained bundle** built on the existing `guix-pack` and container
  helpers, so a project can ship its automation as an artefact that runs on a
  machine with neither Guile nor maak installed.

## Contributions that would move the needle

1. **Promote `contrib/bin/maak-tasks` into `maak --list --json`.** Highest value
   per hour here: one reader replacing seven integrations' worth of duplicated
   parsing, with `contrib/test/maak-tasks.sh` already covering every task shape.
2. **A Veritas spec that keeps `--list` and the docs in step.**
   `test/veritas/unit/guide.scm` already resolves every `file:` link and every
   `CUSTOM_ID`; the same technique applied to "every exported helper appears in
   `docs/builtins.org`" retires a whole class of silent rot.
3. **Extract the completion scripts from one description.** Three files, one
   source of truth, and a shell-level test that compares generated output to the
   checked-in text.
4. **Document the task metadata record before implementing it.** A paragraph in
   the guide that argues for three fields and rejects the fourth is worth more
   than the code, because it is the decision that is hard to reverse.
5. **Startup benchmarks under `statprof`.** AGENTS.md asks for a measurement
   before a speedup; nobody has published one, and module resolution is the
   obvious place to look.
6. **One more migration page, and one more spec.** `docs/why-maak.org` has the
   Make table; a page for `just` and Task users is where most newcomers actually
   come from, and a Veritas spec exercising engine detection against a fake
   container binary would harden `container.scm` cheaply.

## Compared to related projects

| Project | What it is | Where maak differs |
| --- | --- | --- |
| GNU Make | The original dependency-driven build tool: file targets, timestamps, pattern rules, a macro language. | maak has no targets and no caching. It runs what it is told and lets Scheme decide what is stale — a generality Make cannot express, at the cost of the free incremental build Make gives away. |
| just | A command runner with a `justfile` of recipes, dependencies, parameters and a deliberately tiny expression language. | just is a better-shaped Make for commands and stays a DSL. maak's tasks are procedures in a general-purpose language, so composition, conditionals and data structures need no extension. |
| Task (go-task) | A YAML task runner with `sources`/`generates` fingerprinting, so tasks skip when inputs are unchanged. | Task has the incremental story maak refuses. maak has a real language where Task has templating and `sh:` blocks, and it wraps Guix, Nix, containers, git and archives as libraries rather than as task-file keys. |
| Babashka tasks (`bb.edn`) | The closest cousin: tasks in a Clojure-flavoured Lisp, run by a fast interpreter, with `:depends` and a task registry. | Both are "the task file is a Lisp program". maak is Guile rather than Clojure, ships a `-j` worker model with block-ordered output, and puts reproducibility (Guix, Nix) in the task language rather than outside it. |
| Mage | Go tasks as functions in a `magefile`, with typed arguments and dependency declarations. | Mage proves the "tasks are functions, not recipes" thesis in a compiled language. maak gets the same shape with an interpreter, a REPL, and docstrings that become the listing without a generated stub. |
| cargo-make | A TOML task runner that reproduces Make's flow: dependencies, conditions, `sources`/`env`, and a large library of ready-made tasks. | cargo-make is configuration and grows a key for every need. maak is code and grows a function instead. |
| Dagger, Earthly | Pipeline engines that execute every step in a container and cache the graph. | Both buy hermetic execution and content-addressed caching by owning the runtime. maak delegates to Guix, Nix and Podman with the same argv discipline and no daemon. |

What the others do that maak could still add: Task's declared input/output
fingerprinting is the one genuinely good idea in the field that maak does not
have, and it should arrive — if it ever does — as an opt-in record on a task
rather than as default semantics. `just --fmt` has no counterpart, and a
canonical printer for Maak files would be a real convenience. Mage's typed
argument parsing is where the lambda-list-as-grammar approach is closest to
winning outright, and the only missing piece is prose for the arguments.

Where maak is ahead and should stay ahead: one language from task definition to
error message; a dry run that is a Scheme parameter and therefore testable, rather
than a flag with special-case behaviour through the whole codebase; argv-first
subprocess handling so a path with a space is never a bug; Guix and Nix as values
a task can decide on, not strings another tool re-reads; and a task listing
generated from docstrings, so documentation cannot drift from behaviour.

## In the long run

**The format outlives the tool.** A Maak file is a Guile module with no private
syntax, so a file written today is readable by any Scheme in fifty years. That is
a claim about substitutes, not about maak: the day maak is unmaintained, the
automation is still a program somebody can run. Anything that would compromise it
— a config file, a task manifest, a compiled intermediate only maak can read — is
out of scope permanently.

**The control plane generalises.** `niri`, `container`, `net`, `git`, `archive`,
`guix`, `nix` and `i18n` are one observation repeated: the interesting automation
in a project's life is rarely the compile. If the bet pays, maak becomes the place
where a machine's release, deployment and environment chores are written down,
and the build is just the first task in the file.

**Agents and editors consume the description, not the output.** A stable,
versioned, machine-readable task description is what lets a tool — an editor, a
CI runner, or an assistant — act on a project without guessing. `maak-tasks` is
the prototype of that interface, and the project's usefulness to non-human callers
rests on promoting it rather than letting seven consumers parse the same file.

## Getting involved

The best first contribution is a Veritas spec for a helper that has none, or a
paragraph in the guide for a behaviour only the code knows; the best second is a
disagreement with something argued here. `AGENTS.md` — generated from the
practices pinned in `maak.scm` — is the repository's own rulebook.

*This document describes intent, not commitments: it names directions the
project would like to take, and no item in it is promised for any release.*
