;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Cargo and Rust helpers, and the pieces a crate reaches for that are not a
;; `cargo build': the metadata `cargo metadata' hands back as JSON, the path an
;; artefact lands at, and the toolchains rustup manages.
;;
;; There is no build engine here either.  `cargo-argv' is a pure builder: it
;; returns the words of a cargo invocation, so a test asserts the command line
;; without a Rust toolchain installed, and `cargo' feeds that list to `run', so
;; `--dry-run' prints it and `--plan' records it.  An option that does not fit
;; the keywords travels as an argument, which is how a subcommand maak has never
;; heard of is still a cargo command.
;;
;; The programs are parameters, honoured from MAAK_CARGO then CARGO and
;; MAAK_RUSTUP then RUSTUP.  A value may carry a wrapper or a toolchain
;; (`"cargo +nightly"'), which is split into words the way an unquoted shell word
;; is, so a task can pin a nightly for one command.
;;
;; `cargo metadata' is read even in a dry run, like pkg-config: it reads the
;; project rather than changing it, and a plan that left out the version a
;; package task would publish would be a plan of a different release.

;;; Code:

(define-module (maak cargo)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak formats)
  #:export (cargo-command
            rustup-command
            cargo-argv
            cargo
            cargo-output
            cargo-metadata
            cargo-package
            cargo-package-version
            cargo-package-names
            cargo-binaries
            cargo-target-directory
            cargo-artifact-path
            cargo-build
            cargo-check
            cargo-test
            cargo-run
            cargo-bench
            cargo-fmt
            cargo-clippy
            cargo-doc
            cargo-clean
            cargo-fetch
            cargo-update
            cargo-publish
            cargo-install
            rustup-argv
            rustup
            rustup-output
            rustup-active-toolchain
            rustup-toolchains
            rustup-install
            rustup-target-add
            rustup-component-add))

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

(define cargo-command
  (make-parameter (env-value '("MAAK_CARGO" "CARGO") "cargo")))

(define rustup-command
  (make-parameter (env-value '("MAAK_RUSTUP" "RUSTUP") "rustup")))

(define (program-words command what)
  "Return COMMAND as the words of a program invocation, or raise.

A value may carry flags or a toolchain, so it is split the way an unquoted
shell word is split; an empty value is a maak error rather than a command with
no program."
  (let ((words (and (string? command)
                    (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "~a must name a program, got ~s" what command)
       #:hint "Set MAAK_CARGO or MAAK_RUSTUP, or pass the program as a string."))
    words))

(define (string-list value what)
  "Return VALUE as a list of strings, accepting one string as a list of one."
  (let ((items (if (string? value) (list value) value)))
    (unless (and (list? items) (every string? items))
      (raise-maak-error
       (format #f "~a takes a string or a list of strings, got ~s" what value)
       #:hint "Write '(\"openssl\" \"serde\") for the feature names."))
    items))

(define (require-string value what)
  (unless (and (string? value) (not (string=? value "")))
    (raise-maak-error
     (format #f "~a takes a non-empty string, got ~s" what value)
     #:hint "Name the value as a string, or leave the option out."))
  value)

;; ====== The cargo command line ======

(define* (cargo-argv subcommand
                     #:key
                     (arguments '())
                     (manifest-path #f)
                     (package #f)
                     (features '())
                     (no-default-features? #f)
                     (all-features? #f)
                     (target #f)
                     (release? #f)
                     (profile #f)
                     (jobs #f)
                     (offline? #f)
                     (locked? #f)
                     (frozen? #f)
                     (quiet? #f)
                     (verbose #f)
                     (color #f)
                     (message-format #f)
                     (timings? #f))
  "Return the argv that runs cargo SUBCOMMAND, without running it.

Global options are placed before SUBCOMMAND and the command's own options after
it, which is the grammar cargo accepts for every subcommand.  ARGUMENTS are
appended as they are, so an option this builder does not name, or the words a
`cargo run' passes to the program after `--', still reach cargo.

FEATURES takes a string or a list and is joined with commas, the way `--features'
spells it.  VERBOSE is #t for one `-v' or a positive number for that many.
RELEASE? and PROFILE cannot both be given: cargo spells the release profile both
ways, and asking for two profiles is a question with no answer.

@example
(cargo-argv \"build\" #:release? #t #:features '(\"openssl\" \"serde\"))
@result{} (\"cargo\" \"build\" \"--features\" \"openssl,serde\" \"--release\")

(cargo-argv \"run\" #:arguments '(\"--\" \"--help\"))
@result{} (\"cargo\" \"run\" \"--\" \"--help\")
@end example"
  (unless (and (string? subcommand) (not (string=? subcommand "")))
    (raise-maak-error
     (format #f "cargo needs a subcommand, got ~s" subcommand)
     #:hint "Write (cargo-build), or (cargo \"build\" #:arguments '(\"--release\"))."))
  (let ((features (string-list features "cargo"))
        (arguments (string-list arguments "cargo")))
    (when (and release? profile)
      (raise-maak-error
       "cargo: --release and --profile ask for two profiles"
       #:hint "Write #:release? #t or #:profile \"release\", not both."))
    (unless (or (not jobs) (and (exact-integer? jobs) (> jobs 0)))
      (raise-maak-error
       (format #f "cargo takes a positive job count, got ~s" jobs)
       #:hint "Write #:jobs 4, or omit it for cargo's default."))
    (unless (or (not verbose)
                (eq? verbose #t)
                (and (exact-integer? verbose) (> verbose 0)))
      (raise-maak-error
       (format #f "cargo's #:verbose takes #t or a positive number, got ~s"
               verbose)
       #:hint "Write #:verbose #t for one -v, or #:verbose 2 for two."))
    (when (and color (not (member color '("auto" "always" "never"))))
      (raise-maak-error
       (format #f "cargo's #:color takes auto, always or never, got ~s" color)
       #:hint "Write #:color \"never\" to keep a log readable."))
    (append
     (program-words (cargo-command) "cargo")
     (if manifest-path (list "--manifest-path" manifest-path) '())
     (if offline? (list "--offline") '())
     (if locked? (list "--locked") '())
     (if frozen? (list "--frozen") '())
     (if quiet? (list "--quiet") '())
     (append (cond ((not verbose) '())
                   ((eq? verbose #t) (list "-v"))
                   (else (make-list verbose "-v")))
             '())
     (if color (list "--color" color) '())
     (if jobs (list "--jobs" (number->string jobs)) '())
     (list subcommand)
     (if package (list "--package" package) '())
     (if (pair? features) (list "--features" (string-join features ",")) '())
     (if no-default-features? (list "--no-default-features") '())
     (if all-features? (list "--all-features") '())
     (if target (list "--target" target) '())
     (cond (release? (list "--release"))
           (profile (list "--profile" profile))
           (else '()))
     (if message-format (list "--message-format" message-format) '())
     (if timings? (list "--timings") '())
     arguments)))

(define* (cargo subcommand #:key (arguments '()) (verbose? #f))
  "Run cargo SUBCOMMAND with ARGUMENTS, as separate words.

This is the short form: the words after SUBCOMMAND are arguments, so
`(cargo-build \"--release\")' passes cargo's own `--release'.  Use `cargo-argv'
when the structured options are wanted, and `run' to execute what it returns.

@example
(cargo \"test\" #:arguments '(\"--workspace\" \"--all-features\"))
@end example"
  (run (cargo-argv subcommand #:arguments arguments) #:verbose? verbose?))

(define* (cargo-output argv #:key (allow-failure? #f))
  "Run the cargo ARGV and return its standard output as a string.

The query runs even in a dry run: it reads the project rather than changing it,
and a plan that left out what `cargo metadata' says would be a plan of a
different project.  A failure raises a maak error naming the command, or comes
back as #f when the caller asked for the exit status instead."
  (unless (and (list? argv) (pair? argv) (every string? argv))
    (raise-maak-error
     "cargo-output expects a non-empty list of strings"
     #:hint "Build it with cargo-argv, which returns exactly that."))
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
       (format #f "cargo was terminated by a signal: ~a" line)
       #:hint "Run the command above by hand to read cargo's own message."))
     (else
      (raise-maak-error
       (format #f "cargo failed (exit code ~a): ~a" code line)
       #:hint "Run the command above to read cargo's own message.")))))

;; ====== What cargo metadata says ======

(define* (cargo-metadata #:key (manifest-path #f) (no-deps? #f)
                         (offline? #f) (locked? #f))
  "Return `cargo metadata' as parsed JSON, a string-keyed alist.

This is how a task learns the workspace's members, a package's version and
where its artefacts land, without guessing at a path or re-reading Cargo.toml.
The query runs even in a dry run."
  (json-read-string
   (cargo-output
    (cargo-argv "metadata"
                #:manifest-path manifest-path
                #:offline? offline?
                #:locked? locked?
                #:arguments (append '("--format-version" "1")
                                    (if no-deps? '("--no-deps") '()))))
   #:source "cargo metadata"))

(define (metadata-packages metadata)
  "Return the packages metadata describes, in the order cargo lists them."
  (data-ref metadata "packages" '()))

(define* (cargo-package #:key (name #f) (manifest-path #f))
  "Return the metadata of one package, or raise when it cannot be chosen.

Without NAME the answer is the only package in the workspace; a workspace with
several packages needs one named, because picking the first would silently
describe the wrong crate."
  (let ((packages (metadata-packages (cargo-metadata #:manifest-path manifest-path))))
    (cond
     ((null? packages)
      (raise-maak-error
       "cargo reports no package"
       #:hint "Check Cargo.toml, and that it is the manifest cargo reads."))
     (name
      (or (find (lambda (package)
                  (string=? name (data-ref package "name" "")))
                packages)
          (raise-maak-error
           (format #f "cargo knows no package called ~a" name)
           #:hint (format #f "The workspace has: ~a"
                          (string-join
                           (map (lambda (package)
                                  (data-ref package "name" ""))
                                packages)
                           ", ")))))
     ((null? (cdr packages)) (car packages))
     (else
      (raise-maak-error
       (format #f "the workspace has ~a packages; name one" (length packages))
       #:hint (format #f "Write #:package \"NAME\"; the workspace has: ~a"
                      (string-join
                       (map (lambda (package)
                              (data-ref package "name" ""))
                            packages)
                       ", ")))))))

(define* (cargo-package-version #:key (package #f) (manifest-path #f))
  "Return the version cargo reports for PACKAGE, or for the only package."
  (data-ref (cargo-package #:name package #:manifest-path manifest-path)
            "version"
            ""))

(define* (cargo-package-names #:key (manifest-path #f))
  "Return the names of the packages in the workspace, in cargo's order."
  (map (lambda (package) (data-ref package "name" ""))
       (metadata-packages (cargo-metadata #:manifest-path manifest-path))))

(define* (cargo-binaries #:key (package #f) (manifest-path #f))
  "Return the names of the binary targets of PACKAGE, or of the only package."
  (let* ((selected (cargo-package #:name package #:manifest-path manifest-path))
         (targets (data-ref selected "targets" '())))
    (filter-map (lambda (target)
                  (let ((kinds (data-ref target "kind" '())))
                    (and (member "bin" kinds)
                         (data-ref target "name"))))
                targets)))

(define* (cargo-target-directory #:key (manifest-path #f))
  "Return the directory cargo writes its artefacts into."
  (data-ref (cargo-metadata #:manifest-path manifest-path #:no-deps? #t)
            "target_directory"
            ""))

(define* (cargo-artifact-path name
                              #:key (profile #f) (release? #f) (triple #f)
                              (manifest-path #f))
  "Return where cargo writes the artefact called NAME.

PROFILE is the cargo profile, `debug' by default; RELEASE? is the shorthand for
`release'.  TRIPLE names a cross-compilation target's directory.  The path is
composed from `cargo metadata''s target directory, so a workspace with
`CARGO_TARGET_DIR' set answers with the directory cargo will really use."
  (require-string name "cargo-artifact-path")
  (when (and release? profile)
    (raise-maak-error
     "cargo-artifact-path: #:release? and #:profile ask for two profiles"
     #:hint "Write #:release? #t or #:profile \"release\", not both."))
  (let ((profile (or profile (if release? "release" "debug")))
        (target-directory (cargo-target-directory #:manifest-path manifest-path)))
    (string-append target-directory "/"
                   (if triple (string-append triple "/") "")
                   profile "/" name)))

;; ====== The common commands ======
;;
;; A wrapper is one line so that `maak --list' shows a task that reads like the
;; command it runs.  Any option the wrapper does not name travels as an
;; argument; the structured builder is `cargo-argv'.

(define (cargo-build . arguments)
  "Build the package with cargo, passing ARGUMENTS."
  (cargo "build" #:arguments arguments))

(define (cargo-check . arguments)
  "Type-check the package with cargo, passing ARGUMENTS."
  (cargo "check" #:arguments arguments))

(define (cargo-test . arguments)
  "Run the tests with cargo, passing ARGUMENTS."
  (cargo "test" #:arguments arguments))

(define (cargo-run . arguments)
  "Run the binary with cargo; put the program's own words after \"--\"."
  (cargo "run" #:arguments arguments))

(define (cargo-bench . arguments)
  "Run the benchmarks with cargo, passing ARGUMENTS."
  (cargo "bench" #:arguments arguments))

(define (cargo-fmt . arguments)
  "Format the sources with `cargo fmt', passing ARGUMENTS."
  (cargo "fmt" #:arguments arguments))

(define (cargo-clippy . arguments)
  "Lint the package with `cargo clippy', passing ARGUMENTS."
  (cargo "clippy" #:arguments arguments))

(define (cargo-doc . arguments)
  "Build the documentation with cargo, passing ARGUMENTS."
  (cargo "doc" #:arguments arguments))

(define (cargo-clean . arguments)
  "Remove the build artefacts with cargo, passing ARGUMENTS."
  (cargo "clean" #:arguments arguments))

(define (cargo-fetch . arguments)
  "Fetch the dependencies with cargo, passing ARGUMENTS."
  (cargo "fetch" #:arguments arguments))

(define (cargo-update . arguments)
  "Update the dependencies with cargo, passing ARGUMENTS."
  (cargo "update" #:arguments arguments))

(define (cargo-publish . arguments)
  "Publish the package with cargo, passing ARGUMENTS.

A dry run of the release is `(cargo-publish \"--dry-run\")', and cargo's own
flag is what makes it one: maak's dry run prints the command instead."
  (cargo "publish" #:arguments arguments))

(define (cargo-install . arguments)
  "Install a cargo binary, passing ARGUMENTS."
  (cargo "install" #:arguments arguments))

;; ====== rustup ======

(define (rustup-argv . arguments)
  "Return the argv that runs rustup with ARGUMENTS."
  (append (program-words (rustup-command) "rustup")
          arguments))

(define (rustup . arguments)
  "Run rustup with ARGUMENTS."
  (run (apply rustup-argv arguments)))

(define* (rustup-output arguments #:key (allow-failure? #f))
  "Run rustup with ARGUMENTS and return its standard output."
  (let* ((line (argv->string (apply rustup-argv arguments)))
         (port (open-input-pipe line))
         (output (get-string-all port))
         (status (close-pipe port))
         (code (status:exit-val status)))
    (cond
     ((and code (zero? code)) output)
     (allow-failure? #f)
     (else
      (raise-maak-error
       (format #f "rustup failed (exit code ~a): ~a" (or code "signal") line)
       #:hint "Install rustup, or run the command above to read its message.")))))

(define (first-word line)
  "Return the first whitespace-separated word of LINE, or the empty string."
  (let ((words (string-tokenize (string-trim-both line))))
    (if (pair? words) (car words) "")))

(define (rustup-active-toolchain)
  "Return the toolchain rustup would use here, as its name."
  (first-word (rustup-output '("show" "active-toolchain"))))

(define (rustup-toolchains)
  "Return the names of the installed toolchains."
  (filter (lambda (name) (not (string=? name "")))
          (map first-word
               (string-split (string-trim-both (rustup-output '("toolchain" "list")))
                             #\newline))))

(define (rustup-install toolchain)
  "Install TOOLCHAIN, for example \"nightly\"."
  (require-string toolchain "rustup-install")
  (rustup "toolchain" "install" toolchain))

(define (rustup-target-add target)
  "Add the compilation TARGET to the active toolchain."
  (require-string target "rustup-target-add")
  (rustup "target" "add" target))

(define (rustup-component-add component)
  "Add COMPONENT, for example \"rustfmt\" or \"clippy\", to the active toolchain."
  (require-string component "rustup-component-add")
  (rustup "component" "add" component))
