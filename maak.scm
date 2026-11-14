;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (maak)
  #:declarative? #t
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (maak maak)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  ;; The git and container helpers are imported under prefixes because the
  ;; tasks below are named the way maak exposes them on the command line, and a
  ;; task may not shadow the helper it calls.
  #:use-module ((maak git) #:prefix git:)
  #:use-module ((maak container) #:prefix ct:))

;; ====== Maak file constants ======

;; The URL for the CSS stylesheet used to style API documentation.
(define api-docs-css-url
  "https://jointhefreeworld.org/static/css/jointhefreeworld.css")

;; The path to the directory where the main project files will be deployed.
(define deploy-dir
  "/srv/http/jointhefreeworld.org")

(define project-name
  'maak)

(define hot-reload-file-types
  '(scm))

;; ====== Maak file tasks ======

(define (help)
  "Display program help screen."
  (program-shell (list "maak" "--help")))

(define (default)
  "Default task to run, call program's help."
  (list-tasks))

(define (man)
  "Preview the maak manual page without installing it."
  (run (list "man" "./man/maak.1")))

(define (version)
  "Print the maak version."
  (print-version))

;; ====== The version's copies ======
;;
;; The version is written by hand once, in src/maak/version.scm.  The RPM spec,
;; the Arch recipe, the manual page and the installation page cannot read a
;; Scheme module, so they carry copies, and `sync-version' rewrites them from
;; that file.
;;
;; Which files carry a copy is this Maak file's business rather than the
;; library's, and the version is read from the working tree rather than from the
;; library the task is running under.  Both halves of that are what keep the
;; task free: a helper module would have to exist in the installed maak before
;; the task could start at all, and the version a library knows is its own, so a
;; release cut under an older maak would have written that older number into
;; every copy.

(define version-source
  "src/maak/version.scm")

;; Each site is a file and a regular expression matching one place in it where
;; the version is written, with the version itself as the first group.  Only
;; that group is replaced, so the field's alignment, the `.TH' quoting and the
;; wording of the installation page's sample output stay the file's business
;; rather than this list's.
(define version-sites
  '(("packaging/opensuse/maak.spec"
     . "Version:[ \t]+([0-9]+\\.[0-9]+\\.[0-9]+)")
    ("packaging/arch/PKGBUILD"
     . "pkgver=([0-9]+\\.[0-9]+\\.[0-9]+)")
    ("man/maak.1"
     . "maak ([0-9]+\\.[0-9]+\\.[0-9]+)")
    ("docs/installation.org"
     . "maak-([0-9]+\\.[0-9]+\\.[0-9]+)")
    ("docs/installation.org"
     . "maak ([0-9]+\\.[0-9]+\\.[0-9]+)")))

(define* (sync-version #:optional (directory "."))
  "Write the version into every file that cannot read it from the module, then
commit and tag the release.

The version is written by hand once, in src/maak/version.scm, which is what
`maak --version' prints and what default.nix reads while it evaluates the
derivation.  The RPM spec, the Arch recipe, the manual page and the
installation page cannot read a Scheme module, so they carry copies, and this
task rewrites them from that file.

A release is then the whole ritual in one command: the copies are made to agree,
everything tracked that changed is committed as `chore: Release vX.Y.Z', and
that commit is tagged `vX.Y.Z'.  The commit is `git commit -am', so it takes
every tracked change in the tree rather than only the ones this task made;
commit anything unrelated first if it should not ride along.  A version whose
tag already exists is refused rather than committed again or retagged, and a
tree with nothing left to commit is tagged where it stands.

DIRECTORY is the checkout to work in, the current directory by default.  The
version is read from the file there rather than from the running library, so
the task writes the checkout's version whether it is run by this checkout's
maak or by an older one already installed.

Each copy is found by a regular expression over the file and only the version
inside the match is replaced, so everything around it -- the spec's alignment,
the manual page's quoting, the installation page's wording -- is left exactly
as it was."
  (define repo
    (if (string=? directory ".") "." directory))

  (define (path file)
    (if (string=? directory ".")
        file
        (~ "~a/~a" directory file)))

  (define (read-text file)
    (let ((full (path file)))
      (if (file-exists? full)
          (call-with-input-file full get-string-all)
          (task-error "sync-version: no ~a in ~a"
                      #:args (list file directory)
                      #:hint "Name the checkout's root as DIRECTORY."))))

  (define (write-text file text)
    (call-with-output-file (path file)
      (lambda (port) (display text port))))

  (define (current-version)
    (let ((found (string-match
                  "maak-version[ \t\n]+\"([0-9]+\\.[0-9]+\\.[0-9]+)\""
                  (read-text version-source))))
      (unless found
        (task-error "sync-version: ~a does not define maak-version as a version"
                    #:args (list (path version-source))))
      (match:substring found 1)))

  (define (sites-of file)
    (filter (lambda (site) (string=? (car site) file)) version-sites))

  (define (named-files)
    (fold (lambda (site files)
            (if (member (car site) files)
                files
                (append files (list (car site)))))
          '()
          version-sites))

  (define (rewrite text pattern version)
    "Return TEXT with the version PATTERN names replaced by VERSION."
    (regexp-substitute/global
     #f pattern text 'pre
     (lambda (match)
       (regexp-substitute/global #f "[0-9]+\\.[0-9]+\\.[0-9]+"
                                 (match:substring match 0)
                                 'pre version 'post))
     'post))

  (define (sync-file file version)
    "Write VERSION into FILE, and return whether it had to."
    (let* ((text (read-text file))
           (rewritten (fold (lambda (site text)
                              (rewrite text (cdr site) version))
                            text
                            (sites-of file))))
      (if (string=? text rewritten)
          #f
          (begin (write-text file rewritten) #t))))

  (define (sync-copies! version)
    "Write VERSION into every copy, and return the files that changed."
    (let ((changed '()))
      (for-each (lambda (file)
                  (when (sync-file file version)
                    (set! changed (append changed (list file)))))
                (named-files))
      changed))

  ;; ---- The commit and the tag --------------------------------------------
  ;;
  ;; The library wrappers take `#:directory', so a release of another checkout
  ;; commits there rather than wherever maak was started from.

  (define (checkout?)
    "Return whether DIRECTORY is a git checkout."
    (file-exists? (path ".git")))

  (define (tag-exists? tag)
    "Return whether TAG is already a tag of the checkout."
    (member tag (git:git-tags #:directory repo)))

  (define (changes-to-commit?)
    "Return whether anything tracked differs from the last commit."
    (not (string=? ""
                   (string-trim-both
                    (git:git-output (list "diff" "--name-only" "HEAD")
                                    #:directory repo)))))

  (define (commit-release version)
    "Commit every tracked change as the release VERSION, and return the message."
    (let ((message (~ "chore: Release v~a" version)))
      (unless (git:git (list "commit" "--all" "-m" message)
                       #:directory repo #:allow-failure? #t)
        (task-error "sync-version: git commit failed, so nothing was tagged"))
      message))

  (define (tag-release version)
    "Tag the current commit as the release VERSION, and return the tag."
    (let ((tag (~ "v~a" version)))
      (unless (git:git (list "tag" tag)
                       #:directory repo #:allow-failure? #t)
        (task-error "sync-version: `git tag ~a' failed; the release was committed, but not tagged"
                    #:args (list tag)))
      tag))

  (let* ((version (current-version))
         (tag (~ "v~a" version)))
    (unless (checkout?)
      (task-error "sync-version: ~a is not a git checkout, so there is nothing to commit or tag"
                  #:args (list repo)))
    (when (tag-exists? tag)
      (task-error "sync-version: ~a is already released as ~a; bump ~a first"
                  #:args (list version tag version-source)))
    (let ((changed (sync-copies! version)))
      (if (null? changed)
          (log-info "Every file that names the version already says ~a." version)
          (for-each (lambda (file)
                      (log-info "Wrote ~a into ~a." version file))
                    changed))
      (if (changes-to-commit?)
          (let ((message (commit-release version)))
            (tag-release version)
            (log-info "Committed `~a' and tagged ~a." message tag))
          (begin
            (tag-release version)
            (log-info "Nothing to commit; tagged ~a at the current commit." tag))))))

;; ====== Git and source archives ======
;;
;; The wrappers are library code, `(maak git)', and this file imports it under
;; the `git:' prefix so the tasks below keep the names maak exposes on the
;; command line without shadowing the helpers they call.

(define (git-init . arguments)
  "Initialise a git repository here, with any arguments git init takes."
  (apply git:git-init arguments))

(define (git-status)
  "Show the current branch and a short status of the working tree."
  (git:git-status))

(define (git-add . paths)
  "Stage PATHS, or every change in the tree when none is named."
  (apply git:git-add paths))

(define (git-commit message)
  "Commit every tracked change with MESSAGE."
  (git:git-commit message))

(define (git-tag name)
  "Tag the current commit as NAME, without an annotation."
  (git:git-tag name))

(define (git-branch . names)
  "List the branches, or create the branches NAMES name."
  (apply git:git-branch names))

(define (git-switch branch)
  "Switch to BRANCH, which must already exist."
  (git:git-switch branch))

(define (git-pull . arguments)
  "Pull from the upstream branch, with any arguments git pull takes."
  (apply git:git-pull arguments))

(define (git-push . arguments)
  "Push the current branch, with any arguments git push takes.

A bare push does not send tags that are not annotated; `maak git-push --
--tags' sends them too."
  (apply git:git-push arguments))

(define (git-log . arguments)
  "Show the history as a decorated one-line graph, twenty commits by default."
  (apply git:git-log arguments))

;; Both archives pack the last commit with `git archive', so what ships is what
;; the release commit contains: the tracked files, without `.git' and without
;; anything untracked or ignored.  The prefix is the directory the RPM spec's
;; `%autosetup -n %{name}-%{version}' expects to unpack into.

(define (package-tar)
  "Write the last commit as maak-VERSION.tar.gz, under a maak-VERSION/ prefix."
  (let ((name (~ "~a-~a" project-name maak-version)))
    (git:git-archive-tar (~ "~a.tar.gz" name) #:prefix (~ "~a/" name))))

(define (package-zip)
  "Write the last commit as maak-VERSION.zip, under a maak-VERSION/ prefix."
  (let ((name (~ "~a-~a" project-name maak-version)))
    (git:git-archive-zip (~ "~a.zip" name) #:prefix (~ "~a/" name))))

(define (arithmetics)
  "Do some quick math."
  (let* ((entries '(1 3 5 7 9))
         (arithmetics (lambda (x)
                        (* x x 3)))
         (some-data (map arithmetics entries)))
    (log-info "Performed some quick math:\nResult: ~a" some-data)))

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  (run (list "find" "." "-maxdepth" "8" "-name" "*.scm" "-type" "f"
             "-exec" "guix" "style" "-f" "{}" ";")
       #:verbose? #t))

(define (vars)
  "Display some system variables."
  (log-info "Home directory: ~a\nShell: ~a\nUser: ~a\n"
            (home-dir)
            (or (getenv "SHELL") "")
            (or (getenv "USER") "")))

(define (quick-math x-arg y-arg)
  (let* ((x (string->number x-arg))
         (y (string->number y-arg)))
    (format #t "~a + ~a = ~a" x y
            (+ x y))))

(define (repl)
  "Development REPL."
  (manifest-shell (list "guile" "-L" "./src"
                        "-c" "'((@ (ares server) run-nrepl-server))'")))

(define (docs)
  "Generate technical API documentation for Maak."
  (delete-file-recursively "doc")
  (manifest-shell (list "documenta" "api" "./src/maak"))
  (manifest-shell (list "texi2any" "-v" "--html"
                        (~ "--css-ref=~a" api-docs-css-url)
                        "-o" "index_html" "./doc/api/index.texi"))
  (mv "./index_html" "./doc/api-dist"))

(define (deploy)
  "Generate documentation for the project and publish to web directory."
  (docs)
  (delete-file-recursively (~ "~a/api-docs/maak" deploy-dir))
  (mv "doc/api-dist"
      (~ "~a/api-docs/maak" deploy-dir))
  (delete-file-recursively "doc"))

(define (package-docker)
  "Build a Docker image for Maak."
  (guix-pack '()
             #:format "docker"
             #:image-tag "maak"
             #:symlinks '("/bin=bin")
             #:extra-packages '("bash" "coreutils" "util-linux-with-udev" "guile")
             #:verbose? #t))

(define (push-docker-latest)
  "Push the latest locally generated Docker image to Dockerhub."
  (ct:container (list "push"
                      (~ "localhost/~a:latest" project-name)
                      (~ "docker.io/jjba23/~a:latest" project-name))
                #:verbose? #t))

;; A push needs an image to push, and the task that builds one is a step of its
;; own rather than something a push runs inline: `--dry-run' then shows the two
;; commands in the order they would run, and `maak package-docker' stays a thing
;; to run on its own.
(task-metadata! push-docker-latest
                #:depends '(package-docker)
                #:example "maak push-docker-latest")

(define (push-docker-tag)
  "Push the locally generated Docker image to Dockerhub."
  (git:git-fetch "--all")
  (let ((latest-tag (git:git-describe-tag)))
    (ct:container (list "push"
                        (~ "localhost/~a:latest" project-name)
                        (~ "docker.io/jjba23/~a:~a" project-name latest-tag))
                  #:verbose? #t)))

(task-metadata! push-docker-tag #:depends '(package-docker))

(define deb-packages
  (make-parameter (map symbol->string
                       '(build-essential debhelper devscripts cme lintian
                                         guile-3.0-dev))))

(define (deb-build)
  "Build .deb package (unsigned)"
  (run (list "apt" "update")
       #:sudo? #t
       #:verbose? #t)
  (run (append (list "apt" "install" "-y")
               (deb-packages))
       #:sudo? #t
       #:verbose? #t)

  (dynamic-wind
   ;; Setup: create temporary symlink at project root
   (lambda ()
     (ln-s "packaging/debian" "debian"
           #:force? #f
           #:verbose? #t))

   ;; Execution: run debian build commands against ./debian
   (lambda ()
     (run (list "mk-build-deps" "-i" "-r" "debian/control")
          #:sudo? #t
          #:verbose? #t)
     (run (list "debuild" "-b" "-uc" "-us")
          #:verbose? #t))

   ;; Cleanup: remove symlink safely (leaves packaging/debian intact)
   (lambda ()
     (rm-rf "debian"
            #:verbose? #t))))

(define (rpm-build)
  "Build RPM package for openSUSE and Fedora/RHEL"
  (let* ((latest-tag (git:git-describe-tag))
         (latest-tag-num (cond ((string=? latest-tag "") "")
                               ((string-prefix? "v" latest-tag)
                                (string-drop latest-tag 1))
                               (else latest-tag)))
         (sources (~ "~a/rpmbuild/SOURCES" (home-dir)))
         (specs (~ "~a/rpmbuild/SPECS" (home-dir)))
         (rpms (~ "~a/rpmbuild/RPMS/x86_64" (home-dir)))
         (srpms (~ "~a/rpmbuild/SRPMS" (home-dir))))
    (run (list "rpmdev-setuptree")
         #:verbose? #t)
    (git:git-archive-tar (~ "~a/maak-~a.tar.gz" sources latest-tag-num)
                         #:prefix (~ "maak-~a/" latest-tag-num)
                         #:revision latest-tag)
    (cp "packaging/opensuse/maak.spec" specs
        #:verbose? #t)
    (run (list "rpmbuild" "-ba" (~ "~a/maak.spec" specs))
         #:verbose? #t)
    (run (list "ls" (~ "~a/maak-[0-9]*.rpm" rpms))
         #:expand? #t)
    (let* ((latest-rpm
            (string-trim-both
             (capture (~ "ls -t ~a/maak-[0-9]*.rpm | head -n 1" rpms))))
           (latest-src-rpm
            (string-trim-both
             (capture (~ "ls -t ~a/maak-[0-9]*.rpm | head -n 1" srpms)))))
      (cp "packaging/opensuse/maak-rpmlintrc" rpms
          #:verbose? #t)
      (run (list "rpmlint" latest-rpm)
           #:verbose? #t)
      (run (list "rpmlint" latest-src-rpm)
           #:verbose? #t))))

(define (hot-reload)
  "Hot-reloading developer's setup."
  (manifest-shell (list "watchexec" "'maak --list'"
                        "--clear"
                        (~ "--exts ~a"
                           (string-join (map symbol->string
                                             hot-reload-file-types) ","))
                        "--on-busy-update=restart"
                        "--shell" "bash"
                        "--watch" ".")))

(define (hr)
  "Alias to hot-reloading."
  (hot-reload))

(define (dev)
  "Developer's setup."
  (hot-reload))

(define (pure-dev)
  "Pure isolated development shell."
  (guix-shell (list "maak" "--list")
              #:pure? #t
              #:file "guix.scm"
              #:packages '("bash" "coreutils" "util-linux-with-udev" "guile")
              #:verbose? #t))

(define (nix-dev)
  "Enter the Nix development shell this project's flake declares."
  (nix-develop (list "maak" "--list")))

(define (flake-check)
  "Validate every output of this project's Nix flake."
  (nix-flake-check #:flake "."))

;; ====== Maak tests ======
;;
;; The suite runs once, in the environment the manifest declares.  The Veritas
;; variables go through `#:env' rather than as a `VAR=value' prefix: everything
;; after `guix shell --' is executed directly, so `env' sets them for the guile
;; that runs the tests.
(define veritas-env
  '("VERITAS_TEST_MODULE=veritas"
    "VERITAS_TEST_TOP_DIR=test"
    "VERITAS_TEST_TYPE=unit"
    "VERITAS_RUN_BENCHMARKS=false"))

(define test-command
  (list "guile" "-L" "./src" "-L" "./test"
        "-c" "'(exit ((@ (veritas runner) run-tests)))'"))

;; There is deliberately no task that audits the suite on top of running it.
;; Two failure modes are worth naming, because deciding against a reader of the
;; run means living with them: a test whose body never builds an
;; assertion, and a case that writes more than one assertion, of which only the
;; last is read.  Both pass silently, so neither the exit status nor a count of
;; tests catches either one; what shows them is the report, where a test with no
;; case under it ran nothing at all.  That is why the suite is kept to one
;; assertion per case and why the rules are written down in docs/development.org.
(define (test)
  "Run the project's unit test suite with Veritas.

The specs live under test/veritas/unit and are located by the Veritas runner,
which needs both the library sources and the test tree on the load path."
  (manifest-shell test-command
                  #:env veritas-env))

(define (container)
  "Run the same unit test suite inside a Guix container, with no network."
  (guix-container test-command
                  #:manifest "manifest.scm"
                  #:env veritas-env))

;; ====== Maak test playground =======

(define (test-playground)
  "Play with temporary files and dry-run scopes."
  (define (new-temp-file)
    (let* ((now (localtime (current-time)))
           (time-string (string-replace-substring (strftime "%c" now) " " "-"))
           (temp-file-name (~ "./tmp/~a" time-string)))
      temp-file-name))

  (let* ((temp-file-name (new-temp-file)))
    (mkdir-p "./tmp")
    (touch temp-file-name)
    (touch temp-file-name)
    (parameterize ((dry-run? #t))
      (touch temp-file-name)
      (delete-file-recursively temp-file-name))
    (delete-file-recursively temp-file-name)))

;; ====== Agent skills ======

;; AGENTS.md is generated, never edited by hand.  The section has two halves:
;; the data below, which is this project's, and the task itself, which is not.
;; Another repository copies the whole section and replaces the data - the
;; header, the pinned skills (repository, commit, path, licence) and the
;; practices - and `maak agents' writes its own AGENTS.md.  Regeneration is
;; deterministic: the same pins and the same text produce the same file, byte
;; for byte, and it needs nothing but curl.  The skill URLs assume GitHub, so a
;; skill hosted elsewhere needs a URL of its own.

(define ponytail-commit
  "96608e078b2d5ea607c5c3b8b5a54d1c1fba6846")

(define tdd-commit
  "22f0b463e7a5e579904dd6211a520321035526f7")

;; Name, repository, commit, the path the repository keeps the skill at and the
;; licence.  A skill arrives whole: trimming it means scanning for headings, and
;; such a scan cannot tell a heading from a `##' line inside a code fence.
(define agent-skills
  (list (list "ponytail"
              "DietrichGebert/ponytail"
              ponytail-commit
              "skills/ponytail/SKILL.md"
              "MIT")
        (list "test-driven-development"
              "obra/superpowers"
              tdd-commit
              "skills/test-driven-development/SKILL.md"
              "MIT")))

(define agent-skills-dir
  "reports/agent-skills")

(define agents-header
  "# AGENTS.md

Generated by `maak agents` from the pinned skills and the practices in
`maak.scm`; edit that task, never this file.  The pins and the text are the whole
input, so regenerating gives the same document.  The repository's own rules win
over general advice, and the narrowest rule here wins over the broadest.
")

;; Practices this project keeps.  They are short on purpose: a rule a reader
;; checks in a second is a rule that gets followed.  The pinned skills are the
;; long form of the same habits.
(define agent-practices
  (list
   "## Documentation

- Every user-visible change reaches the documentation: the page that owns the
  feature, the manual page for a flag or an environment variable, and the
  release notes under the unreleased heading.
- Keep the table of contents in step with the headings.  A section with no entry
  is a section nobody finds.
- The guide is Org: one `*` heading per page, `**` for sections, `#+TITLE` and
  `#+EMAIL` at the top, a `CUSTOM_ID` on anything linked from elsewhere, and
  `file:` links between pages.
- Explain why and when, never what: the code already says what.  A docstring's
  first line works as a summary on its own.
- Spell an identifier the way the code spells it - the exact name, not a
  description of it.
- An example must run as written, copied from a real invocation.
- No promises about the future and no changelog in the guide; history lives in
  the release notes.
"
   "## Security

- No secrets in the tree.  Tokens, keys and passwords arrive through the
  environment, and no recipe, workflow or example embeds one.
- Validate at the edge: anything from a file, a report, a path or a subprocess
  is untrusted until it has been checked.
- Never build a shell string out of data.  Tasks pass argument lists, and Guile
  calls `(system \"program\" \"arg\")` or `open-pipe*`, never a command
  assembled with `string-append`.
- Do not `eval` or `read` input you did not write; parse with `(json)` or a
  reader built for the format.
- Escape what leaves the process: a name that came from input is escaped when
  it is written to XML, HTML or a data file.
- Fail closed.  A file that cannot be read is an error, not an empty result.
- Input never chooses the path of a written file; writes stay inside the
  directory reserved for them.
- A dependency is a security decision: it must be needed, MIT compatible and
  pinned.
- Report a vulnerability privately, not in a public issue.
"
   "## Performance

- Measure first.  A before and after belongs in the benchmark, and a speedup
  claimed without a number is not a speedup.
- Prefer not doing the work: cache it, skip it, batch it, or delete the call,
  before making the call faster.
- Then the algorithm, then the data structure (hash table, vector), then the
  micro-optimisation.  In that order, for that reason.
- Guile specifics: keep module resolution and long `assoc` chains out of loops,
  use `#:declarative?` modules and compiled `.go` files at startup, and reach
  for `statprof` instead of guessing where the time goes.
- A shortcut that buys speed costs clarity; it gets a measurement and a
  `shortcut:` comment saying what it costs and when to revisit it.
- Coverage instrumentation is slow by design: iterate without it, and measure
  before finishing.
"
   "## Architecture decisions

- A decision that is expensive to reverse gets a short record: the context, the
  decision, the consequences, and the alternatives that lost.  A small decision
  belongs in the commit body instead.
- Two paragraphs beat a design document; record the choice while it is still a
  choice, not after it has become a fact.
- An accepted record is not rewritten.  Supersede it with a new one and link the
  two directions.
- The record lives next to its subject: the documentation for anything a user
  would notice, the release notes for the release-level result.
- Changes to the public API, a file format, the pipeline or the licence get this
  treatment at a minimum in the commit body.
"
   "## Semantic versioning

- The project follows SemVer 2.0.0 for its public API: the exported functions,
  the CLI flags and environment variables, and the shape of its file formats.
- A breaking change bumps MAJOR, a new feature MINOR, a fix PATCH.  A new export
  is MINOR; removing or changing one is MAJOR.
- One version string, named everywhere it appears: the source, the build
  recipes, the manual page and the packaging.
- A release is a dated release-notes section, an annotated `vX.Y.Z` tag on the
  release commit, and a packaging checksum updated afterwards from the published
  archive.
"
   "## Conventional commits

- `type(scope): summary`, imperative, lower case, no full stop, about seventy
  characters.
- Types in use: `feat`, `fix`, `perf`, `refactor`, `test`, `docs`, `build`, `ci`,
  `chore`.  A `!` after the type or a `BREAKING CHANGE:` footer marks a MAJOR
  change, and says what breaks.
- The subject says what changed.  The body says why, and what was rejected.
- One logical change per commit: a refactor and a fix travel apart, so either
  can be reverted alone.
- Run the full validation task before committing, and never commit a red suite
  without saying so in the message.
- Reference the issue in the footer - `Closes #12` - not in the subject.
"
   "## Functional programming and immutability

- Pure functions in the core, effects at the edges.  Whatever touches the file
  system or the clock sits at the edge; the core does not.
- Prefer `let` and new values to `set!`.  Build results with `map`, `filter`,
  `fold` and named `let`, not with index loops that push into a list.
- A record is a value: construct a new one instead of mutating a shared one, and
  pass configuration as arguments or parameters rather than as globals.
- Handle an exception at the boundary: catch it there, turn it into a result
  record, and let the caller decide what it means.
- A function that both computes and prints is two functions.
- Temporary effects - a changed directory, a bound environment variable - go
  through `dynamic-wind`.
- Prefer the expression style: `if` and `cond` that return a value, not a branch
  that assigns one.
"
   "## Do not reinvent the wheel

- Guile's standard library first: `(ice-9 ...)` for the runtime, `(srfi srfi-1)`
  for lists, `(srfi srfi-9)` for records, `(srfi srfi-13)` for strings, `(json)`
  for JSON.  The project already depends on all of them.
- Then this codebase: read the existing modules before writing a helper, and use
  the one that is nearly right instead of adding a second one.
- A new dependency needs a reason the standard library cannot meet, a licence
  the project can carry, and a paragraph in the commit body.
- Prefer deleting a special case to writing code that handles it.
- The best diff removes lines and keeps the behaviour; the second best is a
  small diff that is obvious a year later.
"))

(define (slurp path)
  "Return the whole of @var{path} as a string."
  (call-with-input-file path
    (lambda (port)
      (let loop ((lines '())
                 (line (read-line port)))
        (if (eof-object? line)
            (string-concatenate-reverse lines)
            (loop (cons (string-append line "\n") lines)
                  (read-line port)))))))

(define (fetch-skill name repository commit path)
  "Download one skill pinned at @var{commit}, into the reports tree.

  Returns the skill's text."
  (let ((target (~ "~a/~a.md" agent-skills-dir name))
        (url (~ "https://raw.githubusercontent.com/~a/~a/~a"
                repository commit path)))
    (run (list "curl" "--fail" "--silent" "--show-error" "--location"
               "--output" target url)
         #:verbose? #t)
    (slurp target)))

(define (agents)
  "Regenerate AGENTS.md from the pinned skills and the practices."
  (run (list "mkdir" "-p" agent-skills-dir))
  ;; Every download happens before the file is opened, so a fetch that fails
  ;; leaves the previous AGENTS.md in place rather than a half-written one.
  (let ((skills (map (lambda (skill)
                       (append skill
                               (list (fetch-skill (list-ref skill 0)
                                                  (list-ref skill 1)
                                                  (list-ref skill 2)
                                                  (list-ref skill 3)))))
                     agent-skills)))
    (call-with-output-file "AGENTS.md"
      (lambda (port)
        (display agents-header port)
        (for-each (lambda (skill)
                    (format port
                            "~%## ~a~%~%`~a` at `~a`, `~a`, ~a licensed.~%~%"
                            (list-ref skill 0)
                            (list-ref skill 1)
                            (list-ref skill 2)
                            (list-ref skill 3)
                            (list-ref skill 4))
                    (display (list-ref skill 5) port))
                  skills)
        (for-each (lambda (practice)
                    (newline port)
                    (display practice port))
                  agent-practices)))))
