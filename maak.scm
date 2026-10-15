;; maak

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; maak is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; maak is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with maak.  If not, see <https://www.gnu.org/licenses/>.

(define-module (maak)
  #:declarative? #t
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module (maak maak)
  #:use-module (maak veritas))

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

;; ====== Maak abstractions ======

(define (syscall-with-output cmd)
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process) process-output))

;; ====== Maak file tasks ======

(define (help)
  "Display program help screen."
  (program-shell '("maak --help")))

(define (default)
  "Default task to run, call program's help."
  (list-tasks))

(define (man)
  "Preview the maak manual page without installing it."
  ($ '("man ./man/maak.1")))

(define (version)
  "Print the maak version."
  (print-version))

(define (arithmetics)
  "Do some quick math."
  (let* ((entries '(1 3 5 7 9))
         (arithmetics (lambda (x)
                        (* x x 3)))
         (some-data (map arithmetics entries)))
    (log-info "Performed some quick math:\nResult: ~a" some-data)))

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  ($ '("find . -maxdepth 8 -name '*.scm'" "-type f -exec guix style -f {} \\;")
     #:verbose? #t))

(define (vars)
  "Display some system variables."
  ($ '("echo -n \"\nHome directory: ${HOME}\"" "echo -n \"\nShell: ${SHELL}\"")
     #:join " && ")
  ($ '("echo -n \"\nUser: ${USER}\"")))

(define (quick-math x-arg y-arg)
  (let* ((x (string->number x-arg))
         (y (string->number y-arg)))
    (format #t "~a + ~a = ~a" x y
            (+ x y))))

(define (repl)
  "Development REPL."
  (manifest-shell '("guile -L ./src"
                    "-c '((@ (ares server) run-nrepl-server))'")))

(define (docs)
  "Generate technical API documentation for Maak."
  (delete-file-recursively "doc")
  (manifest-shell (list "documenta api ./src/maak && "
                        (~ "texi2any -v --html --css-ref=~a -o index_html"
                           api-docs-css-url) "./doc/api/index.texi"))
  (mv "./index_html" "./doc/api-dist"))

(define (deploy)
  "Generate documentation for the project and publish to web directory."
  (docs)
  (remkdir-p (~ "~a/api-docs/maak" deploy-dir))
  (mv "doc/api-dist/*"
      (~ "~a/api-docs/maak" deploy-dir))
  (delete-file-recursively "doc"))

(define (package-docker)
  "Create a Docker image for Maak."
  ($ '("guix pack --file=./guix.scm" "--format=docker" "--image-tag=maak"
       "-S /bin=bin" "bash coreutils util-linux-with-udev guile")))

(define (push-docker-latest)
  "Push the latest locally generated Docker image to Dockerhub."
  ($ (list (~ "podman push localhost/~a:latest" project-name)
           (~ "docker.io/jjba23/~a:latest" project-name))
     #:verbose? #t))

(define (push-docker-tag)
  "Push the locally generated Docker image to Dockerhub."
  ($ '("git fetch --all"))
  (let* ((latest-tag (syscall-with-output "git describe --tags --abbrev=0")))
    ($ (list (~ "podman push localhost/~a:latest" project-name)
             (~ "docker.io/jjba23/~a:~a" project-name latest-tag))
       #:verbose? #t)))

(define deb-packages
  (make-parameter (map symbol->string
                       '(build-essential debhelper devscripts cme lintian
                                         guile-3.0-dev))))

(define (deb-build)
  "Build .deb package (unsigned)"
  ($ '("sudo apt update")
     #:verbose? #t)
  ($ (cons "sudo apt install -y"
           (deb-packages))
     #:verbose? #t)

  (dynamic-wind
   ;; Setup: create temporary symlink at project root
   (lambda ()
     ($ '("ln -s packaging/debian debian")
        #:verbose? #t))

   ;; Execution: run debian build commands against ./debian
   (lambda ()
     ($ '("sudo mk-build-deps -i -r debian/control")
        #:verbose? #t)
     ($ '("debuild -b -uc -us")
        #:verbose? #t))

   ;; Cleanup: remove symlink safely (leaves packaging/debian intact)
   (lambda ()
     ($ '("rm -rf debian")
        #:verbose? #t))))

(define (rpm-build)
  "Build RPM package for openSUSE and Fedora/RHEL"
  (let* ((latest-tag (syscall-with-output "git describe --tags --abbrev=0"))
         (latest-tag-num (string-trim-both (string-drop latest-tag 1))))
    ($ '("rpmdev-setuptree")
       #:verbose? #t)
    ($ (list "git archive --format=tar.gz"
             (~ "--prefix=maak-~a/" latest-tag-num)
             (~ "-o ~~/rpmbuild/SOURCES/maak-~a.tar.gz" latest-tag-num)
             latest-tag)
       #:verbose? #t)
    ($ '("cp packaging/opensuse/maak.spec ~/rpmbuild/SPECS/")
       #:verbose? #t)
    ($ '("rpmbuild -ba ~/rpmbuild/SPECS/maak.spec")
       #:verbose? #t)
    ($ '("ls ~/rpmbuild/RPMS/x86_64/maak-[0-9]*.rpm"))
    (let* ((latest-rpm (syscall-with-output
                        "ls -t ~/rpmbuild/RPMS/x86_64/maak-[0-9]*.rpm | head -n 1"))
           (latest-src-rpm (syscall-with-output
                            "ls -t ~/rpmbuild/SRPMS/maak-[0-9]*.rpm | head -n 1")))
      ($ '("cp packaging/opensuse/maak-rpmlintrc ~/rpmbuild/RPMS/x86_64/")
         #:verbose? #t)
      ($ (list (~ "rpmlint ~a" latest-rpm))
         #:verbose? #t)
      ($ (list (~ "rpmlint ~a" latest-src-rpm))
         #:verbose? #t))))

(define (hot-reload)
  "Hot-reloading developer's setup."
  (manifest-shell (list "watchexec 'maak --list'"
                        "--clear"
                        (~ "--exts ~a"
                           (string-join (map symbol->string
                                             hot-reload-file-types) ","))
                        "--on-busy-update=restart"
                        "--shell bash"
                        "--watch .")))

(define (hr)
  "Alias to hot-reloading."
  (hot-reload))

(define (dev)
  "Developer's setup."
  (hot-reload))

(define (pure-dev)
  "Pure isolated development shell."
  ($ '("guix shell --pure -f guix.scm"
       "bash coreutils util-linux-with-udev guile" "-- maak --list")
     #:verbose? #t))

(define (nix-dev)
  "Enter the Nix development shell this project's flake declares."
  (nix-develop (list "maak --list")))

(define (flake-check)
  "Validate every output of this project's Nix flake."
  (nix-flake-check #:flake "."))

;; ====== Maak tests ======
;;
;; The suite runs once, and the run is read twice.  Reported as JSON it is data,
;; which is the only form in which a test that asserted nothing can be told from
;; one that asserted something: both look the same in a report meant for a
;; person.  Reported through Veritas' own reporter it is still the report a
;; person reads, because that reporter decorates the very events the JSON lines
;; carry, and the report is replayed through it.  One run, two readers -- so
;; `test' can fail on a test that checked nothing without running the suite a
;; second time to find out.  The specs are read alongside the report for the one
;; thing no report can hold: an assertion written in a case the run did read and
;; then dropped.

;; The veritas invocation is shared by the `test', `container' and `audit-tests'
;; tasks so the environments can never drift apart.  The variables are set
;; through `env' rather than as a `VAR=value' prefix: everything after
;; `guix shell --' is executed directly, so a prefix would be looked up as a
;; program name.
(define veritas-command-head
  (list "env VERITAS_TEST_MODULE=veritas VERITAS_TEST_TOP_DIR=test VERITAS_TEST_TYPE=unit"
        "VERITAS_RUN_BENCHMARKS=false"))

(define veritas-command-tail
  (list "guile -L ./src -L ./test -c '(exit ((@ (veritas runner) run-tests)))'"))

(define test-command
  (append veritas-command-head veritas-command-tail))

;; The same suite, reported as the lines a program can read rather than the
;; lines a person reads.  The capture is kept under `reports/', which is where
;; the test runs already write and which git ignores.
(define report-command
  (append veritas-command-head
          (list "VERITAS_REPORTER=json-reporter")
          veritas-command-tail))

(define test-report-file
  "reports/veritas-test.json")

(define audit-report-file
  "reports/veritas-audit.json")

;; The report names a file the way the runner ran it -- `dsl.scm' -- which is
;; relative to the directory of the test type.
(define veritas-test-dir
  "test/veritas/unit")

(define (run-suite-to report-file)
  "Run the unit test suite, leaving its JSON report at REPORT-FILE.

The specs live under test/veritas/unit and are located by the Veritas runner,
which needs both the library sources and the test tree on the load path.  The
status is returned rather than raised: the report is worth reading either way,
and what a failure means is the audit's to say."
  (mkdir-p "reports")
  (manifest-shell (append report-command
                          (list (~ "> ~a 2>&1"
                                   (shell-quote report-file))))
                  #:allow-failure? #t))

(define (report-events report-file)
  "Return the events of the report at REPORT-FILE, or none when there is none."
  (veritas-report-events (read-file report-file #:default "")))

(define (unread-findings events)
  "Return the cases the run's specs write assertions a run cannot read.

A case is read for its last form only, so a body of several assertions leaves
all but the last one unread -- and nothing about that reaches a report, because
an assertion that is never called never logs.  The specs are read instead, and
only the ones the report says the runner ran, so a filtered run is judged on
what it ran.  Each finding is `(FILE NAME COUNT)'."
  (veritas-unread-findings events
                          (lambda (file)
                            (read-file (~ "~a/~a" veritas-test-dir file)
                                       #:default ""))))

(define (describe-test test)
  "Return the reported TEST as `NAME (FILE)' when the report named its file."
  (let ((file (veritas-test-file test)))
    (if file
        (~ "~s (~a/~a)" (veritas-test-name test) veritas-test-dir file)
        (~ "~s" (veritas-test-name test)))))

(define (describe-unread finding)
  "Return one unread finding as `NAME (FILE)', with the count of its assertions."
  (let ((file (car finding))
        (name (cadr finding))
        (count (caddr finding)))
    (~ "~a writes ~a assertion~a it never reads (~a/~a)"
       (if name (~ "~s" name) "a test the source does not name")
       count
       (if (= 1 count) "" "s")
       veritas-test-dir
       file)))

(define (audit-report report-file passed?)
  "Fail the run when REPORT-FILE holds a test or a case that asserted nothing.

Three questions are asked of one run.  Did it report any tests at all?  Did any
test produce no case of its own?  Did any assertion written inside a case the
run did read go unread?  The last is what the report cannot answer -- an
assertion that is never called logs nothing, so the run leaves no trace of it --
and the specs are read for it instead."
  (let* ((events (report-events report-file))
         (totals (veritas-report-totals events))
         (vacuous (veritas-vacuous-tests events))
         (unread (unread-findings events)))
    (cond
      ((null? (veritas-report-tests events))
       (task-error "the Veritas report holds no tests at all"
                   #:hint (format #f
                                  "The suite did not get as far as writing one; `~a' holds what it printed there."
                                  report-file)))
      ((pair? vacuous)
       (task-error (~ "~a test~a of the suite asserted nothing: ~a"
                      (length vacuous)
                      (if (= 1 (length vacuous)) "" "s")
                      (string-join (map describe-test vacuous) ", "))
                   #:hint "A test has to evaluate to an assertion: a body ending in a side effect is no case at all, and such a test passes in silence.  See the Tests section of the README."))
      ((pair? unread)
       (task-error (~ "~a case~a of the suite write~a assertions the run never reads: ~a"
                      (length unread)
                      (if (= 1 (length unread)) "" "s")
                      (if (= 1 (length unread)) "s" "")
                      (string-join (map describe-unread unread) ", "))
                   #:hint "A case is read for its last form only, so one assertion per case is what the run can check: give each case the setup it needs, or end the body in the one assertion that matters.  See the Tests section of the README."))
      ((not passed?)
       (task-error "the suite failed"
                   #:hint (format #f
                                  "The failures are in the report above; `~a' holds the same run as data."
                                  report-file)))
      (else
       (log-info "  ~a tests, ~a cases, and no test that asserted nothing and no assertion the run never read.  Report: ~a\n"
                 (assoc-ref totals 'tests)
                 (assoc-ref totals 'cases)
                 report-file)))))

(define (test)
  "Run the project's unit test suite with Veritas, and audit it as it runs.

The suite runs once, reported as JSON, and that one report is read twice: once
through Veritas' own reporter, so the output is the report a person expects, and
once as data, so a test that asserted nothing, or an assertion the run never
read, fails the run.  The audit is `audit-tests', run here rather than
remembered: a suite that is only checked when someone asks to check it is not
checked."
  (if (dry-run?)
      (log-info "  A dry run reads no report, so nothing was checked here.\n")
      (let ((passed? (run-suite-to test-report-file)))
        (veritas-report-render (read-file test-report-file #:default ""))
        (audit-report test-report-file passed?))))

(define (container)
  "Run the same unit test suite inside a Guix container, with no network."
  (guix-container test-command
                  #:manifest "manifest.scm"))

(define (audit-tests)
  "Fail when a test asserted nothing, or an assertion of it went unread.

A case has to evaluate to an assertion.  A test whose body ends in a side
effect -- a cleanup, a `display' -- evaluates to something that is not a
procedure instead, so Veritas gives the test no case, and the test passes
without checking anything.  Worse, a case whose body holds several assertions
gives the run its last one and drops the rest, so a suite can go green while
checking a fraction of what it says it checks.

`maak test' runs this audit on the suite it already ran.  This task is the audit
on its own: the same suite, reported as JSON and left as data rather than read,
which is what a machine wants and a person does not."
  (if (dry-run?)
      (log-info "  A dry run reads no report, so nothing was checked here.\n")
      (let ((passed? (run-suite-to audit-report-file)))
        (audit-report audit-report-file passed?))))

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
    ($ (list (~ "touch ~a" temp-file-name)))
    ($ (list (~ "touch ~a" temp-file-name)))
    (parameterize ((dry-run? #t))
      ($ (list (~ "touch ~a" temp-file-name)))
      (delete-file-recursively temp-file-name))
    (delete-file-recursively temp-file-name)))
