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

;; The same suite, reported as JSON, so the audit can read the lines back as
;; data rather than read the decoration.  The capture is kept under `reports/',
;; which is where the test runs already write and which git ignores.
(define audit-command
  (append veritas-command-head
          (list "VERITAS_REPORTER=json-reporter")
          veritas-command-tail))

(define audit-report-file
  "reports/veritas-audit.json")

;; The report names a file the way the runner ran it -- `dsl.scm' -- which is
;; relative to the directory of the test type.
(define veritas-test-dir
  "test/veritas/unit")

(define (test)
  "Run the project's unit test suite with Veritas.

The specs live under test/veritas/unit and are located by the veritas runner,
which needs both the library sources and the test tree on the load path."
  (manifest-shell test-command))

(define (container)
  "Run the same unit test suite inside a Guix container, with no network."
  (guix-container test-command
                  #:manifest "manifest.scm"))

(define (audit-tests)
  "Fail when any Veritas test of the suite runs no case at all.

A case has to evaluate to an assertion.  A test whose body ends in a side
effect -- a cleanup, a `display' -- evaluates to something that is not a
procedure instead, so Veritas gives the test no case and the test passes
without checking anything.  This task runs the same suite as `test', with the
reporter that writes its lines as JSON, reads that report back, and fails with
the name and file of every test that reported no case of its own."
  (define (describe test)
    (let ((file (veritas-test-file test)))
      (if file
          (~ "~s (~a/~a)" (veritas-test-name test) veritas-test-dir file)
          (~ "~s" (veritas-test-name test)))))
  (if (dry-run?)
      (log-info "  A dry run reads no report, so nothing was checked here.\n")
      (let* ((_ (mkdir-p "reports"))
             (passed? (manifest-shell
                       (append audit-command
                               (list (~ "> ~a 2>&1"
                                        (shell-quote audit-report-file))))
                       #:allow-failure? #t))
             (events (veritas-report-events
                      (read-file audit-report-file #:default "")))
             (totals (veritas-report-totals events))
             (vacuous (veritas-vacuous-tests events)))
        (cond
          ((null? (veritas-report-tests events))
           (task-error "the Veritas report holds no tests at all"
                       #:hint (format #f
                                      "The suite did not get as far as writing one; `~a' holds what it printed.  `maak test' runs the same suite with the readable reporter."
                                      audit-report-file)))
          ((pair? vacuous)
           (task-error (~ "~a test~a of the suite asserted nothing: ~a"
                          (length vacuous)
                          (if (= 1 (length vacuous)) "" "s")
                          (string-join (map describe vacuous) ", "))
                       #:hint "A test has to evaluate to an assertion: a body ending in a side effect is no case at all, and such a test passes in silence.  See the Tests section of the README."))
          ((not passed?)
           (task-error "the suite failed, so the scan of it stopped early"
                       #:hint "Fix what failed first: `maak test' runs the same suite and prints the failures."))
          (else
           (log-info "  ~a tests, ~a cases, and no test without a case.  Report: ~a\n"
                     (assoc-ref totals 'tests)
                     (assoc-ref totals 'cases)
                     audit-report-file))))))

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
