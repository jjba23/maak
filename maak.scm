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
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (maak maak))

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

;; ====== The version's copies ======
;;
;; The version is written by hand once, in src/maak/version.scm.  The RPM spec,
;; the manual page and the README cannot read a Scheme module, so they carry
;; copies, and `sync-version' rewrites them from that file.
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
;; wording of the README's sample output stay the file's business rather than
;; this list's.
(define version-sites
  '(("packaging/opensuse/maak.spec"
     . "Version:[ \t]+([0-9]+\\.[0-9]+\\.[0-9]+)")
    ("man/maak.1"
     . "maak ([0-9]+\\.[0-9]+\\.[0-9]+)")
    ("README.org"
     . "maak-([0-9]+\\.[0-9]+\\.[0-9]+)")
    ("README.org"
     . "maak ([0-9]+\\.[0-9]+\\.[0-9]+)")))

(define* (sync-version #:optional (directory "."))
  "Write the version into every file that cannot read it from the module.

The version is written by hand once, in src/maak/version.scm, which is what
`maak --version' prints and what default.nix reads while it evaluates the
derivation.  The RPM spec, the manual page and the README cannot read a Scheme
module, so they carry copies, and this task rewrites them from that file.

DIRECTORY is the checkout to work in, the current directory by default.  The
version is read from the file there rather than from the running library, so
the task writes the checkout's version whether it is run by this checkout's
maak or by an older one already installed.

Each copy is found by a regular expression over the file and only the version
inside the match is replaced, so everything around it -- the spec's alignment,
the manual page's quoting, the README's wording -- is left exactly as it was."
  (define (path file)
    (if (string=? directory ".")
        file
        (string-append directory "/" file)))

  (define (read-text file)
    (let ((full (path file)))
      (if (file-exists? full)
          (call-with-input-file full get-string-all)
          (error (format #f
                         "sync-version: no ~a in ~a; name the checkout's root"
                         file directory)))))

  (define (write-text file text)
    (call-with-output-file (path file)
      (lambda (port) (display text port))))

  (define (current-version)
    (let ((found (string-match
                  "maak-version[ \t\n]+\"([0-9]+\\.[0-9]+\\.[0-9]+)\""
                  (read-text version-source))))
      (unless found
        (error (format #f
                       "sync-version: ~a does not define maak-version as a version"
                       (path version-source))))
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

  (let ((version (current-version))
        (changed '()))
    (for-each (lambda (file)
                (when (sync-file file version)
                  (set! changed (append changed (list file)))))
              (named-files))
    (if (null? changed)
        (log-info "Every file that names the version already says ~a." version)
        (for-each (lambda (file)
                    (log-info "Wrote ~a into ~a." version file))
                  changed))))

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
  (delete-file-recursively-p (~ "~a/api-docs/maak" deploy-dir))
  (mv "doc/api-dist"
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
;; The suite runs once, in the environment the manifest declares.  The Veritas
;; variables are set through `env' rather than as a `VAR=value' prefix:
;; everything after `guix shell --' is executed directly, so a prefix would be
;; looked up as a program name.
(define veritas-command-head
  (list "env VERITAS_TEST_MODULE=veritas VERITAS_TEST_TOP_DIR=test VERITAS_TEST_TYPE=unit"
        "VERITAS_RUN_BENCHMARKS=false"))

(define veritas-command-tail
  (list "guile -L ./src -L ./test -c '(exit ((@ (veritas runner) run-tests)))'"))

(define test-command
  (append veritas-command-head veritas-command-tail))

;; There is deliberately no task that audits the suite on top of running it.
;; Two failure modes are worth naming, because deciding against a reader of the
;; run means living with them: a test whose body never builds an
;; assertion, and a case that writes more than one assertion, of which only the
;; last is read.  Both pass silently, so neither the exit status nor a count of
;; tests catches either one; what shows them is the report, where a test with no
;; case under it ran nothing at all.  That is why the suite is kept to one
;; assertion per case and why the rules are written down in the README.
(define (test)
  "Run the project's unit test suite with Veritas.

The specs live under test/veritas/unit and are located by the Veritas runner,
which needs both the library sources and the test tree on the load path."
  (manifest-shell test-command))

(define (container)
  "Run the same unit test suite inside a Guix container, with no network."
  (guix-container test-command
                  #:manifest "manifest.scm"))

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
