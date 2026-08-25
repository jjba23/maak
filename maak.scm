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

;; ====== Maak test playground =======

(define (test)
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
