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

;;; Commentary:
;;
;; Unit tests for the command-building helpers -- the Guix and Nix shells and
;; packs, and the i18n toolchain.
;;
;; None of these run a real tool: dry-run mode makes `$' print the command it
;; would have run, and the tests assert on that printed command.  This is the
;; interface a user checks with `maak --dry-run', so testing it is testing the
;; thing that matters without shelling out to Guix, Nix or gettext.

;;; Code:

(define-module (veritas unit commands)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak nix)
  #:use-module (maak i18n)
  #:export (spec))

(define escape-character
  (integer->char 27))

(define (strip-ansi str)
  "Return STR without ANSI escape sequences."
  (let loop ((chars (string->list str))
             (out '())
             (escaping? #f))
    (cond
      ((null? chars) (list->string (reverse out)))
      ((and escaping? (char=? (car chars) #\m))
       (loop (cdr chars) out #f))
      (escaping? (loop (cdr chars) out #t))
      ((char=? (car chars) escape-character)
       (loop (cdr chars) out #t))
      (else (loop (cdr chars) (cons (car chars) out) #f)))))

(define (dry-run-command thunk)
  "Return the command THUNK would run, as printed by dry-run mode."
  (parameterize ((dry-run? #t)
                 (quiet? #f))
    (strip-ansi (with-output-to-string thunk))))

(define guix-suite
  (suite "Guix command construction"
         #:concurrent? #f
         #:shuffle? #f
         (test "guix-shell lists its packages"
               (let ((command (dry-run-command
                               (lambda ()
                                 (guix-shell '("echo hi")
                                             #:packages '("guile"
                                                          "coreutils"))))))
                 (assert-contains #:expect command
                                  #:got "guix shell guile coreutils -- echo hi")))
         (test "guix-shell can be pure and use a manifest"
               (let ((command (dry-run-command
                               (lambda ()
                                 (guix-shell '("maak test")
                                             #:pure? #t
                                             #:manifest "manifest.scm")))))
                 (assert-contains #:expect command
                                  #:got "guix shell --pure -m manifest.scm -- maak test")))
         (test "guix-container isolates with a container"
               (let ((command (dry-run-command
                               (lambda ()
                                 (guix-container '("echo hi")
                                                 #:manifest "manifest.scm"
                                                 #:network? #t
                                                 #:share '("/home/joe"))))))
                 (assert-contains #:expect command
                                  #:got "guix shell --container -m manifest.scm --network -S /home/joe -- echo hi")))
         (test "guix-pack builds a docker image"
               (let ((command (dry-run-command
                               (lambda ()
                                 (guix-pack '()
                                            #:format "docker"
                                            #:image-tag "maak"
                                            #:symlinks '("/bin=bin")
                                            #:extra-packages '("bash"))))))
                 (assert-contains #:expect command
                                  #:got "guix pack --file=guix.scm --format=docker --image-tag=maak -S /bin=bin bash")))))

(define nix-suite
  (suite "Nix command construction"
         #:concurrent? #f
         #:shuffle? #f
         (test "nix-shell passes packages and a run command"
               (let ((command (dry-run-command
                               (lambda ()
                                 (nix-shell '("echo hi")
                                            #:packages '("python3"))))))
                 (assert-contains #:expect command #:got "nix-shell")
                 (assert-contains #:expect command #:got "--packages python3")
                 (assert-contains #:expect command #:got "--run 'echo hi'")))
         (test "nix-shell can be pure and file based"
               (let ((command (dry-run-command
                               (lambda ()
                                 (nix-shell '("echo hi")
                                            #:pure? #t
                                            #:file "shell.nix")))))
                 (assert-contains #:expect command #:got "--pure")
                 (assert-contains #:expect command #:got "--file shell.nix")))
         (test "nix-develop runs a flake's shell"
               (let ((command (dry-run-command
                               (lambda ()
                                 (nix-develop '("make test"))))))
                 (assert-contains #:expect command
                                  #:got "nix develop . --command make test")))
         (test "nix-build builds a file"
               (let ((command (dry-run-command
                               (lambda ()
                                 (nix-build #:file "default.nix")))))
                 (assert-contains #:expect command
                                  #:got "nix-build default.nix")))
         (test "nix-flake-build selects an attribute"
               (let ((command (dry-run-command
                               (lambda ()
                                 (nix-flake-build
                                  #:attr "packages.x86_64-linux.maak")))))
                 (assert-contains #:expect command
                                  #:got "nix build .#packages.x86_64-linux.maak")))
         (test "nix-flake-check checks all systems"
               (let ((command (dry-run-command
                               (lambda ()
                                 (nix-flake-check #:all-systems? #t)))))
                 (assert-contains #:expect command
                                  #:got "nix flake check --all-systems .")))
         (test "nix-run runs an app of a flake"
               (let ((command (dry-run-command
                               (lambda ()
                                 (nix-run '("--version") #:app "maak")))))
                 (assert-contains #:expect command
                                  #:got "nix run .#maak -- --version")))))

(define i18n-suite
  (suite "i18n helpers"
         #:concurrent? #f
         #:shuffle? #f
         (test "the catalogue paths follow the gettext layout"
               (assert-equal #:expect "po/nl_NL/LC_MESSAGES"
                             #:got (i18n-locale-dir "nl_NL"))
               (assert-equal #:expect "po/nl_NL/LC_MESSAGES/messages.po"
                             #:got (i18n-po-file "nl_NL"))
               (assert-equal #:expect "po/nl_NL/LC_MESSAGES/maak.mo"
                             #:got (i18n-mo-file "nl_NL"
                                                 #:textdomain "maak"))
               (assert-equal #:expect "po/maak.pot"
                             #:got (i18n-pot-file #:textdomain "maak")))
         (test "the directory and its trailing slash are handled"
               (assert-equal #:expect "sys/i18n/locale/nl_NL/LC_MESSAGES"
                             #:got (i18n-locale-dir "nl_NL"
                                                    #:dir "sys/i18n/locale/"))
               (assert-equal #:expect "sys/i18n/locale/translate.pot"
                             #:got (i18n-pot-file #:dir "sys/i18n/locale/"
                                                  #:textdomain "translate")))
         (test "source globs skip directories that do not exist"
               (let ((globs (i18n-source-globs '("src"
                                                 "no-such-directory"))))
                 (assert-true (member "src/*.scm" globs))
                 (assert-false (any (lambda (glob)
                                      (string-contains glob
                                                       "no-such-directory"))
                                    globs))))
         (test "i18n-compile builds a msgfmt command"
               (let ((command (dry-run-command
                               (lambda ()
                                 (i18n-compile "nl_NL")))))
                 (assert-contains #:expect command
                                  #:got "msgfmt -v -o po/nl_NL/LC_MESSAGES/messages.mo")
                 (assert-contains #:expect command
                                  #:got "po/nl_NL/LC_MESSAGES/messages.po")))
         (test "i18n-merge builds a msgmerge command"
               (let ((command (dry-run-command
                               (lambda ()
                                 (i18n-merge "nl_NL")))))
                 (assert-contains #:expect command #:got "msgmerge -U")))
         (test "i18n-init builds a msginit command"
               (let ((command (dry-run-command
                               (lambda ()
                                 (i18n-init "nl_NL")))))
                 (assert-contains #:expect command
                                  #:got "msginit -l nl_NL.UTF8")
                 (assert-contains #:expect command
                                  #:got "--no-translator")))
         (test "the first extraction does not join an existing template"
               (let* ((dir (format #f "~a/maak-test-i18n-~a"
                                   (tmp-dir) (getpid)))
                      (sources (string-append dir "/sources"))
                      (command (begin
                                 (mkdir-p sources)
                                 (dry-run-command
                                  (lambda ()
                                    (i18n-extract #:dir dir
                                                  #:textdomain "maak"
                                                  #:directories
                                                  (list sources)))))))
                 (assert-contains #:expect command #:got "xgettext")
                 (assert-contains #:expect command #:got "--language=Scheme")
                 (assert-contains #:expect command #:got "--keyword=G_")
                 (assert-false (string-contains command "--join-existing"))))
         (test "i18n-language strips the encoding and modifier"
               (let ((language (i18n-language)))
                 (assert-true (string? language))
                 (assert-false (string-contains language "."))
                 (assert-false (string-contains language "@"))))))

(define decoration-suite
  (suite "Command decorations"
         #:concurrent? #f
         #:shuffle? #f
         (test "an environment is handed over through env"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            ($ '("make") #:env '(("CI" . "1")))))
                #:got "env CI=1 make")
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            ($ '("make") #:env '("A=1" "B=2"))))
                #:got "env A=1 B=2 make")
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            ($ '("make") #:env "ONLY=1")))
                #:got "env ONLY=1 make"))
         (test "the decorations come in the order a shell would want"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            ($ '("systemctl daemon-reload")
                               #:env '("CI=1")
                               #:sudo? #t
                               #:dev-null? #t)))
                #:got "sudo env CI=1 systemctl daemon-reload >/dev/null 2>&1"))
         (test "a backgrounded command is started, not waited for"
               (let ((command (dry-run-command
                               (lambda ()
                                 ($ '("make -j") #:async? #t)))))
                 (assert-contains #:expect command #:got "make -j &")))
         (test "an allowed failure is not turned into a `|| true' shell"
               ;; The command line is left exactly as the task wrote it: the
               ;; exit code is read instead, so that the task is still told
               ;; whether the command worked.
               (let ((command (dry-run-command
                               (lambda ()
                                 ($ '("make failing-target")
                                    #:allow-failure? #t)))))
                 (assert-contains #:expect command #:got "make failing-target")
                 (assert-false (string-contains command "|| true"))))
         (test "the Guix shells forward the decorations"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (manifest-shell '("maak test")
                                            #:env '("CI=1")
                                            #:allow-failure? #t)))
                #:got "env CI=1 guix shell -m manifest.scm -- maak test")
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (time-machine '("guix build hello")
                                          #:env '("A=1"))))
                #:got "env A=1 guix time-machine --channels=channels.scm -- guix build hello")
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (program-shell '("x") #:env '("A=1"))))
                #:got "env A=1 guix shell -f guix.scm -- x")
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (time-machine-manifest-shell '("x")
                                                        #:env '("A=1"))))
                #:got "env A=1 guix time-machine --channels=channels.scm -- shell -m manifest.scm")
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (time-machine-program-shell '("x")
                                                       #:env '("A=1"))))
                #:got "env A=1 guix time-machine --channels=channels.scm -- shell -f guix.scm"))

         (test "a container and a pack forward the decorations"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (guix-container '("maak test")
                                            #:env '("CI=1")
                                            #:dev-null? #t)))
                #:got "env CI=1 guix shell --container -m manifest.scm -- maak test >/dev/null 2>&1")
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (guix-pack '() #:env '("A=1"))))
                #:got "env A=1 guix pack --file=guix.scm"))
         (test "the Nix helpers forward the decorations"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (nix-shell '("make test")
                                       #:packages '("gcc")
                                       #:env '("NIX=1")
                                       #:allow-failure? #t)))
                #:got "env NIX=1 nix-shell --packages gcc --run 'make test'")
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (nix-develop '("make") #:env '("A=1"))))
                #:got "env A=1 nix develop . --command make")
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (nix-flake-check #:env '("A=1"))))
                #:got "env A=1 nix flake check ."))
         (test "the shells honour the join separator they are given"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (manifest-shell '("a" "b") #:join " && ")))
                #:got "a && b"))))

(define execution-suite
  (suite "Running decorated commands"
         #:concurrent? #f
         #:shuffle? #f
         (test "a failing command stops a task"
               (assert-error (lambda ()
                               (parameterize ((quiet? #t))
                                 ($ '("false")))))
               (assert-error (lambda ()
                               (parameterize ((quiet? #t))
                                 ($ '("exit 3"))))))
         (test "a failure that is allowed is reported, not raised"
               (parameterize ((quiet? #t))
                 (assert-equal #:expect #f
                               #:got ($ '("false") #:allow-failure? #t)))
               ;; The task is told the command failed, which a `|| true' in the
               ;; line would have hidden.
               (parameterize ((quiet? #t))
                 (assert-equal #:expect #f
                               #:got ($ '("sh -c 'exit 4'")
                                        #:allow-failure? #t)))
               (parameterize ((quiet? #t))
                 (assert-equal #:expect #t
                               #:got ($ '("true") #:allow-failure? #t))))
         (test "a backgrounded command is started, not waited for"
               (let* ((start (get-internal-real-time))
                      (result ($ '("sleep 3") #:async? #t))
                      (elapsed (/ (- (get-internal-real-time) start)
                                  (exact->inexact
                                   internal-time-units-per-second))))
                 (assert-equal #:expect #t #:got result)
                 ;; Back in under the time the command needs: it was started,
                 ;; not waited for.
                 (assert-true (< elapsed 1.0))))
         (test "a discarded command still runs"
               (parameterize ((quiet? #t))
                 (assert-equal #:expect #t
                               #:got ($ '("echo ignored") #:dev-null? #t))))
         ;; A case has to evaluate to an assertion (a procedure of one
         ;; validation client), so the setup a case needs is done in the `let'
         ;; that wraps it and only the assertion is left as the last form.
         (test "#:env reaches the command itself"
               (let* ((path (format #f "~a/maak-env-~a"
                                    (tmp-dir) (getpid)))
                      (value (begin
                               (env-unset! "MAAK_TEST_ENV")
                               (parameterize ((quiet? #t))
                                 ($ (list (format #f
                                                  "sh -c 'printf %s \"$MAAK_TEST_ENV\" > ~a'"
                                                  path))
                                    ;; Reaches the command, not the shell that
                                    ;; expands the redirection: it is `env'
                                    ;; that sets the name for it.
                                    #:env '("MAAK_TEST_ENV=from-dollar")))
                               (read-file path))))
                 (delete-file path)
                 (assert-equal #:expect "from-dollar" #:got value)
                 ;; The parent environment is left alone: the name was set for
                 ;; the one command, not for the whole task.
                 (assert-false (env-get "MAAK_TEST_ENV"))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               guix-suite
               nix-suite
               i18n-suite
               decoration-suite
               execution-suite))
