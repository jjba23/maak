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

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               guix-suite
               nix-suite
               i18n-suite))
