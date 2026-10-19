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
;; Unit tests for `(maak container)'.  Nothing here starts a container: the
;; argument builders are asserted directly, and the procedures are checked
;; through dry-run mode, which is the interface `maak --dry-run` shows a user.
;; Engine detection runs against fake PATH directories so the result does not
;; depend on what is installed on the machine running the suite.

;;; Code:

(define-module (veritas unit container)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak container)
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
  (parameterize ((dry-run? #t) (quiet? #f))
    (strip-ansi (with-output-to-string thunk))))

(define (scratch-directory label)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (format #f "~a/maak-test-container-~a-~a"
                     (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (remove-directory dir)
  "Remove DIR quietly, whatever the tests left in it."
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

(define (fake-program dir name)
  "Write an executable called NAME in DIR and return DIR."
  (let ((path (string-append dir "/" name)))
    (write-file path "#!/bin/sh\nexit 0\n")
    (parameterize ((quiet? #t))
      (chmod-file "755" path))
    dir))

(define detection-suite
  (suite "Container engine detection"
         #:concurrent? #f
         #:shuffle? #f
         (test "an empty search path falls back to podman"
               (let ((dir (scratch-directory "empty-path")))
                 (let ((engine (detect-container-engine
                                #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "podman" #:got engine))))
         (test "docker is found when podman is not"
               (let ((dir (scratch-directory "docker-only")))
                 (fake-program dir "docker")
                 (let ((engine (detect-container-engine
                                #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "docker" #:got engine))))
         (test "podman wins when both are installed"
               (let ((dir (scratch-directory "both")))
                 (fake-program dir "docker")
                 (fake-program dir "podman")
                 (let ((engine (detect-container-engine
                                #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "podman" #:got engine))))
         (test "an explicit preference wins"
               (let ((dir (scratch-directory "preferred")))
                 (fake-program dir "podman")
                 (let ((engine (detect-container-engine
                                #:path dir #:preferred "docker")))
                   (remove-directory dir)
                   (assert-equal #:expect "docker" #:got engine))))
         (test "an empty preference is ignored"
               (let ((dir (scratch-directory "empty-preference")))
                 (fake-program dir "docker")
                 (let ((engine (detect-container-engine
                                #:path dir #:preferred "")))
                   (remove-directory dir)
                   (assert-equal #:expect "docker" #:got engine))))
         (test "docker compose is the docker program plus its subcommand"
               (assert-equal #:expect '("docker" "compose")
                             #:got (detect-compose-command
                                    #:engine "docker"
                                    #:path "/nonexistent"
                                    #:preferred #f)))
         (test "podman-compose is preferred for podman"
               (let ((dir (scratch-directory "podman-compose")))
                 (fake-program dir "podman-compose")
                 (let ((program (detect-compose-command
                                 #:engine "podman"
                                 #:path dir
                                 #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect '("podman-compose") #:got program))))
         (test "podman compose is the fallback"
               (let ((dir (scratch-directory "podman-compose-fallback")))
                 (let ((program (detect-compose-command
                                 #:engine "podman"
                                 #:path dir
                                 #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect '("podman" "compose") #:got program))))
         (test "a compose preference is split into words"
               (assert-equal #:expect '("podman-compose" "--ansi" "never")
                             #:got (detect-compose-command
                                    #:engine "docker"
                                    #:preferred "podman-compose --ansi never")))
         (test "a compose preference splits on any whitespace"
               (assert-equal #:expect '("podman-compose" "--ansi" "never")
                             #:got (detect-compose-command
                                    #:engine "docker"
                                    #:preferred "podman-compose\t--ansi   never")))
         (test "the default engine is a non-empty string"
               (assert-true (and (string? (container-engine))
                                 (not (string=? (container-engine) "")))))
         (test "container-available? looks on PATH"
               (let ((dir (scratch-directory "available")))
                 (fake-program dir "podman")
                 (let ((found? (with-env "PATH" dir
                                 (lambda ()
                                   (container-available? #:engine "podman")))))
                   (remove-directory dir)
                   (assert-true found?))))
         (test "container-available? is false for a missing engine"
               (let ((dir (scratch-directory "unavailable")))
                 (let ((found? (with-env "PATH" dir
                                 (lambda ()
                                   (container-available? #:engine "podman")))))
                   (remove-directory dir)
                   (assert-false found?))))
         (test "compose-available? looks for the first word"
               (let ((dir (scratch-directory "compose-available")))
                 (fake-program dir "podman-compose")
                 (let ((found? (with-env "PATH" dir
                                 (lambda ()
                                   (compose-available?
                                    #:program '("podman-compose"))))))
                   (remove-directory dir)
                   (assert-true found?))))
         (test "compose-available? is false for a missing program"
               (let ((dir (scratch-directory "compose-unavailable")))
                 (let ((found? (with-env "PATH" dir
                                 (lambda ()
                                   (compose-available?
                                    #:program '("podman-compose"))))))
                   (remove-directory dir)
                   (assert-false found?))))
         (test "an empty engine is refused"
               (assert-error (lambda () (container-argv (list "ps")
                                                        #:engine ""))))))

(define container-suite
  (suite "Container commands"
         #:concurrent? #f
         #:shuffle? #f
         (test "the engine is the first word"
               (assert-equal #:expect '("docker" "ps" "--all")
                             #:got (container-argv (list "ps" "--all")
                                                   #:engine "docker")))
         (test "run carries every option in a stable order"
               (assert-equal
                #:expect '("podman" "run" "--detach" "--rm" "--interactive"
                           "--tty" "--privileged" "--name" "db"
                           "--workdir" "/app" "--entrypoint" "/bin/sh"
                           "--network" "host" "--user" "1000"
                           "--platform" "linux/amd64" "--pull" "always"
                           "--env" "A=1" "--publish" "8080:80"
                           "--volume" "v:/data" "--mount" "type=bind"
                           "--label" "role=db" "postgres" "postgres" "-c" "x")
                #:got (container-run-argv
                       "postgres"
                       #:engine "podman"
                       #:detach? #t
                       #:interactive? #t
                       #:tty? #t
                       #:privileged? #t
                       #:name "db"
                       #:workdir "/app"
                       #:entrypoint "/bin/sh"
                       #:network "host"
                       #:user "1000"
                       #:platform "linux/amd64"
                       #:pull "always"
                       #:env '("A=1")
                       #:publish '("8080:80")
                       #:volume '("v:/data")
                       #:mount '("type=bind")
                       #:labels '("role=db")
                       #:command '("postgres" "-c" "x"))))
         (test "run removes the container by default"
               (assert-equal #:expect '("podman" "run" "--rm" "alpine")
                             #:got (container-run-argv "alpine"
                                                       #:engine "podman")))
         (test "build carries tags, files, args and labels"
               (assert-equal
                #:expect '("docker" "build" "--tag" "p:dev" "--file" "Containerfile"
                           "--target" "runtime" "--platform" "linux/arm64"
                           "--network" "host" "--no-cache" "--pull" "--quiet"
                           "--build-arg" "VERSION=1" "--label" "a=b" ".")
                #:got (container-build-argv
                       "."
                       #:engine "docker"
                       #:tag "p:dev"
                       #:file "Containerfile"
                       #:target "runtime"
                       #:platform "linux/arm64"
                       #:network "host"
                       #:no-cache? #t
                       #:pull? #t
                       #:quiet? #t
                       #:build-args '("VERSION=1")
                       #:labels '("a=b"))))
         (test "ps can include stopped containers"
               (assert-equal #:expect '("podman" "ps" "--all")
                             #:got (container-ps-argv #:engine "podman"
                                                      #:all? #t)))
         (test "images passes extra arguments through"
               (assert-equal #:expect '("docker" "images" "--quiet")
                             #:got (container-images-argv
                                    #:engine "docker"
                                    #:arguments '("--quiet"))))
         (test "stop names every container"
               (assert-equal #:expect '("podman" "stop" "a" "b")
                             #:got (container-stop-argv '("a" "b")
                                                        #:engine "podman")))
         (test "rm forces and removes volumes"
               (assert-equal #:expect '("podman" "rm" "--force" "--volumes" "db")
                             #:got (container-rm-argv '("db")
                                                      #:engine "podman"
                                                      #:volumes? #t)))
         (test "exec passes environment and user"
               (assert-equal
                #:expect '("docker" "exec" "--interactive" "--tty" "--workdir"
                           "/app" "--user" "postgres" "--env" "PGPASSWORD=x"
                           "db" "psql" "-c" "select 1")
                #:got (container-exec-argv
                       "db"
                       (list "psql" "-c" "select 1")
                       #:engine "docker"
                       #:interactive? #t
                       #:tty? #t
                       #:workdir "/app"
                       #:user "postgres"
                       #:env '("PGPASSWORD=x"))))
         (test "logs can follow, tail and filter by time"
               (assert-equal
                #:expect '("podman" "logs" "--follow" "--tail" "50"
                           "--since" "1h" "db")
                #:got (container-logs-argv "db"
                                           #:engine "podman"
                                           #:follow? #t
                                           #:tail 50
                                           #:since "1h")))
         (test "pull is a plain subcommand"
               (assert-equal #:expect '("docker" "pull" "img")
                             #:got (container-pull-argv "img"
                                                        #:engine "docker")))
         (test "push is a plain subcommand"
               (assert-equal #:expect '("docker" "push" "img")
                             #:got (container-push-argv "img"
                                                        #:engine "docker")))
         (test "tag names the source and the new name"
               (assert-equal #:expect '("docker" "tag" "a" "b")
                             #:got (container-tag-argv "a" "b"
                                                       #:engine "docker")))
         (test "inspect can format a field"
               (assert-equal
                #:expect '("podman" "inspect" "--format" "{{.State.Status}}"
                           "--type" "container" "db")
                #:got (container-inspect-argv '("db")
                                              #:engine "podman"
                                              #:format "{{.State.Status}}"
                                              #:type "container")))
         (test "the generic runner prints the engine command"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (container (list "volume" "ls")
                                       #:engine "docker")))
                #:got "docker volume ls"))
         (test "capture returns nothing in a dry run"
               (assert-equal #:expect ""
                             #:got (parameterize ((dry-run? #t)
                                                  (quiet? #t))
                                     (container-output (list "ps")
                                                       #:engine "docker"))))
         (test "an empty image is refused"
               (assert-error (lambda () (container-run-argv ""
                                                           #:engine "podman"))))
         (test "an empty context is refused"
               (assert-error (lambda () (container-build-argv ""
                                                             #:engine "podman"))))))

(define compose-suite
  (suite "Compose commands"
         #:concurrent? #f
         #:shuffle? #f
         (test "global options come before the subcommand"
               (assert-equal
                #:expect '("docker" "compose" "--file" "compose.yml"
                           "--project-name" "p" "--env-file" ".env"
                           "--profile" "dev" "ps")
                #:got (compose-argv (list "ps")
                                    #:program '("docker" "compose")
                                    #:file "compose.yml"
                                    #:project "p"
                                    #:env-files '(".env")
                                    #:profiles '("dev"))))
         (test "up detaches, builds and waits"
               (assert-equal
                #:expect '("podman-compose" "up" "--detach" "--build"
                           "--force-recreate" "--remove-orphans" "--wait" "web")
                #:got (compose-up-argv
                       (list "web")
                       #:program '("podman-compose")
                       #:build? #t
                       #:force-recreate? #t
                       #:wait? #t)))
         (test "up can avoid starting the services"
               (assert-equal
                #:expect '("docker" "compose" "up" "--detach"
                           "--remove-orphans" "--no-start")
                #:got (compose-up-argv '() #:program '("docker" "compose")
                                       #:no-start? #t)))
         (test "down can remove volumes and images"
               (assert-equal
                #:expect '("docker" "compose" "down" "--volumes"
                           "--remove-orphans" "--rmi" "all" "--timeout" "10" "db")
                #:got (compose-down-argv
                       (list "db")
                       #:program '("docker" "compose")
                       #:volumes? #t
                       #:rmi "all"
                       #:timeout 10)))
         (test "build passes no-cache and pull"
               (assert-equal
                #:expect '("docker" "compose" "build" "--no-cache" "--pull" "web")
                #:got (compose-build-argv
                       (list "web")
                       #:program '("docker" "compose")
                       #:no-cache? #t
                       #:pull? #t)))
         (test "logs can follow and drop colour"
               (assert-equal
                #:expect '("docker" "compose" "logs" "--follow" "--tail" "20"
                           "--no-color" "web")
                #:got (compose-logs-argv
                       (list "web")
                       #:program '("docker" "compose")
                       #:follow? #t
                       #:tail 20
                       #:no-color? #t)))
         (test "ps can list everything and format it"
               (assert-equal
                #:expect '("docker" "compose" "ps" "--all" "--format" "json" "web")
                #:got (compose-ps-argv
                       (list "web")
                       #:program '("docker" "compose")
                       #:all? #t
                       #:format "json")))
         (test "exec allocates no terminal by default"
               (assert-equal
                #:expect '("docker" "compose" "exec" "-T" "--user" "root"
                           "web" "id")
                #:got (compose-exec-argv
                       "web"
                       (list "id")
                       #:program '("docker" "compose")
                       #:user "root")))
         (test "exec can allocate a terminal when asked"
               (assert-equal
                #:expect '("docker" "compose" "exec" "--tty" "--index" "2"
                           "web" "sh")
                #:got (compose-exec-argv
                       "web"
                       (list "sh")
                       #:program '("docker" "compose")
                       #:no-tty? #f
                       #:tty? #t
                       #:index 2)))
         (test "tty wins over the default -T"
               (assert-equal
                #:expect '("docker" "compose" "exec" "--tty" "web" "sh")
                #:got (compose-exec-argv
                       "web"
                       (list "sh")
                       #:program '("docker" "compose")
                       #:tty? #t)))
         (test "run uses a one-off container"
               (assert-equal
                #:expect '("docker" "compose" "run" "--rm" "--no-deps"
                           "--workdir" "/app" "--user" "root" "web" "make")
                #:got (compose-run-argv
                       "web"
                       (list "make")
                       #:program '("docker" "compose")
                       #:no-deps? #t
                       #:workdir "/app"
                       #:user "root")))
         (test "pull and restart take service names"
               (assert-equal
                #:expect '("docker" "compose" "pull" "--ignore-pull-failures" "web")
                #:got (compose-pull-argv
                       (list "web")
                       #:program '("docker" "compose")
                       #:ignore-pull-failures? #t))
               (assert-equal
                #:expect '("docker" "compose" "restart" "--timeout" "5" "web")
                #:got (compose-restart-argv
                       (list "web")
                       #:program '("docker" "compose")
                       #:timeout 5)))
         (test "config can validate without printing"
               (assert-equal
                #:expect '("docker" "compose" "config" "-q" "--services")
                #:got (compose-config-argv '() #:program '("docker" "compose")
                                           #:quiet? #t
                                           #:services? #t)))
         (test "the generic runner prints the compose command"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (run-compose (list "top")
                                         #:program '("docker" "compose")
                                         #:file "compose.yml")))
                #:got "docker compose --file compose.yml top"))
         (test "capture returns nothing in a dry run"
               (assert-equal #:expect ""
                             #:got (parameterize ((dry-run? #t)
                                                  (quiet? #t))
                                     (compose-output
                                      (list "ps")
                                      #:program '("docker" "compose")))))
         (test "an empty program is refused"
               (assert-error (lambda ()
                               (compose-argv (list "ps") #:program '()))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               detection-suite
               container-suite
               compose-suite))
