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
;; Container helpers for the two engines a Linux machine is likely to have.
;; Podman and Docker share almost all of their command line, so a task written
;; against one should not have to be rewritten for the other: the engine is a
;; parameter, detected once from PATH and overridable per call or with
;; MAAK_CONTAINER_ENGINE.
;;
;; The same is true of Compose, with one extra wrinkle: Podman has both
;; `podman compose` and the older `podman-compose`, so the compose program is a
;; parameter of its own.  MAAK_COMPOSE names it, and a call can pass
;; `#:program`.
;;
;; Every helper has an `-argv` builder next to it.  The builder returns the
;; list of words without running anything: that is what the procedures feed to
;; `run`, what --dry-run prints, and what a test can assert without needing a
;; container runtime.
;;
;; Nothing here starts a container as a side effect of loading the module.

;;; Code:

(define-module (maak container)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (container-engine
            container-compose-command
            detect-container-engine
            detect-compose-command
            container-available?
            compose-available?
            container-argv
            container
            container-output
            container-run-argv
            container-run
            container-build-argv
            container-build
            container-ps-argv
            container-ps
            container-images-argv
            container-images
            container-stop-argv
            container-stop
            container-rm-argv
            container-rm
            container-exec-argv
            container-exec
            container-logs-argv
            container-logs
            container-pull-argv
            container-pull
            container-push-argv
            container-push
            container-tag-argv
            container-tag
            container-inspect-argv
            container-inspect
            compose-argv
            run-compose
            compose-output
            compose-up-argv
            compose-up
            compose-down-argv
            compose-down
            compose-build-argv
            compose-build
            compose-logs-argv
            compose-logs
            compose-ps-argv
            compose-ps
            compose-exec-argv
            compose-exec
            compose-run-argv
            compose-run
            compose-pull-argv
            compose-pull
            compose-restart-argv
            compose-restart
            compose-config-argv
            compose-config))

;; ====== Choosing the engine ======

(define* (detect-container-engine
          #:key (path (getenv "PATH"))
          (preferred (getenv "MAAK_CONTAINER_ENGINE")))
  "Return the container engine maak should use.

PREFERRED, when non-empty, wins unchanged.  Otherwise Podman is chosen when it
is on PATH, then Docker.  The fallback is Podman even when neither is
installed, so a missing engine is reported as a missing program rather than
silently replaced by another one.

PATH is the search path, PATH itself by default, so the choice can be tested
without changing the process environment.

@example
(detect-container-engine #:path search-path)
(detect-container-engine #:preferred engine-name)
@end example"
  (cond
    ((and preferred (not (string=? preferred ""))) preferred)
    ((command-on-path? "podman" path) "podman")
    ((command-on-path? "docker" path) "docker")
    (else "podman")))

(define container-engine
  (make-parameter (detect-container-engine)))

(define* (detect-compose-command
          #:key (engine (container-engine))
          (path (getenv "PATH"))
          (preferred (getenv "MAAK_COMPOSE")))
  "Return the words that run Compose, as a list.

PREFERRED, when non-empty, is split on whitespace and used as it stands.  For
Podman, the standalone `podman-compose` is preferred when it is installed, and
`podman compose` is used otherwise; Docker always answers `docker compose`.
The engine is passed in rather than read from the parameter, so the result is a
pure function of its arguments."
  (cond
    ((and preferred (not (string=? preferred "")))
     (string-tokenize (string-trim-both preferred)))
    ((string=? engine "podman")
     (if (command-on-path? "podman-compose" path)
         (list "podman-compose")
         (list "podman" "compose")))
    (else (list "docker" "compose"))))

(define container-compose-command
  (make-parameter (detect-compose-command)))

(define* (container-available? #:key (engine (container-engine)))
  "Return #t when ENGINE can be found on PATH."
  (command-on-path? engine (getenv "PATH")))

(define* (compose-available? #:key (program (container-compose-command)))
  "Return #t when the Compose PROGRAM can be found on PATH.

PROGRAM is the list of words container-compose-command returns; its first word
is what is looked for."
  (and (pair? program)
       (command-on-path? (car program) (getenv "PATH"))))

;; ====== Running the engine ======

(define* (container-argv arguments #:key (engine (container-engine)))
  "Return the argv that runs ENGINE with ARGUMENTS.

This is the shared shape of every helper below: the engine first, then the
subcommand and its flags.  It is exported because a subcommand without a
wrapper is still a subcommand -- pass `(list \"volume\" \"ls\")` and it works."
  (when (or (not (string? engine)) (string=? engine ""))
    (raise-maak-error
     (format #f "container engine must be a non-empty string, got ~s" engine)
     #:hint "Set MAAK_CONTAINER_ENGINE to podman or docker."))
  (cons engine arguments))

(define* (container arguments
                    #:key (engine (container-engine))
                    (allow-failure? #f) (verbose? #f))
  "Run ENGINE with ARGUMENTS, a list of strings.

The escape hatch for subcommands without a wrapper, such as `volume ls`.
Nothing is handed to a shell, so a label or a path with a space in it stays one
argument."
  (run (container-argv arguments #:engine engine)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (container-output arguments
                           #:key (engine (container-engine))
                           (allow-failure? #f))
  "Run ENGINE with ARGUMENTS and return its output as a string.

This is the reading counterpart of `container`: the output is a value a task
can trim and compare rather than a line on the terminal.  In dry-run mode
nothing runs, the command is printed, and the empty string comes back."
  (capture (string-join
            (map shell-quote (container-argv arguments #:engine engine))
            " ")
           #:allow-failure? allow-failure?))

;; ====== Containers ======

(define* (container-run-argv image
                             #:key
                             (engine (container-engine))
                             (command '())
                             (name #f)
                             (detach? #f)
                             (remove? #t)
                             (interactive? #f)
                             (tty? #f)
                             (env '())
                             (publish '())
                             (volume '())
                             (mount '())
                             (workdir #f)
                             (entrypoint #f)
                             (network #f)
                             (user #f)
                             (platform #f)
                             (pull #f)
                             (labels '())
                             (privileged? #f))
  "Return the argv that runs IMAGE as a container.

COMMAND is the list of arguments passed to the image, after its name.  The
defaults are the ones an unattended task wants: REMOVE? is #t, so a finished
container leaves nothing behind, and nothing is attached to the terminal unless
INTERACTIVE? or TTY? asks for it.  ENV, PUBLISH, VOLUME, MOUNT and LABELS are
lists of strings, each passed with its flag."
  (when (or (not (string? image)) (string=? image ""))
    (raise-maak-error
     "container-run expects the image to run"
     #:hint "Write (container-run image #:command (list program argument))."))
  (container-argv
   (append (list "run")
           (if detach? (list "--detach") '())
           (if remove? (list "--rm") '())
           (if interactive? (list "--interactive") '())
           (if tty? (list "--tty") '())
           (if privileged? (list "--privileged") '())
           (if name (list "--name" name) '())
           (if workdir (list "--workdir" workdir) '())
           (if entrypoint (list "--entrypoint" entrypoint) '())
           (if network (list "--network" network) '())
           (if user (list "--user" user) '())
           (if platform (list "--platform" platform) '())
           (if pull (list "--pull" pull) '())
           (append-map (lambda (entry) (list "--env" entry)) env)
           (append-map (lambda (entry) (list "--publish" entry)) publish)
           (append-map (lambda (entry) (list "--volume" entry)) volume)
           (append-map (lambda (entry) (list "--mount" entry)) mount)
           (append-map (lambda (entry) (list "--label" entry)) labels)
           (list image)
           command)
   #:engine engine))

(define* (container-run image
                        #:key
                        (engine (container-engine))
                        (command '())
                        (name #f)
                        (detach? #f)
                        (remove? #t)
                        (interactive? #f)
                        (tty? #f)
                        (env '())
                        (publish '())
                        (volume '())
                        (mount '())
                        (workdir #f)
                        (entrypoint #f)
                        (network #f)
                        (user #f)
                        (platform #f)
                        (pull #f)
                        (labels '())
                        (privileged? #f)
                        (allow-failure? #f)
                        (verbose? #f))
  "Run IMAGE as a container; see `container-run-argv` for the options."
  (run (container-run-argv image
                           #:engine engine
                           #:command command
                           #:name name
                           #:detach? detach?
                           #:remove? remove?
                           #:interactive? interactive?
                           #:tty? tty?
                           #:env env
                           #:publish publish
                           #:volume volume
                           #:mount mount
                           #:workdir workdir
                           #:entrypoint entrypoint
                           #:network network
                           #:user user
                           #:platform platform
                           #:pull pull
                           #:labels labels
                           #:privileged? privileged?)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (container-build-argv context
                               #:key
                               (engine (container-engine))
                               (tag #f)
                               (file #f)
                               (target #f)
                               (build-args '())
                               (labels '())
                               (no-cache? #f)
                               (pull? #f)
                               (platform #f)
                               (network #f)
                               (quiet? #f))
  "Return the argv that builds the image described by CONTEXT.

TAG is --tag, FILE is --file, TARGET is --target, and BUILD-ARGS and LABELS
are lists of strings.  CONTEXT is the directory sent to the engine, normally
the last argument."
  (when (or (not (string? context)) (string=? context ""))
    (raise-maak-error
     "container-build expects a build context"
     #:hint "Write (container-build build-context #:tag image-name)."))
  (container-argv
   (append (list "build")
           (if tag (list "--tag" tag) '())
           (if file (list "--file" file) '())
           (if target (list "--target" target) '())
           (if platform (list "--platform" platform) '())
           (if network (list "--network" network) '())
           (if no-cache? (list "--no-cache") '())
           (if pull? (list "--pull") '())
           (if quiet? (list "--quiet") '())
           (append-map (lambda (entry) (list "--build-arg" entry)) build-args)
           (append-map (lambda (entry) (list "--label" entry)) labels)
           (list context))
   #:engine engine))

(define* (container-build context
                          #:key
                          (engine (container-engine))
                          (tag #f)
                          (file #f)
                          (target #f)
                          (build-args '())
                          (labels '())
                          (no-cache? #f)
                          (pull? #f)
                          (platform #f)
                          (network #f)
                          (quiet? #f)
                          (allow-failure? #f)
                          (verbose? #f))
  "Build the image described by CONTEXT; see `container-build-argv`."
  (run (container-build-argv context
                             #:engine engine
                             #:tag tag
                             #:file file
                             #:target target
                             #:build-args build-args
                             #:labels labels
                             #:no-cache? no-cache?
                             #:pull? pull?
                             #:platform platform
                             #:network network
                             #:quiet? quiet?)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (container-ps-argv #:key (engine (container-engine)) (all? #f))
  "Return the argv that lists containers, including stopped ones with ALL?."
  (container-argv (append (list "ps") (if all? (list "--all") '()))
                  #:engine engine))

(define* (container-ps #:key (engine (container-engine)) (all? #f)
                       (allow-failure? #f))
  "List containers; see `container-ps-argv`."
  (run (container-ps-argv #:engine engine #:all? all?)
       #:allow-failure? allow-failure?))

(define* (container-images-argv #:key (engine (container-engine))
                                (arguments '()))
  "Return the argv that lists images, with any extra ARGUMENTS."
  (container-argv (append (list "images") arguments) #:engine engine))

(define* (container-images #:key (engine (container-engine))
                           (arguments '()) (allow-failure? #f))
  "List images; see `container-images-argv`."
  (run (container-images-argv #:engine engine #:arguments arguments)
       #:allow-failure? allow-failure?))

(define* (container-stop-argv names #:key (engine (container-engine)))
  "Return the argv that stops the containers NAMES."
  (container-argv (append (list "stop") names) #:engine engine))

(define* (container-stop names #:key (engine (container-engine))
                         (allow-failure? #f))
  "Stop the containers NAMES, a list of strings."
  (run (container-stop-argv names #:engine engine)
       #:allow-failure? allow-failure?))

(define* (container-rm-argv names
                            #:key (engine (container-engine))
                            (force? #t) (volumes? #f))
  "Return the argv that removes the containers NAMES.

FORCE? is the default because a task that removes a container means it, and a
running container would otherwise fail the whole task."
  (container-argv (append (list "rm")
                          (if force? (list "--force") '())
                          (if volumes? (list "--volumes") '())
                          names)
                  #:engine engine))

(define* (container-rm names
                       #:key (engine (container-engine))
                       (force? #t) (volumes? #f) (allow-failure? #f))
  "Remove the containers NAMES; see `container-rm-argv`."
  (run (container-rm-argv names
                          #:engine engine
                          #:force? force?
                          #:volumes? volumes?)
       #:allow-failure? allow-failure?))

(define* (container-exec-argv name command
                              #:key
                              (engine (container-engine))
                              (interactive? #f)
                              (tty? #f)
                              (env '())
                              (workdir #f)
                              (user #f))
  "Return the argv that runs COMMAND in the container NAME.

COMMAND is a list of strings.  INTERACTIVE? and TTY? are the usual -i and -t; a
non-interactive command needs neither, which is what a build step wants."
  (when (or (not (string? name)) (string=? name ""))
    (raise-maak-error
     "container-exec expects a container name"
     #:hint "Write (container-exec container-name (list program argument))."))
  (container-argv
   (append (list "exec")
           (if interactive? (list "--interactive") '())
           (if tty? (list "--tty") '())
           (if workdir (list "--workdir" workdir) '())
           (if user (list "--user" user) '())
           (append-map (lambda (entry) (list "--env" entry)) env)
           (list name)
           command)
   #:engine engine))

(define* (container-exec name command
                         #:key
                         (engine (container-engine))
                         (interactive? #f)
                         (tty? #f)
                         (env '())
                         (workdir #f)
                         (user #f)
                         (allow-failure? #f)
                         (verbose? #f))
  "Run COMMAND in the container NAME; see `container-exec-argv`."
  (run (container-exec-argv name command
                            #:engine engine
                            #:interactive? interactive?
                            #:tty? tty?
                            #:env env
                            #:workdir workdir
                            #:user user)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (container-logs-argv name
                              #:key (engine (container-engine))
                              (follow? #f) (tail #f) (since #f))
  "Return the argv that prints the logs of the container NAME.

FOLLOW? is --follow; TAIL is a number or the word all, passed as --tail; SINCE
is passed as --since."
  (container-argv (append (list "logs")
                          (if follow? (list "--follow") '())
                          (if tail (list "--tail" (format #f "~a" tail)) '())
                          (if since (list "--since" since) '())
                          (list name))
                  #:engine engine))

(define* (container-logs name
                         #:key (engine (container-engine))
                         (follow? #f) (tail #f) (since #f)
                         (allow-failure? #f))
  "Print the logs of the container NAME; see `container-logs-argv`."
  (run (container-logs-argv name
                            #:engine engine
                            #:follow? follow?
                            #:tail tail
                            #:since since)
       #:allow-failure? allow-failure?))

(define* (container-pull-argv image #:key (engine (container-engine)))
  "Return the argv that pulls IMAGE."
  (container-argv (list "pull" image) #:engine engine))

(define* (container-pull image #:key (engine (container-engine))
                         (allow-failure? #f))
  "Pull IMAGE; see `container-pull-argv`."
  (run (container-pull-argv image #:engine engine)
       #:allow-failure? allow-failure?))

(define* (container-push-argv image #:key (engine (container-engine)))
  "Return the argv that pushes IMAGE."
  (container-argv (list "push" image) #:engine engine))

(define* (container-push image #:key (engine (container-engine))
                         (allow-failure? #f))
  "Push IMAGE; see `container-push-argv`."
  (run (container-push-argv image #:engine engine)
       #:allow-failure? allow-failure?))

(define* (container-tag-argv source target #:key (engine (container-engine)))
  "Return the argv that gives SOURCE the additional name TARGET."
  (container-argv (list "tag" source target) #:engine engine))

(define* (container-tag source target #:key (engine (container-engine))
                        (allow-failure? #f))
  "Tag SOURCE as TARGET; see `container-tag-argv`."
  (run (container-tag-argv source target #:engine engine)
       #:allow-failure? allow-failure?))

(define* (container-inspect-argv names
                                 #:key (engine (container-engine))
                                 (format #f) (type #f))
  "Return the argv that inspects NAMES.

FORMAT is passed as --format, which is how a task reads a field rather than the
whole JSON document.  TYPE is --type for engines that accept it."
  (container-argv (append (list "inspect")
                          (if format (list "--format" format) '())
                          (if type (list "--type" type) '())
                          names)
                  #:engine engine))

(define* (container-inspect names
                            #:key (engine (container-engine))
                            (format #f) (type #f) (allow-failure? #f))
  "Inspect NAMES; see `container-inspect-argv`."
  (run (container-inspect-argv names #:engine engine #:format format #:type type)
       #:allow-failure? allow-failure?))

;; ====== Compose ======

(define* (compose-argv arguments
                       #:key
                       (program (container-compose-command))
                       (file #f)
                       (project #f)
                       (env-files '())
                       (profiles '()))
  "Return the argv that runs Compose with ARGUMENTS.

FILE is --file, PROJECT is --project-name, ENV-FILES are passed as --env-file,
and PROFILES as --profile.  They come before the subcommand, which is where
both Docker Compose and podman-compose expect them."
  (when (or (not (list? program)) (null? program) (not (every string? program)))
    (raise-maak-error
     "compose expects a non-empty program list"
     #:hint "Set MAAK_COMPOSE, or use container-compose-command."))
  (append program
          (if file (list "--file" file) '())
          (if project (list "--project-name" project) '())
          (append-map (lambda (path) (list "--env-file" path)) env-files)
          (append-map (lambda (name) (list "--profile" name)) profiles)
          arguments))

(define* (run-compose arguments
                      #:key
                      (program (container-compose-command))
                      (file #f)
                      (project #f)
                      (env-files '())
                      (profiles '())
                      (allow-failure? #f)
                      (verbose? #f))
  "Run Compose with ARGUMENTS, a list of strings.

The escape hatch for subcommands without a wrapper, such as `top`.  FILE,
PROJECT, ENV-FILES and PROFILES are passed before the subcommand, the way both
Compose implementations expect."
  (run (compose-argv arguments
                     #:program program
                     #:file file
                     #:project project
                     #:env-files env-files
                     #:profiles profiles)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (compose-output arguments
                         #:key
                         (program (container-compose-command))
                         (file #f)
                         (project #f)
                         (env-files '())
                         (profiles '())
                         (allow-failure? #f))
  "Run Compose with ARGUMENTS and return its output as a string."
  (capture (string-join
            (map shell-quote
                 (compose-argv arguments
                               #:program program
                               #:file file
                               #:project project
                               #:env-files env-files
                               #:profiles profiles))
            " ")
           #:allow-failure? allow-failure?))

(define* (compose-up-argv services
                          #:key
                          (program (container-compose-command))
                          (file #f)
                          (project #f)
                          (env-files '())
                          (profiles '())
                          (detach? #t)
                          (build? #f)
                          (force-recreate? #f)
                          (remove-orphans? #t)
                          (no-start? #f)
                          (wait? #f))
  "Return the argv that starts SERVICES, or every service when it is empty.

DETACH? (the default) is --detach, because a task that starts a stack wants its
shell back.  BUILD? rebuilds images, FORCE-RECREATE? replaces the containers,
REMOVE-ORPHANS? is on by default, and WAIT? waits for the services to be healthy
before returning."
  (compose-argv (append (list "up")
                        (if detach? (list "--detach") '())
                        (if build? (list "--build") '())
                        (if force-recreate? (list "--force-recreate") '())
                        (if remove-orphans? (list "--remove-orphans") '())
                        (if no-start? (list "--no-start") '())
                        (if wait? (list "--wait") '())
                        services)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-up services
                     #:key
                     (program (container-compose-command))
                     (file #f)
                     (project #f)
                     (env-files '())
                     (profiles '())
                     (detach? #t)
                     (build? #f)
                     (force-recreate? #f)
                     (remove-orphans? #t)
                     (no-start? #f)
                     (wait? #f)
                     (allow-failure? #f)
                     (verbose? #f))
  "Start SERVICES with Compose; see `compose-up-argv`."
  (run (compose-up-argv services
                        #:program program
                        #:file file
                        #:project project
                        #:env-files env-files
                        #:profiles profiles
                        #:detach? detach?
                        #:build? build?
                        #:force-recreate? force-recreate?
                        #:remove-orphans? remove-orphans?
                        #:no-start? no-start?
                        #:wait? wait?)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (compose-down-argv services
                            #:key
                            (program (container-compose-command))
                            (file #f)
                            (project #f)
                            (env-files '())
                            (profiles '())
                            (volumes? #f)
                            (remove-orphans? #t)
                            (rmi #f)
                            (timeout #f))
  "Return the argv that stops and removes SERVICES, or the whole project.

VOLUMES? also removes named volumes, which is what makes a test environment
return to a clean state; it is off by default because the data is otherwise
kept between runs."
  (compose-argv (append (list "down")
                        (if volumes? (list "--volumes") '())
                        (if remove-orphans? (list "--remove-orphans") '())
                        (if rmi (list "--rmi" rmi) '())
                        (if timeout (list "--timeout" (format #f "~a" timeout)) '())
                        services)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-down services
                       #:key
                       (program (container-compose-command))
                       (file #f)
                       (project #f)
                       (env-files '())
                       (profiles '())
                       (volumes? #f)
                       (remove-orphans? #t)
                       (rmi #f)
                       (timeout #f)
                       (allow-failure? #f)
                       (verbose? #f))
  "Stop and remove SERVICES with Compose; see `compose-down-argv`."
  (run (compose-down-argv services
                          #:program program
                          #:file file
                          #:project project
                          #:env-files env-files
                          #:profiles profiles
                          #:volumes? volumes?
                          #:remove-orphans? remove-orphans?
                          #:rmi rmi
                          #:timeout timeout)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (compose-build-argv services
                             #:key
                             (program (container-compose-command))
                             (file #f)
                             (project #f)
                             (env-files '())
                             (profiles '())
                             (no-cache? #f)
                             (pull? #f))
  "Return the argv that builds SERVICES, or every service when it is empty."
  (compose-argv (append (list "build")
                        (if no-cache? (list "--no-cache") '())
                        (if pull? (list "--pull") '())
                        services)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-build services
                        #:key
                        (program (container-compose-command))
                        (file #f)
                        (project #f)
                        (env-files '())
                        (profiles '())
                        (no-cache? #f)
                        (pull? #f)
                        (allow-failure? #f)
                        (verbose? #f))
  "Build SERVICES with Compose; see `compose-build-argv`."
  (run (compose-build-argv services
                           #:program program
                           #:file file
                           #:project project
                           #:env-files env-files
                           #:profiles profiles
                           #:no-cache? no-cache?
                           #:pull? pull?)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (compose-logs-argv services
                            #:key
                            (program (container-compose-command))
                            (file #f)
                            (project #f)
                            (env-files '())
                            (profiles '())
                            (follow? #f)
                            (tail #f)
                            (no-color? #f))
  "Return the argv that prints the logs of SERVICES, or of the whole project."
  (compose-argv (append (list "logs")
                        (if follow? (list "--follow") '())
                        (if tail (list "--tail" (format #f "~a" tail)) '())
                        (if no-color? (list "--no-color") '())
                        services)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-logs services
                       #:key
                       (program (container-compose-command))
                       (file #f)
                       (project #f)
                       (env-files '())
                       (profiles '())
                       (follow? #f)
                       (tail #f)
                       (no-color? #f)
                       (allow-failure? #f))
  "Print the logs of SERVICES with Compose; see `compose-logs-argv`."
  (run (compose-logs-argv services
                          #:program program
                          #:file file
                          #:project project
                          #:env-files env-files
                          #:profiles profiles
                          #:follow? follow?
                          #:tail tail
                          #:no-color? no-color?)
       #:allow-failure? allow-failure?))

(define* (compose-ps-argv services
                          #:key
                          (program (container-compose-command))
                          (file #f)
                          (project #f)
                          (env-files '())
                          (profiles '())
                          (all? #f)
                          (format #f))
  "Return the argv that lists the containers of SERVICES, or of the project.

ALL? is Docker Compose's --all; podman-compose has no equivalent flag and
lists the project's containers already, so leave it off with that program."
  (compose-argv (append (list "ps")
                        (if all? (list "--all") '())
                        (if format (list "--format" format) '())
                        services)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-ps services
                     #:key
                     (program (container-compose-command))
                     (file #f)
                     (project #f)
                     (env-files '())
                     (profiles '())
                     (all? #f)
                     (format #f)
                     (allow-failure? #f))
  "List the containers of SERVICES with Compose; see `compose-ps-argv`."
  (run (compose-ps-argv services
                        #:program program
                        #:file file
                        #:project project
                        #:env-files env-files
                        #:profiles profiles
                        #:all? all?
                        #:format format)
       #:allow-failure? allow-failure?))

(define* (compose-exec-argv service command
                            #:key
                            (program (container-compose-command))
                            (file #f)
                            (project #f)
                            (env-files '())
                            (profiles '())
                            (tty? #f)
                            (user #f)
                            (workdir #f)
                            (index #f)
                            (no-tty? #t))
  "Return the argv that runs COMMAND in SERVICE.

NO-TTY? (the default) passes -T, which both Docker Compose and podman-compose
spell the same way.  A task needs it: without it Compose tries to allocate a
terminal, and a command run from a pipeline or a CI job does not have one.
TTY? asks for a terminal instead, and wins over the default: the two flags
contradict each other, so only one is ever emitted.  Pass #:no-tty? #f for an
interactive shell that should not be told what to do either way."
  (compose-argv (append (list "exec")
                        (if (and no-tty? (not tty?)) (list "-T") '())
                        (if tty? (list "--tty") '())
                        (if workdir (list "--workdir" workdir) '())
                        (if user (list "--user" user) '())
                        (if index (list "--index" (format #f "~a" index)) '())
                        (list service)
                        command)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-exec service command
                       #:key
                       (program (container-compose-command))
                       (file #f)
                       (project #f)
                       (env-files '())
                       (profiles '())
                       (tty? #f)
                       (user #f)
                       (workdir #f)
                       (index #f)
                       (no-tty? #t)
                       (allow-failure? #f)
                       (verbose? #f))
  "Run COMMAND in SERVICE with Compose; see `compose-exec-argv`."
  (run (compose-exec-argv service command
                          #:program program
                          #:file file
                          #:project project
                          #:env-files env-files
                          #:profiles profiles
                          #:tty? tty?
                          #:user user
                          #:workdir workdir
                          #:index index
                          #:no-tty? no-tty?)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (compose-run-argv service command
                           #:key
                           (program (container-compose-command))
                           (file #f)
                           (project #f)
                           (env-files '())
                           (profiles '())
                           (remove? #t)
                           (no-deps? #f)
                           (user #f)
                           (workdir #f))
  "Return the argv that runs COMMAND in a one-off container for SERVICE.

REMOVE? (the default) passes --rm; NO-DEPS? keeps Compose from starting the
service's dependencies."
  (compose-argv (append (list "run")
                        (if remove? (list "--rm") '())
                        (if no-deps? (list "--no-deps") '())
                        (if workdir (list "--workdir" workdir) '())
                        (if user (list "--user" user) '())
                        (list service)
                        command)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-run service command
                      #:key
                      (program (container-compose-command))
                      (file #f)
                      (project #f)
                      (env-files '())
                      (profiles '())
                      (remove? #t)
                      (no-deps? #f)
                      (user #f)
                      (workdir #f)
                      (allow-failure? #f)
                      (verbose? #f))
  "Run COMMAND in a one-off container for SERVICE; see `compose-run-argv`."
  (run (compose-run-argv service command
                         #:program program
                         #:file file
                         #:project project
                         #:env-files env-files
                         #:profiles profiles
                         #:remove? remove?
                         #:no-deps? no-deps?
                         #:user user
                         #:workdir workdir)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (compose-pull-argv services
                            #:key
                            (program (container-compose-command))
                            (file #f)
                            (project #f)
                            (env-files '())
                            (profiles '())
                            (ignore-pull-failures? #f))
  "Return the argv that pulls the images of SERVICES.

IGNORE-PULL-FAILURES? is Docker Compose's flag; podman-compose does not accept
it, so leave it off with that program."
  (compose-argv (append (list "pull")
                        (if ignore-pull-failures? (list "--ignore-pull-failures") '())
                        services)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-pull services
                       #:key
                       (program (container-compose-command))
                       (file #f)
                       (project #f)
                       (env-files '())
                       (profiles '())
                       (ignore-pull-failures? #f)
                       (allow-failure? #f)
                       (verbose? #f))
  "Pull the images of SERVICES with Compose; see `compose-pull-argv`."
  (run (compose-pull-argv services
                          #:program program
                          #:file file
                          #:project project
                          #:env-files env-files
                          #:profiles profiles
                          #:ignore-pull-failures? ignore-pull-failures?)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (compose-restart-argv services
                               #:key
                               (program (container-compose-command))
                               (file #f)
                               (project #f)
                               (env-files '())
                               (profiles '())
                               (timeout #f))
  "Return the argv that restarts SERVICES."
  (compose-argv (append (list "restart")
                        (if timeout (list "--timeout" (format #f "~a" timeout)) '())
                        services)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-restart services
                          #:key
                          (program (container-compose-command))
                          (file #f)
                          (project #f)
                          (env-files '())
                          (profiles '())
                          (timeout #f)
                          (allow-failure? #f)
                          (verbose? #f))
  "Restart SERVICES with Compose; see `compose-restart-argv`."
  (run (compose-restart-argv services
                             #:program program
                             #:file file
                             #:project project
                             #:env-files env-files
                             #:profiles profiles
                             #:timeout timeout)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (compose-config-argv services
                              #:key
                              (program (container-compose-command))
                              (file #f)
                              (project #f)
                              (env-files '())
                              (profiles '())
                              (quiet? #f)
                              (services? #f))
  "Return the argv that renders the resolved Compose configuration.

QUIET? asks Compose to validate without printing; SERVICES? asks for only the
service definitions, and the service names are passed as arguments when given.
The quiet flag is spelled -q because that is the form both Compose
implementations accept."
  (compose-argv (append (list "config")
                        (if quiet? (list "-q") '())
                        (if services? (list "--services") '())
                        services)
                #:program program
                #:file file
                #:project project
                #:env-files env-files
                #:profiles profiles))

(define* (compose-config services
                         #:key
                         (program (container-compose-command))
                         (file #f)
                         (project #f)
                         (env-files '())
                         (profiles '())
                         (quiet? #f)
                         (services? #f)
                         (allow-failure? #f)
                         (verbose? #f))
  "Render the Compose configuration; see `compose-config-argv`."
  (run (compose-config-argv services
                            #:program program
                            #:file file
                            #:project project
                            #:env-files env-files
                            #:profiles profiles
                            #:quiet? quiet?
                            #:services? services?)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))
