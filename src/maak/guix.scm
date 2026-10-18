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
;; Guix asked for values.  The helpers in `(maak dsl)' build command lines --
;; `guix shell', `guix pack', `guix time-machine' -- because that is what a
;; task usually wants: the environment, the container, the binary.  The
;; helpers here answer the other question: what *is* the package at this-pin,
;; and where did building it put the result?  `guix time-machine' answers that
;; by starting a whole second Guix; a Scheme program can ask it directly, and
;; keep the answer as a value.
;;
;; The asking happens in `guix repl', which is Guix's own Guile with Guix's
;; own modules on the load path.  Nothing in maak imports `(guix ...)`: a maak
;; built on a machine without Guix has to compile and run exactly like one
;; built with it, so the dependency is on the *program* at the moment a task
;; calls one of these, not on the build.  A machine without Guix gets a
;; one-line error with a hint, from the call that needed it, and every other
;; part of maak is unaffected.
;;
;; The price is a process per query: about a third of a second for the repl
;; plus the store connection.  `cached-channel-instance' keeps the second
;; query against the same pins cheap, because the channel checkout is already
;; in the store.

;;; Code:

(define-module (maak guix)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (guix-available?
            guix-channels
            guix-derivation
            guix-build
            guix-program
            guix-derivation-at
            guix-build-at
            guix-program-at))

;; Guix's own default, and the one a `guix describe' writes.  The project's
;; own channels.scm uses the project's mirror; a caller that pins a revision
;; can name either with `#:url'.
(define default-guix-channel-url
  "https://git.savannah.gnu.org/git/guix.git")

;; Files written for the queries below are named from this counter, so two
;; queries in one process never share a file even when the second starts while
;; the first is unwinding.
(define guix-script-counter 0)

(define (guix-available?)
  "Return #t when a `guix' program is on `PATH'."
  (and (which "guix") #t))

(define (require-guix)
  "Return the path of `guix', raising a maak error when it is missing.

The message names the program rather than Guix's libraries, because that is
what a caller can install and what the rest of maak does without."
  (or (which "guix")
      (raise-maak-error
       "Guix is not available: no `guix' program on PATH"
       #:hint
       "Install Guix and put `guix' on PATH.  Only the (maak guix) helpers need it; every other task runs without it.")))

;; ====== Asking Guix ======
;;
;; `guix repl -- FILE' runs FILE in Guix's environment.  The file writes its
;; answer with `write', and the parent turns that back into a value; a list of
;; strings and numbers survives the trip, a store object does not, which is
;; why a derivation travels as its file name.
;;
;; The answer is tagged: `(maak/ok VALUE)' when there is one, and
;; `(maak/no-package NAME)' when the lookup found nothing, so a mistyped
;; package name is a sentence about the name rather than a backtrace from
;; Guix's side of the pipe.

(define (guix-script-path)
  "Return a file name of this run's own for the program to evaluate.

The file is written just before the query and removed just after it, so the
name only has to be unique within the process."
  (set! guix-script-counter (+ guix-script-counter 1))
  (format #f "~a/maak-guix-~a-~a.scm"
          (tmp-dir) (getpid) guix-script-counter))

(define (guix-answer text command)
  "Return the value a Guix query wrote, or raise with what it said instead.

TEXT is everything the child printed; COMMAND is the line that produced it,
named in the error when the answer makes no sense."
  (match (catch #t
           (lambda () (read (open-input-string text)))
           (lambda args #f))
    (('maak/ok value) value)
    (('maak/no-package name)
     (raise-maak-error
      (format #f "no package named ~a in that Guix revision" name)
      #:hint
      "Check the name with `guix search', or use a channels file whose channels provide the package."))
    (_
     (raise-maak-error
      (format #f "Guix did not answer the query as expected: ~a" command)
      #:hint "Run the command above by hand to read what Guix said."))))

(define (guix-evaluate program)
  "Evaluate PROGRAM with `guix repl' and return the value it wrote.

PROGRAM is one expression, ending in a `write' of the answer.  It goes into a
temporary file because `guix repl' takes a file rather than an expression, and
the file is removed afterwards -- unless the query failed, in which case it is
kept and named in the error, since it is the only copy of what Guix objected
to.  In a dry run nothing is evaluated: the command is printed and #f is
returned."
  (require-guix)
  (let* ((path (guix-script-path))
         (command (format #f "guix repl -- ~a" (shell-quote path)))
         (keep? #f))
    (if (dry-run?)
        (begin
          (unless (quiet?)
            (display (bold-output (format #f "\n[DRY-RUN]: ~a\n" command))))
          #f)
        (dynamic-wind
          (lambda () (write-file path program))
          (lambda ()
            (guix-answer (catch #t
                           (lambda () (capture command))
                           (lambda (key . args)
                             ;; The query file is what a reader needs to see
                             ;; what went wrong, so it outlives the failure.
                             (set! keep? #t)
                             (let ((error (find-maak-error key args)))
                               (unless error (apply throw key args))
                               (raise-maak-error
                                (maak-error-message error)
                                #:hint
                                (format #f
                                        "The query Guix ran is kept in ~a; run `~a' by hand to read Guix's own message."
                                        path command)))))
                         command))
          (lambda ()
            (unless keep?
              (when (file-exists? path)
                (delete-file path))))))))

(define (guix-channels-file file)
  "Return FILE as an absolute path, raising a maak error when it is missing."
  (let ((path (if (string-prefix? "/" file)
                  file
                  (string-append (getcwd) "/" file))))
    (unless (file-exists? path)
      (raise-maak-error
       (format #f "no channels file at ~a" path)
       #:hint
       "Write one with `guix describe -f channels > channels.scm', or pin a revision with the `-at' helpers."))
    path))

(define (guix-channels-expression file)
  "Return the Scheme text that names the channels pinned in FILE."
  (format #f "(load ~s)" (guix-channels-file file)))

(define (guix-pinned-expression revision url)
  "Return the Scheme text of a channel list pinned to REVISION of URL."
  (unless (and (string? revision) (not (string=? revision "")))
    (raise-maak-error
     (format #f "a Guix revision must be a commit, got ~s" revision)
     #:hint
     "Use the full commit `guix describe' prints, for example \"0fb918d61609f3aadc2c8cc212926b3bdfabb4c\"."))
  (unless (and (string? url) (not (string=? url "")))
    (raise-maak-error
     (format #f "a Guix channel URL must be a string, got ~s" url)
     #:hint "The default is Guix's own repository; name a mirror with #:url."))
  (format #f "(list (channel (name 'guix) (url ~s) (commit ~s)))"
          url revision))

(define (guix-package-query channels package system answer)
  "Return the program that asks Guix for ANSWER about PACKAGE.

CHANNELS is the Scheme text of the channel list to evaluate Guix at, PACKAGE a
package name and SYSTEM the target system, or #f for the one Guix runs on.
ANSWER is an expression naming what to return, with `store' and `derivation'
in scope."
  (unless (and (string? package) (not (string=? package "")))
    (raise-maak-error
     (format #f "a Guix package name must be a string, got ~s" package)
     #:hint "Name the package the way `guix search' spells it, for example \"hello\"."))
  (string-append
   "(use-modules (guix channels) (guix store) (guix inferior)\n"
   "             (guix derivations) (srfi srfi-1))\n"
   "(define store (open-connection))\n"
   (format #f "(define inferior (open-inferior (cached-channel-instance store ~a)))\n"
           channels)
   (format #f "(define candidates (lookup-inferior-packages inferior ~s))\n"
           package)
   "(if (null? candidates)\n"
   (format #f "    (write (list 'maak/no-package ~s))\n" package)
   "    (let* ((package (first candidates))\n"
   (format #f "           (derivation (inferior-package-derivation store package~a)))\n"
           (if system (format #f " ~s" system) ""))
   (format #f "      (write (list 'maak/ok ~a))))\n" answer)))

;; ====== Channels ======

(define* (guix-channels #:key (file "channels.scm"))
  "Return the channel pins FILE names, as an alist of name and commit.

@example
(guix-channels)
@result{} ((\"guix\" . \"0fb918d61609f3aadc2d8ecc212926b3bdfabb4c\"))
@end example

The file is a normal `channels.scm', the one `guix describe -f channels'
writes; the answer names every channel it lists, in the order they appear.
This is the pin the `-at' helpers replace, and the one `guix time-machine'
would read."
  (guix-evaluate
   (string-append
    "(use-modules (guix channels))\n"
    "(write (list 'maak/ok\n"
    "             (map (lambda (channel)\n"
    "                    (cons (symbol->string (channel-name channel))\n"
    "                          (channel-commit channel)))\n"
    (format #f "                  (load ~s))))\n"
            (guix-channels-file file)))))

;; ====== Packages at a pin ======

(define* (guix-derivation package #:key (channels "channels.scm") (system #f))
  "Return the derivation file name of PACKAGE as CHANNELS pins it.

PACKAGE is a package name, looked up in the Guix that CHANNELS names rather
than in the one running.  SYSTEM is the target system, or #f for this
machine's.  Nothing is built: the derivation is the recipe, and its file name
is enough for `guix build' or for a later `build-derivations'.

@example
(guix-derivation \"hello\")
@result{} \"/gnu/store/…-hello-2.12.3.drv\"
@end example

A channels file that has never been fetched is fetched here, with the network
and the time that takes; afterwards Guix's channel cache answers at once."
  (guix-evaluate
   (guix-package-query (guix-channels-expression channels)
                       package
                       system
                       "(derivation-file-name derivation)")))

(define* (guix-build package #:key (channels "channels.scm") (system #f))
  "Build PACKAGE as CHANNELS pins it and return its output path.

Building is what makes the answer usable: the output path is the directory the
package installed into, with its `bin', `lib' and `share' underneath.

@example
(guix-build \"hello\")
@result{} \"/gnu/store/…-hello-2.12.3\"
@end example

Guix's own build log goes to standard error as it does for `guix build', so a
long build still shows its progress."
  (guix-evaluate
   (guix-package-query (guix-channels-expression channels)
                       package
                       system
                       "(begin (build-derivations store (list derivation))\n         (derivation->output-path derivation))")))

(define (guix-output-program output name)
  "Return the path of NAME inside the store OUTPUT.

A NAME with a slash in it is a path from the root of the output, for the
packages that install their programs somewhere other than `bin'."
  (if (string-contains name "/")
      (string-append output "/" name)
      (string-append output "/bin/" name)))

(define* (guix-program package args
                       #:key (program #f) (channels "channels.scm") (system #f)
                       (verbose? #f) (env '()) (sudo? #f) (dev-null? #f)
                       (allow-failure? #f) (async? #f))
  "Run a program from PACKAGE as CHANNELS pins it.

ARGS are the arguments the program gets, each a string.  PROGRAM names the
program inside the package's output and defaults to the package's own name: a
bare name is looked for in `bin', and a name with a slash in it is taken as a
path from the output's root.  The package is built first, so the program that
runs is the one that pin names and not whatever the machine happens to have
installed.

@example
(guix-program \"hello\" '(\"--version\"))
(guix-program \"coreutils\" '(\"-l\" \"/gnu/store\") #:program \"ls\")
(guix-program \"diffutils\" (list \"-u\" a b) #:program \"diff\" #:async? #t)
@end example

The decorations are `run''s: `#:env', `#:sudo?', `#:dev-null?',
`#:allow-failure?' and `#:async?', so the program can be a job like any other.

In a dry run the store query is printed instead of the program line, because
the path is only known once Guix has answered; #t is returned."
  (when (or (not (list? args)) (not (every string? args)))
    (raise-maak-error
     "guix-program expects its arguments as a list of strings"
     #:hint "Write (guix-program \"hello\" '(--version))."))
  (let ((output (guix-build package #:channels channels #:system system)))
    (cond
      ((not output) #t)
      (else
       (let ((name (or program package)))
         (run (cons (guix-output-program output name) args)
              #:verbose? verbose?
              #:env env
              #:sudo? sudo?
              #:dev-null? dev-null?
              #:allow-failure? allow-failure?
              #:async? async?))))))

;; ====== One revision, no channels file ======

(define* (guix-derivation-at revision package
                             #:key (url default-guix-channel-url) (system #f))
  "Return the derivation file name of PACKAGE at Guix REVISION.

REVISION is a commit of the Guix repository at URL, which defaults to the
canonical one.  This is the `guix time-machine --commit=REVISION' question
asked as a value: which Guix is not the one running, and no channels file has
to exist for it.

@example
(guix-derivation-at \"0fb918d61609f3aadc2d8ecc212926b3bdfabb4c\" \"hello\")
@end example"
  (guix-evaluate
   (guix-package-query (guix-pinned-expression revision url)
                       package
                       system
                       "(derivation-file-name derivation)")))

(define* (guix-build-at revision package
                        #:key (url default-guix-channel-url) (system #f))
  "Build PACKAGE at Guix REVISION and return its output path.

The pinned form of `guix-build': REVISION is a commit of the Guix repository at
URL, and no channels file is read."
  (guix-evaluate
   (guix-package-query (guix-pinned-expression revision url)
                       package
                       system
                       "(begin (build-derivations store (list derivation))\n         (derivation->output-path derivation))")))

(define* (guix-program-at revision package args
                          #:key (program #f) (url default-guix-channel-url)
                          (system #f) (verbose? #f) (env '()) (sudo? #f)
                          (dev-null? #f) (allow-failure? #f) (async? #f))
  "Run a program from PACKAGE, built by Guix REVISION, with ARGS.

The pinned form of `guix-program': the package comes from the Guix at REVISION
of URL rather than from the pinned channels file, which is what a task wants
when it has to run one old tool for one step.  PROGRAM and the decorations are
`guix-program''s.

@example
(guix-program-at \"0fb918d61609f3aadc2d8ecc212926b3bdfabb4c\"
                 \"hello\" '(--version))
@end example"
  (when (or (not (list? args)) (not (every string? args)))
    (raise-maak-error
     "guix-program-at expects its arguments as a list of strings"
     #:hint "Write (guix-program-at revision \"hello\" '(--version))."))
  (let ((output (guix-build-at revision package #:url url #:system system)))
    (cond
      ((not output) #t)
      (else
       (let ((name (or program package)))
         (run (cons (guix-output-program output name) args)
              #:verbose? verbose?
              #:env env
              #:sudo? sudo?
              #:dev-null? dev-null?
              #:allow-failure? allow-failure?
              #:async? async?))))))
