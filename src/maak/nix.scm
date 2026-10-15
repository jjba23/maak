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
;; Nix helpers, mirroring the Guix ones in `(maak dsl)'.  maak is Guix-first,
;; but a task that has to build on a Nix machine should not have to hand-roll
;; `nix-shell' or `nix develop' invocations either.
;;
;; The two families are deliberately separate: `nix-shell' and `nix-build' are
;; the classic, `shell.nix'/`default.nix' based commands, while
;; `nix-develop', `nix-flake-build' and `nix-run' are the flake based ones.
;; `nix-flake-check' validates a flake the way `guix build' would validate a
;; package.

;;; Code:

(define-module (maak nix)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (nix-shell
            nix-develop
            nix-build
            nix-flake-build
            nix-flake-check
            nix-run))

(define* (nix-shell cmd-parts
                    #:key (file #f)
                    (pure? #f)
                    (packages '())
                    (join " ")
                    (verbose? #f))
  "Run CMD-PARTS inside a Nix shell.

With FILE the shell is described by that file (typically `shell.nix'); without
it, Nix looks for `shell.nix' or `default.nix' in the current directory.  Each
of PACKAGES is added with `-p', and PURE? passes `--pure' so the host
environment is hidden.

The command is handed to Nix's own `--run', so it executes inside the shell
rather than after it.

@example\n(nix-shell '(\"python -c 'print(1)'\") #:packages '(\"python3\"))\n@end example"
  (let ((command (string-join cmd-parts join)))
    ($ (append (list "nix-shell")
               (if pure? (list "--pure") '())
               (if file (list (format #f "--file ~a" file)) '())
               (map (lambda (pkg)
                      (format #f "--packages ~a" pkg)) packages)
               (list (format #f "--run ~a" (shell-quote command))))
       #:join " "
       #:verbose? verbose?)))

(define* (nix-develop cmd-parts
                      #:key (flake ".")
                      (impure? #f)
                      (join " ")
                      (verbose? #f))
  "Run CMD-PARTS inside the development shell of the flake FLAKE.

FLAKE is a path or flake reference such as `.` or `github:owner/repo'.  With
IMPURE? the shell inherits the host environment (`--impure'), which some
tools that read environment variables need.

@example\n(nix-develop '(\"make test\") #:flake \".\")\n@end example"
  ($ (append (list (format #f "nix develop ~a" flake))
             (if impure? (list "--impure") '())
             (list "--command") cmd-parts)
     #:join " "
     #:verbose? verbose?))

(define* (nix-build #:key (file "default.nix")
                    (arguments '())
                    (out-link #f)
                    (verbose? #f))
  "Build the derivation in FILE with the classic `nix-build'.

ARGUMENTS are extra words passed through verbatim, for example
`--argstr system x86_64-linux'.  OUT-LINK names the result symlink
(`--out-link'), defaulting to Nix's own `result'."
  ($ (append (list (format #f "nix-build ~a" file))
             (if out-link (list (format #f "--out-link ~a" out-link)) '())
             arguments)
     #:join " "
     #:verbose? verbose?))

(define* (nix-flake-build #:key (flake ".")
                          (attr #f)
                          (verbose? #f))
  "Build the flake FLAKE, optionally selecting the output ATTR.

@example\n(nix-flake-build #:flake \".\" #:attr \"packages.x86_64-linux.maak\")\n@end example"
  ($ (list (format #f "nix build ~a"
                   (if attr (format #f "~a#~a" flake attr) flake)))
     #:verbose? verbose?))

(define* (nix-flake-check #:key (flake ".")
                          (all-systems? #f)
                          (verbose? #f))
  "Check every output of the flake FLAKE, as `nix flake check' does.

With ALL-SYSTEMS? the check runs for all systems the flake declares rather
than just the current one."
  ($ (append (list "nix flake check")
             (if all-systems? (list "--all-systems") '())
             (list flake))
     #:join " "
     #:verbose? verbose?))

(define* (nix-run cmd-parts
                  #:key (flake ".")
                  (app #f)
                  (join " ")
                  (verbose? #f))
  "Run the application APP of the flake FLAKE with CMD-PARTS as its arguments.

@example\n(nix-run '(\"--version\") #:flake \".\" #:app \"maak\")\n@end example"
  ($ (append (list (format #f "nix run ~a"
                           (if app (format #f "~a#~a" flake app) flake)))
             (list "--") cmd-parts)
     #:join " "
     #:verbose? verbose?))
