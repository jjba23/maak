;; kmk

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; kmk is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; kmk is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with kmk.  If not, see <https://www.gnu.org/licenses/>.help

(define-module (kmk)
  #:declarative? #t
  #:use-module (kmk kmk))

;; ====== KMK file constants ======

(define api-docs-css-url
  "https://jointhefreeworld.org/static-assets/api-docs/css/style.css")

(define deploy-www
  "/srv/http/jointhefreeworld.org")

;; ====== KMK file tasks ======

(define (default)
  "Default task run when no arguments are passed: do something fun."
  (let* ((entries '(1 3 5 7 9))
         (arithmetics (lambda (x)
                        (* x x 3)))
         (some-data (map arithmetics entries)))
    (log-info "Performed some fun arithmetics:
Result: ~a" some-data)))

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  ($ '("find . -maxdepth 8 -name '*.scm'" "-type f -exec guix style -f {} \\;")
     #:verbose #t))

(define (vars)
  "Display some system variables."
  ($ '("echo -n \"\nHome directory: ${HOME}\"" "echo -n \"\nShell: ${SHELL}\"")
     #:join " && ")
  ($ '("echo -n \"\nUser: ${USER}\"")))

(define (repl)
  "Development REPL."
  ($ '("guix shell -m manifest.scm --" "guile -L ./src"
       "-c '((@ (ares server) run-nrepl-server))'")))

(define (help)
  "Display program help screen."
  ($ '("guix shell -f guix.scm -- kmk --help")
     #:verbose #t))

(define (docs)
  "Generate technical API documentation for KMK."
  ($ '("rm -rfv doc"))
  ($ (list "guix shell -m manifest.scm --" "documenta api ./src/kmk &&"
           (format #f "texi2any -v --html --css-ref=~a" api-docs-css-url)
           "./doc/api/index.texi"))
  ($ '("mv ./index ./doc/api-dist")))

(define (deploy)
  "Generate documentation for the project and publish to web directory."
  (docs)
  ($ (list (format #f "rm -rfv ~a/api-docs/kmk" deploy-www)
           (format #f "mkdir -p ~a/api-docs/kmk" deploy-www)
           (format #f "mv doc/api-dist/* ~a/api-docs/kmk" deploy-www))
     #:join " && ")
  ($ '("rm -rfv doc")))


