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

;; Default task run when no arguments are passed: do something fun
(define (default)
  (let* ((entries '(1 3 5 7 9))
         (arithmetics (lambda(x) (* x x 3)))
         (some-data (map arithmetics entries)))
    (log-info "Running default task")
    (log-info "Performed some fun arithmetics:\nResult: ~a" some-data)))

;; Format Scheme source code files according to the Guix style guide
(define (fmt)
  (log-info "Format Scheme files using Guix Style")
  ($ '("find . -maxdepth 8 -name '*.scm'" "-type f -exec guix style -f {} \\;")
     #:verbose #t))

;; Development REPL
(define (repl)
  ($ '("guix shell -m manifest.scm --" "guile -L ./src"
       "-c '((@ (ares server) run-nrepl-server))'")))

;; Display program help screen
(define (help)
  ($ '("guix shell -f guix.scm -- kmk --help")
     #:verbose #t))

