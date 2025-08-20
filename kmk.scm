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

;; Default target
;; (define (default)
;;   (log-info "This is a test\n")
;;   ($ "echo 'I am in a shell'")
;;   ($ "echo \"My pwd is: $(pwd)\""))

;; Target to format source code files according to the style guide
;; Find all .scm files and format them using 'guix style'
(define (fmt)
  (log-info "Format Scheme files using Guix Style")
  ($ '("find . -maxdepth 8 -name \"*.scm\""
       "-type f -exec guix style -f {} \\;")
     #:verbose #t))

;; Development REPL
(define (repl)
  ($ '("guix shell -m manifest.scm --" "guile -L ./src"
       "-c '((@ (ares server) run-nrepl-server))'")))

(define (help)
  ($ '("guix shell -f guix.scm -- kmk --help")
     #:verbose #t))

