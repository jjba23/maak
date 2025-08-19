(define-module (kmk)
  #:declarative? #t
  #:use-module (kmk prelude))

;; Target to format source code files according to the style guide
;; Find all .scm files and format them using 'guix style'
(define (fmt)
  ($ "find . -maxdepth 8 -name \"*.scm\"" "-type f -exec guix style -f {} \\;"))

;; Development REPL
(define (repl)
  ($ "guix shell -m manifest.scm --" "guile -L ./src"
     "-c '((@ (ares server) run-nrepl-server))'"))

