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

(define-module (maak dsl)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 getopt-long)
  #:export (log-message log-info
                        log-error
                        get-module-var
                        delete-file-recursively
                        manifest-shell
                        program-shell
                        mkdir-p
                        cp
                        mv
                        remkdir-p
                        maak-proc?
                        maak-task?
                        $
                        ~
                        syscall
                        cat))

(define preferred-strftime-format
  (make-parameter "%Y-%m-%d %H:%M:%S%z"))

(define (log-message msg . args)
  (display (format #f "\n~a"
                   (apply format
                          (append (list #f msg) args)))))

(define (log-info msg . args)
  (log-message (format #f "[INFO][~a] ~a"
                       (strftime (preferred-strftime-format)
                                 (localtime (current-time)))
                       (apply format
                              (append (list #f msg) args)))))

(define (log-error msg . args)
  (log-message (format #f "[ERROR][~a] ~a"
                       (strftime (preferred-strftime-format)
                                 (localtime (current-time)))
                       (apply format
                              (append (list #f msg) args)))))

(define* (syscall cmd)
  "Executes a shell command and checks its exit code.

Args:
  cmd: A string representing the shell command to be executed.

Returns:
  #t if the command completes with an exit code of 0.

Errors:
  Signals an error if the command returns a non-zero exit code,
  providing the command and the exit code in the error message."
  (let* ((exit-val (system cmd))
         (exit-code (status:exit-val exit-val)))
    (cond
      ((equal? 0 exit-code)
       #t)
      (else (error (format #f "
Non-zero exit code when running!
Command: ~a
Exit code: ~a
" cmd exit-code))))))

(define* ($ cmd-parts
            #:key (verbose? #f)
            (join " "))
  "Executes a shell command constructed from a list of strings.
  
Args:
  cmd-parts: A list of strings that will be joined to form the command.
  #:key:
    verbose?: A boolean. If true, the command to be executed will be logged
             to the info stream before execution.
    join: A string to use as a separator when joining the command parts.
          Defaults to a space ("
  ").

Returns:
  The result of the `syscall` function, which typically is the exit status
  of the executed command."
  (let* ((cmd (string-join cmd-parts join)))
    (when verbose?
      (log-info "  Execute command:\n~a\n" cmd))
    (syscall cmd)))

(define-syntax-rule (get-module-var setting)
  "Get a variable from the currently loaded maak file module."
  (let* ((val (module-variable (resolve-module '(maak)) setting)))
    (catch #t
           (lambda ()
             (variable-ref val))
           (lambda (key . args)
             #f))))

(define (maak-task? x)
  "Predicate that indicates whether it is a valid maak task."
  (let* ((proc (variable-ref x)))
    (and proc
         (maak-proc? proc))))

(define (maak-proc? x)
  "Predicate that indicates whether a procedure is a valid maak proc."
  (and (procedure? x)
       (equal? '(0 0 #f)
               (procedure-minimum-arity x))))

(define* (delete-file-recursively dir
                                  #:key (verbose? #f))
  ($ (list (~ "rm -rfv ~a" dir))
     #:verbose? verbose?))

(define* (mkdir-p dir
                  #:key (verbose? #f))
  ($ (list (~ "mkdir -pv ~a" dir))))

(define* (remkdir-p dir
                    #:key (verbose? #f))
  (delete-file-recursively dir
                           #:verbose? verbose?)
  (mkdir-p dir
           #:verbose? verbose?))

(define* (mv x y
             #:key (verbose? #f))
  ($ (list (~ "mv -v ~a ~a" x y))
     #:verbose? verbose?))

(define* (cat file
              #:key (verbose? #f))
  ($ (list (~ "cat ~a" file))
     #:verbose? verbose?))

(define* (cp x
             y
             #:key (recursive? #t)
             (force? #f)
             (verbose? #f)
             (verbose-copy? #t))
  ($ (list (~ "cp ~a ~a ~a ~a ~a"
              (if recursive? "-r" "")
              (if force? "-f" "")
              (if verbose-copy? "-v" "")
              x
              y))
     #:verbose? verbose?))

(define* (~ #:rest args)
  (apply format
         (append '(#f) args)))

(define* (manifest-shell cmd-parts
                         #:key (verbose? #f)
                         (join " ")
                         (manifest "manifest.scm"))
  ($ (append (list (~ "guix shell -m ~a -- " manifest)) cmd-parts)
     #:verbose? verbose?))

(define* (program-shell cmd-parts
                        #:key (verbose? #f)
                        (join " ")
                        (file "guix.scm"))
  ($ (append (list (~ "guix shell -f ~a -- " file)) cmd-parts)
     #:verbose? verbose?))

