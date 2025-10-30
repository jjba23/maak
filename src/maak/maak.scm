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

(define-module (maak maak)
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
  #:export (log-message log
                        log-info
                        log-error
                        get-module-var
                        delete-file-recursively
                        manifest-shell
                        program-shell
                        mkdir-p
                        cp
                        mv
                        remkdir-p
                        $
                        ~
                        syscall
                        cat
                        list-tasks
                        run-tasks
                        run-task))

(define preferred-strftime-format
  (make-parameter "%Y-%m-%d %H:%M:%S%z"))

(define (log-message msg)
  (display (format #f "\n~a" msg)))

(define (log msg . args)
  (log-message (apply format
                      (append (list #f msg) args))))

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

(define (list-tasks)
  "List documentation for all maak tasks defined in the currently
loaded module."
  (let* ((mod (resolve-module '(maak)))
         (tt (filter (lambda (x)
                       (not (equal? #f x)))
                     (module-map (lambda (name var)
                                   (cond
                                     ((and (variable? var)
                                           (maak-task? (variable-ref var)))
                                      (cons name
                                            (lambda ()
                                              (let* ((function-docstring (procedure-documentation
                                                                          (variable-ref
                                                                           var))))
                                                (if (string?
                                                     function-docstring)
                                                    (log-info "  ~a: ~a" name
                                                     function-docstring)
                                                    (log-info "  ~a" name))))))
                                     (else #f))) mod))))
    (for-each (lambda (t)
                (apply (cdr t)
                       '())) tt)))

(define (maak-task? proc)
  "Predicate that indicates whether a procedure is a valid maak task.
The conditions for that include it being a valid Guile Scheme procedure,
as well as being a no-argument proc (thunk)."
  (and (procedure? proc)
       (equal? '(0 0 #f)
               (procedure-minimum-arity proc))))

(define (run-tasks tasks)
  "Runs a list of tasks sequentially."
  (for-each (lambda (task)
              (unless (get-module-var task)
                (raise-exception (make-exception-with-message (format #f
                                                               "Could not find task: ~a"
                                                               task))))
              (run-task task)) tasks))

(define (run-task task)
  "Runs a given task if it is a valid maak task (a no-argument procedure).
Logs information about the task and its documentation before execution.
  
   Args:
     task: A symbol representing the name of the task to be executed.
  
   Returns:
     The result of calling the task procedure.
  
   Errors:
     Signals an error if the task is not a valid thunk (no-argument function)
     as determined by `maak-task?`."
  (let* ((proc (get-module-var task))
         (proc-doc (procedure-documentation proc)))
    (cond
      ((maak-task? proc)
       (log-info "Start task: ~a\n" task proc)
       (log-info "  Calling procedure: ~a"
                 (procedure-name proc))
       (when (and (not (equal? #f proc-doc))
                  (not (equal? "" proc-doc)))
         (log-info "  ~a" proc-doc))
       (newline)
       (apply proc
              '()))
      (else (error (format #f
                    "Task ~a was not a valid thunk (no-argument function)"
                    task)) #f))))

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

