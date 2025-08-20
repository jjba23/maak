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
;; along with kmk.  If not, see <https://www.gnu.org/licenses/>.

(define-module (kmk kmk)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
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
                        assoc-ref-or
                        get-module-var
                        $
                        syscall
                        list-tasks
                        run-tasks
                        run-task))

(define preferred-strftime-format
  (make-parameter "%Y-%m-%d %H:%M:%S%z"))

(define (log-message msg)
  (display (format #f "\n\x1b[1m~a\x1b[0m" msg)))

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

(define (assoc-ref-or alist key default)
  (let ((x (assoc-ref alist key)))
    (if (not x) default x)))

(define* (syscall cmd)
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
            #:key (verbose #f)
            (join " "))
  (let* ((cmd (string-join cmd-parts join)))
    (when verbose
      (log-info "  Execute command:\n~a\n" cmd))
    (syscall cmd)))

(define-syntax-rule (get-module-var setting)
  "Get a setting for kmk, from kmk file."
  (let* ((val (module-variable (resolve-module '(kmk)) setting)))
    (catch #t
           (lambda ()
             (variable-ref val))
           (lambda (key . args)
             #f))))

(define (list-tasks)
  (let* ((mod (resolve-module '(kmk)))
         (tt (filter procedure?
                     (module-map (lambda (name var)
                                   (cond
                                     ((variable? var)
                                      (cond
                                        ((kmk-task? (variable-ref var))
                                         (lambda ()
                                           (log-info "  ~a: ~a" name
                                                     (procedure-documentation (variable-ref
                                                                               var)))))
                                        (else #f)))
                                     (else #f))) mod))))
    (for-each (lambda (t)
                (apply t
                       '())) tt)))


(define (kmk-task? proc)
  (and (procedure? proc)
       (equal? '(0 0 #f)
               (procedure-minimum-arity proc))))

(define (run-tasks tasks)
  (for-each run-task tasks))

(define (run-task task)
  (let* ((proc (get-module-var task)))
    (cond
      ((kmk-task? proc)
       (log-info "Start task: ~a\n" task proc)
       (log-info "  Calling procedure: ~a"
                 (procedure-name proc))
       (log-info "  ~a"
                 (procedure-documentation proc))
       (apply proc
              '()))
      (else (error (format #f
                    "Task ~a was not a valid thunk (no-argument function)"
                    task)) #f))))
