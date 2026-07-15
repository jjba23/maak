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
  #:use-module (maak dsl)
  #:re-export (log-message log-info
                           log-error
                           log-separator
                           bold-output
                           get-module-var
                           delete-file-recursively
                           manifest-shell
                           time-machine
                           time-machine-manifest-shell
                           program-shell
                           time-machine-program-shell
                           mkdir-p
                           cp
                           mv
                           quiet?
                           dry-run?
                           remkdir-p
                           maak-proc?
                           maak-task?
                           $
                           ~
                           syscall
                           cat)
  #:export (resolve-maak-tasks list-tasks run-tasks run-task))

(define* (resolve-maak-tasks #:key (module-name '(maak)))
  (let* ((mod (resolve-module module-name))
         (public-mod (module-public-interface mod))
         ;; Check if a public interface exists AND contains at least one export.
         (has-exports? (and public-mod
                            (not (null? (module-map (lambda (name var)
                                                      name) public-mod)))))
         ;; If it has exports, search only those. Otherwise, fall back to the whole module.
         (target-mod (if has-exports? public-mod mod))
         (tasks (filter (lambda (x)
                          (not (equal? #f x)))
                        (module-map (lambda (name var)
                                      (cond
                                        ((maak-task? var)
                                         (cons name var))
                                        (else #f))) target-mod))))
    tasks))

(define* (list-tasks #:key (as-is? #f))
  "List documentation for all maak tasks defined in the currently
loaded module. Sorts alphabetically by default unless AS-IS? is #t."
  (let* ((raw-tasks (resolve-maak-tasks))
         (tasks (if as-is? raw-tasks
                    (sort raw-tasks
                          (lambda (a b)
                            (string<? (symbol->string (car a))
                                      (symbol->string (car b))))))))
    (log-separator)
    (for-each (lambda (task)
                (let* ((function-docstring (procedure-documentation (variable-ref
                                                                     (cdr task)))))
                  (newline)
                  (if (string? function-docstring)
                      (log-message (format #f "~a: ~a"
                                           (bold-output (car task))
                                           function-docstring))
                      (log-message (format #f "~a"
                                           (bold-output (car task))))))) tasks)
    (newline)
    (newline)))

(define* (run-tasks tasks
                    #:key (task-args '()))
  "Runs a list of tasks sequentially."
  (for-each (lambda (task)
              (unless (get-module-var task)
                (cond
                  ((equal? 'default task)
                   (raise-exception (make-exception-with-message (format #f
                                                                         (string-join '
                                                                          ("Could not find default task."
                                                                           "Have you defined a function named `default' in your Maak file?"))
                                                                         task))))
                  (else (raise-exception (make-exception-with-message (format
                                                                              #f
                                                                              "Could not find task: ~a"
                                                                              task))))))
              (run-task task
                        #:task-args task-args)) tasks))

(define* (run-task task
                   #:key (task-args '()))
  "Runs a given task if it is a valid maak task.
Logs information about the task and its documentation before execution.

   Args:
     task: A symbol representing the name of the task to be executed.
     task-args: Additional arguments to be passed to the task.

   Returns:
     The result of calling the task procedure.

   Errors:
     Signals an error if the task is not a valid function
     as determined by `maak-task?`."
  (let* ((proc (get-module-var task))
         (proc-doc (procedure-documentation proc))
         (valid-tasks (resolve-maak-tasks)))
    (cond
      ;; Ensure the procedure is valid AND exists in the exported tasks list
      ((and (maak-proc? proc)
            (assoc task valid-tasks))
       (log-separator)
       (log-info "=> Running Maak task: ~a\n" task proc)
       (when (and (not (equal? #f proc-doc))
                  (not (equal? "" proc-doc)))
         (log-info "  ~a" proc-doc))
       (newline)
       (apply proc task-args))
      (else (error (format #f
                    "Task ~a was not a valid maak task. Please double-check for typos and on the function visibility!"
                    task)) #f))))
