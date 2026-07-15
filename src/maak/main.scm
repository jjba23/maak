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

(define-module (maak main)
  #:declarative? #f
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 getopt-long)
  #:use-module (maak maak)
  #:use-module (maak dsl)
  #:export (main))

(define* (main #:key (args (command-line)))
  (let* ((option-spec '((file (single-char #\f)
                              (value #t)
                              (required? #f))
                        (resources (single-char #\r)
                                   (value #t))
                        (list (single-char #\l)
                              (value #f)
                              (required? #f))
                        (dry-run (single-char #\n)
                                 (value #f)
                                 (required? #f))
                        (quiet (single-char #\q)
                               (value #f)
                               (required? #f))
                        (tasks (single-char #\t)
                               (value #t)
                               (required? #f))))
         (options (getopt-long args option-spec))
         (task-args (option-ref options
                                '()
                                '()))
         (resources (option-ref options
                                'resources #f))
         (file (option-ref options
                           'file #f))
         (should-list (option-ref options
                                  'list #f))
         (should-dry-run (option-ref options
                                     'dry-run #f))
         (should-quiet (option-ref options
                                   'quiet #f))
         (raw-tasks (filter (lambda (t)
                              (not (equal? t "")))
                            (string-split (option-ref options
                                                      'tasks "") #\,)))
         (string-tasks (if (or (not raw-tasks)
                               (equal? 0
                                       (length raw-tasks)))
                           '("default") raw-tasks))
         (tasks (map string->symbol string-tasks)))
    (parameterize ((dry-run? should-dry-run)
                   (quiet? should-quiet))
      (unless (quiet?)
        (log-separator)
        (log-info "Maak: the infinitely extensible command runner
")
        (log-info "Loading tasks from Maak file: ~a\n" file))

      (load file)

      (if should-list
          (list-tasks #:as-is? #f)
          (begin
            (unless (quiet?)
              (log-info "Executing Maak tasks: ~a"
                        (string-join (map symbol->string tasks) " =>")))
            (run-tasks tasks
                       #:task-args task-args))))))
