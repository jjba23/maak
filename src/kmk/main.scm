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

(define-module (kmk main)
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
  #:use-module (kmk kmk)
  #:export (main))

(define* (main #:key (args (command-line)))
  (let* ((option-spec '((file (single-char #\f)
                              (value #t)
                              (required? #f))
                        (resources (single-char #\r)
                                   (value #t))
                        (tasks (single-char #\t)
                               (value #t)
                               (required? #f))))
         (options (getopt-long args option-spec))
         (resources (option-ref options
                                'resources #f))
         (file (option-ref options
                           'file #f))
         (raw-tasks (filter (lambda (t)
                              (not (equal? t "")))
                            (string-split (option-ref options
                                                      'tasks "") #\,)))
         (string-tasks (if (or (not raw-tasks)
                               (equal? 0
                                       (length raw-tasks)))
                           '("default") raw-tasks))
         (tasks (map string->symbol string-tasks)))
    (log-info "Start loading tasks from file: ~a" file)
    (use-modules (kmk kmk))
    (load file)
    (log-info "Start executing user tasks: ~a" tasks)
    (run-tasks tasks)))
