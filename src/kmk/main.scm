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
  #:use-module (kmk prelude)
  #:export (main))

(define* (main #:key (args (command-line)))
  (log-info (format #f "args: ~a" args))
  (let* ((option-spec '((file (single-char #\f)
                              (value #t)
                              (required? #f))
                        (tasks (single-char #\t)
                               (value #t))))
         (options (getopt-long args option-spec))
         (file (option-ref options
                           'file #f))
         (tasks (option-ref options
                            'tasks #f)))
    (log-info "HELLO WORLD FROM KMK")
    (log-info (format #f "file: ~a" file))
    (log-info (format #f "tasks: ~a" tasks))))

