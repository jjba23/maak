;; ggg: guile glyph generator

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; ggg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ggg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with ggg.  If not, see <https://www.gnu.org/licenses/>.

(define-module (ggg prelude)
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
                        serialize-style
                        assoc-ref-or
                        get-setting
                        syscall))

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

(define (serialize-style alist)
  (string-join (map (lambda (p)
                      (format #f "~a: ~a"
                              (car p)
                              (cdr p))) alist) ";"))

(define (assoc-ref-or alist key default)
  (let ((x (assoc-ref alist key)))
    (if (not x) default x)))

(define (syscall cmd)
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process) process-output))

(define-syntax-rule (get-setting setting)
  "Get a setting for ggg, from glyph file."
  (let* ((val (module-variable (resolve-module '(ggg config)) setting)))
    (catch #t
           (lambda ()
             (variable-ref val))
           (lambda (key . args)
             #f))))
