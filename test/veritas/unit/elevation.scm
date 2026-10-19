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

;;; Commentary:
;;
;; Unit tests for the privilege escalation parameter.  `#:sudo?` is kept as
;; the name of the request, but the program that answers it is chosen once:
;; `doas` when it is on PATH, `sudo` otherwise, or whatever `MAAK_ELEVATE`
;; names.  The tests check the choice against fake PATH directories and check
;; that `$`, `run` and a pipeline stage all put the chosen program first.

;;; Code:

(define-module (veritas unit elevation)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (spec))

(define escape-character
  (integer->char 27))

(define (strip-ansi str)
  "Return STR without ANSI escape sequences."
  (let loop ((chars (string->list str))
             (out '())
             (escaping? #f))
    (cond
      ((null? chars) (list->string (reverse out)))
      ((and escaping? (char=? (car chars) #\m))
       (loop (cdr chars) out #f))
      (escaping? (loop (cdr chars) out #t))
      ((char=? (car chars) escape-character)
       (loop (cdr chars) out #t))
      (else (loop (cdr chars) (cons (car chars) out) #f)))))

(define (dry-run-command thunk)
  "Return the command THUNK would run, as printed by dry-run mode."
  (parameterize ((dry-run? #t) (quiet? #f))
    (strip-ansi (with-output-to-string thunk))))

(define (dry-run-line thunk)
  "Return just the command line of a dry run, without the prefix."
  (let* ((text (string-trim-both (dry-run-command thunk)))
         (prefix "[DRY-RUN]: "))
    (string-trim-both (string-drop text (string-length prefix)))))

(define (scratch-directory label)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (format #f "~a/maak-test-elevation-~a-~a"
                     (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (remove-directory dir)
  "Remove DIR quietly, whatever the tests left in it."
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

(define* (fake-program dir name #:key (executable? #t))
  "Write a file called NAME in DIR, executable unless EXECUTABLE? is #f."
  (let ((path (string-append dir "/" name)))
    (write-file path "#!/bin/sh\nexit 0\n")
    (when executable?
      (parameterize ((quiet? #t))
        (chmod-file "755" path)))
    dir))

(define detection-suite
  (suite "Choosing the escalation command"
         #:concurrent? #f
         #:shuffle? #f
         (test "sudo is the fallback"
               (let ((dir (scratch-directory "fallback")))
                 (let ((command (detect-elevation-command
                                 #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "sudo" #:got command))))
         (test "doas is chosen when it is installed"
               (let ((dir (scratch-directory "doas")))
                 (fake-program dir "doas")
                 (let ((command (detect-elevation-command
                                 #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "doas" #:got command))))
         (test "doas wins over sudo when both are installed"
               (let ((dir (scratch-directory "both")))
                 (fake-program dir "doas")
                 (fake-program dir "sudo")
                 (let ((command (detect-elevation-command
                                 #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "doas" #:got command))))
         (test "an explicit preference wins"
               (let ((dir (scratch-directory "preferred")))
                 (fake-program dir "doas")
                 (let ((command (detect-elevation-command
                                 #:path dir #:preferred "my-escalator")))
                   (remove-directory dir)
                   (assert-equal #:expect "my-escalator" #:got command))))
         (test "a file that is not executable is not a command"
               (let ((dir (scratch-directory "not-executable")))
                 (fake-program dir "doas" #:executable? #f)
                 (let ((command (detect-elevation-command
                                 #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "sudo" #:got command))))
         (test "the default is a non-empty string"
               (assert-true (and (string? (elevation-command))
                                 (not (string=? (elevation-command) "")))))))

(define usage-suite
  (suite "Using the escalation command"
         #:concurrent? #f
         #:shuffle? #f
         (test "dollar puts the chosen command first"
               (assert-equal
                #:expect "doas echo hi"
                #:got (parameterize ((elevation-command "doas"))
                        (dry-run-line
                         (lambda () ($ '("echo hi") #:sudo? #t))))))
         (test "dollar keeps the decoration order"
               (assert-equal
                #:expect "doas env A=1 echo hi >/dev/null 2>&1"
                #:got (parameterize ((elevation-command "doas"))
                        (dry-run-line
                         (lambda () ($ '("echo hi")
                                        #:sudo? #t
                                        #:env '("A=1")
                                        #:dev-null? #t))))))
         (test "run puts the chosen command in its argv"
               (assert-equal
                #:expect "doas env A=1 echo hi"
                #:got (parameterize ((elevation-command "doas"))
                        (dry-run-line
                         (lambda () (run (list "echo" "hi")
                                         #:sudo? #t
                                         #:env '("A=1")))))))
         (test "a pipeline stage uses the chosen command"
               (assert-equal
                #:expect "doas env A=1 true"
                #:got (parameterize ((elevation-command "doas"))
                        (dry-run-line
                         (lambda ()
                           (run-pipeline
                            (list (stage (list "true")
                                         #:sudo? #t
                                         #:env '("A=1")))))))))
         (test "the escalation command may carry options"
               (assert-equal
                #:expect "doas -n echo hi"
                #:got (parameterize ((elevation-command "doas -n"))
                        (dry-run-line
                         (lambda () ($ '("echo hi") #:sudo? #t))))))
         (test "options may be separated by any whitespace"
               (assert-equal
                #:expect "doas -n echo hi"
                #:got (parameterize ((elevation-command "doas\t-n  "))
                        (dry-run-line
                         (lambda () ($ '("echo hi") #:sudo? #t))))))
         (test "an empty escalation command turns the request off"
               (assert-equal
                #:expect "echo hi"
                #:got (parameterize ((elevation-command ""))
                        (dry-run-line
                         (lambda () ($ '("echo hi") #:sudo? #t))))))
         (test "an empty escalation command runs for real too"
               (assert-equal
                #:expect #t
                #:got (parameterize ((elevation-command "")
                                    (quiet? #t))
                        (run (list "true") #:sudo? #t))))
         (test "decorate-command is the single place the choice is applied"
               (assert-equal
                #:expect "doas env A=1 true"
                #:got (parameterize ((elevation-command "doas"))
                        (decorate-command "true"
                                          #:sudo? #t
                                          #:env '("A=1")))))
         (test "without the request nothing is added"
               (assert-equal
                #:expect "true"
                #:got (parameterize ((elevation-command "doas"))
                        (decorate-command "true"))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               detection-suite
               usage-suite))
