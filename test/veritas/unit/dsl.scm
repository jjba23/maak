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
;; Unit tests for `(maak dsl)': the pretty multiline string helpers, the load
;; path facilities and the maak error type.
;;
;; The suites run sequentially (`#:concurrent? #f') because the load path is
;; process-wide state: two tests adding directories at the same time would
;; observe each other's changes.

;;; Code:

(define-module (veritas unit dsl)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (spec))

(define strings-suite
  (suite "Pretty multiline strings"
         #:concurrent? #f
         #:shuffle? #f
         (test "dedent removes the common indentation"
               (assert-equal #:expect "hello"
                             #:got (dedent-string "   hello"))
               (assert-equal #:expect "a\n  b"
                             #:got (dedent-string "  a\n    b")))
         (test "dedent drops surrounding blank lines"
               (assert-equal #:expect "a\nb"
                             #:got (dedent-string "\n  a\n  b\n")))
         (test "dedent keeps interior blank lines"
               (assert-equal #:expect "a\n\nb"
                             #:got (dedent-string "  a\n\n  b")))
         (test "dedent expands tabs"
               (assert-equal #:expect "a\nb"
                             #:got (dedent-string "\ta\n\tb")))
         (test "dedent of a single word is the word"
               (assert-equal #:expect "word"
                             #:got (dedent-string "word")))
         (test "indent adds spaces to every line"
               (assert-equal #:expect "  a\n  b"
                             #:got (indent-string "a\nb" #:by 2)))
         (test "indent can leave the first line flush"
               (assert-equal #:expect "a\n  b"
                             #:got (indent-string "a\nb" #:by 2
                                                  #:first? #f)))
         (test "the text macro joins and dedents its lines"
               (assert-equal #:expect "a\nb"
                             #:got (text "a" "b"))
               (assert-equal #:expect "x\ny"
                             #:got (text "  x" "  y")))
         (test "string-lines splits on newlines"
               (assert-equal #:expect '("a" "b")
                             #:got (string-lines "a\nb")))
         (test "unlines joins with newlines"
               (assert-equal #:expect "a\nb"
                             #:got (unlines '("a" "b"))))
         (test "unwords joins with spaces"
               (assert-equal #:expect "a b"
                             #:got (unwords '("a" "b"))))
         (test "blank-line? recognises whitespace"
               (assert-true (blank-line? ""))
               (assert-true (blank-line? "  \t "))
               (assert-false (blank-line? "x")))
         (test "leading-whitespace-count counts spaces and tabs"
               (assert-equal #:expect 3
                             #:got (leading-whitespace-count "   x"))
               (assert-equal #:expect 2
                             #:got (leading-whitespace-count "\t\tx"))
               (assert-equal #:expect 0
                             #:got (leading-whitespace-count "x")))))

(define load-path-suite
  (suite "Load paths"
         #:concurrent? #f
         #:shuffle? #f
         (test "split-load-path splits on colons"
               (assert-equal #:expect '("a" "b")
                             #:got (split-load-path "a:b")))
         (test "split-load-path drops empty entries"
               (assert-equal #:expect '("a" "b")
                             #:got (split-load-path "a::b:")))
         (test "split-load-path handles the empty cases"
               (assert-equal #:expect '()
                             #:got (split-load-path ""))
               (assert-equal #:expect '()
                             #:got (split-load-path #f)))
         (test "with-load-path adds the directories"
               (assert-true
                (with-load-path '("/tmp/maak-test-load-path")
                  (lambda ()
                    (and (member "/tmp/maak-test-load-path" %load-path)
                         #t)))))
         (test "with-load-path restores the load path"
               (with-load-path '("/tmp/maak-test-restore")
                              (lambda ()
                                'ignored))
               (assert-false (member "/tmp/maak-test-restore" %load-path)))
         (test "with-load-path restores the load path after an error"
               (assert-error
                (lambda ()
                  (with-load-path '("/tmp/maak-test-error")
                    (lambda ()
                      (error "deliberate failure")))))
               (assert-false (member "/tmp/maak-test-error" %load-path)))
         (test "add-load-path! records the directory"
               (assert-equal #:expect "/tmp/maak-test-add"
                             #:got (add-load-path! "/tmp/maak-test-add"))
               (assert-true (and (member "/tmp/maak-test-add" %load-path)
                                 #t))
               (assert-true (and (member "/tmp/maak-test-add"
                                         (maak-load-path))
                                 #t))
               ;; Leave the process as the test found it.
               (set! %load-path (delete "/tmp/maak-test-add" %load-path))
               (maak-load-path (delete "/tmp/maak-test-add"
                                       (maak-load-path))))
         (test "add-load-paths! adds every directory"
               (add-load-paths! '("/tmp/maak-test-many-a"
                                  "/tmp/maak-test-many-b"))
               (assert-true (and (member "/tmp/maak-test-many-a" %load-path)
                                 #t))
               (assert-true (and (member "/tmp/maak-test-many-b" %load-path)
                                 #t))
               (set! %load-path
                     (delete "/tmp/maak-test-many-a"
                             (delete "/tmp/maak-test-many-b" %load-path)))
               (maak-load-path (delete "/tmp/maak-test-many-a"
                                       (delete "/tmp/maak-test-many-b"
                                               (maak-load-path)))))))

(define error-suite
  (suite "Maak errors"
         #:concurrent? #f
         #:shuffle? #f
         (test "make-maak-error builds a typed error"
               (assert-true (maak-error? (make-maak-error "message"
                                                          "hint")))
               (assert-equal #:expect "message"
                             #:got (maak-error-message
                                    (make-maak-error "message" "hint")))
               (assert-equal #:expect "hint"
                             #:got (maak-error-hint
                                    (make-maak-error "message" "hint")))
               (assert-equal #:expect #f
                             #:got (maak-error-hint
                                    (make-maak-error "message" #f))))
         (test "raise-maak-error raises a catchable maak error"
               (assert-equal #:expect "boom"
                             #:got (catch #t
                                     (lambda ()
                                       (raise-maak-error "boom"))
                                     (lambda (key . vals)
                                       (maak-error-message
                                        (find-maak-error key vals))))))
         (test "raise-maak-error carries its hint"
               (assert-equal #:expect "try this"
                             #:got (catch #t
                                     (lambda ()
                                       (raise-maak-error
                                        "boom"
                                        #:hint "try this"))
                                     (lambda (key . vals)
                                       (maak-error-hint
                                        (find-maak-error key vals))))))
         (test "find-maak-error returns #f for a foreign error"
               (assert-equal #:expect #f
                             #:got (catch #t
                                     (lambda ()
                                       (error "plain guile error"))
                                     (lambda (key . vals)
                                       (find-maak-error key vals)))))
         (test "raise-maak-error is catchable with assert-error"
               (assert-error (lambda ()
                               (raise-maak-error "nope"))))))

(define (process-scratch-directory)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (string-append (or (getenv "TMPDIR") "/tmp")
                            "/maak-test-processes-"
                            (number->string (getpid)))))
    (parameterize ((quiet? #t))
      (when (file-exists? dir)
        (delete-file-recursively dir))
      (mkdir-p dir))
    dir))

(define processes-suite
  (suite "Programs, arguments and jobs"
         #:concurrent? #f
         #:shuffle? #f
         (test "run returns #t when the program exits with 0"
               (assert-true (parameterize ((quiet? #t))
                              (run (list "true")))))
         (test "run keeps a space inside one argument"
               (assert-true
                (parameterize ((quiet? #t))
                  ;; sh counts the arguments after the script: the path is one
                  ;; word only if the runner never handed it to a shell.
                  (run (list "sh" "-c" "test \"$#\" -eq 1" "sh" "a b")))))
         (test "run returns #f when a failure is allowed"
               (assert-false
                (parameterize ((quiet? #t))
                  (run (list "sh" "-c" "exit 3") #:allow-failure? #t))))
         (test "run raises when a command fails"
               (assert-error
                (lambda ()
                  (parameterize ((quiet? #t))
                    (run (list "sh" "-c" "exit 3"))))))
         (test "run refuses an empty argument list"
               (assert-error (lambda () (run '()))))
         (test "run refuses an argument that is not a string"
               (assert-error (lambda () (run (list "true" 3)))))
         (test "a dry run does not start the program"
               (let* ((dir (process-scratch-directory))
                      (path (string-append dir "/not-created"))
                      (created? (parameterize ((dry-run? #t) (quiet? #t))
                                  (run (list "mkdir" path))
                                  (file-exists? path))))
                 (parameterize ((quiet? #t))
                   (delete-file-recursively dir))
                 (assert-false created?)))
         (test "an async run returns a job with a pid"
               (let ((job (parameterize ((quiet? #t))
                            (run (list "sh" "-c" "exit 0")
                                 #:async? #t))))
                 (wait-job job)
                 (assert-true (and (job? job) (integer? (job-pid job))))))
         (test "wait-job is #t for a successful job"
               (assert-true
                (wait-job (parameterize ((quiet? #t))
                            (run (list "sh" "-c" "exit 0")
                                 #:async? #t)))))
         (test "wait-job is #f for a failing job"
               (assert-false
                (wait-job (parameterize ((quiet? #t))
                            (run (list "sh" "-c" "exit 4")
                                 #:async? #t)))))
         (test "job-exit-code reports the exit code"
               (assert-equal #:expect 4
                             #:got (job-exit-code
                                    (parameterize ((quiet? #t))
                                      (run (list "sh" "-c" "exit 4")
                                           #:async? #t)))))
         (test "a job can be waited on twice"
               (let* ((job (parameterize ((quiet? #t))
                             (run (list "sh" "-c" "exit 0")
                                  #:async? #t)))
                      (first (wait-job job)))
                 (assert-equal #:expect first #:got (wait-job job))))
         (test "job-done? does not wait for a running job"
               (let* ((job (parameterize ((quiet? #t))
                             (run (list "sh" "-c" "sleep 0.2")
                                  #:async? #t)))
                      (before (job-done? job)))
                 (wait-job job)
                 (assert-false before)))
         (test "job-done? is true once the job has finished"
               (let* ((job (parameterize ((quiet? #t))
                             (run (list "sh" "-c" "exit 0")
                                  #:async? #t)))
                      (done? (begin
                               (usleep 200000)
                               (job-done? job))))
                 (assert-true done?)))
         (test "a background shell command is a job too"
               (let* ((job (parameterize ((quiet? #t))
                             ($ (list "sh -c 'exit 0'") #:async? #t)))
                      (result (wait-job job)))
                 (assert-true (and (job? job) result))))
         (test "job-command keeps the command as written"
               (assert-equal #:expect "sh -c 'exit 0'"
                             #:got (job-command
                                    (parameterize ((quiet? #t))
                                      ($ (list "sh -c 'exit 0'")
                                         #:async? #t)))))))

(define (with-standard-output-file path thunk)
  "Call THUNK with the process's standard output connected to PATH.

A process that copies a stage's output writes to the file descriptor it was
forked with rather than to a Scheme port, so redirecting the descriptor is the
only way for a test to see what such a process printed."
  (let ((saved (dup 1))
        (file (open-file path "w")))
    (force-output (current-output-port))
    (dynamic-wind
      (lambda () (dup2 (port->fdes file) 1))
      thunk
      (lambda ()
        (force-output (current-output-port))
        (dup2 saved 1)
        (close-fdes saved)
        (close-port file)))))

(define pipeline-suite
  (suite "Pipelines and shell expansion"
         #:concurrent? #f
         #:shuffle? #f
         (test "a pipeline feeds each stage into the next"
               (let* ((dir (process-scratch-directory))
                      (path (string-append dir "/out.txt")))
                 (parameterize ((quiet? #t))
                   (run-pipeline (list (list "printf" "hello\n")
                                       (list "sh" "-c"
                                             (string-append
                                              "cat > "
                                              (shell-quote path))))))
                 (let ((text (call-with-input-file path get-string-all)))
                   (parameterize ((quiet? #t))
                     (delete-file-recursively dir))
                   (assert-equal #:expect "hello\n" #:got text))))
         (test "a failing stage fails the whole pipeline"
               (assert-false
                (parameterize ((quiet? #t))
                  (run-pipeline (list (list "false") (list "true"))
                                #:allow-failure? #t))))
         (test "a pipeline raises when a stage fails"
               (assert-error
                (lambda ()
                  (parameterize ((quiet? #t))
                    (run-pipeline (list (list "false") (list "true")))))) )
         (test "a pipeline with no stages is refused"
               (assert-error (lambda () (run-pipeline '()))))
         (test "a pipeline with an empty stage is refused"
               (assert-error (lambda () (run-pipeline (list '())))))
         (test "a dry run does not start the pipeline"
               (let* ((dir (process-scratch-directory))
                      (path (string-append dir "/not-created"))
                      (created? (parameterize ((dry-run? #t) (quiet? #t))
                                  (run-pipeline (list (list "touch" path)))
                                  (file-exists? path))))
                 (parameterize ((quiet? #t))
                   (delete-file-recursively dir))
                 (assert-false created?)))
         (test "a stage whose program is missing fails the pipeline"
               (assert-false
                (parameterize ((quiet? #t))
                  (run-pipeline (list (list "definitely-not-a-program-maak"))
                                #:allow-failure? #t))))
         (test "a stage takes environment variables of its own"
               (let* ((dir (process-scratch-directory))
                      (path (string-append dir "/stage-env.txt")))
                 (parameterize ((quiet? #t))
                   (run-pipeline (list (stage (list "sh" "-c"
                                                   "printf %s \"$MAAK_STAGE_VALUE\"")
                                             #:env '("MAAK_STAGE_VALUE=stage"))
                                       (list "sh" "-c"
                                             (string-append
                                              "cat > "
                                              (shell-quote path))))))
                 (let ((text (call-with-input-file path get-string-all)))
                   (parameterize ((quiet? #t))
                     (delete-file-recursively dir))
                   (assert-equal #:expect "stage" #:got text))))
         (test "a pipeline gives its environment to every stage"
               (let* ((dir (process-scratch-directory))
                      (path (string-append dir "/pipeline-env.txt")))
                 (parameterize ((quiet? #t))
                   (run-pipeline (list (stage (list "sh" "-c"
                                                   "printf %s \"$MAAK_PIPE_VALUE\""))
                                       (list "sh" "-c"
                                             (string-append
                                              "cat > "
                                              (shell-quote path))))
                                 #:env '("MAAK_PIPE_VALUE=shared")))
                 (let ((text (call-with-input-file path get-string-all)))
                   (parameterize ((quiet? #t))
                     (delete-file-recursively dir))
                   (assert-equal #:expect "shared" #:got text))))
         (test "a stage's environment wins over the pipeline's"
               (let* ((dir (process-scratch-directory))
                      (path (string-append dir "/override.txt")))
                 (parameterize ((quiet? #t))
                   (run-pipeline (list (stage (list "sh" "-c"
                                                   "printf %s \"$MAAK_VALUE\"")
                                             #:env '("MAAK_VALUE=stage"))
                                       (list "sh" "-c"
                                             (string-append
                                              "cat > "
                                              (shell-quote path))))
                                 #:env '("MAAK_VALUE=pipeline")))
                 (let ((text (call-with-input-file path get-string-all)))
                   (parameterize ((quiet? #t))
                     (delete-file-recursively dir))
                   (assert-equal #:expect "stage" #:got text))))
         (test "a dry run shows a stage's environment and sudo"
               (let ((printed (parameterize ((dry-run? #t) (quiet? #f)
                                            (elevation-command "sudo"))
                                (with-output-to-string
                                  (lambda ()
                                    (run-pipeline
                                     (list (stage (list "true")
                                                  #:env '("A=1")
                                                  #:sudo? #t))))))))
                 ;; `string-contains' answers with the position it found, so
                 ;; the assertion asks a question with one answer.
                 (assert-true (and (string-contains printed
                                                   "sudo env A=1 true")
                                   #t))))
         (test "a captured pipeline returns what every stage wrote"
               (let ((outputs (pipeline-outputs
                               (parameterize ((quiet? #t))
                                 (run-pipeline
                                  (list (list "printf" "one\ntwo\n")
                                        (list "tr" "a-z" "A-Z"))
                                  #:capture? #t)))))
                 (assert-equal #:expect '("one\ntwo\n" "ONE\nTWO\n")
                               #:got outputs)))
         (test "a capture of more than a pipeful loses nothing"
               (let ((outputs (pipeline-outputs
                               (parameterize ((quiet? #t))
                                 (run-pipeline
                                  (list (list "seq" "20000")
                                        (list "wc" "-l"))
                                  #:capture? #t)))))
                 (assert-equal #:expect "20000\n" #:got (second outputs))))
         (test "a captured pipeline prints nothing itself"
               (let* ((dir (process-scratch-directory))
                      (printed (string-append dir "/printed.txt")))
                 (parameterize ((quiet? #t))
                   (with-standard-output-file
                    printed
                    (lambda ()
                      (run-pipeline (list (list "printf" "hello\n"))
                                    #:capture? #t))))
                 (let ((text (call-with-input-file printed get-string-all)))
                   (parameterize ((quiet? #t))
                     (delete-file-recursively dir))
                   (assert-equal #:expect "" #:got text))))
         (test "a capture leaves no temporary file behind"
               (let* ((path (string-append (or (getenv "TMPDIR") "/tmp")
                                           "/maak-pipeline-"
                                           (number->string (getpid))
                                           "-0.log"))
                      (left? (begin
                               (parameterize ((quiet? #t))
                                 (run-pipeline (list (list "printf" "x\n"))
                                               #:capture? #t))
                               (file-exists? path))))
                 (assert-false left?)))
         (test "a captured pipeline reports the stages that failed"
               (let ((result (parameterize ((quiet? #t))
                               (run-pipeline (list (list "printf" "x\n")
                                                   (list "false"))
                                             #:capture? #t
                                             #:allow-failure? #t))))
                 (assert-false (pipeline-ok? result))))
         (test "a stage's tee file keeps what the stage wrote"
               (let* ((dir (process-scratch-directory))
                      (path (string-append dir "/teed.txt")))
                 (parameterize ((quiet? #t))
                   (run-pipeline (list (stage (list "printf" "one\ntwo\n")
                                              #:tee path)
                                       (list "tr" "a-z" "A-Z"))))
                 (let ((text (call-with-input-file path get-string-all)))
                   (parameterize ((quiet? #t))
                     (delete-file-recursively dir))
                   (assert-equal #:expect "one\ntwo\n" #:got text))))
         (test "the stage after a tee reads the same bytes"
               (let* ((dir (process-scratch-directory))
                      (path (string-append dir "/teed.txt"))
                      (outputs (pipeline-outputs
                                (parameterize ((quiet? #t))
                                  (run-pipeline
                                   (list (stage (list "printf" "one\n")
                                                #:tee path)
                                         (list "cat"))
                                   #:capture? #t)))))
                 (parameterize ((quiet? #t))
                   (delete-file-recursively dir))
                 (assert-equal #:expect "one\n" #:got (second outputs))))
         (test "a tee on the last stage prints what it keeps"
               (let* ((dir (process-scratch-directory))
                      (printed (string-append dir "/printed.txt"))
                      (kept (string-append dir "/kept.txt")))
                 (parameterize ((quiet? #t))
                   (with-standard-output-file
                    printed
                    (lambda ()
                      (run-pipeline
                       (list (stage (list "printf" "hello\n")
                                    #:tee kept))))))
                 (let ((text (call-with-input-file printed get-string-all)))
                   (parameterize ((quiet? #t))
                     (delete-file-recursively dir))
                   (assert-equal #:expect "hello\n" #:got text))))
         (test "a tee that cannot be written is refused before the pipeline"
               (let* ((dir (process-scratch-directory))
                      (path (string-append dir "/missing/log.txt")))
                 (assert-error
                  (lambda ()
                    (parameterize ((quiet? #t))
                      (run-pipeline (list (stage (list "printf" "x\n")
                                                 #:tee path))))))))
         (test "a stage refuses a tee that is not a file name"
               (assert-error (lambda () (stage (list "true") #:tee 42))))
         (test "a background pipeline cannot capture its output"
               (assert-error
                (lambda ()
                  (parameterize ((quiet? #t))
                    (run-pipeline (list (list "true"))
                                  #:capture? #t #:async? #t)))))
         (test "a dry run shows the tee a stage writes to"
               (let ((printed (parameterize ((dry-run? #t) (quiet? #f))
                                (with-output-to-string
                                  (lambda ()
                                    (run-pipeline
                                     (list (stage (list "printf" "x\n")
                                                  #:tee "/tmp/teed.txt"))))))))
                 (assert-true (and (string-contains printed
                                                   "| tee /tmp/teed.txt")
                                   #t))))
         (test "a dry run of a captured pipeline returns an empty result"
               (assert-equal #:expect '("")
                             #:got (pipeline-outputs
                                    (parameterize ((dry-run? #t))
                                      (run-pipeline (list (list "printf" "x\n"))
                                                    #:capture? #t)))))
         (test "a stage refuses an empty argument list"
               (assert-error (lambda () (stage '()))))
         (test "a pipeline refuses a stage that is not an argument list"
               (assert-error (lambda () (run-pipeline (list 42)))))
         (test "an async pipeline returns a job"
               (let ((job (parameterize ((quiet? #t))
                            (run-pipeline (list (list "true"))
                                          #:async? #t))))
                 (assert-true (and (job? job)
                                   (integer? (job-pid job))
                                   (wait-job job)))))
         (test "a failing async pipeline fails its job"
               (assert-false
                (wait-job (parameterize ((quiet? #t))
                            (run-pipeline (list (list "false") (list "true"))
                                          #:async? #t)))))
         (test "run expands a wildcard when asked to"
               (let* ((dir (process-scratch-directory))
                      (file (string-append dir "/a b.txt"))
                      (keep (string-append dir "/keep.log"))
                      (removed? (begin
                                  (parameterize ((quiet? #t))
                                    (run (list "touch" file))
                                    (run (list "touch" keep))
                                    ;; One wildcard covers the name with a
                                    ;; space in it: only the shell can match
                                    ;; it, which is what the opt-in asks for.
                                    (run (list "rm" "-f"
                                               (string-append dir "/*.txt"))
                                         #:expand? #t))
                                  (and (not (file-exists? file))
                                       (file-exists? keep)))))
                 (parameterize ((quiet? #t))
                   (delete-file-recursively dir))
                 (assert-true removed?)))
         (test "run takes a wildcard literally without the opt-in"
               (let* ((dir (process-scratch-directory))
                      (file (string-append dir "/*.txt"))
                      (created? (begin
                                  (parameterize ((quiet? #t))
                                    (run (list "touch" file)))
                                  (file-exists? file))))
                 (parameterize ((quiet? #t))
                   (delete-file-recursively dir))
                 (assert-true created?)))))

(define file-helpers-suite
  (suite "File helpers"
         #:concurrent? #f
         #:shuffle? #f
         (test "rm-rf removes a directory and its contents"
               (let* ((dir (process-scratch-directory))
                      (sub (string-append dir "/sub"))
                      (file (string-append sub "/a.txt"))
                      (removed? (begin
                                  (parameterize ((quiet? #t))
                                    (mkdir-p sub)
                                    (touch file)
                                    (rm-rf dir))
                                  (not (file-exists? dir)))))
                 (assert-true removed?)))
         (test "rm-rf is a dry run under dry-run?"
               (let* ((dir (process-scratch-directory))
                      (file (string-append dir "/keep.txt"))
                      (kept? (begin
                               (parameterize ((quiet? #t))
                                 (touch file))
                               (parameterize ((dry-run? #t) (quiet? #t))
                                 (rm-rf dir))
                               (file-exists? file))))
                 (parameterize ((quiet? #t))
                   (delete-file-recursively dir))
                 (assert-true kept?)))
         (test "rm-rf expands a wildcard when asked to"
               (let* ((dir (process-scratch-directory))
                      (victim (string-append dir "/a.txt"))
                      (keep (string-append dir "/keep.log"))
                      (removed? (begin
                                  (parameterize ((quiet? #t))
                                    (touch victim)
                                    (touch keep)
                                    (rm-rf (string-append dir "/*.txt")
                                           #:expand? #t))
                                  (and (not (file-exists? victim))
                                       (file-exists? keep)))))
                 (parameterize ((quiet? #t))
                   (delete-file-recursively dir))
                 (assert-true removed?)))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               strings-suite
               load-path-suite
               error-suite
               processes-suite
               pipeline-suite
               file-helpers-suite))
