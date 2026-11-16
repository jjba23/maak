;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak gradle)': the pure argv builder, the runner, the output
;; query and the wrapper check.  No Gradle is needed.

;;; Code:

(define-module (veritas unit gradle)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak gradle)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-gradle-~a-~a" (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (with-scratch label thunk)
  (let ((dir (scratch-directory label)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk dir))
      (lambda ()
        (when (file-exists? dir)
          (parameterize ((quiet? #t))
            (delete-file-recursively dir)))))))

(define (fake-gradle directory)
  (let ((path (in-vicinity directory "gradle")))
    (write-text path
                (string-append
                 "#!/bin/sh\n"
                 "printf '%s\\n' \"gradle-output-ok\"\n"))
    (chmod path #o755)
    path))

(define argv-suite
  (suite "The Gradle command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "a task is the program and its name"
               (parameterize ((gradle-command "gradle"))
                 (assert-equal #:expect '("gradle" "build")
                               #:got (gradle-argv "build"))))
         (test "the project, properties and excludes reach the command"
               (parameterize ((gradle-command "gradle"))
                 (assert-equal
                  #:expect '("gradle" "--project-dir" "service"
                             "-Pversion=1.2.3" "-Pflag"
                             "-x" "test"
                             "--no-daemon" "--parallel" "--build-cache"
                             "--warning-mode" "all" "--console" "plain"
                             "--max-workers" "4"
                             "clean" "build")
                  #:got (gradle-argv '("clean" "build")
                                     #:project-dir "service"
                                     #:properties '(("version" . "1.2.3")
                                                    "flag")
                                     #:exclude-tasks '("test")
                                     #:no-daemon? #t
                                     #:parallel? #t
                                     #:build-cache? #t
                                     #:warning-mode "all"
                                     #:console "plain"
                                     #:max-workers 4))))
         (test "the offline, refresh and rerun flags are words"
               (parameterize ((gradle-command "gradle"))
                 (assert-equal
                  #:expect '("gradle" "--offline" "--refresh-dependencies"
                             "--rerun-tasks" "--continue" "--stacktrace"
                             "test")
                  #:got (gradle-argv "test"
                                     #:offline? #t
                                     #:refresh-dependencies? #t
                                     #:rerun-tasks? #t
                                     #:continue? #t
                                     #:stacktrace? #t))))
         (test "arguments are appended as they are"
               (parameterize ((gradle-command "gradle"))
                 (assert-equal
                  #:expect '("gradle" "test" "--tests" "Foo")
                  #:got (gradle-argv "test"
                                     #:arguments '("--tests" "Foo")))))
         (test "an empty task list is refused"
               (assert-error (lambda () (gradle-argv '()))))
         (test "a warning mode Gradle does not know is refused"
               (assert-error (lambda ()
                               (gradle-argv "build" #:warning-mode "loud"))))
         (test "a worker count that is not positive is refused"
               (assert-error (lambda ()
                               (gradle-argv "build" #:max-workers 0))))))

(define running-suite
  (suite "Running Gradle"
         #:concurrent? #f
         #:shuffle? #f
         (test "a wrapper passes its arguments to the task"
               (parameterize ((gradle-command "gradle"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (gradle-test "--tests" "Foo"))
                   (assert-equal #:expect (list #t)
                                 #:got (list (and (string-contains
                                                   (get-output-string out)
                                                   "gradle test --tests Foo")
                                                  #t))))))
         (test "the output query returns what Gradle wrote"
               (with-scratch "output"
                (lambda (dir)
                  (assert-equal
                   #:expect "gradle-output-ok\n"
                   #:got (gradle-output (list (fake-gradle dir)))))))))

(define wrapper-suite
  (suite "The Gradle wrapper"
         #:concurrent? #f
         #:shuffle? #f
         (test "a directory with gradlew answers with it"
               (with-scratch "wrapper"
                (lambda (dir)
                  (write-text (in-vicinity dir "gradlew") "#!/bin/sh\n")
                  (chmod (in-vicinity dir "gradlew") #o755)
                  (assert-equal #:expect (list #t (in-vicinity dir "gradlew"))
                                #:got (list (gradle-wrapper? dir)
                                            (gradle-wrapper-command dir))))))
         (test "a directory without gradlew answers with the program"
               (with-scratch "no-wrapper"
                (lambda (dir)
                  (parameterize ((gradle-command "gradle"))
                    (assert-equal #:expect (list #f "gradle")
                                  #:got (list (gradle-wrapper? dir)
                                              (gradle-wrapper-command dir)))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               running-suite
               wrapper-suite))
