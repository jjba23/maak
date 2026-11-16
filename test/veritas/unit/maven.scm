;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak maven)': the pure argv builder, the runner, the output
;; query and the wrapper check.  No Maven is needed: the builder is asserted as
;; a list of words and the stand-in programs are scripts this spec writes.

;;; Code:

(define-module (veritas unit maven)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak maven)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-maven-~a-~a" (tmp-dir) label (getpid))))
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

(define (error-of thunk)
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . values)
      (and (maak-error? (car values))
           (maak-error-message (car values))))))

(define (fake-maven directory)
  "Write a Maven stand-in in DIRECTORY and return its path."
  (let ((path (in-vicinity directory "mvn")))
    (write-text path
                (string-append
                 "#!/bin/sh\n"
                 "printf '%s\\n' \"maven-output-ok\"\n"))
    (chmod path #o755)
    path))

(define argv-suite
  (suite "The Maven command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "a goal is the program and its name"
               (parameterize ((maven-command "mvn"))
                 (assert-equal #:expect '("mvn" "package")
                               #:got (maven-argv "package"))))
         (test "profiles and properties are words before the goals"
               (parameterize ((maven-command "mvn"))
                 (assert-equal
                  #:expect '("mvn" "-P" "release" "-DskipTests=true" "-Dfoo=bar"
                             "clean" "package")
                  #:got (maven-argv '("clean" "package")
                                    #:profiles '("release")
                                    #:properties '(("skipTests" . "true")
                                                   "foo=bar")))))
         (test "the project, settings and CI options reach the command"
               (parameterize ((maven-command "mvn"))
                 (assert-equal
                  #:expect '("mvn" "-f" "pom.xml" "-s" "settings.xml"
                             "-o" "-U" "-B" "--no-transfer-progress"
                             "-T" "4" "-fae" "-am" "-amd"
                             "-pl" "a,b" "-rf" ":core"
                             "verify")
                  #:got (maven-argv "verify"
                                    #:file "pom.xml"
                                    #:settings "settings.xml"
                                    #:offline? #t
                                    #:update-snapshots? #t
                                    #:batch-mode? #t
                                    #:no-transfer-progress? #t
                                    #:threads 4
                                    #:fail-at-end? #t
                                    #:also-make? #t
                                    #:also-make-dependents? #t
                                    #:projects '("a" "b")
                                    #:resume-from ":core"))))
         (test "the thread value may be the string Maven accepts"
               (parameterize ((maven-command "mvn"))
                 (assert-equal #:expect '("mvn" "-T" "1C" "test")
                               #:got (maven-argv "test" #:threads "1C"))))
         (test "arguments are appended as they are"
               (parameterize ((maven-command "mvn"))
                 (assert-equal #:expect '("mvn" "test" "-DskipITs")
                               #:got (maven-argv "test"
                                                 #:arguments '("-DskipITs")))))
         (test "an empty goal list is refused"
               (assert-error (lambda () (maven-argv '()))))
         (test "a thread count that is not positive is refused"
               (assert-error (lambda () (maven-argv "test" #:threads 0))))
         (test "a property that is not a word or a pair is refused"
               (assert-error (lambda ()
                               (maven-argv "test" #:properties '(42)))))))

(define running-suite
  (suite "Running Maven"
         #:concurrent? #f
         #:shuffle? #f
         (test "a wrapper passes its arguments to the goal"
               (parameterize ((maven-command "mvn"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (maven-package "-DskipTests"))
                   (assert-equal #:expect (list #t)
                                 #:got (list (and (string-contains
                                                   (get-output-string out)
                                                   "mvn package -DskipTests")
                                                  #t))))))
         (test "the output query returns what Maven wrote"
               (with-scratch "output"
                (lambda (dir)
                  (assert-equal
                   #:expect "maven-output-ok\n"
                   #:got (maven-output (list (fake-maven dir)))))))))

(define wrapper-suite
  (suite "The Maven wrapper"
         #:concurrent? #f
         #:shuffle? #f
         (test "a directory with mvnw answers with it"
               (with-scratch "wrapper"
                (lambda (dir)
                  (write-text (in-vicinity dir "mvnw") "#!/bin/sh\n")
                  (chmod (in-vicinity dir "mvnw") #o755)
                  (assert-equal #:expect (list #t (in-vicinity dir "mvnw"))
                                #:got (list (maven-wrapper? dir)
                                            (maven-wrapper-command dir))))))
         (test "a directory without mvnw answers with the program"
               (with-scratch "no-wrapper"
                (lambda (dir)
                  (parameterize ((maven-command "mvn"))
                    (assert-equal #:expect (list #f "mvn")
                                  #:got (list (maven-wrapper? dir)
                                              (maven-wrapper-command dir)))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               running-suite
               wrapper-suite))
