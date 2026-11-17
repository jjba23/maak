;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak frontend)': which package manager a checkout uses, the words
;; each one spells an operation with, the package.json readers and the
;; convention tasks.  No npm, pnpm or yarn is needed: a builder is asserted as a
;; list of words, and the difference between two managers is asserted as two
;; lists rather than by running either of them.
;;
;; The suites run sequentially because they change process-wide state: the
;; package-manager parameter, the working directory and a few scratch
;; directories.

;;; Code:

(define-module (veritas unit frontend)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak frontend)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-frontend-~a-~a" (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (fake-program directory name text)
  "Write a stand-in program NAME in DIRECTORY and return its path."
  (let ((path (in-vicinity directory name)))
    (write-text path (string-append "#!/bin/sh\n" text))
    (chmod path #o755)
    path))

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
  "Return the message THUNK raises, or #f when it returns."
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . values)
      (and (maak-error? (car values))
           (maak-error-message (car values))))))

(define (dry-run-output thunk)
  "Return what THUNK prints in a dry run."
  (let ((out (open-output-string)))
    (parameterize ((current-output-port out)
                   (dry-run? #t)
                   (quiet? #f))
      (thunk))
    (get-output-string out)))

(define (prints? thunk text)
  "Return #t when the dry run of THUNK prints TEXT."
  (and (string-contains (dry-run-output thunk) text) #t))

(define package-json-text
  "{\"name\": \"example-web\",
    \"version\": \"0.4.1\",
    \"scripts\": {\"build\": \"vite build\", \"test\": \"vitest run\"},
    \"dependencies\": {\"react\": \"^18.0.0\"},
    \"devDependencies\": {\"vite\": \"^5.0.0\"}}
")

(define detection-suite
  (suite "Which package manager"
         #:concurrent? #f
         #:shuffle? #f
         (test "a package-lock names npm"
               (with-scratch "lock-npm"
                (lambda (dir)
                  (write-text (in-vicinity dir "package-lock.json") "{}")
                  (assert-equal #:expect "npm"
                                #:got (detect-package-manager
                                       #:directory dir)))))
         (test "a pnpm lock names pnpm"
               (with-scratch "lock-pnpm"
                (lambda (dir)
                  (write-text (in-vicinity dir "pnpm-lock.yaml") "")
                  (assert-equal #:expect "pnpm"
                                #:got (detect-package-manager
                                       #:directory dir)))))
         (test "a yarn lock names yarn"
               (with-scratch "lock-yarn"
                (lambda (dir)
                  (write-text (in-vicinity dir "yarn.lock") "")
                  (assert-equal #:expect "yarn"
                                #:got (detect-package-manager
                                       #:directory dir)))))
         (test "a directory with no lock file names npm"
               (with-scratch "lock-none"
                (lambda (dir)
                  (assert-equal #:expect "npm"
                                #:got (detect-package-manager
                                       #:directory dir)))))
         (test "a wrapper before the manager still names the manager"
               (assert-equal #:expect 'pnpm
                             #:got (package-manager-kind "corepack pnpm")))
         (test "a manager maak does not know is refused"
               (let ((message (error-of (lambda ()
                                          (package-manager-kind "bun")))))
                 (assert-true (and message
                                   (string-contains message "npm, pnpm")
                                   #t))))))

(define argv-suite
  (suite "The frontend command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "a script is run with the manager's own words"
               (parameterize ((package-manager "npm"))
                 (assert-equal #:expect '("npm" "run" "build")
                               #:got (frontend-argv '("run" "build")))))
         (test "the directory is spelled the way each manager spells it"
               (assert-equal
                #:expect '(("npm" "--prefix" "web" "run" "build")
                           ("pnpm" "--dir" "web" "run" "build")
                           ("yarn" "--cwd" "web" "run" "build"))
                #:got (map (lambda (manager)
                             (parameterize ((package-manager manager))
                               (frontend-argv '("run" "build")
                                              #:directory "web")))
                           '("npm" "pnpm" "yarn"))))
         (test "a wrapper before the manager keeps its words"
               (parameterize ((package-manager "corepack pnpm"))
                 (assert-equal #:expect '("corepack" "pnpm" "run" "build")
                               #:got (frontend-argv '("run" "build")))))
         (test "words that are not strings are refused"
               (assert-error (lambda () (frontend-argv 'build))))))

(define spelling-suite
  (suite "The verbs the managers spell differently"
         #:concurrent? #f
         #:shuffle? #f
         (test "npm installs a lock file with npm ci"
               (parameterize ((package-manager "npm"))
                 (assert-true (prints? (lambda () (frontend-ci)) "npm ci"))))
         (test "pnpm installs a lock file frozen"
               (parameterize ((package-manager "pnpm"))
                 (assert-true (prints? (lambda () (frontend-ci))
                                       "pnpm install --frozen-lockfile"))))
         (test "yarn 1 installs a lock file frozen"
               (with-scratch "yarn-1"
                (lambda (dir)
                  (with-directory
                   dir
                   (lambda ()
                     (parameterize ((package-manager "yarn"))
                       (assert-true (prints? (lambda () (frontend-ci))
                                             "yarn install --frozen-lockfile"))))))))
         (test "yarn berry installs a lock file immutably"
               (with-scratch "yarn-berry"
                (lambda (dir)
                  (write-text (in-vicinity dir ".yarnrc.yml") "")
                  (with-directory
                   dir
                   (lambda ()
                     (parameterize ((package-manager "yarn"))
                       (assert-true (prints? (lambda () (frontend-ci))
                                             "yarn install --immutable"))))))))
         (test "adding a dependency is each manager's verb"
               (assert-equal
                #:expect '(#t #t #t)
                #:got (map (lambda (manager words)
                             (prints? (lambda ()
                                        (parameterize ((package-manager manager))
                                          (frontend-add "react")))
                                      words))
                           '("npm" "pnpm" "yarn")
                           '("npm install react" "pnpm add react"
                             "yarn add react"))))
         (test "a development dependency carries the manager's flag"
               (assert-equal
                #:expect '(#t #t #t)
                #:got (map (lambda (manager)
                             (let ((output
                                    (dry-run-output
                                     (lambda ()
                                       (parameterize ((package-manager manager))
                                         (frontend-add "vite" #:dev? #t))))))
                               (and (string-contains output
                                                     (if (string=? manager "yarn")
                                                         "--dev"
                                                         "--save-dev"))
                                    #t)))
                           '("npm" "pnpm" "yarn"))))
         (test "removing a dependency is each manager's verb"
               (assert-equal
                #:expect '(#t #t #t)
                #:got (map (lambda (manager verb)
                             (prints? (lambda ()
                                        (parameterize ((package-manager manager))
                                          (frontend-remove "react")))
                                      verb))
                           '("npm" "pnpm" "yarn")
                           '("npm uninstall react" "pnpm remove react"
                             "yarn remove react"))))
         (test "a binary in node_modules runs through exec"
               (assert-equal
                #:expect '(#t #t #t)
                #:got (map (lambda (manager words)
                             (prints? (lambda ()
                                        (parameterize ((package-manager manager))
                                          (frontend-exec "eslint" ".")))
                                      words))
                           '("npm" "pnpm" "yarn")
                           '("npm exec -- eslint ." "pnpm exec eslint ."
                             "yarn exec eslint ."))))
         (test "publishing is each manager's word"
               (assert-equal
                #:expect '(#t #t)
                #:got (list (prints? (lambda ()
                                       (parameterize ((package-manager "npm"))
                                         (frontend-publish)))
                                     "npm publish")
                            (prints? (lambda ()
                                       (parameterize ((package-manager "pnpm"))
                                         (frontend-publish)))
                                     "pnpm publish"))))))

(define package-json-suite
  (suite "The package.json"
         #:concurrent? #f
         #:shuffle? #f
         (test "the name and the version are read"
               (with-scratch "package-json"
                (lambda (dir)
                  (let ((path (in-vicinity dir "package.json")))
                    (write-text path package-json-text)
                    (parameterize ((package-json-file path))
                      (assert-equal #:expect '("example-web" "0.4.1")
                                    #:got (list (package-name)
                                                (package-version))))))))
         (test "the scripts are the keys of the scripts table"
               (with-scratch "scripts"
                (lambda (dir)
                  (let ((path (in-vicinity dir "package.json")))
                    (write-text path package-json-text)
                    (parameterize ((package-json-file path))
                      (assert-equal #:expect '("build" "test")
                                    #:got (package-scripts)))))))
         (test "a dependency keeps the range the file gives it"
               (with-scratch "dependencies"
                (lambda (dir)
                  (let ((path (in-vicinity dir "package.json")))
                    (write-text path package-json-text)
                    (parameterize ((package-json-file path))
                      (assert-equal #:expect '(("react" . "^18.0.0"))
                                    #:got (package-dependencies)))))))
         (test "a missing file is refused by name"
               (with-scratch "missing"
                (lambda (dir)
                  (parameterize ((package-json-file
                                  (in-vicinity dir "package.json")))
                    (let ((message (error-of package-json)))
                      (assert-true (and message
                                        (string-contains message "package.json")
                                        #t)))))))
         (test "a file that is not an object is refused"
               (with-scratch "not-an-object"
                (lambda (dir)
                  (let* ((path (in-vicinity dir "package.json"))
                         (message
                          (begin
                            (write-text path "\"a string\"")
                            (parameterize ((package-json-file path))
                              (error-of package-name)))))
                    (assert-true (and message
                                      (string-contains message
                                                       "not a JSON object")
                                      #t))))))))

(define query-suite
  (suite "Asking the package manager"
         #:concurrent? #f
         #:shuffle? #f
         (test "a query returns what the manager printed"
               (with-scratch "npm-query"
                (lambda (dir)
                  (parameterize ((package-manager
                                  (fake-program dir "npm" "echo '1.2.3'\n")))
                    (assert-equal #:expect "1.2.3\n"
                                  #:got (frontend-output
                                         (frontend-argv '("pkg" "get"
                                                          "version"))))))))
         (test "a failing query is refused by the command it ran"
               (with-scratch "npm-failure"
                (lambda (dir)
                  (parameterize ((package-manager
                                  (fake-program dir "npm" "exit 3\n")))
                    (let ((message (error-of (lambda ()
                                               (frontend-output
                                                (frontend-argv '("ci")))))))
                      (assert-true (and message
                                        (string-contains message "exit code 3")
                                        #t)))))))))

(define task-suite
  (suite "What a frontend task runs"
         #:concurrent? #f
         #:shuffle? #f
         (test "the build script runs through the manager"
               (parameterize ((package-manager "npm"))
                 (assert-true (prints? (lambda () (frontend-build))
                                       "npm run build"))))
         (test "the test script runs through the manager"
               (parameterize ((package-manager "pnpm"))
                 (assert-true (prints? (lambda () (frontend-test))
                                       "pnpm run test"))))
         (test "a check runs the lint, the test and the build in that order"
               (parameterize ((package-manager "npm"))
                 (let ((output (dry-run-output (lambda () (frontend-check)))))
                   (assert-equal
                    #:expect '(#t #t #t #t)
                    #:got (list (and (string-contains output "run lint") #t)
                                (and (string-contains output "run test") #t)
                                (and (string-contains output "run build") #t)
                                (< (string-contains output "run lint")
                                   (string-contains output "run build")))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               detection-suite
               argv-suite
               spelling-suite
               package-json-suite
               query-suite
               task-suite))
