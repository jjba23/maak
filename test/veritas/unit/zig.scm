;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>
;; Copyright © FuncProgLinux <softwarelibre@urutau-ltd.org>

;;; Commentary:
;;
;; Tests for `(maak zig)': the pure build-command builder, the wrappers that
;; feed it to `run', and the output query.  No Zig is needed: a builder is
;; asserted as a list of words, and the program a query runs is a small script
;; this spec writes, so its answer is read rather than assumed.
;;
;; The suites run sequentially because they change process-wide state: the
;; program parameter and a few scratch directories.

;;; Code:

(define-module (veritas unit zig)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak zig)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-zig-~a-~a" (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (fake-zig directory text)
  "Write a zig stand-in in DIRECTORY that prints TEXT, and return its path."
  (let ((path (in-vicinity directory "zig")))
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

(define argv-suite
  (suite "The zig command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "a plain subcommand is the program and its name"
               (parameterize ((zig-command "zig"))
                 (assert-equal #:expect '("zig" "version")
                               #:got (zig-argv "version"))))
         (test "arguments are passed after the subcommand"
               (parameterize ((zig-command "zig"))
                 (assert-equal #:expect '("zig" "fmt" "src")
                               #:got (zig-argv "fmt" #:arguments '("src")))))
         (test "a program value may carry a wrapper"
               (parameterize ((zig-command "ccache zig"))
                 (assert-equal #:expect '("ccache" "zig" "version")
                               #:got (zig-argv "version"))))
         (test "a subcommand that is not a name is refused"
               (assert-error (lambda () (zig-argv #f))))))

(define build-argv-suite
  (suite "The zig build command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "no step builds the default install step"
               (parameterize ((zig-command "zig"))
                 (assert-equal #:expect '("zig" "build")
                               #:got (zig-build-argv))))
         (test "an empty step list builds the default install step"
               (parameterize ((zig-command "zig"))
                 (assert-equal #:expect '("zig" "build")
                               #:got (zig-build-argv '()))))
         (test "a step may be one string or a list of them"
               (parameterize ((zig-command "zig"))
                 (assert-equal #:expect '(("zig" "build" "test")
                                          ("zig" "build" "test" "run"))
                               #:got (list (zig-build-argv "test")
                                           (zig-build-argv '("test" "run"))))))
         (test "the target, the cpu and the build options reach the command"
               (parameterize ((zig-command "zig"))
                 (assert-equal
                  #:expect '("zig" "build" "install"
                             "-Doptimize=ReleaseSafe"
                             "-Dtarget=aarch64-linux-gnu"
                             "-Dcpu=baseline"
                             "-Dfoo=bar" "-Dflag")
                  #:got (zig-build-argv '("install")
                                        #:optimize "ReleaseSafe"
                                        #:target "aarch64-linux-gnu"
                                        #:cpu "baseline"
                                        #:options '(("foo" . "bar") "flag")))))
         (test "the prefix and the cache directories are words"
               (parameterize ((zig-command "zig"))
                 (assert-equal
                  #:expect '("zig" "build"
                             "--prefix" "out"
                             "--prefix-lib-dir" "out/lib"
                             "--prefix-exe-dir" "out/bin"
                             "--prefix-include-dir" "out/include"
                             "--build-file" "build.zig"
                             "--cache-dir" "cache"
                             "--global-cache-dir" "global-cache"
                             "--zig-lib-dir" "lib")
                  #:got (zig-build-argv
                         '()
                         #:prefix "out"
                         #:prefix-lib-dir "out/lib"
                         #:prefix-exe-dir "out/bin"
                         #:prefix-include-dir "out/include"
                         #:build-file "build.zig"
                         #:cache-dir "cache"
                         #:global-cache-dir "global-cache"
                         #:zig-lib-dir "lib"))))
         (test "the system paths and the fetch mode are repeated"
               (parameterize ((zig-command "zig"))
                 (assert-equal
                  #:expect '("zig" "build"
                             "--search-prefix" "deps"
                             "--search-prefix" "vendor"
                             "--system" "/nix/store"
                             "--fetch=all")
                  #:got (zig-build-argv '()
                                        #:search-prefixes '("deps" "vendor")
                                        #:systems '("/nix/store")
                                        #:fetch "all"))))
         (test "the build report is the flags zig spells"
               (parameterize ((zig-command "zig"))
                 (assert-equal
                  #:expect '("zig" "build" "test"
                             "--summary" "all"
                             "--color" "off"
                             "-j4"
                             "--verbose")
                  #:got (zig-build-argv '("test")
                                        #:summary "all"
                                        #:color "off"
                                        #:jobs 4
                                        #:verbose? #t))))
         (test "arguments are appended as they are"
               (parameterize ((zig-command "zig"))
                 (assert-equal #:expect '("zig" "build" "run" "--" "--help")
                               #:got (zig-build-argv '("run")
                                                     #:arguments '("--" "--help")))))
         (test "steps that are not strings are refused"
               (assert-error (lambda () (zig-build-argv '(build)))))
         (test "an optimization mode zig does not know is refused"
               (assert-error (lambda ()
                               (zig-build-argv '() #:optimize "Fastest"))))
         (test "a summary mode zig does not know is refused"
               (assert-error (lambda ()
                               (zig-build-argv '() #:summary "everything"))))
         (test "a colour zig does not know is refused"
               (assert-error (lambda () (zig-build-argv '() #:color "rainbow"))))
         (test "a job count that is not positive is refused"
               (assert-error (lambda () (zig-build-argv '() #:jobs 0))))
         (test "a fetch mode zig does not know is refused"
               (assert-error (lambda () (zig-build-argv '() #:fetch "sometimes"))))))

(define running-suite
  (suite "Running zig"
         #:concurrent? #f
         #:shuffle? #f
         (test "the build wrapper runs the default step"
               (parameterize ((zig-command "zig"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (zig-build))
                   (assert-equal #:expect (list #t)
                                 #:got (list (and (string-contains
                                                   (get-output-string out)
                                                   "zig build")
                                                  #t))))))
         (test "the named build steps are one word each"
               (parameterize ((zig-command "zig"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (zig-build-test)
                     (zig-build-run "--" "one"))
                   (assert-equal
                    #:expect '(#t #t)
                    #:got (list (and (string-contains
                                      (get-output-string out)
                                      "zig build test")
                                     #t)
                                (and (string-contains
                                      (get-output-string out)
                                      "zig build run -- one")
                                     #t))))))
         (test "the direct commands run through the same program"
               (parameterize ((zig-command "zig"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (zig-build-exe "src/main.zig")
                     (zig-test "src/main.zig")
                     (zig-fmt "src"))
                   (let ((text (get-output-string out)))
                     (assert-equal
                      #:expect '(#t #t #t)
                      #:got (list (and (string-contains
                                        text "zig build-exe src/main.zig")
                                       #t)
                                  (and (string-contains
                                        text "zig test src/main.zig")
                                       #t)
                                  (and (string-contains text "zig fmt src")
                                       #t)))))))
         (test "the remaining wrappers name their own command"
               (parameterize ((zig-command "zig"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (zig-build-install)
                     (zig-build-steps)
                     (zig-build-lib "lib.zig")
                     (zig-build-obj "obj.zig")
                     (zig-fetch "pkg.tar.gz"))
                   (let ((text (get-output-string out)))
                     (assert-equal
                      #:expect '(#t #t #t #t #t)
                      #:got (list (and (string-contains text "zig build install") #t)
                                  (and (string-contains text "zig build --list-steps") #t)
                                  (and (string-contains text "zig build-lib lib.zig") #t)
                                  (and (string-contains text "zig build-obj obj.zig") #t)
                                  (and (string-contains text "zig fetch pkg.tar.gz") #t)))))))
         (test "the output query returns what zig printed"
               (with-scratch "version"
                (lambda (dir)
                  (parameterize ((zig-command
                                  (fake-zig dir "echo '0.16.0'\n")))
                    (assert-equal #:expect "0.16.0"
                                  #:got (zig-version))))))
         (test "a failing query is refused by the command it ran"
               (with-scratch "failure"
                (lambda (dir)
                  (parameterize ((zig-command (fake-zig dir "exit 3\n")))
                    (let ((message (error-of
                                    (lambda ()
                                      (zig-output (zig-argv "version"))))))
                      (assert-true (and message
                                        (string-contains message
                                                         "exit code 3")
                                        #t)))))))))

(define clean-suite
  (suite "Cleaning a Zig install prefix"
         #:concurrent? #f
         #:shuffle? #f
         (test "the default prefix is removed"
               (with-scratch "clean-default"
                (lambda (dir)
                  (let ((out (in-vicinity dir "zig-out")))
                    (mkdir-p out)
                    (write-text (in-vicinity out "app") "binary")
                    (with-directory dir (lambda () (zig-build-clean)))
                    (assert-false (file-exists? out))))))
         (test "a named prefix is removed"
               (with-scratch "clean-named"
                (lambda (dir)
                  (let ((out (in-vicinity dir "build/out")))
                    (mkdir-p out)
                    (write-text (in-vicinity out "app") "binary")
                    (with-directory dir
                                    (lambda ()
                                      (zig-build-clean #:prefix "build/out")))
                    (assert-false (file-exists? out))))))
         (test "an absolute prefix is refused"
               (let ((message (error-of
                               (lambda ()
                                 (zig-build-clean #:prefix "/tmp/zig-out")))))
                 (assert-true (and message
                                   (string-contains message "relative")
                                   #t))))
         (test "a prefix that leaves the project is refused"
               (let ((message (error-of
                               (lambda ()
                                 (zig-build-clean #:prefix "../zig-out")))))
                 (assert-true (and message
                                   (string-contains message "..")
                                   #t))))
         (test "the project root is refused"
               (let ((message (error-of
                               (lambda ()
                                 (zig-build-clean #:prefix "./.")))))
                 (assert-true (and message
                                   (string-contains message "project root")
                                   #t))))
         (test "uninstall names zig's missing implementation"
               (let ((message (error-of (lambda () (zig-build-uninstall)))))
                 (assert-true (and message
                                   (string-contains message "not implemented")
                                   #t))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               build-argv-suite
               running-suite
               clean-suite))
