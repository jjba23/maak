;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak cc)': the pure argv builders a C or C++ task composes, the
;; explicit `up-to-date?' check, the environment flag reader, and the
;; pkg-config queries.  No C compiler is needed: a builder is asserted as a list
;; of words, and pkg-config is a small script the test writes, so the parsing of
;; its quoted output is tested rather than assumed.
;;
;; The suites run sequentially because they change process-wide state: the
;; compiler parameters, the environment, and a few scratch directories.

;;; Code:

(define-module (veritas unit cc)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak cc)
  #:export (spec))

(define (scratch-directory label)
  "Return a fresh directory under the temporary directory."
  (let ((dir (format #f "~a/maak-cc-~a-~a" (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (with-scratch label thunk)
  "Call THUNK with a fresh directory, removing it afterwards."
  (let ((dir (scratch-directory label)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk dir))
      (lambda ()
        (when (file-exists? dir)
          (parameterize ((quiet? #t))
            (delete-file-recursively dir)))))))

(define (with-default-programs thunk)
  "Call THUNK with the compiler and archiver a plain environment has."
  (parameterize ((c-compiler "cc")
                 (cxx-compiler "c++")
                 (ar-command "ar")
                 (pkg-config-command "pkg-config"))
    (thunk)))

(define (fake-pkg-config directory)
  "Write a pkg-config stand-in in DIRECTORY and return its path."
  (let ((path (in-vicinity directory "pkg-config")))
    (write-text path
                (string-append
                 "#!/bin/sh\n"
                 "case \"$1\" in\n"
                 "  --cflags) printf '%s\\n' '-I/a\\ b -DFOO=1' ;;\n"
                 "  --libs) printf '%s\\n' '-L/x -lfoo' ;;\n"
                 "  --static) if [ \"$2\" = --libs ]; then printf '%s\\n' '-L/x -lfoo -lbar'; else exit 1; fi ;;\n"
                 "  --modversion) printf '%s\\n' '1.2.3' ;;\n"
                 "  --exists) if [ \"$2\" = good ]; then exit 0; else exit 1; fi ;;\n"
                 "  *) exit 1 ;;\n"
                 "esac\n"))
    (chmod path #o755)
    path))

(define (with-fake-pkg-config thunk)
  (with-scratch "pkg-config"
    (lambda (directory)
      (parameterize ((pkg-config-command (fake-pkg-config directory)))
        (thunk)))))

(define object-suite
  (suite "Object names"
         #:concurrent? #f
         #:shuffle? #f
         (test "an object keeps the source's directory"
               (assert-equal #:expect "src/a.o"
                             #:got (object-file "src/a.c")))
         (test "an object can be placed in a build directory"
               (assert-equal #:expect "build/a.o"
                             #:got (object-file "src/a.c" "build")))
         (test "a source without an extension still names an object"
               (assert-equal #:expect "a.o"
                             #:got (object-file "a")))))

(define compile-suite
  (suite "Compiling"
         #:concurrent? #f
         #:shuffle? #f
         (test "a plain compile names the compiler, the object and the source"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("cc" "-c" "-o" "src/a.o" "src/a.c")
                                #:got (compile-argv "src/a.c")))))
         (test "includes, defines and flags reach the command"
               (with-default-programs
                (lambda ()
                  (assert-equal
                   #:expect '("cc" "-c" "-std=c11" "-fPIC" "-MMD" "-MP"
                              "-Iinclude" "-Isrc" "-DNDEBUG" "-O2" "-Wall"
                              "-o" "build/a.o" "src/a.c")
                   #:got (compile-argv "src/a.c"
                                       #:output "build/a.o"
                                       #:includes '("include" "src")
                                       #:defines '("NDEBUG")
                                       #:flags '("-O2" "-Wall")
                                       #:standard "c11"
                                       #:pic? #t
                                       #:depfile? #t)))))
         (test "a C++ source uses the C++ compiler"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("c++" "-c" "-o" "a.o" "a.cc")
                                #:got (compile-argv "a.cc" #:language 'c++)))))
         (test "the compiler can be pinned for one command"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("clang" "-c" "-o" "a.o" "a.c")
                                #:got (compile-argv "a.c" #:compiler "clang")))))
         (test "a compiler value may carry a wrapper and its flags"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("ccache" "gcc" "-c" "-o" "a.o" "a.c")
                                #:got (compile-argv "a.c" #:compiler "ccache gcc")))))
         (test "a language that is not C or C++ is refused"
               (assert-error (lambda () (compile-argv "a.c" #:language 'rust))))
         (test "a source that is not a path is refused"
               (assert-error (lambda () (compile-argv 42))))))

(define compile-many-suite
  (suite "Compiling several objects"
         #:concurrent? #f
         #:shuffle? #f
         (test "a dry run prints every compile"
               (with-default-programs
                (lambda ()
                  (let ((out (open-output-string)))
                    (parameterize ((current-output-port out)
                                   (dry-run? #t)
                                   (quiet? #f))
                      (compile-objects '("a.c" "b.c") #:directory "build"))
                    (assert-equal
                     #:expect (list #t #t)
                     #:got (list (and (string-contains (get-output-string out)
                                                       "build/a.o")
                                      #t)
                                 (and (string-contains (get-output-string out)
                                                       "build/b.o")
                                      #t)))))))
         (test "an incremental compile skips an object newer than its source"
               (with-default-programs
                (lambda ()
                  (with-scratch "incremental"
                    (lambda (dir)
                      (let ((a (in-vicinity dir "a.c"))
                            (b (in-vicinity dir "b.c"))
                            (build (in-vicinity dir "build")))
                        (mkdir-p build)
                        (write-text a "(void)a;\n")
                        (write-text b "(void)b;\n")
                        (write-text (in-vicinity build "a.o") "object")
                        (utime a 100 100)
                        (utime (in-vicinity build "a.o") 200 200)
                        (let ((out (open-output-string)))
                          (parameterize ((current-output-port out)
                                         (dry-run? #t)
                                         (quiet? #f))
                            (compile-objects (list a b) #:directory build
                                             #:incremental? #t))
                          (assert-equal
                           #:expect (list #f #t)
                           #:got (list (and (string-contains
                                             (get-output-string out) "a.c")
                                            #t)
                                       (and (string-contains
                                             (get-output-string out) "b.c")
                                            #t))))))))))))

(define link-suite
  (suite "Linking"
         #:concurrent? #f
         #:shuffle? #f
         (test "a plain link names the output before its objects"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("cc" "-o" "app" "a.o")
                                #:got (link-argv '("a.o") #:output "app")))))
         (test "library paths, flags and libraries are ordered for the linker"
               (with-default-programs
                (lambda ()
                  (assert-equal
                   #:expect '("cc" "-Lbuild" "-pthread" "-o" "app"
                              "a.o" "b.o" "-lm" "-lpthread")
                   #:got (link-argv '("a.o" "b.o")
                                    #:output "app"
                                    #:library-paths '("build")
                                    #:flags '("-pthread")
                                    #:libraries '("m" "pthread"))))))
         (test "a shared library names its soname"
               (with-default-programs
                (lambda ()
                  (assert-equal
                   #:expect '("cc" "-shared" "-Wl,-soname,libfoo.so.1"
                              "-o" "libfoo.so.1" "a.o")
                   #:got (link-argv '("a.o")
                                    #:output "libfoo.so.1"
                                    #:shared? #t
                                    #:soname "libfoo.so.1")))))
         (test "a static link asks the linker for static libraries"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("cc" "-static" "-o" "app" "a.o")
                                #:got (link-argv '("a.o") #:output "app"
                                                 #:static? #t)))))
         (test "link flags follow the objects and the libraries"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("cc" "-o" "app" "a.o" "-lm" "-L/x" "-lfoo")
                                #:got (link-argv '("a.o")
                                                 #:output "app"
                                                 #:libraries '("m")
                                                 #:link-flags '("-L/x" "-lfoo"))))))
         (test "a link with no object is refused"
               (assert-error (lambda () (link-argv '() #:output "app"))))))

(define static-suite
  (suite "Static libraries"
         #:concurrent? #f
         #:shuffle? #f
         (test "ar writes the archive and its index"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("ar" "rcs" "liba.a" "a.o" "b.o")
                                #:got (static-library-argv "liba.a"
                                                           '("a.o" "b.o"))))))
         (test "the archiver can be pinned"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("llvm-ar" "rcs" "liba.a" "a.o")
                                #:got (static-library-argv
                                       "liba.a" '("a.o")
                                       #:archiver "llvm-ar")))))
         (test "an archive with no object is refused"
               (assert-error (lambda ()
                               (static-library-argv "liba.a" '()))))))

(define pkg-config-suite
  (suite "pkg-config"
         #:concurrent? #f
         #:shuffle? #f
         (test "cflags are asked for in the shape pkg-config expects"
               (with-default-programs
                (lambda ()
                  (assert-equal #:expect '("pkg-config" "--cflags" "gtk+-3.0")
                                #:got (pkg-config-argv '("gtk+-3.0"))))))
         (test "static libs ask for the static answer"
               (with-default-programs
                (lambda ()
                  (assert-equal
                   #:expect '("pkg-config" "--static" "--libs" "zlib")
                   #:got (pkg-config-argv '("zlib") #:mode 'libs
                                          #:static? #t)))))
         (test "a mode that means nothing is refused"
               (assert-error (lambda ()
                               (pkg-config-argv '("zlib") #:mode 'fancy))))
         (test "the quoted output of pkg-config is split like a shell"
               (with-fake-pkg-config
                (lambda ()
                  (assert-equal #:expect '("-I/a b" "-DFOO=1")
                                #:got (pkg-config-cflags '("good"))))))
         (test "the linker flags are returned as a list"
               (with-fake-pkg-config
                (lambda ()
                  (assert-equal #:expect '("-L/x" "-lfoo")
                                #:got (pkg-config-libs '("good"))))))
         (test "the static linker flags include the transitive libraries"
               (with-fake-pkg-config
                (lambda ()
                  (assert-equal #:expect '("-L/x" "-lfoo" "-lbar")
                                #:got (pkg-config-libs '("good") #:static? #t)))))
         (test "a package pkg-config knows is present"
               (with-fake-pkg-config
                (lambda ()
                  (assert-true (pkg-config-exists? '("good"))))))
         (test "a package pkg-config does not know is absent"
               (with-fake-pkg-config
                (lambda ()
                  (assert-false (pkg-config-exists? '("bad"))))))
         (test "the version is read from pkg-config"
               (with-fake-pkg-config
                (lambda ()
                  (assert-equal #:expect "1.2.3"
                                #:got (pkg-config-version "good")))))))

(define incremental-suite
  (suite "The explicit up-to-date check"
         #:concurrent? #f
         #:shuffle? #f
         (test "an output newer than its input is up to date"
               (with-scratch "newer"
                (lambda (dir)
                  (let ((out (in-vicinity dir "a.o"))
                        (in (in-vicinity dir "a.c")))
                    (write-text out "object")
                    (write-text in "source")
                    (utime out 200 200)
                    (utime in 100 100)
                    (assert-true (up-to-date? out (list in)))))))
         (test "an output older than its input is not up to date"
               (with-scratch "older"
                (lambda (dir)
                  (let ((out (in-vicinity dir "a.o"))
                        (in (in-vicinity dir "a.c")))
                    (write-text out "object")
                    (write-text in "source")
                    (utime out 100 100)
                    (utime in 200 200)
                    (assert-false (up-to-date? out (list in)))))))
         (test "a missing output is not up to date"
               (with-scratch "no-output"
                (lambda (dir)
                  (let ((in (in-vicinity dir "a.c")))
                    (write-text in "source")
                    (assert-false (up-to-date? (in-vicinity dir "a.o")
                                               (list in)))))))
         (test "a missing input is not up to date"
               (with-scratch "no-input"
                (lambda (dir)
                  (let ((out (in-vicinity dir "a.o")))
                    (write-text out "object")
                    (assert-false (up-to-date? out
                                               (list (in-vicinity dir "a.c"))))))))
         (test "an output with no inputs is up to date"
               (with-scratch "no-inputs"
                (lambda (dir)
                  (let ((out (in-vicinity dir "a.o")))
                    (write-text out "object")
                    (assert-true (up-to-date? out '()))))))))

(define environment-suite
  (suite "Flags from the environment"
         #:concurrent? #f
         #:shuffle? #f
         (test "a variable's words become flags"
               (dynamic-wind
                 (lambda () (env-set! "MAAK_TEST_CFLAGS" " -O2 -g -Wall "))
                 (lambda () (assert-equal #:expect '("-O2" "-g" "-Wall")
                                          #:got (environment-flags
                                                 "MAAK_TEST_CFLAGS")))
                 (lambda () (env-unset! "MAAK_TEST_CFLAGS"))))
         (test "a quoted value in a variable stays one flag"
               (dynamic-wind
                 (lambda () (env-set! "MAAK_TEST_CFLAGS"
                                      "-DNAME='a b' -O2"))
                 (lambda () (assert-equal #:expect '("-DNAME=a b" "-O2")
                                          #:got (environment-flags
                                                 "MAAK_TEST_CFLAGS")))
                 (lambda () (env-unset! "MAAK_TEST_CFLAGS"))))
         (test "an unset variable uses the default"
               (dynamic-wind
                 (lambda () (env-unset! "MAAK_TEST_CFLAGS"))
                 (lambda () (assert-equal #:expect '("-O2")
                                          #:got (environment-flags
                                                 "MAAK_TEST_CFLAGS" '("-O2"))))
                 (lambda () (env-unset! "MAAK_TEST_CFLAGS"))))))

(define install-suite
  (suite "Installing"
         #:concurrent? #f
         #:shuffle? #f
         (test "install-file copies a program and creates its directory"
               (with-scratch "install-file"
                (lambda (dir)
                  (let ((source (in-vicinity dir "app"))
                        (destination (in-vicinity dir "sub/bin/app")))
                    (write-text source "#!/bin/sh\n")
                    (parameterize ((quiet? #t))
                      (install-file source destination #:mode "755"))
                    (assert-equal #:expect (list #t #o755)
                                  #:got (list (file-exists? destination)
                                              (stat:perms (stat destination))))))))
         (test "install-directory creates a directory with its mode"
               (with-scratch "install-directory"
                (lambda (dir)
                  (let ((destination (in-vicinity dir "share/app")))
                    (parameterize ((quiet? #t))
                      (install-directory destination #:mode "750"))
                    (assert-equal #:expect (list #t #o750)
                                  #:got (list (file-exists? destination)
                                              (stat:perms (stat destination))))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               object-suite
               compile-suite
               compile-many-suite
               link-suite
               static-suite
               pkg-config-suite
               incremental-suite
               environment-suite
               install-suite))
