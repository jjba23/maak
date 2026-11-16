;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak cargo)': the pure argv builder, the wrappers that feed it to
;; `run', and the metadata queries.  No Rust toolchain is needed: a builder is
;; asserted as a list of words, and the programs the queries call are small
;; scripts this spec writes, so the JSON `cargo metadata' prints is parsed
;; rather than assumed.
;;
;; The suites run sequentially because they change process-wide state: the
;; program parameters and a few scratch directories.

;;; Code:

(define-module (veritas unit cargo)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak cargo)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-cargo-~a-~a" (tmp-dir) label (getpid))))
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
  "Return the message THUNK raises, or #f when it returns."
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . values)
      (and (maak-error? (car values))
           (maak-error-message (car values))))))

(define (fake-cargo directory)
  "Write a cargo stand-in in DIRECTORY and return its path."
  (let ((path (in-vicinity directory "cargo")))
    (write-text
     path
     (string-append
      "#!/bin/sh\n"
      "cat <<'JSON'\n"
      "{\"target_directory\":\"/tmp/target\",\"packages\":[\n"
      " {\"name\":\"app\",\"version\":\"1.2.3\",\"targets\":["
      "{\"name\":\"app\",\"kind\":[\"bin\"]},"
      "{\"name\":\"applib\",\"kind\":[\"lib\"]},"
      "{\"name\":\"helper\",\"kind\":[\"bin\"]}]},\n"
      " {\"name\":\"libcrate\",\"version\":\"0.1.0\",\"targets\":["
      "{\"name\":\"libcrate\",\"kind\":[\"lib\"]}]}\n"
      "]}\n"
      "JSON\n"))
    (chmod path #o755)
    path))

(define (fake-rustup directory)
  (let ((path (in-vicinity directory "rustup")))
    (write-text
     path
     (string-append
      "#!/bin/sh\n"
      "case \"$2\" in\n"
      "  active-toolchain) echo 'stable-x86_64-unknown-linux-gnu (default)' ;;\n"
      "  list) printf 'stable-x86_64-unknown-linux-gnu (default)\\nnightly-x86_64-unknown-linux-gnu\\n' ;;\n"
      "  *) exit 0 ;;\n"
      "esac\n"))
    (chmod path #o755)
    path))

(define argv-suite
  (suite "The cargo command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "a plain subcommand is the program and its name"
               (parameterize ((cargo-command "cargo"))
                 (assert-equal #:expect '("cargo" "build")
                               #:got (cargo-argv "build"))))
         (test "features are joined with commas after the subcommand"
               (parameterize ((cargo-command "cargo"))
                 (assert-equal
                  #:expect '("cargo" "build" "--features" "openssl,serde" "--release")
                  #:got (cargo-argv "build" #:release? #t
                                    #:features '("openssl" "serde")))))
         (test "global options come before the subcommand"
               (parameterize ((cargo-command "cargo"))
                 (assert-equal
                  #:expect '("cargo" "--manifest-path" "Cargo.toml" "--offline"
                             "--locked" "-v" "-v" "--color" "never"
                             "--jobs" "4" "build")
                  #:got (cargo-argv "build"
                                    #:manifest-path "Cargo.toml"
                                    #:offline? #t #:locked? #t
                                    #:verbose 2 #:color "never"
                                    #:jobs 4))))
         (test "a package, a target and a profile reach the command"
               (parameterize ((cargo-command "cargo"))
                 (assert-equal
                  #:expect '("cargo" "build" "--package" "app"
                             "--all-features" "--target" "wasm32-unknown-unknown"
                             "--profile" "release")
                  #:got (cargo-argv "build" #:package "app" #:all-features? #t
                                    #:target "wasm32-unknown-unknown"
                                    #:profile "release"))))
         (test "arguments are passed through, including a program's own words"
               (parameterize ((cargo-command "cargo"))
                 (assert-equal #:expect '("cargo" "run" "--" "--help")
                               #:got (cargo-argv "run"
                                                 #:arguments '("--" "--help")))))
         (test "a program value may carry a wrapper"
               (parameterize ((cargo-command "ccache cargo"))
                 (assert-equal #:expect '("ccache" "cargo" "build")
                               #:got (cargo-argv "build"))))
         (test "release and profile cannot both be given"
               (assert-error (lambda ()
                               (cargo-argv "build" #:release? #t
                                           #:profile "release"))))
         (test "a subcommand that is not a name is refused"
               (assert-error (lambda () (cargo-argv #f))))
         (test "a job count that is not positive is refused"
               (assert-error (lambda () (cargo-argv "build" #:jobs 0))))
         (test "a colour that cargo does not know is refused"
               (assert-error (lambda () (cargo-argv "build" #:color "rainbow"))))))

(define run-suite
  (suite "Running cargo"
         #:concurrent? #f
         #:shuffle? #f
         (test "a wrapper passes its arguments after the subcommand"
               (parameterize ((cargo-command "cargo"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (cargo-build "--release"))
                   (assert-equal #:expect (list #t)
                                 #:got (list (and (string-contains
                                                   (get-output-string out)
                                                   "cargo build --release")
                                                  #t))))))))

(define metadata-suite
  (suite "What cargo metadata says"
         #:concurrent? #f
         #:shuffle? #f
         (test "the package version is read from the metadata"
               (with-scratch "version"
                (lambda (dir)
                  (parameterize ((cargo-command (fake-cargo dir)))
                    (assert-equal #:expect "1.2.3"
                                  #:got (cargo-package-version
                                         #:package "app"))))))
         (test "the workspace's package names are read in cargo's order"
               (with-scratch "names"
                (lambda (dir)
                  (parameterize ((cargo-command (fake-cargo dir)))
                    (assert-equal #:expect '("app" "libcrate")
                                  #:got (cargo-package-names))))))
         (test "the binary targets are told from the library targets"
               (with-scratch "binaries"
                (lambda (dir)
                  (parameterize ((cargo-command (fake-cargo dir)))
                    (assert-equal #:expect '("app" "helper")
                                  #:got (cargo-binaries #:package "app"))))))
         (test "the target directory is read from the metadata"
               (with-scratch "target"
                (lambda (dir)
                  (parameterize ((cargo-command (fake-cargo dir)))
                    (assert-equal #:expect "/tmp/target"
                                  #:got (cargo-target-directory))))))
         (test "an artefact path names the profile and the triple"
               (with-scratch "artifact"
                (lambda (dir)
                  (parameterize ((cargo-command (fake-cargo dir)))
                    (assert-equal
                     #:expect '("/tmp/target/release/app"
                                "/tmp/target/aarch64-unknown-linux-gnu/debug/app")
                     #:got (list (cargo-artifact-path "app" #:release? #t)
                                 (cargo-artifact-path
                                  "app" #:triple "aarch64-unknown-linux-gnu")))))))
         (test "a workspace with several packages needs one named"
               (with-scratch "ambiguous"
                (lambda (dir)
                  (parameterize ((cargo-command (fake-cargo dir)))
                    (let ((message (error-of (lambda () (cargo-package)))))
                      (assert-true (and message
                                        (string-contains message "name one")
                                        #t)))))))))

(define rustup-suite
  (suite "rustup"
         #:concurrent? #f
         #:shuffle? #f
         (test "the argv names the program and its words"
               (parameterize ((rustup-command "rustup"))
                 (assert-equal #:expect '("rustup" "toolchain" "list")
                               #:got (rustup-argv "toolchain" "list"))))
         (test "the active toolchain is the first word of the answer"
               (with-scratch "active"
                (lambda (dir)
                  (parameterize ((rustup-command (fake-rustup dir)))
                    (assert-equal
                     #:expect "stable-x86_64-unknown-linux-gnu"
                     #:got (rustup-active-toolchain))))))
         (test "the installed toolchains are read one per line"
               (with-scratch "toolchains"
                (lambda (dir)
                  (parameterize ((rustup-command (fake-rustup dir)))
                    (assert-equal
                     #:expect '("stable-x86_64-unknown-linux-gnu"
                                "nightly-x86_64-unknown-linux-gnu")
                     #:got (rustup-toolchains))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               run-suite
               metadata-suite
               rustup-suite))
