;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(use-modules (guix packages)
             (gnu)
             (guix utils)
             (guix git-download)
             (guix download)
             (gnu packages guile)
             (gnu packages nss)
             (gnu packages gnupg)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (guix build-system guile)
             (gnu packages base)
             (gnu packages linux)
             (gnu packages compression)
             (gnu packages texinfo)
             (gnu packages autotools)
             (gnu packages rust-apps)
             (gnu packages version-control)
             ((guix licenses)
              #:prefix license:))

(define-public guile-veritas
  (package
    (name "guile-veritas")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/jjba23/veritas.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pr8yjnxkrp2fylmn8ihjapin7iz94jqi4hbc6yd0ajll11ajxc9"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src"))
    (native-inputs (list guile-3.0))
    (propagated-inputs (list guile-fibers guile-json-4)) ;for versions >= 2.0.0
    (home-page "https://codeberg.org/jjba23/veritas")
    (synopsis "Testing framework for Guile")
    (description
     "Veritas is a testing framework for Guile with an @acronym{EDSL,
embedded domain specific language} to define test suites.  Emphasis is placed
on legible, maintainable tests.  Veritas shuffles tests and runs them
concurrently by default.")
    (license license:lgpl3+)))

(packages->manifest (list guile-3.0-latest
                          guile-documenta
                          texinfo
                          guile-ares-rs
                          guile-veritas
                          coreutils
                          util-linux
                          ;; The archive tests build and unpack real tar and zip
                          ;; files, and the git tests need git itself.
                          tar
                          zip
                          unzip
                          git
                          watchexec))
