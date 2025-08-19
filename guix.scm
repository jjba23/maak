(use-modules (guix gexp)
             (guix packages)
             (guix git-download)
             ((guix licenses)
              #:prefix license:)
             (guix build-system guile)
             ((guix build utils)
              #:select (with-directory-excursion ))
             (gnu packages)
             (gnu packages bash)
             (gnu packages guile)
             (gnu packages imagemagick)
             (gnu packages guile-xyz))

(define %source-dir
  (dirname (current-filename)))

(define-public kmk
  (package
    (name "kmk")
    (version "0.0.1")
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (git-predicate %source-dir)))
    (arguments
     `(#:source-directory "src"
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'install-program-files
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin"))
                            (share (string-append (assoc-ref outputs "out")
                                                  "/share")))
                        (mkdir-p (string-append share "/scripts"))
                        (mkdir-p (string-append share "/resources"))
                        (install-file "resources/help.txt"
                                      (string-append share "/resources"))
                        (install-file "scripts/kmk" bin)
                        (install-file "scripts/log.bash"
                                      (string-append share "/scripts/"))
                        (chmod (string-append bin "/kmk") #o755)))))))
    (build-system guile-build-system)
    (native-inputs (list guile-3.0))
    (inputs (list guile-3.0 bash-minimal))
    (propagated-inputs (list imagemagick))
    (synopsis "KMK")
    (description "KMK")
    (home-page "https://codeberg.org/jjba23/kmk")
    (license license:agpl3+)))

kmk
