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
             (gnu packages guile-xyz))

(define %source-dir
  (dirname (current-filename)))

(define-public maak
  (package
    (name "maak")
    (version "0.1.3")
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
                        (install-file "scripts/maak" bin)
                        (install-file "scripts/log.bash"
                                      (string-append share "/scripts/"))
                        (chmod (string-append bin "/maak") #o755)))))))
    (build-system guile-build-system)
    (native-inputs (list guile-3.0))
    (inputs (list guile-3.0 bash-minimal))
    (synopsis "Command runner à la Make using Guile Scheme")
    (description
     "Maak is a command runner and control plane for your
projects.  It allows you to use the power of Lisp (Guile Scheme) to define
your tasks, build steps, repetitive tasks or other automation.

With Maak you can easily call external shell commands and integrate with
your existing scripts and tools.")
    (home-page "https://codeberg.org/jjba23/maak")
    (license license:gpl3+)))

maak
