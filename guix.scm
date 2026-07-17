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

(use-modules (guix gexp)
             (guix packages)
             (guix git-download)
             ((guix licenses)
              #:prefix license:)
             (guix build-system guile)
             (gnu packages)
             (gnu packages bash)
             (gnu packages base)
             (gnu packages linux)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (ice-9 match))

(define %source-dir
  (dirname (current-filename)))

(define-public maak
  (package
    (name "maak")
    (version "dev")
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (git-predicate %source-dir)))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src"
      #:modules '((guix build guile-build-system)
                  (guix build utils)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'install-program-files
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (share (string-append #$output "/share")))
                (install-file "resources/help.txt"
                              (string-append share "/resources"))
                (install-file "scripts/maak" bin)
                (chmod (string-append bin "/maak") #o755))))
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each (match-lambda
                          ((pattern program format-string)
                           (substitute* "scripts/maak"
                             ((pattern)
                              (format #f format-string
                                      (search-input-file inputs
                                                         (string-append "bin/"
                                                          program)))))))
                        '(("readlink -f " "readlink" "~s -f ")
                          ("realpath " "realpath" "~s ")
                          ("dirname " "dirname" "~s ")
                          ("getopt " "getopt" "~s ")
                          ("exec guile " "guile" "exec ~s ")))))
          (add-after 'build 'install-completions
            (lambda _
              (for-each (match-lambda
                          ((src-file dest-dir dest-name)
                           (let ((target-dir (string-append #$output dest-dir)))
                             (mkdir-p target-dir)
                             (copy-file src-file
                                        (string-append target-dir "/"
                                                       dest-name)))))
                        '(("scripts/maak-completion.bash"
                           "/share/bash-completion/completions" "maak")
                          ("scripts/maak-completion.fish"
                           "/share/fish/vendor_completions.d" "maak.fish")
                          ("scripts/maak-completion.zsh"
                           "/share/zsh/site-functions" "_maak"))))))))
    (inputs (list guile-3.0 bash-minimal coreutils util-linux))
    (home-page "https://codeberg.org/jjba23/maak")
    (synopsis "Command runner à la Make using Guile Scheme")
    (description
     "Maak is a command runner and control plane for your
projects.  It allows you to use the power of Lisp (Guile Scheme) to define
your tasks, build steps, repetitive tasks or other automation.

With Maak you can easily call external shell commands and integrate with
your existing scripts and tools.  It is inspired by the GNU Make utility
but it does away with a lot of the complexity that comes with its history.")
    (license license:gpl3+)))

maak
