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
             (gnu packages guile-xyz))

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
    (arguments
     (list
      #:source-directory "src"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'install-program-files
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (share (string-append #$output "/share")))
                (install-file "resources/help.txt"
                              (string-append share "/resources"))
                (install-file "scripts/maak" bin)
                (install-file "scripts/log.bash"
                              (string-append share "/scripts/"))
                (install-file "scripts/maak-completion.bash"
                              (string-append share "/scripts/"))
                (install-file "scripts/maak-completion.zsh"
                              (string-append share "/scripts/"))
                (install-file "scripts/maak-completion.fish"
                              (string-append share "/scripts/"))
                (chmod (string-append bin "/maak") #o755))))
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each (lambda (spec)
                          (let ((pattern (car spec))
                                (program (cadr spec))
                                (format-string (caddr spec)))
                            (substitute* "scripts/maak"
                              ((pattern)
                               (format #f format-string
                                       (search-input-file inputs
                                                          (string-append
                                                           "bin/" program)))))))
                        '(("readlink -f " "readlink" "~s -f ")
                          ("realpath " "realpath" "~s ")
                          ("dirname " "dirname" "~s ")
                          ("getopt " "getopt" "~s ")
                          ("exec guile " "guile" "exec ~s "))))))))
    (build-system guile-build-system)
    (inputs (list guile-3.0 bash-minimal coreutils util-linux))
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
