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

;;; Commentary:
;;
;; The single place the program's version is written by hand, together with the
;; handful of facts the CLI reports about itself.  `--version' prints it, the
;; Nix derivation reads this file while it evaluates, and the copies that cannot
;; read Scheme -- the RPM spec, the manual page and the README -- are rewritten
;; from it by the `sync-version' task, which reads this file rather than the
;; library it happens to be running under.  A release is therefore one edit,
;; not a search of the tree, and the suite fails while any copy disagrees with
;; this one.

;;; Code:

(define-module (maak version)
  ;; Deliberately not declarative.  A declarative module lets Guile inline its
  ;; bindings into the modules that use them, and the version is the one value
  ;; here that changes between builds: inlined, it would be baked into a
  ;; consumer's compiled file while that file's source stayed the same, and a
  ;; bump would leave `maak --version' answering with the version the consumer
  ;; was compiled against.  Compiled from source in a store the two agree, but
  ;; a warm cache should not decide what a release is called.
  #:declarative? #f
  #:export (maak-version maak-homepage maak-license maak-version-string
                         print-version))

(define maak-version
  "0.10.0")

(define maak-homepage
  "https://codeberg.org/jjba23/maak")

(define maak-license
  "GPL-3.0-or-later")

(define (maak-version-string)
  "Return the one-line version banner printed by `maak --version'."
  (format #f "maak ~a" maak-version))

(define* (print-version #:optional (port (current-output-port)))
  "Print the version banner, and the program's homepage, to PORT."
  (format port "~a\n~a\n~a\n"
          (maak-version-string)
          (format #f "License: ~a" maak-license)
          (format #f "Homepage: ~a" maak-homepage)))
