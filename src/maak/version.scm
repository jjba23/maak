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
;; The single source of truth for the program's version and the handful of
;; facts the CLI reports about itself.  Package recipes (Guix, Nix, RPM, DEB)
;; and the `--version' flag all read the version from here, so bumping a
;; release is one edit rather than a scavenger hunt.

;;; Code:

(define-module (maak version)
  #:declarative? #t
  #:export (maak-version
            maak-homepage
            maak-license
            maak-version-string
            print-version))

(define maak-version
  "0.8.17")

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
