;; ggg: guile glyph generator

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; ggg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ggg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with ggg.  If not, see <https://www.gnu.org/licenses/>.

(define-module (ggg svg)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 getopt-long)
  #:export (get-path-for-svg mk-simple-svg ggg-logos length-for-text))

(define* (get-path-for-svg name
                           #:key resources)
  (get-string-all (open-input-file (format #f "~a/svg-paths/~a" resources
                                           name))))

(define* (mk-simple-svg id
                        #:key resources
                        (width 18)
                        (height 18)
                        (view-box "0 0 18 20")
                        (fill "#ffffff")
                        (fill-rule 'nonzero)
                        (clip-rule 'nonzero)
                        (x 6)
                        (y 5))
  `((svg (@ (width ,width)
            (height ,height)
            (viewBox ,view-box)
            (x ,x)
            (y ,y)
            (xmlns "http://www.w3.org/2000/svg")
            (fill none))
         (path (@ (fill ,fill)
                  (fill-rule ,fill-rule)
                  (clip-rule ,clip-rule)
                  (d ,(get-path-for-svg id
                                        #:resources resources))) ""))))

(define* (length-for-text text-length
                          #:key (font-size 12))
  "Estimates text container widths in pixels. It looks up a rough width per character based on the font size.
Then, it multiplies this by the number of characters and adds some padding.
Finally, it slightly adjusts the width based on whether the text is very short, short,
medium, or long to make the badge look better. It rounds the final width to a whole number."
  (let* ((base-char-width (case font-size
                            ((10)
                             8)
                            ((11)
                             9)
                            ((12)
                             9.5)
                            ((14)
                             10.5)
                            ((16)
                             11.5)
                            (else 7)))
         (horizontal-padding 16)
         (scaling-factor (cond
                           ((> text-length 7)
                            0.8)
                           ((> text-length 6)
                            0.9)
                           ((> text-length 5)
                            1.0)
                           ((< text-length 5)
                            1.3)
                           (else 1.0)))
         (calculated-width (+ horizontal-padding
                              (* base-char-width text-length scaling-factor))))
    (round calculated-width)))

(define* (ggg-logos #:key resources)
  `((emacs unquote
           (mk-simple-svg 'emacs
                          #:resources resources
                          #:view-box "0 0 24 26"))
    (lambda unquote
      (mk-simple-svg 'lambda
                     #:resources resources))
    (gnu unquote
         (mk-simple-svg 'gnu
                        #:resources resources
                        #:view-box "0 0 24 26"))
    (scala unquote
           (mk-simple-svg 'scala
                          #:resources resources
                          #:view-box "0 0 36 26"
                          #:y 3))
    (scales unquote
            (mk-simple-svg 'scales
                           #:resources resources
                           #:view-box "0 0 20 20"))
    (paint unquote
           (mk-simple-svg 'paint
                          #:resources resources
                          #:view-box "0 0 42 42"
                          #:y 7))
    (language unquote
              (mk-simple-svg 'language
                             #:resources resources
                             #:view-box "0 0 20 24"
                             #:y 4))
    (hashtag unquote
             (mk-simple-svg 'hashtag
                            #:resources resources
                            #:fill-rule "evenodd"
                            #:clip-rule "evenodd"
                            #:view-box "0 0 30 30"
                            #:y 6))
    (gear unquote
          (mk-simple-svg 'gear
                         #:resources resources
                         #:view-box "0 0 640 640"
                         #:y 6))
    (bull unquote
          (mk-simple-svg 'bull
                         #:resources resources
                         #:view-box "0 0 30 30"))
    (fish unquote
          (mk-simple-svg 'fish
                         #:resources resources
                         #:view-box "0 0 250 100"))))
