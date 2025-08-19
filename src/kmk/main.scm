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

(define-module (ggg main)
  #:declarative? #f
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 getopt-long)
  #:use-module (ggg sxml)
  #:use-module (ggg prelude)
  #:use-module (ggg glyph)
  #:export (main render-glyphs))

(define* (render-glyphs #:key file
                        resources
                        destination
                        with-stdin?
                        with-convert?
                        with-stdout?)
  (let* ((clean-dist-cmd (format #f "rm -rfv ~a" destination))
         (mkdir-dist-cmd (format #f "mkdir -p ~a" destination)))
    (if with-stdin?
        ;; Read from stdin
        (begin
          (unless with-stdout?
            (log-info "\tloading glyphs from stdin"))
          (eval-string (get-string-all (current-input-port))))
        ;; Read from file
        (begin
          (unless with-stdout?
            (log-info "\tloading glyphs file: ~a" file))
          (load file)))

    ;; Prepare dist
    (unless with-stdout?
      (log-info "\texecuting: ~a" clean-dist-cmd)
      (syscall clean-dist-cmd))
    (unless with-stdout?
      (log-info "\texecuting: ~a" mkdir-dist-cmd)
      (syscall mkdir-dist-cmd))

    (unless with-stdout?
      (log-info "+-----------------------------------------------+")
      (log-info "Start producing SVG images"))

    (define produce-svg-to-file
      (lambda (vector-image)
        (log-info "\tproducing ~a/~a.svg" destination
                  (assoc-ref vector-image
                             'id))

        (with-output-to-file (format #f "~a/~a.svg" destination
                                     (assoc-ref vector-image
                                                'id))
          (lambda ()
            (sxml->xml (mk-glyph vector-image
                                 #:resources resources))))))

    (define produce-svg-to-stdout
      (lambda (vector-image)
        (newline)
        (newline)
        (sxml->xml (mk-glyph vector-image
                             #:resources resources)
                   (current-output-port))
        (newline)
        (newline)))

    (if with-stdout?
        (for-each produce-svg-to-stdout
                  (get-setting 'ggg-glyphs))
        (for-each produce-svg-to-file
                  (get-setting 'ggg-glyphs)))

    (when with-convert?
      (log-info "+-----------------------------------------------+")
      (log-info "Start producing PNG images")
      (map (lambda (vector-image)
             (log-info "\tproducing ~a/~a.png" destination
                       (assoc-ref vector-image
                                  'id))
             (syscall (format #f
                       "convert -quality 100 -density 1000 -background none -resize 10% ~a/~a.svg ~a/~a.png"
                       destination
                       (assoc-ref vector-image
                                  'id)
                       destination
                       (assoc-ref vector-image
                                  'id))))
           (get-setting 'ggg-glyphs)))

    (unless with-stdout?
      (log-info "+-----------------------------------------------+")
      (log-info "GGG is done generating images\n"))))

(define* (main #:key (args (command-line)))
  
  (let* ((option-spec '((destination (single-char #\d)
                                     (value #t)
                                     (required? #f))
                        (resources (single-char #\r)
                                   (value #t))
                        (stdin (single-char #\i)
                               (value #f)
                               (required? #f))
                        (stdout (single-char #\o)
                                (value #f)
                                (required? #f))
                        (convert (single-char #\c)
                                 (value #f)
                                 (required? #f))
                        (file (single-char #\f)
                              (value #t)
                              (required? #f))))
         (options (getopt-long args option-spec))
         (file (option-ref options
                           'file #f))
         (resources (option-ref options
                                'resources #f))
         (destination (option-ref options
                                  'destination "./dist"))
         (with-convert? (option-ref options
                                    'convert #f))
         (with-stdout? (option-ref options
                                   'stdout #f))
         (with-stdin? (option-ref options
                                  'stdin #f)))
    (when (and (not file)
               (not with-stdin?))
      (error "Either the file or stdin options are required!"))

    (when (and (not with-stdout?)
               (not destination))
      (error "Either the file or stdin options are required!"))

    (when (and with-stdout? with-convert?)
      (error "Cannot combine stdout and convert options!"))

    (unless with-stdout?
      (log-info "GGG: Guile Glyph Generator"))

    (render-glyphs #:file file
                   #:with-stdin? with-stdin?
                   #:with-stdout? with-stdout?
                   #:with-convert? with-convert?
                   #:resources resources
                   #:destination destination)))

