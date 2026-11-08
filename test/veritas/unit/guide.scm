;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; The guide is one page per topic under docs/, indexed by docs/index.org, with
;; the README as the landing page.  That shape only holds while the links do:
;; a page that nobody lists is a page nobody finds, and a `file:' link that
;; points at a moved page is a dead end a reader discovers before the author
;; does.  Both were written by hand in one afternoon, so both are checked here.
;;
;; The runner is started from the project root -- the same assumption the
;; integration specs make about their fixture paths -- so a page is read at the
;; path it has in the checkout, and a link is resolved against the directory
;; that page lives in.
;;
;; Every case collects what is wrong and asserts on the whole list, so one run
;; names every broken link rather than the first one.

;;; Code:

(define-module (veritas unit guide)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:export (spec))

(define docs-dir "docs")

(define (slurp path)
  "Return the whole of PATH as a string."
  (call-with-input-file path get-string-all))

(define (org-file? name)
  (string-suffix? ".org" name))

(define (docs-pages)
  "Return the pages of the guide under DOCS-DIR, as paths from the root."
  (sort (map (lambda (name) (string-append docs-dir "/" name))
             (scandir docs-dir org-file?))
        string<?))

(define (guide-pages)
  "Return every page of the guide: the README, the news and the docs pages."
  (cons* "README.org" "NEWS.org" (docs-pages)))

(define (directory-of page)
  "Return the prefix a link in PAGE is resolved against.

Every page is either at the root or one directory down in DOCS-DIR, which is
what makes a prefix enough; a page that moved deeper would be joined to the
wrong path, and the link cases are what would say so."
  (if (string-prefix? (string-append docs-dir "/") page)
      (string-append docs-dir "/")
      ""))

(define (file-links page)
  "Return the `file:' targets PAGE links to, anchors and all."
  (map match:substring
       (list-matches "\\[\\[file:([^]]+)\\]" (slurp page))
       (circular-list 1)))

(define (heading-links page)
  "Return the `[[*Heading]]' links PAGE still carries."
  (map match:substring (list-matches "\\[\\[\\*[^]]+\\]\\]" (slurp page))))

(define (split-anchor target)
  "Return two values: TARGET without its `::#anchor', and the anchor or #f."
  (let ((mark (string-contains target "::#")))
    (if mark
        (values (substring target 0 mark) (substring target (+ mark 3)))
        (values target #f))))

(define (custom-ids path)
  "Return the CUSTOM_IDs PATH declares."
  (map (lambda (found) (match:substring found 1))
       (list-matches ":CUSTOM_ID:[ \t]+([^ \t\n]+)" (slurp path))))

(define (broken-links page)
  "Return the links of PAGE that name a file which is not there."
  (filter-map (lambda (target)
                (let-values (((path anchor) (split-anchor target)))
                  (and (not (string-prefix? "http" target))
                       (not (file-exists?
                             (string-append (directory-of page) path)))
                       (string-append page " -> " target))))
              (file-links page)))

(define (broken-anchors page)
  "Return the links of PAGE whose `#anchor' names no CUSTOM_ID."
  (filter-map (lambda (target)
                (let-values (((path anchor) (split-anchor target)))
                  (and anchor
                       (let ((full (string-append (directory-of page) path)))
                         (and (file-exists? full)
                              (not (member anchor (custom-ids full)))
                              (string-append page " -> " target))))))
              (file-links page)))

(define (unlisted-pages)
  "Return the pages under DOCS-DIR that the index does not link to."
  (let ((index (slurp (string-append docs-dir "/index.org"))))
    (filter (lambda (page)
              (let* ((name (substring page (+ 1 (string-length docs-dir))))
                     (self? (string=? name "index.org")))
                (and (not self?)
                     (not (string-contains index
                                           (string-append "file:" name))))))
            (docs-pages))))

(define guide-suite
  (suite "The guide"
         #:concurrent? #f
         #:shuffle? #f
         (test "every page of the guide is a file that exists"
               (assert-equal #:expect '()
                             #:got (filter (lambda (page)
                                             (not (file-exists? page)))
                                           (guide-pages))))

         (test "every file: link in the guide names a file that exists"
               (assert-equal #:expect '()
                             #:got (append-map broken-links (guide-pages))))

         (test "every ::#anchor names a CUSTOM_ID in the page it points at"
               (assert-equal #:expect '()
                             #:got (append-map broken-anchors (guide-pages))))

         (test "every page under docs/ is linked from the index"
               (assert-equal #:expect '()
                             #:got (unlisted-pages)))

         (test "no page links to a heading with [[*Heading]]"
               (assert-equal #:expect '()
                             #:got (append-map heading-links (guide-pages))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               guide-suite))
