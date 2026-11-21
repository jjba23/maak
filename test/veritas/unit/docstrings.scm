;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Unit tests for the two layouts `maak --list' prints: `format-task-doc', the
;; `name: documentation' blocks the listing had before the table and still
;; prints for `--list=plain', and `tasks-table', the aligned table a run gets by
;; default.  The name is printed through `bold-output', which wraps it in ANSI
;; escapes; the tests strip those before comparing, so they assert on what the
;; user sees rather than on the exact escape codes.

;;; Code:

(define-module (veritas unit docstrings)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak maak)
  #:export (spec))

(define escape-character
  (integer->char 27))

(define (strip-ansi str)
  "Return STR without ANSI escape sequences."
  (let loop ((chars (string->list str))
             (out '())
             (escaping? #f))
    (cond
      ((null? chars) (list->string (reverse out)))
      ((and escaping? (char=? (car chars) #\m))
       (loop (cdr chars) out #f))
      (escaping? (loop (cdr chars) out #t))
      ((char=? (car chars) escape-character)
       (loop (cdr chars) out #t))
      (else (loop (cdr chars) (cons (car chars) out) #f)))))

(define (strip-ansi-list lines)
  (map strip-ansi lines))

(define layout-suite
  (suite "Task docstring layout"
         #:concurrent? #f
         #:shuffle? #f
         (test "a single line stays inline"
               (assert-equal #:expect '("build: Compile the project.")
                             #:got (format-task-doc
                                    "build"
                                    "Compile the project.")))
         (test "two lines align the continuation"
               (assert-equal #:expect '("t: first" "   second")
                             #:got (format-task-doc "t"
                                                    "first\nsecond")))
         (test "one paragraph keeps every line aligned under the name"
               (assert-equal
                #:expect '("t: first" "   second" "   third" "   fourth")
                #:got (format-task-doc
                       "t"
                       "first\nsecond\nthird\nfourth")))
         (test "a paragraph break starts a block under the name"
               (assert-equal
                #:expect '("t:" "   first" "   second" "" "   third")
                #:got (strip-ansi-list (format-task-doc
                                        "t"
                                        "first\nsecond\n\nthird"))))
         (test "a wrapped paragraph aligns with the widest name"
               (assert-equal
                #:expect '("collect-artefacts: Gather the build output."
                          "                   The result is a tarball.")
                #:got (format-task-doc
                       "collect-artefacts"
                       "Gather the build output.\nThe result is a tarball.")))
         (test "an indented multi-line docstring keeps its alignment"
               (assert-equal
                #:expect '("t: first" "   second")
                #:got (format-task-doc
                       "t"
                       "\n     first\n     second\n     ")))
         (test "a block keeps its blank lines blank"
               (assert-equal
                #:expect '("t:" "   a" "" "   b" "" "   c")
                #:got (strip-ansi-list (format-task-doc
                                        "t"
                                        "a\n\nb\n\nc"))))
         (test "a docstring with no documentation prints the name only"
               (assert-equal #:expect '("t:")
                             #:got (strip-ansi-list
                                    (format-task-doc "t" #f)))
               (assert-equal #:expect '("t:")
                             #:got (strip-ansi-list
                                    (format-task-doc "t" "")))
               (assert-equal #:expect '("t:")
                             #:got (strip-ansi-list
                                    (format-task-doc "t" "   "))))
         (test "source indentation is removed before laying out"
               (assert-equal #:expect '("t: first" "   second")
                             #:got (format-task-doc "t"
                                                    "   first\n   second")))
         (test "the margin follows the length of the name"
               (assert-equal #:expect '("longname: a" "          b")
                             #:got (format-task-doc "longname" "a\nb")))
         (test "blank lines inside a block are not padded"
               (assert-equal #:expect '("t:" "   a" "" "   b")
                             #:got (strip-ansi-list
                                    (format-task-doc "t" "a\n\nb"))))
         (test "the name is emphasised with ANSI escapes"
               (assert-equal #:expect #t
                             #:got (let ((label (car (format-task-doc
                                                      "t" #f))))
                                     (and (string-contains label
                                                           (string-append
                                                            (string
                                                             escape-character)
                                                            "[1m"))
                                          #t))))))

(define (rows-of text)
  "Return the lines of TEXT, without the blank ones at either end.

The table begins with a blank line before its first entry and ends with one
after its last, and the internal blank lines -- the paragraph breaks inside a
docstring -- are what the layout is asserted on, so they are kept."
  (let ((lines (string-split (strip-ansi text) #\newline)))
    (reverse (drop-while string-null?
                         (reverse (drop-while string-null? lines))))))

(define* (made-task name doc #:key (needs '()) (example #f) (hidden? #f))
  "Return NAME as a task maak could list, with DOC and what it declares.

A procedure carries its own documentation and metadata, which is what a listing
reads, so a task is built here rather than loaded from a file: these are the
layout's tests and they should not depend on a fixture's docstrings.  The pair
is the shape `resolve-maak-tasks' hands a listing -- a name and the variable it
is bound to, not the procedure."
  (let ((value (list name doc)))
    (let ((proc (lambda () value)))
      (set-procedure-property! proc 'documentation doc)
      (task-metadata! proc #:depends needs #:hidden? hidden? #:example example)
      (cons (string->symbol name) (make-variable proc)))))

(define (two-tasks)
  "Return two tasks whose names are of different lengths."
  (list (made-task "t" "one line")
        (made-task "long" "another")))

(define table-suite
  (suite "The aligned listing"
         #:concurrent? #f
         #:shuffle? #f
         (test "a task with no documentation prints its name alone"
               (assert-equal #:expect '("  t")
                             #:got (rows-of (tasks-table (list (made-task "t" ""))))))
         (test "the documentation starts after the widest name in the listing"
               (assert-equal #:expect '("  t     one line" "" "  long  another")
                             #:got (rows-of (tasks-table (two-tasks)))))
         (test "the paragraph breaks inside a docstring are kept"
               (assert-equal #:expect '("  t  first" "" "     second")
                             #:got (rows-of
                                    (tasks-table
                                     (list (made-task "t" "first\n\nsecond"))))))
         (test "what a task needs first is printed under it, aligned with its documentation"
               (assert-equal #:expect '("  deploy  Ship it."
                                       "          needs: build, test")
                             #:got (rows-of
                                    (tasks-table
                                     (list (made-task "deploy" "Ship it."
                                                      #:needs '("build" "test")))))))
         (test "a long layout adds the invocation a task holds up as an example"
               (assert-equal #:expect '("  deploy  Ship it."
                                       "          example: maak deploy prod")
                             #:got (rows-of
                                    (tasks-table
                                     (list (made-task "deploy" "Ship it."
                                                      #:example "maak deploy prod"))
                                     #:long? #t))))
         (test "the table keeps a task that asked to be left out out of it"
               (assert-equal #:expect '("  keep  List me.")
                             #:got (rows-of
                                    (tasks-table
                                     (list (made-task "keep" "List me.")
                                           (made-task "tidy" "Not me."
                                                      #:hidden? #t))))))
         (test "a heading names the file and how many tasks it holds"
               (assert-equal #:expect #t
                             #:got (and (string-contains
                                         (tasks-table (two-tasks)
                                                      #:file "p/maak.scm")
                                         "2 tasks in p/maak.scm")
                                        #t)))
         (test "a listing with no file to name has no heading"
               (assert-equal #:expect '("  t     one line" "" "  long  another")
                             #:got (rows-of (tasks-table (two-tasks)))))
         (test "a name wider than the column keeps a line of its own"
               (assert-equal
                #:expect
                (list (string-append "  " "twenty-five-characters-here")
                      "                            Compile it.")
                #:got (rows-of
                       (tasks-table
                        (list (made-task "twenty-five-characters-here"
                                         "Compile it."))))))
         (test "a width folds a docstring that does not fit in it"
               (assert-true
                (every (lambda (line) (<= (string-length line) 40))
                       (rows-of (tasks-table
                                 (list (made-task
                                        "build"
                                        (string-join (make-list 12 "words") " ")))
                                 #:width 40)))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               layout-suite
               table-suite))
