;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak formats)': the three readers a task file can be written in.
;; Each suite asserts on data rather than on text, because the data is what the
;; rest of maak consumes; the cases that assert on an error are the important
;; half, since a reader that accepted what it does not understand would hand a
;; task a command nobody wrote.
;;
;; No external parser is involved and no network is needed: the inputs are
;; strings written in the spec.

;;; Code:

(define-module (veritas unit formats)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak formats)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-formats-~a-~a" (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (with-scratch label thunk)
  (let ((dir (scratch-directory label)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk dir))
      (lambda ()
        (when (file-exists? dir)
          (parameterize ((quiet? #t))
            (delete-file-recursively dir)))))))

(define json-suite
  (suite "JSON"
         #:concurrent? #f
         #:shuffle? #f
         (test "an object becomes a string-keyed alist"
               (assert-equal #:expect '(("a" . 1) ("b" . "two"))
                             #:got (json-read-string "{\"a\": 1, \"b\": \"two\"}")))
         (test "an array becomes a list, in order"
               (assert-equal #:expect '(1 2 3)
                             #:got (json-read-string "[1, 2, 3]")))
         (test "true, false and null keep their meaning"
               (assert-equal #:expect '(#t #f null)
                             #:got (json-read-string "[true, false, null]")))
         (test "an escape is decoded"
               (assert-equal #:expect "a\nb"
                             #:got (json-read-string "\"a\\nb\"")))
         (test "a surrogate pair is one character"
               (assert-equal #:expect "😀"
                             #:got (json-read-string "\"\\ud83d\\ude00\"")))
         (test "a duplicate key is refused"
               (assert-error (lambda ()
                               (json-read-string "{\"a\": 1, \"a\": 2}"))))
         (test "text after the value is refused"
               (assert-error (lambda ()
                               (json-read-string "{} {}"))))
         (test "a trailing comma is refused"
               (assert-error (lambda ()
                               (json-read-string "[1,]"))))))

(define toml-suite
  (suite "TOML"
         #:concurrent? #f
         #:shuffle? #f
         (test "a key and a value become a pair"
               (assert-equal #:expect '(("version" . 1))
                             #:got (toml-read-string "version = 1")))
         (test "a table header nests what follows it"
               (assert-equal
                #:expect '(("tasks" ("build" ("command" "cargo" "build"))))
                #:got (toml-read-string
                       "[tasks.build]\ncommand = [\"cargo\", \"build\"]\n")))
         (test "strings, booleans, numbers and comments are read"
               (assert-equal
                #:expect '(("name" . "maak") ("ok" . #t) ("n" . 3))
                #:got (toml-read-string
                       "name = \"maak\" # the name\nok = true\nn = 3\n")))
         (test "an inline table becomes an alist"
               (assert-equal
                #:expect '(("env" ("A" . "1") ("B" . "2")))
                #:got (toml-read-string "env = { A = \"1\", B = \"2\" }")))
         (test "a key defined twice is refused"
               (assert-error (lambda ()
                               (toml-read-string "a = 1\na = 2"))))
         (test "an array of tables is refused rather than guessed"
               (assert-error (lambda ()
                               (toml-read-string "[[tasks]]\nname = \"build\""))))))

(define yaml-suite
  (suite "YAML"
         #:concurrent? #f
         #:shuffle? #f
         (test "a block mapping becomes an alist"
               (assert-equal #:expect '(("a" . 1) ("b" . 2))
                             #:got (yaml-read-string "a: 1\nb: 2\n")))
         (test "a nested mapping follows its key"
               (assert-equal
                #:expect '(("a" ("b" . 1) ("c" . 2)) ("d" . 3))
                #:got (yaml-read-string "a:\n  b: 1\n  c: 2\nd: 3\n")))
         (test "a block sequence becomes a list"
               (assert-equal #:expect '("cargo" "build")
                             #:got (yaml-read-string "- cargo\n- build\n")))
         (test "a flow sequence becomes a list"
               (assert-equal #:expect '(("command" "cargo" "build"))
                             #:got (yaml-read-string "command: [cargo, build]")))
         (test "a sequence of mappings keeps each item"
               (assert-equal
                #:expect '(("tasks"
                            (("name" . "build") ("command" "cargo" "build"))
                            (("name" . "test") ("command" . "cargo test"))))
                #:got (yaml-read-string
                       "tasks:\n  - name: build\n    command: [cargo, build]\n  - name: test\n    command: cargo test\n")))
         (test "a quoted scalar keeps its spaces"
               (assert-equal #:expect '(("doc" . "build it"))
                             #:got (yaml-read-string "doc: \"build it\"")))
         (test "a comment is not part of the value"
               (assert-equal #:expect '(("a" . 1))
                             #:got (yaml-read-string "a: 1 # one\n")))
         (test "a duplicate key is refused"
               (assert-error (lambda ()
                               (yaml-read-string "a: 1\na: 2\n"))))
         (test "an anchor is refused rather than guessed"
               (assert-error (lambda ()
                               (yaml-read-string "a: &x 1\nb: *x\n"))))))

(define data-suite
  (suite "Data files and accessors"
         #:concurrent? #f
         #:shuffle? #f
         (test "an extension names the format"
               (assert-equal #:expect '(json toml yaml yaml)
                             #:got (map data-file-format
                                        '("maak.json" "maak.toml"
                                          "maak.yaml" "maak.yml"))))
         (test "an unknown extension names no format"
               (assert-false (data-file-format "maak.scm")))
         (test "a mapping is told from a sequence"
               (assert-equal #:expect '(#t #f #t)
                             #:got (list (data-mapping? '(("a" . 1)))
                                         (data-mapping? '("a" "b"))
                                         (data-sequence? '("a" "b")))))
         (test "a reference reads a key and its default"
               (assert-equal #:expect '(1 missing)
                             #:got (list (data-ref '(("a" . 1)) "a")
                                         (data-ref '(("a" . 1)) "b" 'missing))))
         (test "a file is read by its extension"
               (with-scratch "read"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.json")))
                    (write-text path "{\"version\": 1}")
                    (assert-equal #:expect '(("version" . 1))
                                  #:got (read-data-file path))))))
         ;; The error is caught here rather than with `assert-error', because an
         ;; assertion is a task the runner executes after this form returns, and
         ;; by then the scratch directory would be gone.
         (test "an unknown extension is refused"
               (with-scratch "unknown"
                (lambda (dir)
                  (let ((path (in-vicinity dir "maak.ini")))
                    (write-text path "version = 1")
                    (let ((message
                           (catch #t
                             (lambda () (read-data-file path) #f)
                             (lambda (key . values)
                               (and (maak-error? (car values))
                                    (maak-error-message (car values)))))))
                      (assert-true (and message
                                        (string-contains message
                                                         "not a task file")
                                        #t)))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               json-suite
               toml-suite
               yaml-suite
               data-suite))
