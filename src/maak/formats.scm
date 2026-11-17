;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Reading JSON, TOML and YAML without a dependency.
;;
;; A Maak file is Scheme, and Scheme is the whole language.  The formats here
;; exist for the task files documented on docs/task-files.org, where a project
;; wants a list of tasks and none of the language, and for `(maak cargo)', which
;; reads `cargo metadata' but cannot ask a Guile build to carry a JSON library
;; just for that.
;;
;; Each reader understands a documented subset and refuses the rest.  That is
;; the important half: a parser that guessed at an anchor, a multi-line scalar
;; or a second document would hand a task a command it did not read.  An
;; unsupported construct is a maak error with its line, never a silently
;; different value.
;;
;; The data representation is deliberately small:
;;
;;   mapping   an alist of (string . value) in file order, so a listing keeps
;;             the order the file wrote;
;;   sequence  a proper list;
;;   scalar    a string, an exact or inexact number, #t, #f, or the symbol
;;             `null' for a missing value.
;;
;; An empty list is both an empty mapping and an empty sequence; the schema
;; that consumes it knows which it meant.  `data-mapping?' and
;; `data-sequence?' answer for every other value.
;;
;; Nothing here evaluates anything.  The text is parsed into data, and the
;; caller decides what the data means, which is why a task file can be read on
;; a machine that has no reason to trust it.

;;; Code:

(define-module (maak formats)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (data-mapping?
            data-sequence?
            data-ref
            data-keys
            data-file-format
            read-data-file
            json-read-string
            toml-read-string
            yaml-read-string))

;; ====== Reading, in general ======

;; Where the text being read came from, and the text itself, so a parser deep
;; in a recursive descent can report a line without carrying both through every
;; call.
(define current-source (make-parameter "<input>"))
(define current-text (make-parameter ""))

(define (position-in text index)
  "Return `(LINE . COLUMN)' of INDEX in TEXT, both counting from one."
  (let loop ((i 0) (line 1) (column 1))
    (if (or (>= i index) (>= i (string-length text)))
        (cons line column)
        (if (char=? (string-ref text i) #\newline)
            (loop (+ i 1) (+ line 1) 1)
            (loop (+ i 1) line (+ column 1))))))

(define* (fail index message
               #:key
               (hint "Check the syntax near the reported position, or use a task file in a format maak reads whole."))
  "Raise a maak error for MESSAGE, at INDEX of the text being read."
  (let* ((text (current-text))
         (position (position-in text index)))
    (raise-maak-error
     (format #f "~a: ~a at line ~a, column ~a"
             (current-source) message (car position) (cdr position))
     #:hint hint)))

(define* (fail-line line message
                    #:key
                    (hint "Check the syntax on that line, or use a task file in a format maak reads whole."))
  "Raise a maak error for MESSAGE, on LINE of the text being read."
  (raise-maak-error
   (format #f "~a: ~a at line ~a" (current-source) message line)
   #:hint hint))

;; ====== Mappings and sequences ======

(define (data-mapping? value)
  "Return #t when VALUE is a mapping: a list of string-keyed pairs.

An empty list is a mapping as well as an empty sequence; a caller that has to
tell them apart knows which one it asked for."
  (and (list? value)
       (every (lambda (entry)
                (and (pair? entry) (string? (car entry))))
              value)))

(define (data-sequence? value)
  "Return #t when VALUE is a sequence rather than a mapping."
  (and (list? value)
       (not (and (pair? value)
                 (pair? (car value))
                 (string? (car (car value)))))))

(define* (data-ref mapping key #:optional (default #f))
  "Return the value KEY names in MAPPING, or DEFAULT when it names none."
  (unless (data-mapping? mapping)
    (raise-maak-error
     (format #f "expected a mapping to read ~s from, got ~s" key mapping)
     #:hint "A task file's tables are mappings; check the shape above the key."))
  (let ((entry (assoc key mapping)))
    (if entry (cdr entry) default)))

(define (data-keys mapping)
  "Return the keys of MAPPING, in the order the file wrote them."
  (map car mapping))

(define (mapping-get mapping key)
  "Return the value KEY names in MAPPING, or `missing'."
  (let ((entry (and (list? mapping) (assoc key mapping))))
    (if entry (cdr entry) 'missing)))

(define (mapping-set mapping key value)
  "Return MAPPING with KEY set to VALUE, keeping the order of the other keys."
  (cond
   ((null? mapping) (list (cons key value)))
   ((equal? (caar mapping) key) (cons (cons key value) (cdr mapping)))
   (else (cons (car mapping) (mapping-set (cdr mapping) key value)))))

;; ====== JSON ======
;;
;; RFC 8259, which is small enough to read whole: objects, arrays, strings,
;; numbers, `true', `false' and `null'.  The reader is a descent over a vector
;; of characters, so an index is an offset into the original text as well.

(define (json-char-at vector index) (vector-ref vector index))

(define (json-skip-space vector index)
  "Return the index of the first character at or after INDEX that is not space."
  (let loop ((i index))
    (if (and (< i (vector-length vector))
             (memv (json-char-at vector i)
                   '(#\space #\tab #\newline #\return)))
        (loop (+ i 1))
        i)))

(define json-number-pattern
  (make-regexp "^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?$"))

(define (json-code-point vector index hex-length)
  "Return `(CODE . NEXT)' reading HEX-LENGTH hexadecimal digits at INDEX."
  (let* ((end (+ index hex-length))
         (hex (if (<= end (vector-length vector))
                  (list->string
                   (map (lambda (k) (json-char-at vector k))
                        (iota hex-length index)))
                  ""))
         (code (and (= (string-length hex) hex-length)
                    (string->number hex 16))))
    (unless code
      (fail index
            (format #f "a \\u escape needs ~a hexadecimal digits" hex-length)))
    (cons code end)))

(define (json-decode-escape vector index)
  "Decode the escape whose backslash is at INDEX, and return `(STRING . NEXT)'."
  (let* ((escaped (json-char-at vector (+ index 1)))
         (simple (case escaped
                   ((#\") #\")
                   ((#\\) #\\)
                   ((#\/) #\/)
                   ((#\b) (integer->char 8))
                   ((#\f) (integer->char 12))
                   ((#\n) #\newline)
                   ((#\r) #\return)
                   ((#\t) #\tab)
                   (else #f))))
    (cond
     (simple (cons (string simple) (+ index 2)))
     ((char=? escaped #\u)
      (let* ((first (json-code-point vector (+ index 2) 4))
             (code (car first))
             (next (cdr first)))
        (if (and (>= code #xD800) (<= code #xDBFF)
                 (<= (+ next 2) (vector-length vector))
                 (char=? (json-char-at vector next) #\\)
                 (char=? (json-char-at vector (+ next 1)) #\u))
            (let* ((second (json-code-point vector (+ next 2) 4))
                   (low (car second)))
              (if (and (>= low #xDC00) (<= low #xDFFF))
                  (cons (string (integer->char
                                 (+ #x10000
                                    (* (- code #xD800) #x400)
                                    (- low #xDC00))))
                        (cdr second))
                  (cons (string (integer->char code)) next)))
            (cons (string (integer->char code)) next))))
     (else
      (fail (+ index 1)
            (format #f "\\~a is not a JSON escape" escaped))))))

(define (json-read-string-at vector index)
  "Read the JSON string that starts at INDEX and return `(STRING . NEXT)'."
  (let loop ((i (+ index 1)) (chars '()))
    (when (>= i (vector-length vector))
      (fail index "unterminated string"))
    (let ((char (json-char-at vector i)))
      (cond
       ((char=? char #\")
        (cons (list->string (reverse chars)) (+ i 1)))
       ((char=? char #\\)
        (when (>= (+ i 1) (vector-length vector))
          (fail i "a backslash ends the file"))
        (let ((decoded (json-decode-escape vector i)))
          (loop (cdr decoded) (cons (string-ref (car decoded) 0) chars))))
       ((< (char->integer char) 32)
        (fail i "a control character must be escaped in a JSON string"))
       (else (loop (+ i 1) (cons char chars)))))))

(define (json-read-number-at vector index)
  "Read the JSON number that starts at INDEX and return `(NUMBER . NEXT)'."
  (let loop ((i index))
    (if (and (< i (vector-length vector))
             (memv (json-char-at vector i)
                   '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                        #\- #\+ #\. #\e #\E)))
        (loop (+ i 1))
        (let* ((token (list->string
                       (map (lambda (k) (json-char-at vector k))
                            (iota (- i index) index))))
               (number (and (regexp-exec json-number-pattern token)
                            (string->number token))))
          (unless number
            (fail index (format #f "~s is not a JSON number" token)))
          (cons number i)))))

(define (json-read-value-at vector index)
  "Read the JSON value at INDEX and return `(VALUE . NEXT)'."
  (let ((start (json-skip-space vector index)))
    (when (>= start (vector-length vector))
      (fail start "the file ends where a value was expected"))
    (let ((char (json-char-at vector start)))
      (cond
       ((char=? char #\{) (json-read-object-at vector start))
       ((char=? char #\[) (json-read-array-at vector start))
       ((char=? char #\") (json-read-string-at vector start))
       ((or (char=? char #\-) (char-numeric? char))
        (json-read-number-at vector start))
       ((char=? char #\t)
        (if (json-word-at? vector start "true")
            (cons #t (+ start 4))
            (fail start "expected `true'")))
       ((char=? char #\f)
        (if (json-word-at? vector start "false")
            (cons #f (+ start 5))
            (fail start "expected `false'")))
       ((char=? char #\n)
        (if (json-word-at? vector start "null")
            (cons 'null (+ start 4))
            (fail start "expected `null'")))
       (else (fail start (format #f "unexpected character ~s" char)))))))

(define (json-word-at? vector index word)
  "Return #t when WORD starts at INDEX of VECTOR."
  (let ((end (+ index (string-length word))))
    (and (<= end (vector-length vector))
         (string=? word
                   (list->string
                    (map (lambda (k) (vector-ref vector k))
                         (iota (string-length word) index)))))))

(define (json-after-comma-next vector index closing)
  "Return the index after the comma at INDEX, refusing a trailing comma."
  (let ((next (json-skip-space vector (+ index 1))))
    (when (and (< next (vector-length vector))
               (char=? (json-char-at vector next) closing))
      (fail index "a trailing comma is not allowed here"))
    next))

(define (json-read-object-at vector index)
  "Read the JSON object at INDEX and return `(ALIST . NEXT)'."
  (let loop ((i (json-skip-space vector (+ index 1))) (pairs '()))
    (when (>= i (vector-length vector))
      (fail index "unterminated object"))
    (if (char=? (json-char-at vector i) #\})
        (cons (reverse pairs) (+ i 1))
        (begin
          (unless (char=? (json-char-at vector i) #\")
            (fail i "an object key must be a string"))
          (let* ((key-result (json-read-string-at vector i))
                 (key (car key-result))
                 (colon (json-skip-space vector (cdr key-result))))
            (when (or (>= colon (vector-length vector))
                      (not (char=? (json-char-at vector colon) #\:)))
              (fail colon "expected `:' after an object key"))
            (when (assoc key pairs)
              (fail i (format #f "duplicate key ~s" key)))
            (let* ((value-result
                    (json-read-value-at vector (json-skip-space vector (+ colon 1))))
                   (pairs (cons (cons key (car value-result)) pairs))
                   (next (json-skip-space vector (cdr value-result))))
              (when (>= next (vector-length vector))
                (fail next "unterminated object"))
              (cond
               ((char=? (json-char-at vector next) #\,)
                (loop (json-after-comma-next vector next #\}) pairs))
               ((char=? (json-char-at vector next) #\})
                (cons (reverse pairs) (+ next 1)))
               (else (fail next "expected `,' or `}' after a value")))))))))

(define (json-read-array-at vector index)
  "Read the JSON array at INDEX and return `(LIST . NEXT)'."
  (let loop ((i (json-skip-space vector (+ index 1))) (items '()))
    (when (>= i (vector-length vector))
      (fail index "unterminated array"))
    (if (char=? (json-char-at vector i) #\])
        (cons (reverse items) (+ i 1))
        (let* ((value-result (json-read-value-at vector i))
               (items (cons (car value-result) items))
               (next (json-skip-space vector (cdr value-result))))
          (when (>= next (vector-length vector))
            (fail next "unterminated array"))
          (cond
           ((char=? (json-char-at vector next) #\,)
            (loop (json-after-comma-next vector next #\]) items))
           ((char=? (json-char-at vector next) #\])
            (cons (reverse items) (+ next 1)))
           (else (fail next "expected `,' or `]' after a value")))))))

(define* (json-read-string text #:key (source "<json>"))
  "Read TEXT as one JSON value.

Objects become string-keyed alists, arrays lists, `null' the symbol `null',
and a duplicate key is an error rather than a value that silently wins."
  (parameterize ((current-source source)
                 (current-text text))
    (let* ((vector (list->vector (string->list text)))
           (result (json-read-value-at vector 0))
           (end (json-skip-space vector (cdr result))))
      (unless (= end (vector-length vector))
        (fail end "unexpected characters after the JSON value"))
      (car result))))

;; ====== TOML ======
;;
;; The subset is the one a task file or a `pyproject.toml' needs: comments,
;; `[table]' headers, `[[arrays of tables]]', dotted keys, keys, basic and
;; literal strings, integers, floats, booleans, and arrays and inline tables
;; that may span lines.  Multi-line strings are refused rather than
;; approximated, and a date or a time comes back as the text it was written as.

(define (toml-strip-comment line)
  "Return LINE without a trailing comment, respecting its strings."
  (let loop ((i 0) (quoting #f))
    (if (>= i (string-length line))
        line
        (let ((char (string-ref line i)))
          (cond
           ((eq? quoting #\")
            (cond ((char=? char #\\) (loop (+ i 2) quoting))
                  ((char=? char #\") (loop (+ i 1) #f))
                  (else (loop (+ i 1) quoting))))
           ((eq? quoting #\')
            (if (char=? char #\') (loop (+ i 1) #f) (loop (+ i 1) quoting)))
           ((or (char=? char #\") (char=? char #\')) (loop (+ i 1) char))
           ((char=? char #\#) (substring line 0 i))
           (else (loop (+ i 1) #f)))))))

(define (toml-line-depth text depth)
  "Return the bracket DEPTH TEXT leaves, counting `[' and `{' out and in.

A quote is entered and left as the line spells it, so a bracket inside a string
is not counted; a string that does not close on its line leaves the depth alone
and the value reader refuses the line, which is why the quote is not carried to
the next line."
  (let loop ((i 0) (depth depth) (quoting #f))
    (if (>= i (string-length text))
        depth
        (let ((char (string-ref text i)))
          (cond
           ((eq? quoting #\")
            (cond ((char=? char #\\) (loop (+ i 2) depth quoting))
                  ((char=? char #\") (loop (+ i 1) depth #f))
                  (else (loop (+ i 1) depth quoting))))
           ((eq? quoting #\')
            (if (char=? char #\') (loop (+ i 1) depth #f)
                (loop (+ i 1) depth quoting)))
           ((or (char=? char #\") (char=? char #\')) (loop (+ i 1) depth char))
           ((memv char '(#\[ #\{)) (loop (+ i 1) (+ depth 1) #f))
           ((memv char '(#\] #\})) (loop (+ i 1) (max 0 (- depth 1)) #f))
           (else (loop (+ i 1) depth #f)))))))

(define (toml-logical-lines text)
  "Return TEXT's non-blank lines as `(LINE-NUMBER TEXT)' pairs.

A line that opens a bracket and does not close it on the same line is joined to
the lines that follow until it does, because TOML lets an array or an inline
table span as many lines as it likes and the readers below want one line per
value.  The number reported is the line the value started on."
  (let loop ((lines (string-split text #\newline))
             (number 1)
             (kept '())
             (pending #f)
             (start #f)
             (depth 0))
    (if (null? lines)
        (reverse (if pending
                     (cons (list start (string-trim-both pending)) kept)
                     kept))
        (let* ((stripped (string-trim-both (toml-strip-comment (car lines))))
               (depth (toml-line-depth stripped depth)))
          (cond
           ((and (not pending) (string=? stripped ""))
            (loop (cdr lines) (+ number 1) kept #f #f 0))
           (else
            (let ((pending (if pending
                               (string-append pending " " stripped)
                               stripped))
                  (start (or start number)))
              (if (zero? depth)
                  (loop (cdr lines) (+ number 1)
                        (cons (list start (string-trim-both pending)) kept)
                        #f #f 0)
                  (loop (cdr lines) (+ number 1) kept pending start depth)))))))))

(define (toml-key-parts key-text line)
  "Return KEY-TEXT as a list of key strings, splitting dotted keys."
  (let loop ((chars (string->list key-text)) (current '()) (parts '()) (quoting #f))
    (define (finish)
      (let ((text (string-trim-both (list->string (reverse current)))))
        (if (string=? text "")
            parts
            (cons text parts))))
    (cond
     ((null? chars)
      (reverse (finish)))
     ((eq? quoting #\")
      (let ((char (car chars)))
        (cond ((char=? char #\\) (loop (cddr chars) (cons (cadr chars) current) parts quoting))
              ((char=? char #\") (loop (cdr chars) current parts #f))
              (else (loop (cdr chars) (cons char current) parts quoting)))))
     ((eq? quoting #\')
      (let ((char (car chars)))
        (if (char=? char #\') (loop (cdr chars) current parts #f)
            (loop (cdr chars) (cons char current) parts quoting))))
     ((or (char=? (car chars) #\") (char=? (car chars) #\'))
      (loop (cdr chars) current parts (car chars)))
     ((char=? (car chars) #\.)
      (loop (cdr chars) '() (finish) #f))
     (else (loop (cdr chars) (cons (car chars) current) parts #f)))))

(define (toml-ensure-key! line key)
  "Return KEY after checking it is a name this reader accepts."
  (unless (and (string? key) (not (string=? key "")))
    (fail-line line "an empty key names nothing"))
  (unless (string-every (lambda (char)
                          (or (or (char-alphabetic? char) (char-numeric? char))
                              (memv char '(#\_ #\-))))
                        key)
    (fail-line line (format #f "~s is not a bare key" key)))
  key)

(define (toml-lookup table key)
  "Return the value KEY names in TABLE, or `missing'.

TABLE is a mapping, or the list an array of tables holds, whose elements KEY
indexes as a number."
  (cond
   ((exact-integer? key)
    (if (< key (length table)) (list-ref table key) 'missing))
   ((data-mapping? table) (mapping-get table key))
   (else 'missing)))

(define (toml-store table key value)
  "Return TABLE with KEY set to VALUE, keeping the order of the other keys."
  (cond
   ((exact-integer? key)
    (map (lambda (item index) (if (= index key) value item))
         table (iota (length table))))
   (else (mapping-set (if (data-mapping? table) table '()) key value))))

(define (toml-at tree path)
  "Return the value PATH names in TREE, or `missing' when it names none."
  (fold (lambda (key table) (toml-lookup table key)) tree path))

(define (toml-parent tree path line)
  "Return TREE with every key of PATH present as a mapping."
  (if (null? path)
      (if (or (null? tree) (data-mapping? tree))
          tree
          (fail-line line "a key is used as a table and as a value"))
      (let* ((key (car path))
             (existing (mapping-get tree key))
             (child (cond ((eq? existing 'missing) '())
                          ((null? existing) '())
                          ((data-mapping? existing) existing)
                          (else (fail-line line "a key is used as a table and as a value")))))
        (mapping-set (if (data-mapping? tree) tree '())
                     key
                     (toml-parent child (cdr path) line)))))

(define (toml-put tree path value)
  "Return TREE with VALUE stored at PATH, replacing whatever table was there."
  (if (null? path)
      value
      (let* ((key (car path))
             (existing (toml-lookup tree key)))
        (toml-store tree key
                    (toml-put (if (eq? existing 'missing) '() existing)
                              (cdr path) value)))))

(define (toml-table-list parent name line)
  "Return the list an array of tables named NAME appends to, in PARENT.

A name that already holds a table rather than a list of them is refused here:
TOML cannot have both, and a reader that picked one would lose the other."
  (let ((existing (mapping-get parent name)))
    (cond
     ((or (eq? existing 'missing) (null? existing)) '())
     ((data-mapping? existing)
      (fail-line line
                 (format #f "~a is a table, not an array of tables" name)))
     (else existing))))

(define (toml-set tree path value line)
  "Return TREE with VALUE at PATH, refusing to overwrite a table with a value."
  (if (null? path)
      value
      (let ((key (car path))
            (rest (cdr path)))
        ;; A number among the keys is an element of an array of tables, which
        ;; the header created and this call only descends into.
        (if (exact-integer? key)
            (toml-store tree key
                        (toml-set (toml-lookup tree key) rest value line))
            (let ((existing (mapping-get tree key)))
              (cond
               ((null? rest)
                (if (or (eq? existing 'missing) (null? existing))
                    (mapping-set (if (data-mapping? tree) tree '()) key value)
                    (fail-line line "a key is defined twice")))
               ((or (exact-integer? (car rest))
                    (eq? existing 'missing)
                    (null? existing)
                    (data-mapping? existing))
                (mapping-set (if (data-mapping? tree) tree '())
                             key
                             (toml-set (if (eq? existing 'missing) '() existing)
                                       rest value line)))
               (else
                (fail-line line "a key is used as a table and as a value"))))))))

(define (toml-hex-escape text index digits)
  "Return `(CHAR . NEXT)' for the hexadecimal escape at INDEX."
  (let* ((start (+ index 2))
         (end (+ start digits))
         (hex (if (<= end (string-length text))
                  (substring text start end)
                  ""))
         (code (and (= (string-length hex) digits)
                    (string->number hex 16))))
    (unless code
      (fail index
            (format #f "an escape needs ~a hexadecimal digits" digits)))
    (cons (integer->char code) end)))

(define (toml-escape-at text index)
  "Return `(CHAR . NEXT)' for the escape whose backslash is at INDEX."
  (let ((escaped (string-ref text (+ index 1))))
    (cond
     ((char=? escaped #\u) (toml-hex-escape text index 4))
     ((char=? escaped #\U) (toml-hex-escape text index 8))
     (else
      (let ((decoded (case escaped
                       ((#\") #\")
                       ((#\\) #\\)
                       ((#\b) (integer->char 8))
                       ((#\t) #\tab)
                       ((#\n) #\newline)
                       ((#\f) (integer->char 12))
                       ((#\r) #\return)
                       (else #f))))
        (unless decoded
          (fail (+ index 1)
                (format #f "\\~a is not a TOML escape" escaped)))
        (cons decoded (+ index 2)))))))

(define (toml-read-escaped-string text index)
  "Read a TOML basic string at INDEX and return `(STRING . NEXT)'."
  (let loop ((i (+ index 1)) (chars '()))
    (when (>= i (string-length text))
      (fail index "unterminated string"))
    (let ((char (string-ref text i)))
      (cond
       ((char=? char #\") (cons (list->string (reverse chars)) (+ i 1)))
       ((char=? char #\\)
        (when (>= (+ i 1) (string-length text))
          (fail i "a backslash ends the file"))
        (let ((decoded (toml-escape-at text i)))
          (loop (cdr decoded) (cons (car decoded) chars))))
       ((< (char->integer char) 32)
        (fail i "a control character must be escaped in a TOML string"))
       (else (loop (+ i 1) (cons char chars)))))))


(define (toml-read-literal-string text index)
  "Read a TOML literal string at INDEX and return `(STRING . NEXT)'."
  (let ((end (string-index text #\' (+ index 1))))
    (unless end
      (fail index "unterminated literal string"))
    (cons (substring text (+ index 1) end) (+ end 1))))

(define (toml-skip-space text index)
  (let loop ((i index))
    (if (and (< i (string-length text))
             (memv (string-ref text i) '(#\space #\tab)))
        (loop (+ i 1))
        i)))

(define (toml-scan-token text index)
  "Return the index after the bare token that starts at INDEX."
  (let loop ((i index))
    (if (and (< i (string-length text))
             (not (memv (string-ref text i)
                        '(#\, #\] #\} #\space #\tab))))
        (loop (+ i 1))
        i)))

(define (toml-value text index)
  "Read the TOML value at INDEX and return `(VALUE . NEXT)'."
  (let ((start (toml-skip-space text index)))
    (when (>= start (string-length text))
      (fail start "a value is missing"))
    (let ((char (string-ref text start)))
      (cond
       ((char=? char #\") (toml-read-escaped-string text start))
       ((char=? char #\') (toml-read-literal-string text start))
       ((char=? char #\[) (toml-read-array text start))
       ((char=? char #\{) (toml-read-inline-table text start))
       (else
        (let* ((end (toml-scan-token text start))
               (token (substring text start end)))
          (when (string=? token "")
            (fail start "a value is missing"))
          (cons (toml-atom token start) end)))))))

(define (toml-number token)
  "Return TOKEN as a number when it is a number this reader accepts."
  (let ((plain (list->string
                (remove (lambda (char) (char=? char #\_))
                        (string->list token)))))
    (cond
     ((string-prefix? "0x" plain) (string->number (substring plain 2) 16))
     ((string-prefix? "0o" plain) (string->number (substring plain 2) 8))
     ((string-prefix? "0b" plain) (string->number (substring plain 2) 2))
     ((string-match "^[+-]?[0-9]+$" plain) (string->number plain 10))
     ((string-match "^[+-]?[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?$" plain)
      (string->number plain))
     (else #f))))

(define (toml-atom token start)
  "Return TOKEN as a TOML scalar, or fail at START."
  (cond
   ((string=? token "true") #t)
   ((string=? token "false") #f)
   ((toml-number token) => (lambda (number) number))
   ;; A date or time is not used by a task file and TOML's grammar for it is
   ;; large; it comes back as the text it was written as, which is honest about
   ;; what was read.
   ((string-any (lambda (char) (memv char '(#\- #\: #\T))) token) token)
   (else (fail start (format #f "~s is not a value this reader accepts" token)))))

(define (toml-read-array text index)
  "Read a single-line TOML array at INDEX and return `(LIST . NEXT)'."
  (let loop ((i (toml-skip-space text (+ index 1))) (items '()))
    (when (>= i (string-length text))
      (fail index "unterminated array"))
    (if (char=? (string-ref text i) #\])
        (cons (reverse items) (+ i 1))
        (let* ((result (toml-value text i))
               (items (cons (car result) items))
               (next (toml-skip-space text (cdr result))))
          (when (>= next (string-length text))
            (fail next "unterminated array"))
          (cond
           ((char=? (string-ref text next) #\,)
            (loop (toml-skip-space text (+ next 1)) items))
           ((char=? (string-ref text next) #\])
            (cons (reverse items) (+ next 1)))
           (else (fail next "expected `,' or `]' in an array")))))))

(define (toml-read-inline-table text index)
  "Read a single-line TOML inline table at INDEX and return `(ALIST . NEXT)'."
  (let loop ((i (toml-skip-space text (+ index 1))) (pairs '()))
    (when (>= i (string-length text))
      (fail index "unterminated inline table"))
    (if (char=? (string-ref text i) #\})
        (cons pairs (+ i 1))
        (let* ((equals (string-index text #\= i)))
          (unless equals
            (fail i "expected `key = value' in an inline table"))
          (let* ((key-parts (toml-key-parts (substring text i equals) 0))
                 (result (toml-value text (+ equals 1)))
                 (value (car result))
                 (next (toml-skip-space text (cdr result))))
            (when (null? key-parts)
              (fail i "an inline table entry needs a key"))
            (when (not (eq? (mapping-get pairs (car key-parts)) 'missing))
              (fail i "a key is defined twice in an inline table"))
            (let ((pairs (mapping-set pairs (car key-parts) value)))
              (when (>= next (string-length text))
                (fail next "unterminated inline table"))
              (cond
               ((char=? (string-ref text next) #\,)
                (loop (toml-skip-space text (+ next 1)) pairs))
               ((char=? (string-ref text next) #\})
                (cons pairs (+ next 1)))
               (else (fail next "expected `,' or `}' in an inline table")))))))))

(define* (toml-read-string text #:key (source "<toml>"))
  "Read TEXT as one TOML document.

Tables become nested string-keyed alists, arrays lists, and a key defined twice
is an error.  The subset is the one a task file needs; see the note above for
what is refused."
  (parameterize ((current-source source)
                 (current-text text))
    (let loop ((lines (toml-logical-lines text)) (tree '()) (path '()))
      (if (null? lines)
          tree
          (let* ((number (car (car lines)))
                 (line (cadr (car lines))))
            (cond
             ((and (string-prefix? "[[" line)
                   (string-suffix? "]]" line)
                   (> (string-length line) 4))
              (let ((parts (toml-key-parts
                            (substring line 2 (- (string-length line) 2))
                            number)))
                (when (null? parts)
                  (fail-line number "a table header names no table"))
                (let* ((keys (map (lambda (key) (toml-ensure-key! number key))
                                  parts))
                       (name (car (last-pair keys)))
                       (parent-keys (list-head keys (- (length keys) 1)))
                       (tree (toml-parent tree parent-keys number))
                       (tables (append (toml-table-list (toml-at tree parent-keys)
                                                        name number)
                                       (list '())))
                       (tree (toml-put tree parent-keys
                                       (mapping-set (toml-at tree parent-keys)
                                                    name tables)))
                       ;; The keys after this line belong to the table that was
                       ;; just appended, which is the last one of the list.
                       (path (append keys (list (- (length tables) 1)))))
                  (loop (cdr lines) tree path))))
             ((string-prefix? "[[" line)
              (fail-line number "an array of tables header ends with `]]'"))
             ((and (string-prefix? "[" line)
                   (string-suffix? "]" line))
              (let ((parts (toml-key-parts (substring line 1 (- (string-length line) 1))
                                           number)))
                (when (null? parts)
                  (fail-line number "a table header names no table"))
                (loop (cdr lines)
                      (toml-parent tree
                                   (map (lambda (key) (toml-ensure-key! number key))
                                        parts)
                                   number)
                      parts)))
             (else
              (let ((equals (string-index line #\=)))
                (unless equals
                  (fail-line number "expected `key = value'"))
                (let* ((key-parts (toml-key-parts (substring line 0 equals) number))
                       (result (toml-value line (+ equals 1)))
                       (value (car result))
                       (rest (string-trim-both (substring line (cdr result)))))
                  (when (null? key-parts)
                    (fail-line number "a value needs a key"))
                  (unless (string=? rest "")
                    (fail-line number (format #f "unexpected text after the value: ~s" rest)))
                  (loop (cdr lines)
                        (toml-set tree
                                  (append path
                                          (map (lambda (key)
                                                 (toml-ensure-key! number key))
                                               key-parts))
                                  value
                                  number)
                        path))))))))))

;; ====== YAML ======
;;
;; The subset is block mappings, block sequences, flow sequences and mappings,
;; quoted and plain scalars, and comments.  Anchors, aliases, tags, directives,
;; multi-line scalars and a second document are refused: each one changes what
;; a value means in a way a reader that guessed would get wrong.

(define (yaml-strip-comment line)
  "Return LINE without a trailing comment, respecting its quotes."
  (let loop ((i 0) (quoting #f))
    (if (>= i (string-length line))
        line
        (let ((char (string-ref line i)))
          (cond
           ((eq? quoting #\")
            (cond ((char=? char #\\) (loop (+ i 2) quoting))
                  ((char=? char #\") (loop (+ i 1) #f))
                  (else (loop (+ i 1) quoting))))
           ((eq? quoting #\')
            (if (char=? char #\') (loop (+ i 1) #f) (loop (+ i 1) quoting)))
           ((or (char=? char #\") (char=? char #\')) (loop (+ i 1) char))
           ((and (char=? char #\#)
                 (or (= i 0) (char-whitespace? (string-ref line (- i 1)))))
            (substring line 0 i))
           (else (loop (+ i 1) #f)))))))

(define (leading-indent line number)
  "Return the number of spaces LINE starts with, refusing a tab."
  (let loop ((i 0))
    (if (>= i (string-length line))
        i
        (let ((char (string-ref line i)))
          (cond
           ((char=? char #\space) (loop (+ i 1)))
           ((char=? char #\tab)
            (fail-line number "a tab cannot be used for indentation in YAML"))
           (else i))))))

(define (yaml-lines text)
  "Return TEXT's lines as `(INDENT TEXT LINE-NUMBER)', skipping blanks."
  (let loop ((lines (string-split text #\newline)) (number 1) (kept '()))
    (if (null? lines)
        (reverse kept)
        (let* ((stripped (string-trim-right (yaml-strip-comment (car lines))))
               (trimmed (string-trim-both stripped)))
          (cond
           ((string=? trimmed "") (loop (cdr lines) (+ number 1) kept))
           ((or (string=? trimmed "---") (string=? trimmed "..."))
            (loop (cdr lines) (+ number 1) kept))
           ((or (string-prefix? "%" trimmed)
                (member (string-ref trimmed 0) '(#\& #\* #\!)))
            (fail-line number "directives, anchors, aliases and tags are not read here"))
           (else
            (let ((indent (leading-indent stripped number)))
              (loop (cdr lines) (+ number 1)
                    (cons (list indent (substring stripped indent) number)
                          kept)))))))))

(define (yaml-split-key text)
  "Return `(KEY . VALUE-TEXT)' when TEXT is a `key: value' line, else #f."
  (let loop ((i 0) (quoting #f))
    (if (>= i (string-length text))
        #f
        (let ((char (string-ref text i)))
          (cond
           ((eq? quoting #\")
            (cond ((char=? char #\\) (loop (+ i 2) quoting))
                  ((char=? char #\") (loop (+ i 1) #f))
                  (else (loop (+ i 1) quoting))))
           ((eq? quoting #\')
            (if (char=? char #\') (loop (+ i 1) #f) (loop (+ i 1) quoting)))
           ((or (char=? char #\") (char=? char #\')) (loop (+ i 1) char))
           ((char=? char #\:)
            (if (or (= (+ i 1) (string-length text))
                    (char-whitespace? (string-ref text (+ i 1))))
                (cons (string-trim-both (substring text 0 i))
                      (string-trim-both (substring text (+ i 1))))
                (loop (+ i 1) #f)))
           (else (loop (+ i 1) #f)))))))

(define (yaml-unquote-key text number)
  "Return the key TEXT names, unquoting it when it is quoted."
  (cond
   ((string=? text "") (fail-line number "a key is empty"))
   ((char=? (string-ref text 0) #\")
    (car (yaml-quoted text 0 number)))
   ((char=? (string-ref text 0) #\')
    (car (yaml-quoted text 0 number)))
   (else text)))

(define (yaml-decode-escape escaped number)
  "Return the character ESCAPED names in a YAML double-quoted string, or #f."
  (case escaped
    ((#\") #\")
    ((#\\) #\\)
    ((#\/) #\/)
    ((#\n) #\newline)
    ((#\t) #\tab)
    ((#\r) #\return)
    ((#\b) (integer->char 8))
    ((#\f) (integer->char 12))
    (else #f)))

(define (yaml-quoted text index number)
  "Read the quoted scalar at INDEX of TEXT and return `(STRING . NEXT)'."
  (let ((quoting (string-ref text index)))
    (let loop ((i (+ index 1)) (chars '()))
      (when (>= i (string-length text))
        (fail-line number "unterminated quoted scalar"))
      (let ((char (string-ref text i)))
        (cond
         ((char=? char quoting)
          (if (and (char=? quoting #\')
                   (< (+ i 1) (string-length text))
                   (char=? (string-ref text (+ i 1)) #\'))
              (loop (+ i 2) (cons #\' chars))
              (cons (list->string (reverse chars)) (+ i 1))))
         ((and (char=? quoting #\") (char=? char #\\))
          (when (>= (+ i 1) (string-length text))
            (fail-line number "a backslash ends the line"))
          (let ((escaped (string-ref text (+ i 1))))
            (if (char=? escaped #\u)
                (let* ((hex (if (<= (+ i 6) (string-length text))
                                (substring text (+ i 2) (+ i 6))
                                ""))
                       (code (and (= (string-length hex) 4)
                                  (string->number hex 16))))
                  (unless code
                    (fail-line number "a \\u escape needs four hexadecimal digits"))
                  (loop (+ i 6) (cons (integer->char code) chars)))
                (let ((decoded (yaml-decode-escape escaped number)))
                  (unless decoded
                    (fail-line number (format #f "\\~a is not a YAML escape" escaped)))
                  (loop (+ i 2) (cons decoded chars))))))
         (else (loop (+ i 1) (cons char chars))))))))


(define (yaml-plain token)
  "Return TOKEN as a YAML scalar."
  (cond
   ((member token '("null" "Null" "NULL" "~")) 'null)
   ((member token '("true" "True" "TRUE")) #t)
   ((member token '("false" "False" "FALSE")) #f)
   (else (or (string->number token) token))))

(define (yaml-scalar text number)
  "Return TEXT as a scalar value."
  (let ((trimmed (string-trim-both text)))
    (cond
     ((string=? trimmed "") 'null)
     ((char=? (string-ref trimmed 0) #\")
      (let ((result (yaml-quoted trimmed 0 number)))
        (unless (= (cdr result) (string-length trimmed))
          (fail-line number "unexpected text after a quoted scalar"))
        (car result)))
     ((char=? (string-ref trimmed 0) #\')
      (let ((result (yaml-quoted trimmed 0 number)))
        (unless (= (cdr result) (string-length trimmed))
          (fail-line number "unexpected text after a quoted scalar"))
        (car result)))
     ((or (char=? (string-ref trimmed 0) #\[)
          (char=? (string-ref trimmed 0) #\{))
      (let ((result (yaml-flow-value trimmed 0 number)))
        (unless (= (cdr result) (string-length trimmed))
          (fail-line number "unexpected text after a flow value"))
        (car result)))
     ((member (string-ref trimmed 0) '(#\| #\> #\& #\* #\!))
      (fail-line number "multi-line scalars, anchors, aliases and tags are not read here"))
     (else (yaml-plain trimmed)))))

(define (yaml-flow-skip text index)
  (let loop ((i index))
    (if (and (< i (string-length text))
             (char-whitespace? (string-ref text i)))
        (loop (+ i 1))
        i)))

(define (yaml-flow-token text index number stop-chars)
  "Read a plain flow token at INDEX, stopping at any of STOP-CHARS.

Returns `(TOKEN . NEXT)'.  A value stops at `,]}` and a key also stops at `:',
so a URL in a value stays one token."
  (let loop ((i index))
    (if (and (< i (string-length text))
             (not (memv (string-ref text i) stop-chars)))
        (loop (+ i 1))
        (let ((token (string-trim-both (substring text index i))))
          (when (string=? token "")
            (fail-line number "a flow value is empty"))
          (cons token i)))))

(define (yaml-flow-key text index number)
  "Read a flow mapping key at INDEX and return `(KEY . NEXT)'."
  (let ((i (yaml-flow-skip text index)))
    (cond
     ((>= i (string-length text)) (fail-line number "a flow mapping key is missing"))
     ((or (char=? (string-ref text i) #\") (char=? (string-ref text i) #\'))
      (let ((result (yaml-quoted text i number)))
        (cons (car result) (yaml-flow-skip text (cdr result)))))
     (else
      (let ((result (yaml-flow-token text i number '(#\: #\, #\] #\}))))
        (cons (car result) (yaml-flow-skip text (cdr result))))))))

(define (yaml-flow-array text index number)
  "Read a YAML flow sequence at INDEX and return `(LIST . NEXT)'."
  (let loop ((i (yaml-flow-skip text (+ index 1))) (items '()))
    (when (>= i (string-length text))
      (fail-line number "unterminated flow sequence"))
    (if (char=? (string-ref text i) #\])
        (cons (reverse items) (+ i 1))
        (let* ((result (yaml-flow-value text i number))
               (items (cons (car result) items))
               (next (yaml-flow-skip text (cdr result))))
          (when (>= next (string-length text))
            (fail-line number "unterminated flow sequence"))
          (cond
           ((char=? (string-ref text next) #\,)
            (loop (yaml-flow-skip text (+ next 1)) items))
           ((char=? (string-ref text next) #\])
            (cons (reverse items) (+ next 1)))
           (else (fail-line number "expected `,' or `]' in a flow sequence")))))))

(define (yaml-flow-map text index number)
  "Read a YAML flow mapping at INDEX and return `(ALIST . NEXT)'."
  (let loop ((i (yaml-flow-skip text (+ index 1))) (pairs '()))
    (when (>= i (string-length text))
      (fail-line number "unterminated flow mapping"))
    (if (char=? (string-ref text i) #\})
        (cons (reverse pairs) (+ i 1))
        (let* ((key-result (yaml-flow-key text i number))
               (key (car key-result))
               (colon (yaml-flow-skip text (cdr key-result))))
          (unless (and (< colon (string-length text))
                       (char=? (string-ref text colon) #\:))
            (fail-line number "expected `:' after a flow mapping key"))
          (when (not (eq? (mapping-get pairs key) 'missing))
            (fail-line number (format #f "duplicate key ~s" key)))
          (let* ((value-result
                  (yaml-flow-value text (yaml-flow-skip text (+ colon 1)) number))
                 (pairs (cons (cons key (car value-result)) pairs))
                 (next (yaml-flow-skip text (cdr value-result))))
            (when (>= next (string-length text))
              (fail-line number "unterminated flow mapping"))
            (cond
             ((char=? (string-ref text next) #\,)
              (loop (yaml-flow-skip text (+ next 1)) pairs))
             ((char=? (string-ref text next) #\})
              (cons (reverse pairs) (+ next 1)))
             (else (fail-line number
                              "expected `,' or `}' in a flow mapping"))))))))

(define (yaml-flow-value text index number)
  "Read a flow value at INDEX and return `(VALUE . NEXT)'."
  (let ((i (yaml-flow-skip text index)))
    (when (>= i (string-length text))
      (fail-line number "a flow value is missing"))
    (let ((char (string-ref text i)))
      (cond
       ((char=? char #\[) (yaml-flow-array text i number))
       ((char=? char #\{) (yaml-flow-map text i number))
       ((or (char=? char #\") (char=? char #\')) (yaml-quoted text i number))
       (else
        (let ((result (yaml-flow-token text i number '(#\, #\] #\}))))
          (cons (yaml-plain (car result)) (cdr result))))))))


(define (yaml-item-line? text)
  (or (string=? text "-")
      (string-prefix? "- " text)))

(define (yaml-block lines)
  "Return `(VALUE . REMAINING-LINES)' for the block that starts LINES."
  (let* ((line (car lines))
         (indent (car line))
         (text (cadr line)))
    (cond
     ((yaml-item-line? text) (yaml-sequence lines indent))
     ((yaml-split-key text) (yaml-mapping lines indent))
     (else
      (when (pair? (cdr lines))
        (when (> (car (cadr lines)) indent)
          (fail-line (caddr (cadr lines)) "unexpected indentation")))
      (cons (yaml-scalar text (caddr line)) (cdr lines))))))

(define (yaml-mapping lines indent)
  "Return `(ALIST . REMAINING-LINES)' for the mapping at INDENT."
  (let loop ((remaining lines) (pairs '()))
    (if (null? remaining)
        (cons (reverse pairs) '())
        (let* ((line (car remaining))
               (line-indent (car line))
               (text (cadr line))
               (number (caddr line)))
          (cond
           ((< line-indent indent) (cons (reverse pairs) remaining))
           ((> line-indent indent)
            (fail-line number "unexpected indentation"))
           (else
            (let ((split (yaml-split-key text)))
              (unless split
                (fail-line number "expected `key: value'"))
              (let* ((key (yaml-unquote-key (car split) number))
                     (rest (cdr split)))
                (when (not (eq? (mapping-get pairs key) 'missing))
                  (fail-line number (format #f "duplicate key ~s" key)))
                (cond
                 ((string=? rest "")
                  (let ((following (cdr remaining)))
                    (if (and (pair? following)
                             (> (car (car following)) indent))
                        (let ((block (yaml-block following)))
                          (loop (cdr block)
                                (cons (cons key (car block)) pairs)))
                        (loop following (cons (cons key 'null) pairs)))))
                 (else
                  (loop (cdr remaining)
                        (cons (cons key (yaml-scalar rest number)) pairs))))))))))))

(define (yaml-sequence lines indent)
  "Return `(LIST . REMAINING-LINES)' for the sequence at INDENT."
  (let loop ((remaining lines) (items '()))
    (if (null? remaining)
        (cons (reverse items) '())
        (let* ((line (car remaining))
               (line-indent (car line))
               (text (cadr line))
               (number (caddr line)))
          (cond
           ((< line-indent indent) (cons (reverse items) remaining))
           ((> line-indent indent)
            (fail-line number "unexpected indentation"))
           ((not (yaml-item-line? text))
            (cons (reverse items) remaining))
           (else
            (let* ((content (string-trim-both (substring text 1)))
                   (following (cdr remaining)))
              (cond
               ((string=? content "")
                (if (and (pair? following)
                         (> (car (car following)) indent))
                    (let ((block (yaml-block following)))
                      (loop (cdr block) (cons (car block) items)))
                    (loop following (cons 'null items))))
               (else
                ;; `- key: value' starts a mapping whose indent is two past the
                ;; dash, and the lines after it belong to the same mapping.
                (let* ((virtual (list (+ indent 2) content number))
                       (block (yaml-block (cons virtual following))))
                  (loop (cdr block) (cons (car block) items))))))))))))

(define* (yaml-read-string text #:key (source "<yaml>"))
  "Read TEXT as one YAML document.

The subset is documented above; anything outside it is a maak error rather than
a value that was guessed at."
  (parameterize ((current-source source)
                 (current-text text))
    (let ((lines (yaml-lines text)))
      (cond
       ((null? lines) 'null)
       (else
        (let ((block (yaml-block lines)))
          (unless (null? (cdr block))
            (fail-line (caddr (car (cdr block)))
                       "unexpected content after the document"))
          (car block)))))))

;; ====== Files ======

(define (data-file-format path)
  "Return `json', `toml' or `yaml' for PATH's extension, or #f."
  (let* ((name (basename path))
         (dot (string-rindex name #\.))
         (extension (and dot (string-downcase (substring name (+ dot 1))))))
    (case (and extension (string->symbol extension))
      ((json) 'json)
      ((toml) 'toml)
      ((yaml yml) 'yaml)
      (else #f))))

(define (read-data-file path)
  "Read PATH as JSON, TOML or YAML, chosen by its extension.

A missing file and an extension maak does not read are maak errors: a task file
that cannot be read is not an empty task list."
  (unless (file-exists? path)
    (raise-maak-error
     (format #f "task file not found: ~a" path)
     #:hint "Create it, or name another one with -f/--file."))
  (let ((kind (data-file-format path)))
    (unless kind
      (raise-maak-error
       (format #f "~a is not a task file maak reads: its extension names neither JSON, TOML nor YAML"
               path)
       #:hint "Name the file with .json, .toml, .yaml or .yml, or write the tasks in Scheme."))
    (let ((text (call-with-input-file path get-string-all)))
      (case kind
        ((json) (json-read-string text #:source path))
        ((toml) (toml-read-string text #:source path))
        (else (yaml-read-string text #:source path))))))
