;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; C and C++ build helpers, and the pieces a Makefile or an Automake project
;; reaches for when it moves to maak.  There is no build engine here: a
;; compiler invocation is still a command built at the moment a task runs, so
;; `--dry-run' prints it, `--plan' records it, and `-j'/`--watch' work as they
;; do for every other task.
;;
;; Two decisions are worth naming.
;;
;; The argv builders are pure.  `compile-argv', `link-argv' and
;; `static-library-argv' return a list of words; the runners `compile', `link'
;; and `static-library' feed that list to `run'.  A test can assert the command
;; line without a C compiler installed, which is the same split the rest of
;; maak keeps between a description and its execution.
;;
;; There is no automatic timestamp-driven rebuild.  `up-to-date?' compares a
;; target with its inputs, and a task decides whether to use it; the cache
;; arrives as an explicit `#:incremental?' or an `unless', never as a hidden
;; second runner.  That is what keeps a dependency from becoming a daemon.
;;
;; The compilers are parameters, so a task can pin them lexically, and their
;; defaults honour the variables a Makefile would: MAAK_CC, then CC, then `cc';
;; MAAK_CXX, then CXX, then `c++'; MAAK_AR, then AR, then `ar'.  A value may
;; carry flags (`"ccache gcc"'), which are split into words before the compiler
;; arguments.  `CFLAGS', `CPPFLAGS', `LDFLAGS' and `LDLIBS' are not read
;; automatically; `environment-flags' reads one when a task asks for it, so the
;; task says which of them reach which command.

;;; Code:

(define-module (maak cc)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  ;; `link' is the POSIX binding in the default module; this module means its
  ;; own linker, and says so rather than letting the duplicate handler warn on
  ;; every run.
  #:replace (link)
  #:export (c-compiler
            cxx-compiler
            ar-command
            pkg-config-command
            compiler-for
            object-file
            compile-argv
            compile
            compile-objects
            link-argv
            link
            static-library-argv
            static-library
            up-to-date?
            environment-flags
            pkg-config-argv
            pkg-config
            pkg-config-cflags
            pkg-config-libs
            pkg-config-exists?
            pkg-config-version))

;; ====== The programs ======

(define (env-value names fallback)
  "Return the first non-empty value among environment NAMES, or FALLBACK."
  (let loop ((names names))
    (if (null? names)
        fallback
        (let ((value (getenv (car names))))
          (if (and value (not (string=? value "")))
              value
              (loop (cdr names)))))))

(define c-compiler
  (make-parameter (env-value '("MAAK_CC" "CC") "cc")))

(define cxx-compiler
  (make-parameter (env-value '("MAAK_CXX" "CXX") "c++")))

(define ar-command
  (make-parameter (env-value '("MAAK_AR" "AR") "ar")))

(define pkg-config-command
  (make-parameter (env-value '("MAAK_PKG_CONFIG") "pkg-config")))

(define (program-words command what)
  "Return COMMAND as the words of a program invocation.

A compiler value may carry flags -- `\"ccache gcc\"' is the usual one -- so it
is split the way an unquoted shell word is split.  An empty value is a maak
error rather than a command with no program."
  (let ((words (and (string? command)
                    (string-tokenize (string-trim-both command)))))
    (unless (pair? words)
      (raise-maak-error
       (format #f "~a must name a program, got ~s" what command)
       #:hint "Set MAAK_CC, MAAK_CXX, MAAK_AR or MAAK_PKG_CONFIG, or pass the program as a string."))
    words))

(define (language-symbol language)
  "Return LANGUAGE as `c' or `c++', or raise with what was given."
  (let ((name (cond ((symbol? language) (symbol->string language))
                    ((string? language) language)
                    (else #f))))
    (cond
     ((and name (string=? (string-downcase name) "c")) 'c)
     ((and name (member (string-downcase name) '("c++" "cxx"))) 'c++)
     (else
      (raise-maak-error
       (format #f "language must be c or c++, got ~s" language)
       #:hint "Write #:language 'c++ (or \"c++\") for a C++ source.")))))

(define* (compiler-for language #:optional compiler)
  "Return the compiler for LANGUAGE as a list of words.

LANGUAGE is `c', `c++', `\"c' or `\"c++'\".  COMPILER, when given, overrides the
`c-compiler' or `cxx-compiler' parameter.  A compiler value with flags is split
into words, so `MAAK_CC=\"ccache gcc\"' works."
  (program-words (or compiler
                     (case (language-symbol language)
                       ((c++) (cxx-compiler))
                       (else (c-compiler))))
                 "the compiler"))

;; ====== Names and checks ======

(define (require-string-list value what)
  "Return VALUE after checking that it is a list of strings."
  (unless (and (list? value) (every string? value))
    (raise-maak-error
     (format #f "~a takes a list of strings, got ~s" what value)
     #:hint "Write a list such as '(\"src/a.c\" \"src/b.c\")."))
  value)

(define* (object-file source #:optional (directory #f))
  "Return the object file a SOURCE compiles to.

The source's extension is replaced with `.o' and its directory is kept, or
DIRECTORY is used instead.  `src/a.c' becomes `src/a.o'; with DIRECTORY
`build', it becomes `build/a.o'.  Only the file name is used for DIRECTORY, so
two sources with the same base name collide there; give them different
directories or different names."
  (unless (and (string? source) (not (string=? source "")))
    (raise-maak-error
     (format #f "object-file expects a source path, got ~s" source)
     #:hint "Write (object-file \"src/a.c\") or (object-file \"src/a.c\" \"build\")."))
  (let* ((base (basename source))
         (dot (string-rindex base #\.))
         (stem (if dot (substring base 0 dot) base))
         (name (string-append stem ".o")))
    (if directory
        (in-vicinity directory name)
        (let ((dir (dirname source)))
          (if (string=? dir ".") name (in-vicinity dir name))))))

(define (file-mtime path)
  "Return PATH's modification time, or #f when it cannot be read."
  (catch #t
    (lambda () (stat:mtime (stat path)))
    (lambda args #f)))

(define (up-to-date? output inputs)
  "Return #t when OUTPUT exists and is not older than any of INPUTS.

A missing output or a missing input is not up to date.  This is the explicit
check a task uses to skip a compile; maak never applies it on its own, because
a dependency that decides whether a task runs is the thing maak was written to
leave to the task.

@example
(unless (up-to-date? \"build/a.o\" '(\"src/a.c\" \"include/a.h\"))
  (compile \"src/a.c\" #:output \"build/a.o\"))
@end example"
  (unless (and (string? output) (not (string=? output "")))
    (raise-maak-error
     (format #f "up-to-date? expects an output path, got ~s" output)
     #:hint "Write (up-to-date? \"build/a.o\" '(\"src/a.c\"))."))
  (require-string-list inputs "up-to-date?")
  (let ((target (file-mtime output)))
    (and target
         (every (lambda (input)
                  (let ((mtime (file-mtime input)))
                    (and mtime (>= target mtime))))
                inputs)
         #t)))

(define* (environment-flags name #:optional (default '()))
  "Return the whitespace-separated words of environment variable NAME.

This is how a migrated task reads `CFLAGS', `CPPFLAGS', `LDFLAGS' or `LDLIBS'
when it decides to honour them.  Nothing is read automatically: the task says
which variable reaches which command.

@example
(compile \"src/a.c\" #:flags (environment-flags \"CFLAGS\"))
@end example"
  (unless (and (string? name) (not (string=? name "")))
    (raise-maak-error
     (format #f "environment-flags expects a variable name, got ~s" name)
     #:hint "Write (environment-flags \"CFLAGS\")."))
  (let ((value (getenv name)))
    (if value (split-shell-words value) default)))

;; ====== Compiling ======

(define* (compile-argv source
                       #:key
                       (output #f)
                       (includes '())
                       (defines '())
                       (flags '())
                       (language 'c)
                       (compiler #f)
                       (standard #f)
                       (pic? #f)
                       (depfile? #f))
  "Return the argv that compiles SOURCE to OUTPUT, without running it.

INCLUDES become `-I' arguments, DEFINES `-D' arguments, and FLAGS are inserted
verbatim.  STANDARD becomes `-std=STANDARD'.  PIC? adds `-fPIC', and DEPFILE?
adds `-MMD -MP', so the compiler writes the dependency file beside the object;
maak does not read that file, and `up-to-date?' is how a task uses it.

@example
(compile-argv \"src/a.c\" #:output \"build/a.o\"
              #:includes '(\"include\") #:defines '(\"NDEBUG\")
              #:flags '(\"-O2\" \"-Wall\") #:standard \"c11\")
@end example"
  (unless (and (string? source) (not (string=? source "")))
    (raise-maak-error
     (format #f "compile expects a source path, got ~s" source)
     #:hint "Write (compile \"src/a.c\")."))
  (require-string-list includes "compile")
  (require-string-list defines "compile")
  (require-string-list flags "compile")
  (unless (or (not standard) (string? standard))
    (raise-maak-error
     (format #f "compile's #:standard takes a string, got ~s" standard)
     #:hint "Write #:standard \"c11\" or #:standard \"c++17\"."))
  (append
   (compiler-for language compiler)
   (list "-c")
   (if standard (list (string-append "-std=" standard)) '())
   (if pic? (list "-fPIC") '())
   (if depfile? (list "-MMD" "-MP") '())
   (map (lambda (include) (string-append "-I" include)) includes)
   (map (lambda (define) (string-append "-D" define)) defines)
   flags
   (list "-o" (or output (object-file source)) source)))

(define* (compile source
                  #:key
                  (output #f)
                  (includes '())
                  (defines '())
                  (flags '())
                  (language 'c)
                  (compiler #f)
                  (standard #f)
                  (pic? #f)
                  (depfile? #f)
                  (verbose? #f))
  "Compile SOURCE to OUTPUT, or to SOURCE's `.o' beside it.

The keywords are `compile-argv''s.  The command goes through `run', so a dry
run prints it, a plan records it, and a failure names the compiler and the
source.

@example
(compile \"src/a.c\" #:output \"build/a.o\" #:includes '(\"include\"))
@end example"
  (run (compile-argv source
                     #:output output
                     #:includes includes
                     #:defines defines
                     #:flags flags
                     #:language language
                     #:compiler compiler
                     #:standard standard
                     #:pic? pic?
                     #:depfile? depfile?)
       #:verbose? verbose?))

(define* (compile-objects sources
                          #:key
                          (directory #f)
                          (jobs 1)
                          (incremental? #f)
                          (includes '())
                          (defines '())
                          (flags '())
                          (language 'c)
                          (compiler #f)
                          (standard #f)
                          (pic? #f)
                          (depfile? #f)
                          (verbose? #f))
  "Compile every SOURCE to its object under DIRECTORY, up to JOBS at once.

Each object is named with `object-file', so `src/a.c' in DIRECTORY `build'
becomes `build/a.o'.  With INCREMENTAL? #t an object newer than its source is
left alone; without it every object is compiled, which is what a clean build
wants.  JOBS above one starts that many compilers at once, each a process of
its own; a failure is reported with how many of them failed, and the compilers'
own messages are above it.

@example
(compile-objects '(\"src/a.c\" \"src/b.c\")
                 #:directory \"build\" #:jobs 4 #:incremental? #t
                 #:includes '(\"include\") #:depfile? #t)
@end example"
  (require-string-list sources "compile-objects")
  (unless (and (exact-integer? jobs) (>= jobs 1))
    (raise-maak-error
     (format #f "compile-objects takes a positive job count, got ~s" jobs)
     #:hint "Write #:jobs 4, or omit it for one compile at a time."))
  (let* ((pairs (map (lambda (source)
                       (cons source (object-file source directory)))
                     sources))
         (wanted (if incremental?
                     (filter (lambda (pair)
                               (not (up-to-date? (cdr pair) (list (car pair)))))
                             pairs)
                     pairs))
         (argvs (map (lambda (pair)
                       (compile-argv (car pair)
                                     #:output (cdr pair)
                                     #:includes includes
                                     #:defines defines
                                     #:flags flags
                                     #:language language
                                     #:compiler compiler
                                     #:standard standard
                                     #:pic? pic?
                                     #:depfile? depfile?))
                     wanted)))
    (cond
     ((null? argvs) #t)
     ;; A dry run is a transcript, not a schedule: run each command through
     ;; `run' so it prints, and no process is started.
     ((dry-run?)
      (for-each (lambda (argv) (run argv #:verbose? verbose?)) argvs)
      #t)
     ((<= jobs 1)
      (for-each (lambda (argv) (run argv #:verbose? verbose?)) argvs)
      #t)
     (else
      (let loop ((pending argvs) (running '()) (failed 0))
        (cond
         ((and (pair? pending) (< (length running) jobs))
          (let ((argv (car pending)))
            (loop (cdr pending)
                  (append running
                          (list (cons argv
                                      (run argv #:async? #t
                                           #:verbose? verbose?))))
                  failed)))
         ((pair? running)
          (let* ((entry (car running))
                 (ok? (wait-job (cdr entry))))
            (loop pending (cdr running) (if ok? failed (+ failed 1)))))
         (else
          (if (zero? failed)
              #t
              (raise-maak-error
               (format #f "~a of ~a compilations failed" failed (length argvs))
               #:hint "Each compiler's own message is above.")))))))))

;; ====== Linking ======

(define* (link-argv objects
                    #:key
                    (output "a.out")
                    (libraries '())
                    (library-paths '())
                    (flags '())
                    (link-flags '())
                    (language 'c)
                    (compiler #f)
                    (shared? #f)
                    (static? #f)
                    (soname #f))
  "Return the argv that links OBJECTS into OUTPUT, without running it.

LIBRARIES become `-l' arguments and LIBRARY-PATHS `-L' arguments.  FLAGS are
inserted before the objects, which is where `LDFLAGS' goes; LINK-FLAGS are
inserted after the objects and the libraries, which is where `LDLIBS' and the
pre-formed flags pkg-config returns go.  SHARED? adds `-shared', STATIC? adds
`-static', and SONAME adds `-Wl,-soname,SONAME', which is the usual three
things a libtool link line was doing.

@example
(link-argv '(\"build/a.o\" \"build/b.o\")
           #:output \"build/app\" #:libraries '(\"m\")
           #:library-paths '(\"build\") #:flags '(\"-pthread\")
           #:link-flags (pkg-config-libs '(\"zlib\")))
@end example"
  (require-string-list objects "link")
  (when (null? objects)
    (raise-maak-error
     "link needs at least one object"
     #:hint "Compile the sources first, then pass their object files."))
  (require-string-list libraries "link")
  (require-string-list library-paths "link")
  (require-string-list flags "link")
  (require-string-list link-flags "link")
  (unless (and (string? output) (not (string=? output "")))
    (raise-maak-error
     (format #f "link's #:output must be a path, got ~s" output)
     #:hint "Write #:output \"build/app\"."))
  (unless (or (not soname) (string? soname))
    (raise-maak-error
     (format #f "link's #:soname must be a string, got ~s" soname)
     #:hint "Write #:soname \"libfoo.so.1\"."))
  (append
   (compiler-for language compiler)
   (if shared? (list "-shared") '())
   (if static? (list "-static") '())
   (if soname (list (string-append "-Wl,-soname," soname)) '())
   (map (lambda (path) (string-append "-L" path)) library-paths)
   flags
   (list "-o" output)
   objects
   (map (lambda (library) (string-append "-l" library)) libraries)
   link-flags))

(define* (link objects
               #:key
               (output "a.out")
               (libraries '())
               (library-paths '())
               (flags '())
               (link-flags '())
               (language 'c)
               (compiler #f)
               (shared? #f)
               (static? #f)
               (soname #f)
               (verbose? #f))
  "Link OBJECTS into OUTPUT.

The keywords are `link-argv''s, plus VERBOSE? for the command line.  Use
`#:flags' for `LDFLAGS' (before the objects) and `#:link-flags' for `LDLIBS'
and pkg-config's libs (after them)."
  (run (link-argv objects
                  #:output output
                  #:libraries libraries
                  #:library-paths library-paths
                  #:flags flags
                  #:link-flags link-flags
                  #:language language
                  #:compiler compiler
                  #:shared? shared?
                  #:static? static?
                  #:soname soname)
       #:verbose? verbose?))

(define* (static-library-argv output objects #:key (archiver #f))
  "Return the argv that writes OBJECTS into a static library OUTPUT.

The archiver is `ar rcs', which creates the archive, inserts the objects and
writes the index; ARCHIVER overrides the `ar-command' parameter.

@example
(static-library-argv \"build/libfoo.a\" '(\"build/a.o\" \"build/b.o\"))
@end example"
  (require-string-list objects "static-library")
  (when (null? objects)
    (raise-maak-error
     "static-library needs at least one object"
     #:hint "Compile the sources first, then pass their object files."))
  (unless (and (string? output) (not (string=? output "")))
    (raise-maak-error
     (format #f "static-library's output must be a path, got ~s" output)
     #:hint "Write (static-library \"build/libfoo.a\" objects)."))
  (append (program-words (or archiver (ar-command)) "the archiver")
          (list "rcs" output)
          objects))

(define* (static-library output objects #:key (archiver #f) (verbose? #f))
  "Write OBJECTS into the static library OUTPUT.

@example
(static-library \"build/libfoo.a\" (map (lambda (source) (object-file source \"build\"))
                                       '(\"src/a.c\" \"src/b.c\")))
@end example"
  (run (static-library-argv output objects #:archiver archiver)
       #:verbose? verbose?))

;; ====== pkg-config ======
;;
;; Asking pkg-config is a read of the system, not a change to it, so these
;; helpers run it even in a dry run: a plan that omitted the flags a compile
;; would use is not a plan.  The command itself is `#:pkg-config-command' or
;; MAAK_PKG_CONFIG, so a test can point it at a script and a cross build can
;; point it at the target's pkg-config.

(define (split-shell-words text)
  "Split TEXT into words the way a POSIX shell splits an unquoted expansion.

Backslash escapes the next character outside single quotes, single quotes keep
everything literally, and double quotes keep everything but still honour a
backslash.  pkg-config writes its flags in this form, so `-I/a\\ b' is one
argument rather than two."
  (let loop ((chars (string->list text))
             (word '())
             (words '())
             (quote-char #f)
             (started? #f))
    (define (finish)
      (if started?
          (cons (list->string (reverse word)) words)
          words))
    (cond
     ((null? chars)
      (reverse (finish)))
     ((and (eq? (car chars) #\\) (not (eq? quote-char #\')))
      (if (null? (cdr chars))
          (loop '() (cons #\\ word) words quote-char #t)
          (loop (cddr chars) (cons (cadr chars) word) words quote-char #t)))
     ((and (eq? (car chars) #\') (not (eq? quote-char #\")))
      (loop (cdr chars) word words
            (if (eq? quote-char #\') #f #\') #t))
     ((and (eq? (car chars) #\") (not (eq? quote-char #\')))
      (loop (cdr chars) word words
            (if (eq? quote-char #\") #f #\") #t))
     ((and (not quote-char) (char-whitespace? (car chars)))
      (loop (cdr chars) '() (finish) #f #f))
     (else
      (loop (cdr chars) (cons (car chars) word) words quote-char #t)))))

(define (packages-list packages)
  "Return PACKAGES as a non-empty list of package-name strings."
  (let ((list (cond ((string? packages) (list packages))
                    ((list? packages) packages)
                    (else #f))))
    (unless (and list (pair? list) (every (lambda (package)
                                            (and (string? package)
                                                 (not (string=? package ""))))
                                          list))
      (raise-maak-error
       (format #f "pkg-config helpers take a package name or a list of names, got ~s"
               packages)
       #:hint "Write (pkg-config-cflags '(\"gtk+-3.0\")) or (pkg-config-cflags \"zlib\")."))
    list))

(define* (pkg-config-argv packages #:key (mode 'cflags) (static? #f))
  "Return the argv that asks pkg-config for PACKAGES, without running it.

MODE is `cflags', `libs', `modversion' or `exists'.  With MODE `libs' and
STATIC? the `--static' flag is added, so pkg-config names the libraries a
static link needs.

@example
(pkg-config-argv '(\"gtk+-3.0\") #:mode 'cflags)
@result{} (\"pkg-config\" \"--cflags\" \"gtk+-3.0\")
@end example"
  (let ((packages (packages-list packages)))
    (append
     (program-words (pkg-config-command) "pkg-config")
     (case mode
       ((cflags) (list "--cflags"))
       ((libs) (append (if static? (list "--static") '()) (list "--libs")))
       ((modversion) (list "--modversion"))
       ((exists) (list "--exists"))
       (else
        (raise-maak-error
         (format #f "pkg-config mode must be cflags, libs, modversion or exists, got ~s"
                 mode)
         #:hint "Write #:mode 'libs, for example.")))
     packages)))

(define* (pkg-config-output argv #:key (allow-failure? #f))
  "Run pkg-config with ARGV and return its output, or #f when ALLOW-FAILURE?.

This does not honour dry-run: pkg-config reads the system rather than changing
it, and a plan that left out the flags a compile would use would be wrong.  A
failure raises a maak error with the exit code and the command, or comes back
as #f when the caller is asking a yes/no question."
  (let* ((line (string-join (map shell-quote argv) " "))
         (port (open-input-pipe line))
         (output (get-string-all port))
         (status (close-pipe port))
         (code (status:exit-val status)))
    (cond
     ((and code (zero? code)) output)
     (allow-failure? #f)
     ((not code)
      (raise-maak-error
       (format #f "pkg-config was terminated by a signal: ~a" line)
       #:hint "Run the command above by hand to read pkg-config's own message."))
     (else
      (raise-maak-error
       (format #f "pkg-config failed (exit code ~a): ~a" code line)
       #:hint "Install the package's development files, or check the name with `pkg-config --list-all'.")))))

(define* (pkg-config packages #:key (mode 'cflags) (static? #f))
  "Run pkg-config for PACKAGES and return its output as a string.

MODE and STATIC? are `pkg-config-argv''s.  The query runs even in a dry run,
because it reads the system: `maak --plan' then shows the flags a compile
would really use.

@example
(pkg-config '(\"gtk+-3.0\"))              ;; => \"-I... -pthread ...\"
(pkg-config '(\"zlib\") #:mode 'libs)     ;; => \"-lz\"
@end example"
  (pkg-config-output (pkg-config-argv packages #:mode mode #:static? static?)))

(define (pkg-config-cflags packages)
  "Return the compiler flags pkg-config names for PACKAGES, as a list.

@example
(pkg-config-cflags '(\"gtk+-3.0\"))
@result{} (\"-I/gnu/store/.../include/gtk-3.0\" ...)
@end example"
  (split-shell-words (pkg-config packages #:mode 'cflags)))

(define* (pkg-config-libs packages #:key (static? #f))
  "Return the linker flags pkg-config names for PACKAGES, as a list.

With STATIC? #t the `--static' answer is used, which names the transitive
libraries a static link needs."
  (split-shell-words (pkg-config packages #:mode 'libs #:static? static?)))

(define (pkg-config-exists? packages)
  "Return #t when pkg-config knows every one of PACKAGES.

@example
(pkg-config-exists? '(\"zlib\" \"openssl\"))
@end example"
  (and (pkg-config-output (pkg-config-argv packages #:mode 'exists)
                          #:allow-failure? #t)
       #t))

(define (pkg-config-version package)
  "Return the version pkg-config reports for PACKAGE.

@example
(pkg-config-version \"zlib\")
@result{} \"1.3.1\"
@end example"
  (unless (and (string? package) (not (string=? package "")))
    (raise-maak-error
     (format #f "pkg-config-version expects one package name, got ~s" package)
     #:hint "Write (pkg-config-version \"zlib\")."))
  (string-trim-both (pkg-config (list package) #:mode 'modversion)))
