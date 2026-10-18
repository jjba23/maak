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
;; The gettext workflow, packaged as maak builtins.
;;
;; Keeping a program translated means running the same four GNU gettext tools
;; in the same order -- `xgettext' to gather the strings, `msginit' to start a
;; language, `msgmerge' to fold new strings into it, and `msgfmt' to compile
;; the catalogue the program actually loads.  Every project ends up writing
;; those invocations by hand in its maak file; this module writes them once.
;;
;; The layout is the conventional one:
;;
;;   DIR/TEXTDOMAIN.pot                    the template
;;   DIR/LANG/LC_MESSAGES/TEXTDOMAIN.po    one language
;;   DIR/LANG/LC_MESSAGES/TEXTDOMAIN.mo    the compiled catalogue
;;
;; so a project that follows the convention only names its DIR, its
;; TEXTDOMAIN, and the LANGUAGES it ships.

;;; Code:

(define-module (maak i18n)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (ice-9 i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:export (i18n-locale-dir
            i18n-po-file
            i18n-mo-file
            i18n-pot-file
            i18n-source-globs
            i18n-extract
            i18n-init
            i18n-merge
            i18n-compile
            i18n-init-all
            i18n-merge-all
            i18n-compile-all
            i18n-refresh
            i18n-translator
            i18n-language))

;; ====== Where the catalogues live ======

(define (i18n-trim-slash dir)
  "Return DIR without a trailing slash, so paths join cleanly."
  (string-trim-right dir #\/))

(define* (i18n-locale-dir language #:key (dir "po"))
  "Return the catalogue directory of LANGUAGE inside DIR.

@example\n(i18n-locale-dir \"nl_NL\")\n@result{} \"po/nl_NL/LC_MESSAGES\"\n@end example"
  (format #f "~a/~a/LC_MESSAGES" (i18n-trim-slash dir) language))

(define* (i18n-po-file language #:key (dir "po") (textdomain "messages"))
  "Return the path of LANGUAGE's portable object file (the one a translator edits)."
  (format #f "~a/~a.po" (i18n-locale-dir language #:dir dir) textdomain))

(define* (i18n-mo-file language #:key (dir "po") (textdomain "messages"))
  "Return the path of LANGUAGE's compiled machine object file (the one the program loads)."
  (format #f "~a/~a.mo" (i18n-locale-dir language #:dir dir) textdomain))

(define* (i18n-pot-file #:key (dir "po") (textdomain "messages"))
  "Return the path of the template TEXTDOMAIN.pot inside DIR."
  (format #f "~a/~a.pot" (i18n-trim-slash dir) textdomain))

(define* (i18n-source-globs directories #:key (extensions '("scm")))
  "Return the file patterns of DIRECTORIES that `xgettext' should read.

A glob that matches nothing is handed to xgettext as itself, and xgettext
refuses the whole invocation over the missing file -- writing no template at
all rather than the strings it did find.  Only directories that exist are
therefore asked for, so adding or removing a source tree is all a refresh
ever needs.

@example\n(i18n-source-globs '(\"lib\" \"app/views\"))\n@end example"
  (append-map (lambda (directory)
                (if (file-exists? directory)
                    (map (lambda (extension)
                           (format #f "~a/*.~a" (i18n-trim-slash directory)
                                   extension))
                         extensions)
                    '()))
              directories))

;; ====== The four gettext tools ======

(define* (i18n-extract #:key (dir "po")
                       (textdomain "messages")
                       (directories '("."))
                       (extensions '("scm"))
                       (keywords '("G_" "N_" "_"))
                       (copyright-holders '())
                       (package-name #f)
                       (from-code "UTF-8")
                       (update? #t)
                       (verbose? #t))
  "Extract translatable strings from source files into the .pot template.

DIRECTIONS is the list of directories to scan, EXTENSIONS the file types in
them, and KEYWORDS the function names xgettext should treat as translatable
(`G_' by default).  With UPDATE? the strings are joined into an existing
template instead of starting a fresh one, which keeps translator comments and
previous line references.

@example\n(i18n-extract #:directories '(\"lib\" \"app/views\"))\n@end example"
  (mkdir-p dir)
  ($ (append (list (format #f "xgettext --default-domain=~a" textdomain)
                   (format #f "--output=~a" (i18n-pot-file #:dir dir
                                                           #:textdomain
                                                           textdomain))
                   "--language=Scheme"
                   "--sort-by-file")
             (map (lambda (keyword)
                    (format #f "--keyword=~a" keyword)) keywords)
             ;; `--join-existing' is refused when the template is not there
             ;; yet, so the very first extraction starts a fresh one.
             (if (and update?
                      (file-exists? (i18n-pot-file #:dir dir
                                                   #:textdomain textdomain)))
                 (list "--join-existing") '())
             (if (or package-name textdomain)
                 (list (format #f "--package-name=~a"
                               (or package-name textdomain))) '())
             (if (null? copyright-holders) '()
                 (list (format #f "--copyright-holder=~a"
                               (string-join copyright-holders ","))))
             (i18n-source-globs directories #:extensions extensions)
             (list (format #f "--from-code ~a" from-code)))
     #:verbose? verbose?))

(define* (i18n-init language #:key (dir "po")
                    (textdomain "messages")
                    (verbose? #t))
  "Create LANGUAGE's .po file from the .pot template.

The language directory is created when missing, so a new translation starts
with one call.  LANGUAGE is a locale name such as nl_NL."
  (mkdir-p (i18n-locale-dir language #:dir dir))
  ($ (list (format #f "msginit -l ~a.UTF8 -o" language)
           (~ "~a/~a/LC_MESSAGES/~a.po" (i18n-trim-slash dir) language
              textdomain)
           (format #f "-i ~a --no-translator"
                   (i18n-pot-file #:dir dir
                                  #:textdomain
                                  textdomain)))
     #:verbose? verbose?))

(define* (i18n-merge language #:key (dir "po")
                     (textdomain "messages")
                     (verbose? #f))
  "Fold the current .pot template into LANGUAGE's .po file, in place."
  ($ (list "msgmerge -U"
           (i18n-po-file language #:dir dir #:textdomain textdomain)
           (i18n-pot-file #:dir dir #:textdomain textdomain))
     #:verbose? verbose?))

(define* (i18n-compile language #:key (dir "po")
                       (textdomain "messages")
                       (verbose? #f))
  "Compile LANGUAGE's .po file into the .mo catalogue the program loads."
  ($ (list (format #f "msgfmt -v -o ~a/~a/LC_MESSAGES/~a.mo"
                   (i18n-trim-slash dir) language textdomain)
           (i18n-po-file language #:dir dir #:textdomain textdomain))
     #:verbose? verbose?))

;; ====== Doing it for every language ======

(define* (i18n-init-all languages #:key (dir "po")
                        (textdomain "messages")
                        (only-missing? #t)
                        (verbose? #t))
  "Run `i18n-init' for every language of LANGUAGES.

With ONLY-MISSING? (the default) a language whose .po already exists is
skipped, so the call can live in a task that is run repeatedly without
clobbering translators' work with a fresh template."
  (for-each (lambda (language)
              (if (and only-missing?
                       (file-exists? (i18n-po-file language #:dir dir
                                                   #:textdomain textdomain)))
                  (begin
                    (log-info "Translation for ~a already exists, skipping.\n"
                              language))
                  (i18n-init language #:dir dir
                             #:textdomain textdomain
                             #:verbose? verbose?)))
            languages))

(define* (i18n-merge-all languages #:key (dir "po")
                         (textdomain "messages")
                         (verbose? #f))
  "Run `i18n-merge' for every language of LANGUAGES, skipping missing files."
  (for-each (lambda (language)
              (if (file-exists? (i18n-po-file language #:dir dir
                                              #:textdomain textdomain))
                  (i18n-merge language #:dir dir
                              #:textdomain textdomain
                              #:verbose? verbose?)
                  (log-info "No translation for ~a yet, skipping merge.\n"
                            language)))
            languages))

(define* (i18n-compile-all languages #:key (dir "po")
                           (textdomain "messages")
                           (verbose? #f))
  "Run `i18n-compile' for every language of LANGUAGES, skipping missing files."
  (for-each (lambda (language)
              (if (file-exists? (i18n-po-file language #:dir dir
                                              #:textdomain textdomain))
                  (i18n-compile language #:dir dir
                                #:textdomain textdomain
                                #:verbose? verbose?)
                  (log-info "No translation for ~a yet, skipping compile.\n"
                            language)))
            languages))

(define* (i18n-refresh languages #:key (dir "po")
                       (textdomain "messages")
                       (directories '("."))
                       (extensions '("scm"))
                       (keywords '("G_" "N_" "_"))
                       (copyright-holders '())
                       (package-name #f)
                       (verbose? #t))
  "Do the whole gettext cycle: extract, merge, then compile.

This is the one call a project normally puts in its maak file; the individual
steps stay available for anything more unusual."
  (i18n-extract #:dir dir
                #:textdomain textdomain
                #:directories directories
                #:extensions extensions
                #:keywords keywords
                #:copyright-holders copyright-holders
                #:package-name package-name
                #:verbose? verbose?)
  (i18n-merge-all languages #:dir dir
                  #:textdomain textdomain
                  #:verbose? verbose?)
  (i18n-compile-all languages #:dir dir
                    #:textdomain textdomain
                    #:verbose? verbose?))

;; ====== Reading a catalogue at run time ======

(define (i18n-language)
  "Return the language the process should translate into.

The first of `LANGUAGE', `LC_ALL', `LC_MESSAGES' and `LANG' that is set, with
its character encoding and any modifier stripped; when none is set, `en_US'
is assumed."
  (let* ((raw (or (getenv "LANGUAGE")
                  (getenv "LC_ALL")
                  (getenv "LC_MESSAGES")
                  (getenv "LANG")
                  "en_US"))
         (without-encoding (car (string-split raw #\.)))
         (language (car (string-split without-encoding #\@))))
    language))

(define* (i18n-translator #:key (dir "po")
                          (textdomain "messages")
                          (language #f))
  "Install the catalogue in DIR for TEXTDOMAIN and return a translate procedure.

The returned procedure takes one message and returns its translation, falling
back to the original when the catalogue has no entry.  LANGUAGE defaults to
`i18n-language'.

@example\n(define G_ (i18n-translator #:dir \"po\" #:textdomain \"maak\"))\n(G_ \"Hello\")\n@end example"
  (let ((locale (or language (i18n-language))))
    (catch #t
           (lambda ()
             (setlocale LC_ALL (string-append locale ".UTF-8")))
           (lambda args #f))
    (catch #t
           (lambda ()
             (bindtextdomain textdomain (canonicalize-path dir)))
           (lambda args #f))
    (textdomain textdomain)
    (lambda (message)
      (gettext message textdomain))))
