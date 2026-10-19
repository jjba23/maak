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
;; The download half of a task, without a shell string: curl and wget with
;; their common flags as keyword arguments, plus the few higher-level answers
;; a task actually asks for -- fetch a URL to a file, read a URL into a string,
;; check a status code, post JSON, mirror a site.
;;
;; Every command helper has an `-argv' builder.  The builder returns the words
;; without running anything: that is what the procedure feeds to `run', what
;; --dry-run prints, and what a test can assert without a network.  The
;; builders are exported for the same reason the archive and container builders
;; are: a task that needs to log or extend a command should not have to rebuild
;; its flags.  `download' is the exception, because it chooses between the two
;; programs; it has no single command line of its own.
;;
;; `download' is the one helper that chooses a program.  `fetch-command' is a
;; parameter, detected as curl when it is installed and wget otherwise, and
;; overridable with MAAK_FETCH or a lexical `parameterize'.  The direct curl-
;; and wget- helpers never consult it: when a task needs a particular tool, it
;; names that tool.
;;
;; Nothing here touches the network when the module is loaded.

;;; Code:

(define-module (maak net)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (fetch-command
            detect-fetch-command
            fetch-available?
            curl
            curl-argv
            curl-output
            curl-get-argv
            curl-get
            curl-text-argv
            curl-text
            curl-head-argv
            curl-head
            curl-status-argv
            curl-status
            curl-post-argv
            curl-post
            curl-download-argv
            curl-download
            curl-upload-argv
            curl-upload
            wget
            wget-argv
            wget-output
            wget-download-argv
            wget-download
            wget-mirror-argv
            wget-mirror
            wget-text-argv
            wget-text
            download))

;; ====== Choosing the downloader ======

(define* (detect-fetch-command
          #:key (path (getenv "PATH")) (preferred (getenv "MAAK_FETCH")))
  "Return the download program `download' should use.

PREFERRED, when non-empty, wins unchanged.  Otherwise curl is chosen when it is
on PATH, then wget.  The fallback is curl even when neither is installed, so a
missing program is reported as a missing program rather than silently replaced
by the other one.

PATH is the search path, PATH itself by default, so the choice can be tested
without changing the process environment.

@example
(detect-fetch-command #:path search-path)
(detect-fetch-command #:preferred \"wget\")
@end example"
  (cond
    ((and preferred (not (string=? preferred ""))) preferred)
    ((command-on-path? "curl" path) "curl")
    ((command-on-path? "wget" path) "wget")
    (else "curl")))

(define fetch-command
  (make-parameter (detect-fetch-command)))

(define* (fetch-available? #:key (command (fetch-command)))
  "Return #t when COMMAND, the downloader `download' would use, is on PATH."
  (and (string? command)
       (not (string=? command ""))
       (command-on-path? command (getenv "PATH"))))

;; ====== Shared building blocks ======

(define (check-url caller url)
  "Raise a maak error unless URL is a non-empty string, naming CALLER."
  (when (or (not (string? url)) (string=? url ""))
    (raise-maak-error
     (format #f "~a expects a URL" caller)
     #:hint "Write (curl-get \"https://example.org/file\").")))

(define (check-string-list caller name value)
  "Raise a maak error unless VALUE is a list of strings, naming CALLER and NAME."
  (when (or (not (list? value)) (not (every string? value)))
    (raise-maak-error
     (format #f "~a expects #:~a to be a list of strings, got ~s"
             caller name value)
     #:hint "Write the values as separate strings, for example (list \"a\" \"b\").")))

(define (count-value caller name value)
  "Return VALUE as a string for a command line, or raise naming CALLER and NAME.

A count is a non-negative integer: a timeout of zero is meaningful, and a
negative one is not."
  (cond
    ((and (integer? value) (>= value 0)) (format #f "~a" value))
    (else
     (raise-maak-error
      (format #f "~a expects #:~a to be a non-negative integer, got ~s"
              caller name value)
      #:hint "Write a non-negative integer, such as 30 or 3."))))

(define* (capture-argv argv #:key (allow-failure? #f))
  "Run ARGV as a shell command and return its output.

The command is quoted the way the dry-run line is, so a URL or a header with a
space in it stays one word.  Used by the helpers that answer with a value --
the body of a response or an HTTP status code -- rather than printing."
  (capture (string-join (map shell-quote argv) " ")
           #:allow-failure? allow-failure?))

;; ====== curl ======

(define* (curl-base-argv arguments
                         #:key
                         (headers '())
                         (follow? #t)
                         (fail? #t)
                         (silent? #t)
                         (show-error? #t)
                         (compressed? #t)
                         (timeout #f)
                         (retry #f)
                         (user #f)
                         (user-agent #f)
                         (insecure? #f)
                         (cookie #f)
                         (cookie-jar #f)
                         (output #f)
                         (output-dir #f)
                         (create-dirs? #f)
                         (resume? #f)
                         (extra '()))
  "Return the curl argv with the common options and ARGUMENTS at the end.

This is the one place the flag order lives.  FOLLOW? is --location, FAIL? is
--fail, SILENT? and SHOW-ERROR? are the pair a task wants, and COMPRESSED? is
--compressed.  TIMEOUT and RETRY are counts, USER and USER-AGENT are passed
through, and INSECURE? turns certificate checking off.  COOKIE and COOKIE-JAR
are paths or a cookie string.  OUTPUT and OUTPUT-DIR write the body to a file
or a directory, CREATE-DIRS? makes the destination's parents, and RESUME?
continues a partial download.  EXTRA is a list of words for flags this builder
does not model."
  (check-string-list 'curl "headers" headers)
  (check-string-list 'curl "extra" extra)
  (append (list "curl")
          (if follow? (list "--location") '())
          (if fail? (list "--fail") '())
          (if silent? (list "--silent") '())
          (if show-error? (list "--show-error") '())
          (if compressed? (list "--compressed") '())
          (if output (list "--output" output) '())
          (if output-dir (list "--output-dir" output-dir) '())
          (if create-dirs? (list "--create-dirs") '())
          (if resume? (list "--continue-at" "-") '())
          (append-map (lambda (header) (list "--header" header)) headers)
          (if user (list "--user" user) '())
          (if user-agent (list "--user-agent" user-agent) '())
          (if timeout (list "--max-time" (count-value 'curl "timeout" timeout)) '())
          (if retry (list "--retry" (count-value 'curl "retry" retry)) '())
          (if insecure? (list "--insecure") '())
          (if cookie (list "--cookie" cookie) '())
          (if cookie-jar (list "--cookie-jar" cookie-jar) '())
          extra
          arguments))

(define (curl-argv arguments)
  "Return the argv that runs curl with ARGUMENTS.

The escape hatch for a flag no helper models; nothing is handed to a shell, so
an argument with a space in it stays one argument."
  (check-string-list 'curl "arguments" arguments)
  (cons "curl" arguments))

(define* (curl arguments #:key (allow-failure? #f) (verbose? #f))
  "Run curl with ARGUMENTS, a list of strings; see `curl-argv'."
  (run (curl-argv arguments)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (curl-output arguments #:key (allow-failure? #f))
  "Run curl with ARGUMENTS and return what it printed.

This is the reading counterpart of `curl': the body is a value a task can trim,
split or parse.  In dry-run mode nothing runs, the command is printed, and the
empty string comes back."
  (capture-argv (curl-argv arguments) #:allow-failure? allow-failure?))

(define* (curl-get-argv url
                        #:key
                        (output #f)
                        (output-dir #f)
                        (create-dirs? #f)
                        (resume? #f)
                        (headers '())
                        (follow? #t)
                        (fail? #t)
                        (silent? #t)
                        (show-error? #t)
                        (compressed? #t)
                        (timeout #f)
                        (retry #f)
                        (user #f)
                        (user-agent #f)
                        (insecure? #f)
                        (cookie #f)
                        (cookie-jar #f)
                        (extra '()))
  "Return the argv that fetches URL with curl.

The defaults are the ones a task wants: redirects are followed, an HTTP error
is a failure, and the progress meter is replaced by silence plus real errors.
OUTPUT writes the body to a file, OUTPUT-DIR names the directory for it, and
CREATE-DIRS? creates the parents.  RESUME? continues a partial file."
  (check-url 'curl-get url)
  (curl-base-argv (list url)
                  #:output output
                  #:output-dir output-dir
                  #:create-dirs? create-dirs?
                  #:resume? resume?
                  #:headers headers
                  #:follow? follow?
                  #:fail? fail?
                  #:silent? silent?
                  #:show-error? show-error?
                  #:compressed? compressed?
                  #:timeout timeout
                  #:retry retry
                  #:user user
                  #:user-agent user-agent
                  #:insecure? insecure?
                  #:cookie cookie
                  #:cookie-jar cookie-jar
                  #:extra extra))

(define* (curl-get url
                   #:key
                   (output #f)
                   (output-dir #f)
                   (create-dirs? #f)
                   (resume? #f)
                   (headers '())
                   (follow? #t)
                   (fail? #t)
                   (silent? #t)
                   (show-error? #t)
                   (compressed? #t)
                   (timeout #f)
                   (retry #f)
                   (user #f)
                   (user-agent #f)
                   (insecure? #f)
                   (cookie #f)
                   (cookie-jar #f)
                   (extra '())
                   (allow-failure? #f)
                   (verbose? #f))
  "Fetch URL with curl; see `curl-get-argv' for the options."
  (run (curl-get-argv url
                      #:output output
                      #:output-dir output-dir
                      #:create-dirs? create-dirs?
                      #:resume? resume?
                      #:headers headers
                      #:follow? follow?
                      #:fail? fail?
                      #:silent? silent?
                      #:show-error? show-error?
                      #:compressed? compressed?
                      #:timeout timeout
                      #:retry retry
                      #:user user
                      #:user-agent user-agent
                      #:insecure? insecure?
                      #:cookie cookie
                      #:cookie-jar cookie-jar
                      #:extra extra)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (curl-text-argv url
                         #:key
                         (headers '())
                         (follow? #t)
                         (fail? #t)
                         (timeout #f)
                         (retry #f)
                         (user #f)
                         (user-agent #f)
                         (insecure? #f)
                         (cookie #f)
                         (extra '()))
  "Return the argv that fetches URL's body for a task to read."
  (check-url 'curl-text url)
  (curl-base-argv (list url)
                  #:headers headers
                  #:follow? follow?
                  #:fail? fail?
                  #:timeout timeout
                  #:retry retry
                  #:user user
                  #:user-agent user-agent
                  #:insecure? insecure?
                  #:cookie cookie
                  #:extra extra))

(define* (curl-text url
                    #:key
                    (headers '())
                    (follow? #t)
                    (fail? #t)
                    (timeout #f)
                    (retry #f)
                    (user #f)
                    (user-agent #f)
                    (insecure? #f)
                    (cookie #f)
                    (extra '())
                    (allow-failure? #f))
  "Fetch URL with curl and return the body as a string.

Use this instead of `curl-get' when the task needs the response, not a line on
the terminal.  A non-zero curl exits non-zero and raises, unless
ALLOW-FAILURE? is #t."
  (capture-argv (curl-text-argv url
                                #:headers headers
                                #:follow? follow?
                                #:fail? fail?
                                #:timeout timeout
                                #:retry retry
                                #:user user
                                #:user-agent user-agent
                                #:insecure? insecure?
                                #:cookie cookie
                                #:extra extra)
                #:allow-failure? allow-failure?))

(define* (curl-head-argv url
                         #:key
                         (headers '())
                         (follow? #t)
                         (fail? #t)
                         (silent? #t)
                         (show-error? #t)
                         (timeout #f)
                         (retry #f)
                         (user #f)
                         (user-agent #f)
                         (insecure? #f)
                         (cookie #f)
                         (extra '()))
  "Return the argv that asks for URL's headers with a HEAD request."
  (check-url 'curl-head url)
  (curl-base-argv (list "--head" url)
                  #:headers headers
                  #:follow? follow?
                  #:fail? fail?
                  #:silent? silent?
                  #:show-error? show-error?
                  #:timeout timeout
                  #:retry retry
                  #:user user
                  #:user-agent user-agent
                  #:insecure? insecure?
                  #:cookie cookie
                  #:extra extra))

(define* (curl-head url
                    #:key
                    (headers '())
                    (follow? #t)
                    (fail? #t)
                    (silent? #t)
                    (show-error? #t)
                    (timeout #f)
                    (retry #f)
                    (user #f)
                    (user-agent #f)
                    (insecure? #f)
                    (cookie #f)
                    (extra '())
                    (allow-failure? #f)
                    (verbose? #f))
  "Ask for URL's headers with curl; see `curl-head-argv'."
  (run (curl-head-argv url
                       #:headers headers
                       #:follow? follow?
                       #:fail? fail?
                       #:silent? silent?
                       #:show-error? show-error?
                       #:timeout timeout
                       #:retry retry
                       #:user user
                       #:user-agent user-agent
                       #:insecure? insecure?
                       #:cookie cookie
                       #:extra extra)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (curl-status-argv url
                           #:key
                           (headers '())
                           (follow? #t)
                           (timeout #f)
                           (retry #f)
                           (user #f)
                           (user-agent #f)
                           (insecure? #f)
                           (cookie #f)
                           (extra '()))
  "Return the argv that prints URL's HTTP status code.

The body is discarded and --fail is deliberately left off, so a 404 is reported
as 404 rather than as a failed download."
  (check-url 'curl-status url)
  (curl-base-argv (list "--write-out" "%{http_code}" url)
                  #:output "/dev/null"
                  #:fail? #f
                  #:silent? #t
                  #:show-error? #f
                  #:headers headers
                  #:follow? follow?
                  #:timeout timeout
                  #:retry retry
                  #:user user
                  #:user-agent user-agent
                  #:insecure? insecure?
                  #:cookie cookie
                  #:extra extra))

(define* (curl-status url
                      #:key
                      (headers '())
                      (follow? #t)
                      (timeout #f)
                      (retry #f)
                      (user #f)
                      (user-agent #f)
                      (insecure? #f)
                      (cookie #f)
                      (extra '())
                      (allow-failure? #f))
  "Return URL's HTTP status code as a number.

A transport failure raises unless ALLOW-FAILURE? is #t, in which case #f is
returned when curl prints no code.  An HTTP error is still a status: 404 comes
back as 404."
  (let ((code (string-trim-both
               (capture-argv
                (curl-status-argv url
                                  #:headers headers
                                  #:follow? follow?
                                  #:timeout timeout
                                  #:retry retry
                                  #:user user
                                  #:user-agent user-agent
                                  #:insecure? insecure?
                                  #:cookie cookie
                                  #:extra extra)
                #:allow-failure? allow-failure?))))
    (string->number code)))

(define* (curl-post-argv url
                         #:key
                         (data #f)
                         (json #f)
                         (data-file #f)
                         (form '())
                         (method "POST")
                         (headers '())
                         (follow? #t)
                         (fail? #t)
                         (silent? #t)
                         (show-error? #t)
                         (compressed? #t)
                         (timeout #f)
                         (retry #f)
                         (user #f)
                         (user-agent #f)
                         (insecure? #f)
                         (cookie #f)
                         (cookie-jar #f)
                         (extra '()))
  "Return the argv that sends a POST request to URL.

DATA is a body string, JSON is a JSON body sent with curl's --json (which also
sets the content type and Accept header), DATA-FILE is a file whose bytes are
the body, and FORM is a list of --form fields.  At least one of them must be
given.  METHOD defaults to POST; pass another method for an API that spells its
update differently."
  (check-url 'curl-post url)
  (check-string-list 'curl-post "form" form)
  (when (and (not data) (not json) (not data-file) (null? form))
    (raise-maak-error
     "curl-post expects a body: data, json, data-file or form"
     #:hint "Write (curl-post url #:json \"{\\\"ok\\\":true}\")."))
  (curl-base-argv (append (if method (list "--request" method) '())
                          (if json (list "--json" json) '())
                          (if data (list "--data" data) '())
                          (if data-file (list "--data-binary"
                                              (string-append "@" data-file))
                              '())
                          (append-map (lambda (field) (list "--form" field))
                                      form)
                          (list url))
                  #:headers headers
                  #:follow? follow?
                  #:fail? fail?
                  #:silent? silent?
                  #:show-error? show-error?
                  #:compressed? compressed?
                  #:timeout timeout
                  #:retry retry
                  #:user user
                  #:user-agent user-agent
                  #:insecure? insecure?
                  #:cookie cookie
                  #:cookie-jar cookie-jar
                  #:extra extra))

(define* (curl-post url
                    #:key
                    (data #f)
                    (json #f)
                    (data-file #f)
                    (form '())
                    (method "POST")
                    (headers '())
                    (follow? #t)
                    (fail? #t)
                    (silent? #t)
                    (show-error? #t)
                    (compressed? #t)
                    (timeout #f)
                    (retry #f)
                    (user #f)
                    (user-agent #f)
                    (insecure? #f)
                    (cookie #f)
                    (cookie-jar #f)
                    (extra '())
                    (allow-failure? #f)
                    (verbose? #f))
  "Send a POST request to URL; see `curl-post-argv'."
  (run (curl-post-argv url
                       #:data data
                       #:json json
                       #:data-file data-file
                       #:form form
                       #:method method
                       #:headers headers
                       #:follow? follow?
                       #:fail? fail?
                       #:silent? silent?
                       #:show-error? show-error?
                       #:compressed? compressed?
                       #:timeout timeout
                       #:retry retry
                       #:user user
                       #:user-agent user-agent
                       #:insecure? insecure?
                       #:cookie cookie
                       #:cookie-jar cookie-jar
                       #:extra extra)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (curl-download-argv url
                             #:key
                             (output #f)
                             (directory #f)
                             (create-dirs? #t)
                             (resume? #f)
                             (headers '())
                             (follow? #t)
                             (fail? #t)
                             (silent? #t)
                             (show-error? #t)
                             (compressed? #t)
                             (timeout #f)
                             (retry #f)
                             (user #f)
                             (user-agent #f)
                             (insecure? #f)
                             (cookie #f)
                             (cookie-jar #f)
                             (extra '()))
  "Return the argv that downloads URL to OUTPUT.

Without OUTPUT curl keeps the remote name, which is what --remote-name means.
DIRECTORY is curl's --output-dir, and CREATE-DIRS? makes the parents of the
destination.  RESUME? continues a partial download."
  (check-url 'curl-download url)
  (curl-base-argv (append (if output '() (list "--remote-name"))
                          (list url))
                  #:output output
                  #:output-dir directory
                  #:create-dirs? create-dirs?
                  #:resume? resume?
                  #:headers headers
                  #:follow? follow?
                  #:fail? fail?
                  #:silent? silent?
                  #:show-error? show-error?
                  #:compressed? compressed?
                  #:timeout timeout
                  #:retry retry
                  #:user user
                  #:user-agent user-agent
                  #:insecure? insecure?
                  #:cookie cookie
                  #:cookie-jar cookie-jar
                  #:extra extra))

(define* (curl-download url
                        #:key
                        (output #f)
                        (directory #f)
                        (create-dirs? #t)
                        (resume? #f)
                        (headers '())
                        (follow? #t)
                        (fail? #t)
                        (silent? #t)
                        (show-error? #t)
                        (compressed? #t)
                        (timeout #f)
                        (retry #f)
                        (user #f)
                        (user-agent #f)
                        (insecure? #f)
                        (cookie #f)
                        (cookie-jar #f)
                        (extra '())
                        (allow-failure? #f)
                        (verbose? #f))
  "Download URL with curl; see `curl-download-argv'."
  (run (curl-download-argv url
                           #:output output
                           #:directory directory
                           #:create-dirs? create-dirs?
                           #:resume? resume?
                           #:headers headers
                           #:follow? follow?
                           #:fail? fail?
                           #:silent? silent?
                           #:show-error? show-error?
                           #:compressed? compressed?
                           #:timeout timeout
                           #:retry retry
                           #:user user
                           #:user-agent user-agent
                           #:insecure? insecure?
                           #:cookie cookie
                           #:cookie-jar cookie-jar
                           #:extra extra)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (curl-upload-argv url file
                           #:key
                           (method "PUT")
                           (headers '())
                           (follow? #t)
                           (fail? #t)
                           (silent? #t)
                           (show-error? #t)
                           (compressed? #t)
                           (timeout #f)
                           (retry #f)
                           (user #f)
                           (user-agent #f)
                           (insecure? #f)
                           (cookie #f)
                           (cookie-jar #f)
                           (extra '()))
  "Return the argv that uploads FILE to URL with curl.

METHOD defaults to PUT, which is what an object store usually expects; pass
POST for an API that takes the file as a form upload."
  (check-url 'curl-upload url)
  (when (or (not (string? file)) (string=? file ""))
    (raise-maak-error
     "curl-upload expects the file to upload"
     #:hint "Write (curl-upload url \"artifact.tar.gz\")."))
  (curl-base-argv (append (if method (list "--request" method) '())
                          (list "--upload-file" file url))
                  #:headers headers
                  #:follow? follow?
                  #:fail? fail?
                  #:silent? silent?
                  #:show-error? show-error?
                  #:compressed? compressed?
                  #:timeout timeout
                  #:retry retry
                  #:user user
                  #:user-agent user-agent
                  #:insecure? insecure?
                  #:cookie cookie
                  #:cookie-jar cookie-jar
                  #:extra extra))

(define* (curl-upload url file
                      #:key
                      (method "PUT")
                      (headers '())
                      (follow? #t)
                      (fail? #t)
                      (silent? #t)
                      (show-error? #t)
                      (compressed? #t)
                      (timeout #f)
                      (retry #f)
                      (user #f)
                      (user-agent #f)
                      (insecure? #f)
                      (cookie #f)
                      (cookie-jar #f)
                      (extra '())
                      (allow-failure? #f)
                      (verbose? #f))
  "Upload FILE to URL with curl; see `curl-upload-argv'."
  (run (curl-upload-argv url file
                         #:method method
                         #:headers headers
                         #:follow? follow?
                         #:fail? fail?
                         #:silent? silent?
                         #:show-error? show-error?
                         #:compressed? compressed?
                         #:timeout timeout
                         #:retry retry
                         #:user user
                         #:user-agent user-agent
                         #:insecure? insecure?
                         #:cookie cookie
                         #:cookie-jar cookie-jar
                         #:extra extra)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

;; ====== wget ======

(define* (wget-base-argv arguments
                         #:key
                         (headers '())
                         (user #f)
                         (password #f)
                         (timeout #f)
                         (tries #f)
                         (user-agent #f)
                         (quiet? #f)
                         (timestamping? #f)
                         (no-clobber? #f)
                         (output #f)
                         (directory #f)
                         (continue? #f)
                         (extra '()))
  "Return the wget argv with the common options and ARGUMENTS at the end.

QUIET? is --quiet, CONTINUE? is --continue, and TIMESTAMPING? and NO-CLOBBER?
choose what happens to a file that is already there.  OUTPUT is
--output-document, DIRECTORY is --directory-prefix, and USER, PASSWORD,
TIMEOUT, TRIES and USER-AGENT are passed through.  EXTRA is a list of words for
flags this builder does not model."
  (check-string-list 'wget "headers" headers)
  (check-string-list 'wget "extra" extra)
  (append (list "wget")
          (if quiet? (list "--quiet") '())
          (if continue? (list "--continue") '())
          (if output (list "--output-document" output) '())
          (if directory (list "--directory-prefix" directory) '())
          (if timestamping? (list "--timestamping") '())
          (if no-clobber? (list "--no-clobber") '())
          (append-map (lambda (header) (list "--header" header)) headers)
          (if user (list "--user" user) '())
          (if password (list "--password" password) '())
          (if timeout (list "--timeout" (count-value 'wget "timeout" timeout)) '())
          (if tries (list "--tries" (count-value 'wget "tries" tries)) '())
          (if user-agent (list "--user-agent" user-agent) '())
          extra
          arguments))

(define (wget-argv arguments)
  "Return the argv that runs wget with ARGUMENTS.

The escape hatch for a flag no helper models; nothing is handed to a shell, so
an argument with a space in it stays one argument."
  (check-string-list 'wget "arguments" arguments)
  (cons "wget" arguments))

(define* (wget arguments #:key (allow-failure? #f) (verbose? #f))
  "Run wget with ARGUMENTS, a list of strings; see `wget-argv'."
  (run (wget-argv arguments)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (wget-output arguments #:key (allow-failure? #f))
  "Run wget with ARGUMENTS and return what it printed.

In dry-run mode nothing runs, the command is printed, and the empty string
comes back."
  (capture-argv (wget-argv arguments) #:allow-failure? allow-failure?))

(define* (wget-download-argv url
                             #:key
                             (output #f)
                             (directory #f)
                             (continue? #f)
                             (quiet? #f)
                             (timestamping? #f)
                             (no-clobber? #f)
                             (headers '())
                             (user #f)
                             (password #f)
                             (timeout #f)
                             (tries #f)
                             (user-agent #f)
                             (extra '()))
  "Return the argv that downloads URL with wget.

OUTPUT names the file, DIRECTORY is the prefix the file is written under, and
CONTINUE? resumes a partial file.  QUIET? silences the progress output;
TIMESTAMPING? and NO-CLOBBER? decide what to do with an existing file."
  (check-url 'wget-download url)
  (wget-base-argv (list url)
                  #:output output
                  #:directory directory
                  #:continue? continue?
                  #:quiet? quiet?
                  #:timestamping? timestamping?
                  #:no-clobber? no-clobber?
                  #:headers headers
                  #:user user
                  #:password password
                  #:timeout timeout
                  #:tries tries
                  #:user-agent user-agent
                  #:extra extra))

(define* (wget-download url
                        #:key
                        (output #f)
                        (directory #f)
                        (continue? #f)
                        (quiet? #f)
                        (timestamping? #f)
                        (no-clobber? #f)
                        (headers '())
                        (user #f)
                        (password #f)
                        (timeout #f)
                        (tries #f)
                        (user-agent #f)
                        (extra '())
                        (allow-failure? #f)
                        (verbose? #f))
  "Download URL with wget; see `wget-download-argv'."
  (run (wget-download-argv url
                           #:output output
                           #:directory directory
                           #:continue? continue?
                           #:quiet? quiet?
                           #:timestamping? timestamping?
                           #:no-clobber? no-clobber?
                           #:headers headers
                           #:user user
                           #:password password
                           #:timeout timeout
                           #:tries tries
                           #:user-agent user-agent
                           #:extra extra)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (wget-mirror-argv url
                           #:key
                           (directory #f)
                           (no-parent? #t)
                           (page-requisites? #f)
                           (convert-links? #f)
                           (cut-dirs #f)
                           (no-host-directories? #t)
                           (quiet? #f)
                           (headers '())
                           (user-agent #f)
                           (timeout #f)
                           (tries #f)
                           (extra '()))
  "Return the argv that mirrors the site at URL with wget.

The defaults are the ones a documentation snapshot wants: recursion that stays
under URL, no host directory at the top, and a DIRECTORY prefix when one is
named.  PAGE-REQUISITES? also fetches the images and stylesheets, CONVERT-LINKS?
rewrites the saved pages to point at them, and CUT-DIRS drops leading path
components from the local tree."
  (check-url 'wget-mirror url)
  (wget-base-argv (append (list "--recursive")
                          (if no-parent? (list "--no-parent") '())
                          (if page-requisites? (list "--page-requisites") '())
                          (if convert-links? (list "--convert-links") '())
                          (if cut-dirs
                              (list (format #f "--cut-dirs=~a"
                                            (count-value 'wget-mirror
                                                         "cut-dirs" cut-dirs)))
                              '())
                          (if no-host-directories?
                              (list "--no-host-directories")
                              '())
                          (list url))
                  #:directory directory
                  #:quiet? quiet?
                  #:headers headers
                  #:user-agent user-agent
                  #:timeout timeout
                  #:tries tries
                  #:extra extra))

(define* (wget-mirror url
                      #:key
                      (directory #f)
                      (no-parent? #t)
                      (page-requisites? #f)
                      (convert-links? #f)
                      (cut-dirs #f)
                      (no-host-directories? #t)
                      (quiet? #f)
                      (headers '())
                      (user-agent #f)
                      (timeout #f)
                      (tries #f)
                      (extra '())
                      (allow-failure? #f)
                      (verbose? #f))
  "Mirror the site at URL with wget; see `wget-mirror-argv'."
  (run (wget-mirror-argv url
                         #:directory directory
                         #:no-parent? no-parent?
                         #:page-requisites? page-requisites?
                         #:convert-links? convert-links?
                         #:cut-dirs cut-dirs
                         #:no-host-directories? no-host-directories?
                         #:quiet? quiet?
                         #:headers headers
                         #:user-agent user-agent
                         #:timeout timeout
                         #:tries tries
                         #:extra extra)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (wget-text-argv url
                         #:key
                         (headers '())
                         (user #f)
                         (password #f)
                         (user-agent #f)
                         (timeout #f)
                         (tries #f)
                         (extra '()))
  "Return the argv that fetches URL's body for a task to read."
  (check-url 'wget-text url)
  (wget-base-argv (list "--output-document=-" url)
                  #:quiet? #t
                  #:headers headers
                  #:user user
                  #:password password
                  #:user-agent user-agent
                  #:timeout timeout
                  #:tries tries
                  #:extra extra))

(define* (wget-text url
                    #:key
                    (headers '())
                    (user #f)
                    (password #f)
                    (user-agent #f)
                    (timeout #f)
                    (tries #f)
                    (extra '())
                    (allow-failure? #f))
  "Fetch URL with wget and return the body as a string.

wget writes the body to standard output with --output-document=-, which is what
lets a task read it instead of finding a file afterwards.  The progress output
is silenced so the result is only the body."
  (capture-argv (wget-text-argv url
                                #:headers headers
                                #:user user
                                #:password password
                                #:user-agent user-agent
                                #:timeout timeout
                                #:tries tries
                                #:extra extra)
                #:allow-failure? allow-failure?))

;; ====== One downloader for both ======

(define* (download url
                   #:key
                   (output #f)
                   (directory #f)
                   (continue? #f)
                   (headers '())
                   (user #f)
                   (password #f)
                   (user-agent #f)
                   (timeout #f)
                   (retries #f)
                   (quiet? #t)
                   (tool (fetch-command))
                   (allow-failure? #f)
                   (verbose? #f))
  "Download URL with the detected downloader.

TOOL is curl or wget; the default comes from `fetch-command', which is curl
when it is installed, wget otherwise, or whatever MAAK_FETCH names.  OUTPUT is
the destination file (the remote name when it is #f), DIRECTORY is the
directory it belongs in, and CONTINUE? resumes a partial file.  HEADERS,
USER-AGENT, TIMEOUT and RETRIES are passed to either program; USER and PASSWORD
become curl's --user or wget's --user/--password.

@example
(download \"https://example.org/release.tar.gz\"
          #:output \"release.tar.gz\"
          #:continue? #t)
(parameterize ((fetch-command \"wget\"))
  (download \"https://example.org/\" #:output \"index.html\"))
@end example"
  (check-url 'download url)
  (when (and password (not user))
    (raise-maak-error
     "download expects a user when a password is given"
     #:hint "Pass #:user too, or put the credentials in the URL."))
  (cond
    ((string=? tool "curl")
     (run (curl-download-argv url
                              #:output output
                              #:directory directory
                              #:resume? continue?
                              #:headers headers
                              #:user (if password
                                        (format #f "~a:~a" user password)
                                        user)
                              #:user-agent user-agent
                              #:timeout timeout
                              #:retry retries
                              #:silent? quiet?)
          #:allow-failure? allow-failure?
          #:verbose? verbose?))
    ((string=? tool "wget")
     (run (wget-download-argv url
                              #:output output
                              #:directory directory
                              #:continue? continue?
                              #:headers headers
                              #:user user
                              #:password password
                              #:user-agent user-agent
                              #:timeout timeout
                              #:tries retries
                              #:quiet? quiet?)
          #:allow-failure? allow-failure?
          #:verbose? verbose?))
    (else
     (raise-maak-error
      (format #f "download expects curl or wget, got ~s" tool)
      #:hint "Set MAAK_FETCH to curl or wget."))))
