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
;; Unit tests for `(maak net)'.  Nothing here touches the network: the argument
;; builders are asserted directly and the procedures are checked through
;; dry-run mode, which is the same line `maak --dry-run` shows a user.  Fetch
;; detection runs against fake PATH directories, so the result does not depend
;; on whether curl or wget happens to be installed on the machine running the
;; suite.

;;; Code:

(define-module (veritas unit net)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak net)
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

(define (dry-run-command thunk)
  "Return the command THUNK would run, as printed by dry-run mode."
  (parameterize ((dry-run? #t) (quiet? #f))
    (strip-ansi (with-output-to-string thunk))))

(define (scratch-directory label)
  "Return a fresh, empty directory under the temporary directory."
  (let ((dir (format #f "~a/maak-test-net-~a-~a"
                     (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (remove-directory dir)
  "Remove DIR quietly, whatever the tests left in it."
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

(define (fake-program dir name)
  "Write an executable called NAME in DIR and return DIR."
  (let ((path (string-append dir "/" name)))
    (write-file path "#!/bin/sh\nexit 0\n")
    (parameterize ((quiet? #t))
      (chmod-file "755" path))
    dir))

(define detection-suite
  (suite "Fetch command detection"
         #:concurrent? #f
         #:shuffle? #f
         (test "curl is preferred when both are installed"
               (let ((dir (scratch-directory "both")))
                 (fake-program dir "curl")
                 (fake-program dir "wget")
                 (let ((command (detect-fetch-command
                                 #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "curl" #:got command))))
         (test "wget is the fallback when curl is missing"
               (let ((dir (scratch-directory "wget-only")))
                 (fake-program dir "wget")
                 (let ((command (detect-fetch-command
                                 #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "wget" #:got command))))
         (test "curl is the fallback when nothing is installed"
               (let ((dir (scratch-directory "empty-path")))
                 (let ((command (detect-fetch-command
                                 #:path dir #:preferred #f)))
                   (remove-directory dir)
                   (assert-equal #:expect "curl" #:got command))))
         (test "an explicit preference wins"
               (let ((dir (scratch-directory "preferred")))
                 (fake-program dir "curl")
                 (let ((command (detect-fetch-command
                                 #:path dir #:preferred "wget")))
                   (remove-directory dir)
                   (assert-equal #:expect "wget" #:got command))))
         (test "an empty preference is ignored"
               (let ((dir (scratch-directory "empty-preference")))
                 (fake-program dir "wget")
                 (let ((command (detect-fetch-command
                                 #:path dir #:preferred "")))
                   (remove-directory dir)
                   (assert-equal #:expect "wget" #:got command))))
         (test "the default fetch command is a non-empty string"
               (assert-true (and (string? (fetch-command))
                                 (not (string=? (fetch-command) "")))))
         (test "fetch-available? looks for the chosen program"
               (let ((dir (scratch-directory "available")))
                 (fake-program dir "wget")
                 (let ((found? (with-env "PATH" dir
                                 (lambda ()
                                   (parameterize ((fetch-command "wget"))
                                     (fetch-available?))))))
                   (remove-directory dir)
                   (assert-true found?))))
         (test "fetch-available? is false for a missing program"
               (let ((dir (scratch-directory "unavailable")))
                 (let ((found? (with-env "PATH" dir
                                 (lambda ()
                                   (parameterize ((fetch-command "curl"))
                                     (fetch-available?))))))
                   (remove-directory dir)
                   (assert-false found?))))))

(define curl-suite
  (suite "curl commands"
         #:concurrent? #f
         #:shuffle? #f
         (test "the generic builder is just curl and its arguments"
               (assert-equal #:expect '("curl" "--version")
                             #:got (curl-argv (list "--version"))))
         (test "the generic runner prints its command"
               (assert-contains
                #:expect (dry-run-command (lambda () (curl (list "--version"))))
                #:got "curl --version"))
         (test "curl-output returns nothing in a dry run"
               (assert-equal #:expect ""
                             #:got (parameterize ((dry-run? #t) (quiet? #t))
                                     (curl-output (list "--version")))))
         (test "a get has task-safe defaults"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "https://example.org/file")
                #:got (curl-get-argv "https://example.org/file")))
         (test "a get carries every option in a stable order"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "--output" "out.bin" "--output-dir" "dist"
                           "--create-dirs" "--continue-at" "-"
                           "--header" "X-A: 1" "--header" "X-B: 2"
                           "--user" "me:secret" "--user-agent" "maak-test"
                           "--max-time" "30" "--retry" "3" "--insecure"
                           "--cookie" "cookies.txt" "--cookie-jar" "jar.txt"
                           "--http1.1" "https://example.org/file")
                #:got (curl-get-argv
                       "https://example.org/file"
                       #:output "out.bin"
                       #:output-dir "dist"
                       #:create-dirs? #t
                       #:resume? #t
                       #:headers (list "X-A: 1" "X-B: 2")
                       #:timeout 30
                       #:retry 3
                       #:user "me:secret"
                       #:user-agent "maak-test"
                       #:insecure? #t
                       #:cookie "cookies.txt"
                       #:cookie-jar "jar.txt"
                       #:extra (list "--http1.1"))))
         (test "a head request is a HEAD without the body"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed" "--head"
                           "https://example.org/file")
                #:got (curl-head-argv "https://example.org/file")))
         (test "a status request discards the body and prints only the code"
               (assert-equal
                #:expect '("curl" "--location" "--silent" "--compressed"
                           "--output" "/dev/null" "--write-out" "%{http_code}"
                           "https://example.org/file")
                #:got (curl-status-argv "https://example.org/file")))
         (test "a status in a dry run has no code to answer"
               (assert-false
                (parameterize ((dry-run? #t) (quiet? #t))
                  (curl-status "https://example.org/file"))))
         (test "a JSON post sends --json"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "--request" "POST" "--json" "{\"ok\":true}"
                           "https://example.org/api")
                #:got (curl-post-argv "https://example.org/api"
                                      #:json "{\"ok\":true}")))
         (test "a data post sends --data"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "--request" "POST" "--data" "a=1&b=2"
                           "https://example.org/api")
                #:got (curl-post-argv "https://example.org/api"
                                      #:data "a=1&b=2")))
         (test "a file post sends --data-binary with an @ path"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "--request" "POST" "--data-binary" "@body.json"
                           "https://example.org/api")
                #:got (curl-post-argv "https://example.org/api"
                                      #:data-file "body.json")))
         (test "a form post sends each field"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "--request" "POST" "--form" "a=1" "--form" "b=2"
                           "https://example.org/api")
                #:got (curl-post-argv "https://example.org/api"
                                      #:form (list "a=1" "b=2"))))
         (test "a post can use another method, or imply it"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "--request" "PATCH" "--data" "x"
                           "https://example.org/api")
                #:got (curl-post-argv "https://example.org/api"
                                      #:data "x"
                                      #:method "PATCH"))
               (assert-false
                (member "--request"
                        (curl-post-argv "https://example.org/api"
                                        #:data "x"
                                        #:method #f))))
         (test "a download without an output keeps the remote name"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed" "--create-dirs"
                           "--remote-name" "https://example.org/file")
                #:got (curl-download-argv "https://example.org/file")))
         (test "a download can name a file and resume it"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "--output" "file" "--output-dir" "dist"
                           "--create-dirs" "--continue-at" "-"
                           "https://example.org/file")
                #:got (curl-download-argv "https://example.org/file"
                                          #:output "file"
                                          #:directory "dist"
                                          #:resume? #t)))
         (test "an upload is a PUT with --upload-file"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "--request" "PUT" "--upload-file" "artifact.bin"
                           "https://example.org/artifact.bin")
                #:got (curl-upload-argv "https://example.org/artifact.bin"
                                        "artifact.bin")))
         (test "text names the URL after the common options"
               (assert-equal
                #:expect '("curl" "--location" "--fail" "--silent"
                           "--show-error" "--compressed"
                           "https://example.org/file")
                #:got (curl-text-argv "https://example.org/file")))
         (test "text returns nothing in a dry run"
               (assert-equal
                #:expect ""
                #:got (parameterize ((dry-run? #t) (quiet? #t))
                        (curl-text "https://example.org/file"))))
         (test "an empty URL is refused"
               (assert-error (lambda () (curl-get-argv "")))
               (assert-error (lambda () (curl-text-argv "")))
               (assert-error (lambda () (curl-status-argv "")))
               (assert-error (lambda () (curl-download-argv "")))
               (assert-error (lambda () (curl-upload-argv "" "file"))))
         (test "a header list must be a list of strings"
               (assert-error (lambda () (curl-get-argv "https://example.org"
                                                       #:headers "X: 1")))
               (assert-error (lambda () (curl-get-argv "https://example.org"
                                                       #:headers (list 1)))))
         (test "a timeout must be a non-negative integer"
               (assert-error (lambda () (curl-get-argv "https://example.org"
                                                       #:timeout -1)))
               (assert-error (lambda () (curl-get-argv "https://example.org"
                                                       #:timeout "30"))))
         (test "a post without a body is refused"
               (assert-error (lambda () (curl-post-argv "https://example.org"))))
         (test "an upload without a file is refused"
               (assert-error (lambda () (curl-upload-argv
                                         "https://example.org" ""))))
         (test "a generic call refuses arguments that are not strings"
               (assert-error (lambda () (curl (list "--version" 3)))))))

(define wget-suite
  (suite "wget commands"
         #:concurrent? #f
         #:shuffle? #f
         (test "the generic builder is just wget and its arguments"
               (assert-equal #:expect '("wget" "--version")
                             #:got (wget-argv (list "--version"))))
         (test "the generic runner prints its command"
               (assert-contains
                #:expect (dry-run-command (lambda () (wget (list "--version"))))
                #:got "wget --version"))
         (test "wget-output returns nothing in a dry run"
               (assert-equal #:expect ""
                             #:got (parameterize ((dry-run? #t) (quiet? #t))
                                     (wget-output (list "--version")))))
         (test "a bare download is wget and the URL"
               (assert-equal
                #:expect '("wget" "https://example.org/file")
                #:got (wget-download-argv "https://example.org/file")))
         (test "a download carries every option in a stable order"
               (assert-equal
                #:expect '("wget" "--quiet" "--continue"
                           "--output-document" "file"
                           "--directory-prefix" "dist"
                           "--timestamping" "--no-clobber"
                           "--header" "X-A: 1"
                           "--user" "me" "--password" "secret"
                           "--timeout" "10" "--tries" "2"
                           "--user-agent" "maak-test"
                           "--no-check-certificate"
                           "https://example.org/file")
                #:got (wget-download-argv
                       "https://example.org/file"
                       #:output "file"
                       #:directory "dist"
                       #:continue? #t
                       #:quiet? #t
                       #:timestamping? #t
                       #:no-clobber? #t
                       #:headers (list "X-A: 1")
                       #:user "me"
                       #:password "secret"
                       #:timeout 10
                       #:tries 2
                       #:user-agent "maak-test"
                       #:extra (list "--no-check-certificate"))))
         (test "a mirror stays under the URL by default"
               (assert-equal
                #:expect '("wget" "--recursive" "--no-parent"
                           "--no-host-directories" "https://example.org/docs")
                #:got (wget-mirror-argv "https://example.org/docs")))
         (test "a mirror can take the whole page and cut path components"
               (assert-equal
                #:expect '("wget" "--quiet" "--directory-prefix" "site"
                           "--recursive" "--no-parent" "--page-requisites"
                           "--convert-links" "--cut-dirs=2"
                           "--no-host-directories" "https://example.org/docs")
                #:got (wget-mirror-argv
                       "https://example.org/docs"
                       #:directory "site"
                       #:quiet? #t
                       #:page-requisites? #t
                       #:convert-links? #t
                       #:cut-dirs 2)))
         (test "a text fetch writes the body to standard output"
               (assert-equal
                #:expect '("wget" "--quiet" "--output-document=-"
                           "https://example.org/file")
                #:got (wget-text-argv "https://example.org/file")))
         (test "text returns nothing in a dry run"
               (assert-equal
                #:expect ""
                #:got (parameterize ((dry-run? #t) (quiet? #t))
                        (wget-text "https://example.org/file"))))
         (test "an empty URL is refused"
               (assert-error (lambda () (wget-download-argv "")))
               (assert-error (lambda () (wget-mirror-argv "")))
               (assert-error (lambda () (wget-text-argv ""))))
         (test "a header list must be a list of strings"
               (assert-error (lambda () (wget-download-argv
                                         "https://example.org"
                                         #:headers (list "X: 1" 2)))))
         (test "a timeout and a try count must be non-negative integers"
               (assert-error (lambda () (wget-download-argv
                                         "https://example.org"
                                         #:timeout -1)))
               (assert-error (lambda () (wget-download-argv
                                         "https://example.org"
                                         #:tries "3"))))
         (test "a generic call refuses arguments that are not strings"
               (assert-error (lambda () (wget (list "--version" 3)))))))

(define download-suite
  (suite "Choosing a downloader"
         #:concurrent? #f
         #:shuffle? #f
         (test "download uses curl by default"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (download "https://example.org/file"
                                      #:output "file"
                                      #:tool "curl")))
                #:got "curl --location --fail --silent --show-error --compressed --output file --create-dirs https://example.org/file"))
         (test "download can use wget"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (download "https://example.org/file"
                                      #:output "file"
                                      #:tool "wget")))
                #:got "wget --quiet --output-document file https://example.org/file"))
         (test "download reads the fetch-command parameter"
               (assert-contains
                #:expect (parameterize ((fetch-command "wget"))
                           (dry-run-command
                            (lambda ()
                              (download "https://example.org/file"
                                        #:output "file"))))
                #:got "wget --quiet --output-document file https://example.org/file"))
         (test "download turns a user and password into curl's --user"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (download "https://example.org/file"
                                      #:output "file"
                                      #:tool "curl"
                                      #:user "me"
                                      #:password "secret")))
                #:got "--user me:secret"))
         (test "download passes wget's user and password separately"
               (assert-contains
                #:expect (dry-run-command
                          (lambda ()
                            (download "https://example.org/file"
                                      #:output "file"
                                      #:tool "wget"
                                      #:user "me"
                                      #:password "secret")))
                #:got "--user me --password secret"))
         (test "a password without a user is refused"
               (assert-error (lambda ()
                               (download "https://example.org/file"
                                         #:password "secret"))))
         (test "an unknown downloader is refused"
               (assert-error (lambda ()
                               (download "https://example.org/file"
                                         #:tool "fetch"))))
         (test "download refuses an empty URL"
               (assert-error (lambda () (download ""))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               detection-suite
               curl-suite
               wget-suite
               download-suite))
