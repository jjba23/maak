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
;; Integration tests: the launcher under `scripts/', run as a real process.
;;
;; `cli.scm' calls `main' in process with the output ports captured, which is
;; how a test asserts on what a user reads while keeping a run cheap.  What it
;; cannot show is everything that belongs to the process rather than to the
;; call:
;;
;; - the exit status, which `main' returns but only a process reports to its
;;   parent -- turning that value into the status a shell reads is the
;;   launcher's job, and nothing below the launcher can test it,
;; - which of the two streams a line went to, as the shell sees it rather than
;;   as a captured port sees it, and
;; - the argument grammar of the launcher itself: `-C', `-f', `-k', `--' and
;;   the rest, which is what turns a command line into the Guile invocation
;;   `main' sees.  A test of `main' starts after that grammar has run, so none
;;   of it is visible from there.
;;
;; Those are tested here, on the launcher, against the fixtures under
;; `test/maak/fixtures' and against this checkout's own Maak file.  The
;; checkout's file is worth a case of its own: it is the one that asks for
;; modules a released library does not have, so a launcher that loads the
;; installed library instead of the sources beside it fails here rather than
;; silently in someone's terminal.
;;
;; Each case runs the launcher once and asserts on one thing about the run.
;; The process is started with `open-pipe*', so the launcher is a program and
;; its arguments are a list of strings that no shell ever splits; its standard
;; error is the only redirection, into a file of its own, because the split
;; between the two streams is half of what is being tested.  Everything else
;; about the launcher -- the Maak file it finds, the directory it enters -- is
;; what it would do for a user, which is the point.
;;
;; Two details of the assertions are deliberate.  A case asks for the facts it
;; needs about one run as a list -- the status, then what each stream holds -- so
;; that a failure shows every fact at once rather than only the first that
;; differed.  And the text asserted on is the message rather than the label in
;; front of it: maak writes its errors in colour, and the escape sits between
;; `maak: error:' and the message, so a needle that spans the two would be a
;; test of the colour codes.
;;
;; The paths are relative to the project root, which is where the test runner
;; is started from (see the Tests section of the README).

;;; Code:

(define-module (veritas unit integration)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak version)
  #:export (spec))

(define launcher "scripts/maak")

(define cli-fixture "test/maak/fixtures/cli.scm")
(define keep-going-fixture "test/maak/fixtures/keep-going.scm")
(define several-fixture "test/maak/fixtures/several.scm")
(define subdir "test/maak/fixtures/subdir")

(define (unique-path name)
  "Return a path of this run's own for NAME, under the temporary directory."
  (format #f "~a/maak-integration-~a-~a" (tmp-dir) (getpid) name))

(define no-maak-file-dir (unique-path "empty"))

(define (launcher-run . arguments)
  "Run the launcher with ARGUMENTS and return `(STATUS STDOUT STDERR)'.

The status is the one the process exited with, which is what an in-process test
cannot see, and the two streams are read apart from each other, which is what
tells a message to a user from a report to a caller.  The launcher is started
with `open-pipe*', so it is a program and its arguments rather than a line for a
shell to split; the one redirection is its standard error, into a file of its
own, because a pipe can only be read once and both streams are wanted."
  (let* ((err-file (unique-path "stderr"))
         (port (with-error-to-file err-file
                 (lambda ()
                   (apply open-pipe* OPEN_READ launcher arguments))))
         (stdout (get-string-all port))
         (status (status:exit-val (close-pipe port)))
         (stderr (read-file err-file #:default "")))
    (delete-file err-file)
    (list status stdout stderr)))

(define (status-of result)
  (list-ref result 0))

(define (stdout-of result)
  (list-ref result 1))

(define (stderr-of result)
  (list-ref result 2))

(define (stream-of result stream)
  "Return the stream of RESULT named by STREAM, `\"stdout\"' or `\"stderr\"'."
  (if (string=? stream "stderr") (stderr-of result) (stdout-of result)))

(define (said? result stream text)
  "Return whether the launcher wrote TEXT to STREAM of RESULT.

The answer is `#t' or `#f' rather than the offset `string-contains' finds, so
that a case can ask about it alongside the status of the run."
  (and (string-contains (stream-of result stream) text) #t))

;; ====== The suite ======
;;
;; The launcher runs the whole CLI in a process of its own, so these cases are
;; cheap but not free; each one starts the launcher once.  They are kept off the
;; concurrent path because they are about the process rather than about the
;; library, and a test that forks while its neighbours are mid-run says less
;; about the launcher than it does about the runner.

(define launcher-suite
  (suite "The launcher as a process"
         #:concurrent? #f
         #:shuffle? #f

         (test "this checkout's Maak file loads through the launcher beside it"
               (let ((result (launcher-run "--no-banner" "--list")))
                 (assert-equal #:expect 0
                               #:got (status-of result))))

         (test "the listing names a task only this checkout's Maak file defines"
               (let ((result (launcher-run "--no-banner" "--list")))
                 (assert-equal #:expect (list 0 #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "container:")))))

         (test "the help text is read from the resources the launcher ships beside itself"
               (let ((result (launcher-run "--help")))
                 (assert-equal #:expect (list 0 #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "Usage: maak")))))

         (test "the version banner is printed and the run succeeds"
               (let ((result (launcher-run "--no-banner" "--version")))
                 (assert-equal #:expect (list 0 #t)
                               #:got (list (status-of result)
                                           (said? result "stdout"
                                                  (maak-version-string))))))

         (test "a task of a named Maak file runs and reports its own output"
               (let ((result (launcher-run "-f" cli-fixture
                                           "--no-banner" "--" "ok")))
                 (assert-equal #:expect (list 0 #t #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "ok-task-ran")
                                           (string=? (stderr-of result) "")))))

         (test "a failing task exits non-zero and leaves its message on standard error"
               (let ((result (launcher-run "-f" cli-fixture
                                           "--no-banner" "--" "boom")))
                 ;; The message is the task's, not the colour codes around it,
                 ;; and the report of the run that a person reads is not the
                 ;; thing a caller capturing standard error wants.
                 (assert-equal #:expect (list 1 #t #f)
                               #:got (list (status-of result)
                                           (said? result "stderr" "no artefact for cli-test")
                                           (said? result "stdout" "no artefact for cli-test")))))

         (test "a task the Maak file does not define is refused, and the run says which exist"
               (let ((result (launcher-run "-f" cli-fixture
                                           "--no-banner" "--" "nope")))
                 (assert-equal #:expect (list 1 #t)
                               #:got (list (status-of result)
                                           (said? result "stderr" "Unknown task: nope")))))

         (test "-C enters the directory before the Maak file is looked for"
               (let ((result (launcher-run "-C" subdir
                                           "--no-banner" "--" "sub-marker")))
                 (assert-equal #:expect (list 0 #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "subdir-marker-ran")))))

         (test "a failure stops the tasks after it"
               (let ((result (launcher-run "-f" keep-going-fixture
                                           "--no-banner" "--"
                                           "first-pass,second-fail,last-pass")))
                 (assert-equal #:expect (list 1 #f)
                               #:got (list (status-of result)
                                           (said? result "stdout" "last-pass-ran")))))

         (test "--keep-going runs the rest and still exits non-zero"
               (let ((result (launcher-run "-f" keep-going-fixture
                                           "--no-banner" "--keep-going" "--"
                                           "first-pass,second-fail,third-fail,last-pass")))
                 (assert-equal #:expect (list 1 #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "last-pass-ran")))))

         ;; ---- The task and argument grammar ------------------------------
         ;;
         ;; Which word names a task and which is passed to one is the whole of
         ;; the command line's grammar, and the launcher is where it is read:
         ;; `main' is handed a task list and the words that follow `--'.  The
         ;; cases below are the grammar as a person meets it, one spelling at a
         ;; time.

         (test "several tasks named one word each after a `--' all run"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner" "--" "alpha" "beta")))
                 (assert-equal #:expect (list 0 #t #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "alpha-ran")
                                           (said? result "stdout" "beta-ran")))))

         (test "a comma separated list names the same run"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner" "--" "alpha,beta")))
                 (assert-equal #:expect (list 0 #t #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "alpha-ran")
                                           (said? result "stdout" "beta-ran")))))

         (test "the words after a task name are its arguments"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner" "echo-args" "one" "two")))
                 (assert-equal #:expect (list 0 #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "args=one,two")))))

         (test "a `--' sets the arguments apart and reads exactly the same"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner"
                                           "echo-args" "--" "one" "two")))
                 (assert-equal #:expect (list 0 #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "args=one,two")))))

         (test "an argument that looks like an option stays an argument"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner" "echo-args" "--" "-n")))
                 (assert-equal #:expect (list 0 #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "args=-n")))))

         (test "a second bare word is an argument, never a second task"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner" "echo-args" "alpha")))
                 ;; `alpha' names a task of the fixture, and it is still passed
                 ;; as an argument: the task list is the first word, or the
                 ;; words a `--' introduces, and never a guess at a word that
                 ;; happens to name a task.
                 (assert-equal #:expect (list 0 #t #f)
                               #:got (list (status-of result)
                                           (said? result "stdout" "args=alpha")
                                           (said? result "stdout" "alpha-ran")))))

         (test "-t names the tasks, so the other words are their arguments"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner"
                                           "-t" "echo-args" "one" "two")))
                 (assert-equal #:expect (list 0 #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "args=one,two")))))

         (test "--tasks names several tasks, the way the library reads them"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner" "--tasks" "alpha,beta")))
                 (assert-equal #:expect (list 0 #t #t)
                               #:got (list (status-of result)
                                           (said? result "stdout" "alpha-ran")
                                           (said? result "stdout" "beta-ran")))))

         (test "a word that names a task, passed as an argument, is pointed out"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner" "alpha" "beta")))
                 ;; The run a shell habit writes: two words, of which only the
                 ;; first names a task.  The launcher reads it exactly as the
                 ;; grammar says, and the failure then teaches that grammar
                 ;; rather than leaving an arity message to be puzzled over.
                 (assert-equal
                  #:expect (list 1 #t #t)
                  #:got (list (status-of result)
                              (said? result "stderr" "Wrong number of arguments")
                              (said? result "stderr"
                                     "To run several tasks, name them in one comma separated list (`maak alpha,beta') or one word each after `--' (`maak -- alpha beta')")))))

         (test "a `--' ends the options, so a late one is read as a task"
               (let ((result (launcher-run "-f" several-fixture
                                           "--no-banner" "--" "alpha" "beta" "-n")))
                 ;; The grammar is deliberately literal here.  Everything after
                 ;; the `--' that introduced the task list is a word for maak,
                 ;; and `-n' is not one of its tasks; the run says which tasks it
                 ;; does have rather than guessing at the dry run the word
                 ;; could have been an option for.
                 (assert-equal #:expect (list 1 #t)
                               #:got (list (status-of result)
                                           (said? result "stderr" "Unknown task: -n")))))

         (test "a directory with no Maak file is refused rather than run"
               ;; The directory is made and removed around the run: the run
               ;; itself needs it to exist and to be empty, and the assertion is
               ;; the last form of the case, so the cleanup goes in the `after'
               ;; thunk of a `dynamic-wind' rather than after the assertion.
               (dynamic-wind
                 (lambda () (mkdir no-maak-file-dir))
                 (lambda ()
                   (let ((result (launcher-run "-C" no-maak-file-dir
                                               "--no-banner")))
                     (assert-equal #:expect (list 1 #t)
                                   #:got (list (status-of result)
                                               (said? result "stderr"
                                                      "No Maak file found")))))
                 (lambda () (rmdir no-maak-file-dir))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               launcher-suite))
