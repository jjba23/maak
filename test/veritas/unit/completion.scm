;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Unit tests for the shell completions: the description they are generated
;; from, and the three scripts that description becomes.
;;
;; Two kinds of case live here.
;;
;; The first compares the checked-in scripts with what `(maak completion)'
;; renders.  That is the whole reason the scripts are generated rather than
;; written: three hand-kept files cannot notice that an option was added above
;; them, and a script that offers an option maak no longer has is worse than one
;; that offers nothing.  A difference here is not a formatting nit -- it means
;; the file a user installs is not the file this maak describes.
;;
;; The second runs the scripts in a real shell.  bash's `_maak' is driven by
;; setting the two variables the completion system would have set and reading
;; COMPREPLY; fish answers `complete -C', which is its own headless completion;
;; zsh's completion cannot be driven without a terminal, so the two helpers it
;; hands candidates to are replaced by stubs and their arguments are asserted
;; on.  All three run against a Maak file fixture, because the question they
;; answer -- which tasks has this project -- is asked of maak, and a fixture
;; whose tasks are known answers it in a test that does not depend on this
;; repository's own Maak file.
;;
;; A shell that is not installed is skipped rather than failed: the scripts are
;; installed by the package, not by this suite, and a machine without zsh can
;; still check that the zsh script is the text maak renders.

;;; Code:

(define-module (veritas unit completion)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak completion)
  #:use-module (maak dsl)
  #:use-module (maak main)
  #:use-module (maak shell)
  #:export (spec))

;; The fixture whose tasks these cases offer: exported `build', `deploy',
;; `report' and `echo', plus `tidy', which asked to be left out of the listings.
(define fixture-directory "contrib/test/fixture")
(define fixture-file "contrib/test/fixture/maak.scm")

(define shells-with-scripts '(bash zsh fish))

(define (script-file shell)
  "Return the path of the checked-in completion script for SHELL."
  (format #f "scripts/maak-completion.~a" shell))

(define (checked-in shell)
  "Return the text of the checked-in completion script for SHELL."
  (read-file (script-file shell) #:default ""))

(define (shell-present? shell)
  "Return whether SHELL can be run here."
  (command-exists? (symbol->string shell)))

(define (unique-path name)
  "Return a path of this run's own for NAME, under the temporary directory."
  (format #f "~a/maak-completion-~a-~a" (tmp-dir) (getpid) name))

(define (temporary-maak-file)
  "Write a Maak file of one task of its own, and return `(DIRECTORY . PATH)'.

A task only this file defines is what tells a completion that read the file the
command line named from one that read the file it found by itself, and a
directory of its own is what tells it the same about `-C'."
  (let* ((directory (unique-path "elsewhere"))
         (path (format #f "~a/maak.scm" directory)))
    (unless (file-exists? directory)
      (mkdir directory))
    (call-with-output-file path
      (lambda (port)
        (display "(define-module (maak)
  #:export (twelve))

(define (twelve)
  \"The twelfth task.\"
  #t)
" port)))
    (cons directory path)))

(define (maak-shim)
  "Return a directory whose `maak' runs this checkout's launcher.

The completion scripts run `maak', which is what a user has on PATH; a test that
ran `scripts/maak' instead would test a spelling no completion ever uses.  The
shim is a two line shell script rather than a symlink because the launcher finds
its library by the directory it was started from, and a link elsewhere would
point it at a tree that holds no sources."
  (let* ((directory (unique-path "bin"))
         (shim (format #f "~a/maak" directory)))
    (unless (file-exists? directory)
      (mkdir directory))
    (call-with-output-file shim
      (lambda (port)
        (format port "#!/bin/sh\nexec ~a \"$@\"\n"
                (canonicalize-path "scripts/maak"))))
    (chmod-file "755" shim)
    directory))

(define (run-program . argv)
  "Run ARGV and return `(STATUS STDOUT STDERR)'.

The two streams are read apart because maak writes a user's error to standard
error and a listing to standard output, and a case that asked whether a refusal
happened has to look where the refusal went.  Standard error goes through a file
of its own: a pipe can be read once, and both streams are wanted."
  (let* ((error-file (unique-path "stderr"))
         (port (with-error-to-file error-file
                 (lambda () (apply open-pipe* OPEN_READ argv))))
         (output (get-string-all port))
         (status (status:exit-val (close-pipe port)))
         (errors (read-file error-file #:default "")))
    (delete-file error-file)
    (list status output errors)))

(define (status-of result)
  (car result))

(define (stdout-of result)
  (second result))

(define (stderr-of result)
  (third result))

;; ====== The scripts are what maak renders ======

(define generation-suite
  (suite "The completions are generated from one description"
         #:concurrent? #f
         #:shuffle? #f

         (test "the bash script is what maak renders"
               (assert-equal #:expect (completion-script 'bash)
                             #:got (checked-in 'bash)))

         (test "the zsh script is what maak renders"
               (assert-equal #:expect (completion-script 'zsh)
                             #:got (checked-in 'zsh)))

         (test "the fish script is what maak renders"
               (assert-equal #:expect (completion-script 'fish)
                             #:got (checked-in 'fish)))

         (test "an unknown shell is refused rather than answered"
               (assert-error (lambda () (completion-script 'tcsh))))))

;; ====== The description and the option grammar agree ======
;;
;; `maak-option-spec' is what the command line obeys and `completion-options' is
;; what a shell is told, so the two are kept equal here rather than by whoever
;; adds the next option remembering both.

(define (spec-field entry key)
  "Return the value of KEY in the option spec ENTRY, or #f when it has none."
  (let ((pair (assq key (cdr entry))))
    (and pair (cadr pair))))

(define (spec-option option)
  "Return the spec entry of OPTION, a name from the completion table."
  (let ((name (string->symbol (string-drop (car option) 2))))
    (or (assq name maak-option-spec)
        (error "the completions describe an option the grammar has not" name))))

;; The launcher's own plumbing: `--resources' names the directory the help
;; screen is read from, and a user never types it.
(define internal-options '(resources))

(define (described-option? name)
  "Return the completion table's entry for the long option called NAME, or #f."
  (find (lambda (option) (string=? (car option) (format #f "--~a" name)))
        completion-options))

(define (shell-offers? shell option)
  "Return whether the script for SHELL offers OPTION.

Each shell spells a long option its own way -- bash writes the word in an array,
zsh inside an `_arguments' spec, and fish as `-l NAME' -- so the needle is the
spelling that shell's script would have."
  (let ((text (completion-script shell))
        (name (string-drop (car option) 2)))
    (if (eq? shell 'fish)
        (string-contains text (format #f "-l ~a " name))
        (string-contains text (car option)))))

(define agreement-suite
  (suite "The option table and the option grammar agree"
         #:concurrent? #f
         #:shuffle? #f

         (test "every option of the grammar is described"
               (assert-equal
                #:expect '()
                #:got (remove (lambda (name) (described-option? name))
                              (remove (lambda (name)
                                        (memq name internal-options))
                                      (map car maak-option-spec)))))

         (test "no described option is one the grammar does not have"
               (assert-equal
                #:expect '()
                #:got (remove (lambda (option)
                                (assq (string->symbol
                                       (string-drop (car option) 2))
                                      maak-option-spec))
                              completion-options)))

         (test "a value is described exactly when the grammar takes one"
               (assert-equal
                #:expect '()
                #:got (remove
                       (lambda (option)
                         (let ((takes-value?
                                (not (eq? (spec-field (spec-option option) 'value)
                                          #f)))
                               (described-value?
                                (not (eq? (third option) 'flag))))
                           (eq? takes-value? described-value?)))
                       completion-options)))

         (test "a short name is described exactly when the grammar has one"
               (assert-equal
                #:expect '()
                #:got (remove
                       (lambda (option)
                         (let ((short (spec-field (spec-option option)
                                                  'single-char))
                               (described (second option)))
                           (if short
                               (and described
                                    (string=? described
                                              (format #f "-~a" short)))
                               (not described))))
                       completion-options)))

         (test "every generated script offers every described option"
               (assert-equal
                #:expect '()
                #:got (remove
                       (lambda (option)
                         (every (lambda (shell)
                                  (shell-offers? shell option))
                                shells-with-scripts))
                       completion-options)))))

;; ====== What `--list=completion' prints ======

(define (listed-tasks layout)
  "Return the fixture's task names as LAYOUT prints them.

The empty line a final newline leaves is dropped, so two layouts can be compared
as the list of tasks they describe rather than as text."
  (remove (lambda (line) (string=? line ""))
          (map (lambda (line) (car (string-split line #\tab)))
               (string-split
                (stdout-of (run-program "scripts/maak" layout "-f" fixture-file))
                #\newline))))

(define listing-suite
  (suite "The listing a shell reads"
         #:concurrent? #f
         #:shuffle? #f

         (test "a task is a name, a tab and the first line of its docstring"
               (assert-contains
                #:expect (stdout-of (run-program "scripts/maak"
                                                    "--list=completion"
                                                    "-f" fixture-file))
                #:got "build\tCompile everything."))

         (test "a task that asked to be left out is left out"
               (assert-false (string-contains
                              (stdout-of (run-program "scripts/maak"
                                                         "--list=completion"
                                                         "-f" fixture-file))
                              "tidy")))

         (test "names and completion describe the same tasks"
               (assert-equal
                #:expect (listed-tasks "--list=names")
                #:got (listed-tasks "--list=completion")))))

;; ====== The scripts in a real shell ======
;;
;; The drivers below are constants: every path a driver needs arrives through
;; the environment, so nothing here builds a shell command out of data.

(define bash-driver
  "source \"$MAAK_COMPLETION\" || exit 9
cd \"$MAAK_COMPLETION_DIR\" || exit 9
COMP_WORDS=(\"$@\")
COMP_CWORD=$(( ${#COMP_WORDS[@]} - 1 ))
_maak
printf '%s\\n' \"${COMPREPLY[@]}\"
")

(define fish-driver
  "source $MAAK_COMPLETION; or exit 9
cd $MAAK_COMPLETION_DIR; or exit 9
complete -C(string join ' ' -- $argv)
")

(define zsh-driver
  "cd $MAAK_COMPLETION_DIR || exit 9
compdef() { : }
source $MAAK_COMPLETION || exit 9
words=(maak \"$@\")
CURRENT=$(( ${#words} ))
_arguments() { print -r -- \"SPECS $#\" }
_maak
# The line and the cursor as `_maak' kept them for the task action.
maak_line=(\"${words[@]}\")
maak_position=$CURRENT
_sequence() { print -r -- \"${entries[@]}\" }
_maak_tasks
")

(define (drive-shell shell driver . words)
  "Complete WORDS in SHELL through DRIVER, and return `(STATUS STDOUT STDERR)'."
  (apply run-program
         (append
          (list "env"
                (format #f "PATH=~a:~a" (maak-shim) (getenv "PATH"))
                (format #f "MAAK_COMPLETION=~a"
                        (canonicalize-path (script-file shell)))
                (format #f "MAAK_COMPLETION_DIR=~a"
                        (canonicalize-path fixture-directory))
                (symbol->string shell) "-c" driver)
          ;; bash and zsh read the first word after the script as the name they
          ;; were started under, so the driver is given one and the words follow
          ;; it; fish hands the words over on their own.
          (if (eq? shell 'fish)
              words
              (cons "maak-completion" words)))))

(define shell-suite
  (suite "The scripts in a real shell"
         #:concurrent? #f
         #:shuffle? #f

         (test "the bash script parses as bash"
               (assert-equal #:expect 0
                             #:got (status-of (run-program
                                               "bash" "-n" (script-file 'bash)))))

         (test "the bash script offers the tasks of the Maak file"
               (assert-contains #:expect (stdout-of (drive-shell 'bash bash-driver
                                                                 "maak" ""))
                                #:got "deploy"))

         (test "the bash script offers only the tasks that start with the word"
               (assert-equal #:expect (list 0 "build")
                             #:got (let ((result (drive-shell 'bash bash-driver
                                                              "maak" "bu")))
                                     (list (status-of result)
                                           (string-trim-both (stdout-of result))))))

         (test "the bash script offers the options of the grammar"
               (assert-contains #:expect (stdout-of (drive-shell 'bash bash-driver
                                                                 "maak" "--li"))
                                #:got "--list-width"))

         (test "the bash script offers the layouts after --list="
               (assert-contains #:expect (stdout-of (drive-shell 'bash bash-driver
                                                                 "maak" "--list="))
                                #:got "--list=completion"))

         (test "the bash script carries a comma separated task list on"
               (assert-contains #:expect (stdout-of (drive-shell 'bash bash-driver
                                                                 "maak" "echo,de"))
                                #:got "echo,deploy"))

         (test "the bash script asks for no number"
               (assert-equal #:expect (list 0 "")
                             #:got (let ((result (drive-shell 'bash bash-driver
                                                              "maak" "-j" "")))
                                     (list (status-of result)
                                           (string-trim-both (stdout-of result))))))

         (test "the bash script offers files after --file"
               (assert-contains #:expect (stdout-of (drive-shell 'bash bash-driver
                                                                 "maak" "-f" "ma"))
                                #:got "maak.scm"))

         (test "the bash script reads the file a --file names"
               (assert-equal #:expect "twelve"
                             #:got (string-trim-both
                                    (stdout-of
                                     (drive-shell 'bash bash-driver
                                                  "maak" "-f"
                                                  (cdr (temporary-maak-file))
                                                  "")))))

         (test "the bash script reads the directory a -C enters, and says nothing else"
               (assert-equal #:expect "twelve"
                             #:got (string-trim-both
                                    (stdout-of
                                     (drive-shell 'bash bash-driver
                                                  "maak" "-C"
                                                  (car (temporary-maak-file))
                                                  "")))))

         (test "the bash script offers no option after a --"
               (assert-false (string-contains (stdout-of (drive-shell
                                                          'bash bash-driver
                                                          "maak" "--" "--j"))
                                              "--jobs")))

         (test "the zsh script parses as zsh"
               (if (shell-present? 'zsh)
                   (assert-equal #:expect 0
                                 #:got (status-of (run-program
                                                   "zsh" "-n"
                                                   (script-file 'zsh))))
                   (assert-true #t)))

         (test "the zsh script describes every option to _arguments"
               (if (shell-present? 'zsh)
                   (assert-contains #:expect (stdout-of (drive-shell 'zsh zsh-driver
                                                                     "ag"))
                                    #:got "SPECS 40")
                   (assert-true #t)))

         (test "the zsh script hands _describe a name and its summary"
               (if (shell-present? 'zsh)
                   (assert-contains
                    #:expect (stdout-of (drive-shell 'zsh zsh-driver "ag"))
                    #:got "build:Compile everything.")
                   (assert-true #t)))

         (test "the zsh script leaves a task that asked to be left out alone"
               (if (shell-present? 'zsh)
                   (assert-false (string-contains
                                  (stdout-of (drive-shell 'zsh zsh-driver "ag"))
                                  "tidy:"))
                   (assert-true #t)))

         (test "the zsh script reads the file a --file names"
               (if (shell-present? 'zsh)
                   (assert-contains
                    #:expect (stdout-of (drive-shell 'zsh zsh-driver
                                                    "-f"
                                                    (cdr (temporary-maak-file))
                                                    "tw"))
                    #:got "twelve:The twelfth task.")
                   (assert-true #t)))

         (test "the fish script parses as fish"
               (if (shell-present? 'fish)
                   (assert-equal #:expect 0
                                 #:got (status-of (run-program
                                                   "fish" "--no-execute"
                                                   (script-file 'fish))))
                   (assert-true #t)))

         (test "the fish script offers a task with its docstring"
               (if (shell-present? 'fish)
                   (assert-contains #:expect (stdout-of (drive-shell
                                                         'fish fish-driver
                                                         "maak" "bu"))
                                    #:got "build\tCompile everything.")
                   (assert-true #t)))

         (test "the fish script offers the layouts after --list="
               (if (shell-present? 'fish)
                   (assert-contains #:expect (stdout-of (drive-shell
                                                         'fish fish-driver
                                                         "maak" "--list="))
                                    #:got "--list=json")
                   (assert-true #t)))))

;; ====== The command line that prints a script ======

(define command-suite
  (suite "`--completion' on the command line"
         #:concurrent? #f
         #:shuffle? #f

         (test "--completion prints the script the file holds"
               (assert-equal #:expect (completion-script 'fish)
                             #:got (stdout-of (run-program
                                               "scripts/maak"
                                               "--completion" "fish"))))

         (test "--completion needs no Maak file"
               (assert-equal
                #:expect (list 0 #t)
                #:got (let ((empty (unique-path "empty")))
                        (mkdir empty)
                        (let ((result
                               (run-program
                                "sh" "-c" "cd \"$1\" || exit 9; shift; exec \"$@\""
                                "sh" empty
                                (canonicalize-path "scripts/maak")
                                "--completion" "bash")))
                          (rmdir empty)
                          (list (status-of result)
                                (and (string-contains (stdout-of result)
                                                      "complete -F _maak maak")
                                     #t))))))

         (test "an unknown shell is refused with the shells maak knows"
               (assert-equal
                #:expect (list 1 #t)
                #:got (let ((result (run-program "scripts/maak"
                                                 "--completion" "tcsh")))
                        (list (status-of result)
                              (and (string-contains (stderr-of result)
                                                    "bash, zsh, fish")
                                   #t)))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               generation-suite
               agreement-suite
               listing-suite
               shell-suite
               command-suite))
