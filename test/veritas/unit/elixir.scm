;; maak
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Tests for `(maak elixir)': the builders behind `mix test', `mix format',
;; `mix release' and `mix cmd', the generic `mix' line, and the two readers --
;; `mix.exs' and `mix.lock' -- that answer what the project is without starting
;; Elixir.
;;
;; No Elixir is needed.  A builder is asserted as a list of words, a wrapper is
;; read from what a dry run prints, and the programs the two version queries run
;; are small scripts this spec writes, so the answer is read rather than
;; assumed.  Every option name a case asserts was checked against
;; `mix help TASK' from Elixir 1.19.3.
;;
;; The suites run sequentially because they change process-wide state: the
;; program parameters and a few scratch directories.

;;; Code:

(define-module (veritas unit elixir)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak elixir)
  #:export (spec))

(define (scratch-directory label)
  (let ((dir (format #f "~a/maak-elixir-~a-~a" (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (write-text path text)
  (call-with-output-file path
    (lambda (port) (display text port))))

(define (fake-program directory name text)
  "Write a stand-in for NAME in DIRECTORY that runs TEXT, and return its path."
  (let ((path (in-vicinity directory name)))
    (write-text path (string-append "#!/bin/sh\n" text))
    (chmod path #o755)
    path))

(define (with-scratch label thunk)
  (let ((dir (scratch-directory label)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk dir))
      (lambda ()
        (when (file-exists? dir)
          (parameterize ((quiet? #t))
            (delete-file-recursively dir)))))))

(define (error-of thunk)
  "Return the message THUNK raises, or #f when it returns."
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . values)
      (and (maak-error? (car values))
           (maak-error-message (car values))))))

;; ====== The files this spec reads ======
;;
;; The project file is the one `mix new' writes, with one change: the commented
;; out dependency it leaves behind is spelled the way a developer edits it, with
;; a `git:' and a `tag:' inside a comment.  A reader that scanned the text
;; without dropping the comments would answer `9.9.9' for one of them, so the
;; case below is the reason comments go first.

(define mix-exs
  "defmodule Example.MixProject do
  use Mix.Project

  def project do
    [
      app: :example,
      version: \"0.3.1\",
      elixir: \"~> 1.15\",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run \"mix help compile.app\" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # {:dep_from_git, git: \"https://example.com/x.git\", tag: \"9.9.9\"},
  defp deps do
    [
      {:decimal, \"~> 2.0\"}
    ]
  end
end
")

(define umbrella-exs
  "defmodule Umbrella.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: \"apps\",
      version: \"0.1.0\",
      deps: deps()
    ]
  end
end
")

(define computed-exs
  "defmodule Computed.MixProject do
  use Mix.Project

  def project do
    [
      app: :computed,
      version: version()
    ]
  end

  defp version, do: \"1.0.0\"
end
")

(define mix-lock
  "%{
  \"decimal\" => {:hex, :decimal, \"2.1.1\", \"a1b2\", [:mix], [], \"hexpm\", \"c3d4\"},
  \"phoenix\" => {:hex, :phoenix, \"1.7.10\", \"e5f6\", [:mix], [{:plug, \"~> 1.14\", [hex: :plug, repo: \"hexpm\"]}], \"hexpm\", \"a7b8\"},
  \"local_dep\" => {:git, \"https://example.com/local.git\", \"0bed9c8\", []}
}
")

;; ====== The generic command line ======

(define argv-suite
  (suite "The mix command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "a plain task is the program and its name"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "compile")
                               #:got (mix-argv "compile"))))
         (test "arguments follow the task"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "ecto.migrate" "-r" "Repo")
                               #:got (mix-argv "ecto.migrate"
                                               #:arguments '("-r" "Repo")))))
         (test "the switches Mix documents go between the task and its arguments"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "test" "--force" "--no-compile" "--no-start"
                             "test/a_test.exs")
                  #:got (mix-argv "test"
                                  #:force? #t
                                  #:no-compile? #t
                                  #:no-start? #t
                                  #:arguments '("test/a_test.exs")))))
         (test "every check Mix documents is spelled out"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "deps.get" "--no-archives-check"
                             "--no-compile" "--no-deps-check"
                             "--no-elixir-version-check" "--no-start")
                  #:got (mix-argv "deps.get"
                                  #:no-archives-check? #t
                                  #:no-compile? #t
                                  #:no-deps-check? #t
                                  #:no-elixir-version-check? #t
                                  #:no-start? #t))))
         (test "a program value may carry a wrapper"
               (parameterize ((mix-command "asdf exec mix"))
                 (assert-equal #:expect '("asdf" "exec" "mix" "compile")
                               #:got (mix-argv "compile"))))
         (test "a task that is not a name is refused"
               (assert-error (lambda () (mix-argv #f))))))

;; ====== mix test ======

(define test-argv-suite
  (suite "The mix test command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "no option is just the task"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "test")
                               #:got (mix-test-argv))))
         (test "a seed and a trace are the words Mix takes"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "test" "--trace" "--seed" "0")
                               #:got (mix-test-argv #:trace? #t #:seed 0))))
         (test "one filter is one flag"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "test" "--only" "integration")
                               #:got (mix-test-argv #:only "integration"))))
         (test "several filters repeat the flag, the way Mix reads them"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "test" "--only" "slow" "--only" "integration")
                  #:got (mix-test-argv #:only '("slow" "integration")))))
         (test "an include and an exclude keep their order"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "test" "--include" "ci" "--exclude" "slow")
                  #:got (mix-test-argv #:include "ci" #:exclude "slow"))))
         (test "coverage, failures and timing are numbers or switches"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "test" "--cover" "--max-failures" "1"
                             "--slowest" "5" "--partitions" "4")
                  #:got (mix-test-argv #:cover? #t
                                       #:max-failures 1
                                       #:slowest 5
                                       #:partitions 4))))
         (test "the switches that only say yes are written bare"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "test" "--failed" "--stale"
                             "--warnings-as-errors" "--force"
                             "--repeat-until-failure" "10")
                  #:got (mix-test-argv #:failed? #t
                                       #:stale? #t
                                       #:warnings-as-errors? #t
                                       #:force? #t
                                       #:repeat-until-failure 10))))
         (test "nothing is stale unless the task asks for it"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "test")
                               #:got (mix-test-argv #:cover? #f))))
         (test "the files come after the options, as Mix expects"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "test" "--trace" "test/a_test.exs:12")
                  #:got (mix-test-argv #:trace? #t
                                       #:arguments '("test/a_test.exs:12")))))
         (test "a seed that is not a number is refused"
               (assert-contains #:got "seed"
                                #:expect (error-of
                                       (lambda () (mix-test-argv #:seed "soon")))))
         (test "a partition count of zero is refused"
               (assert-contains #:got "partitions"
                                #:expect (error-of
                                       (lambda ()
                                         (mix-test-argv #:partitions 0)))))))

;; ====== mix format ======

(define format-argv-suite
  (suite "The mix format command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "no option is just the task"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "format")
                               #:got (mix-format-argv))))
         (test "the gate a CI writes is two switches"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "format" "--check-formatted"
                                          "--dry-run")
                               #:got (mix-format-argv #:check-formatted? #t
                                                      #:dry-run? #t))))
         (test "a formatter file and a stdin name take their values"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "format" "--dot-formatter" ".formatter.exs"
                             "--stdin-filename" "lib/a.ex")
                  #:got (mix-format-argv #:dot-formatter ".formatter.exs"
                                         #:stdin-filename "lib/a.ex"))))
         (test "migration is a switch, and the files come last"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "format" "--migrate" "lib" "test")
                               #:got (mix-format-argv #:migrate? #t
                                                      #:arguments '("lib" "test")))))))

;; ====== mix release ======

(define release-argv-suite
  (suite "The mix release command line"
         #:concurrent? #f
         #:shuffle? #f
         (test "no name is the release the project declares"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "release")
                               #:got (mix-release-argv))))
         (test "a name comes first, before its switches"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "release" "example" "--overwrite")
                               #:got (mix-release-argv "example" #:overwrite? #t))))
         (test "a path, a quiet run and a version take their values"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "release" "--path" "dist" "--quiet"
                             "--version" "1.2.3")
                  #:got (mix-release-argv #:path "dist"
                                          #:quiet? #t
                                          #:version "1.2.3"))))
         (test "the build checks are the switches Mix documents"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "release" "--no-archives-check" "--no-compile"
                             "--no-deps-check" "--no-elixir-version-check")
                  #:got (mix-release-argv #:no-archives-check? #t
                                          #:no-compile? #t
                                          #:no-deps-check? #t
                                          #:no-elixir-version-check? #t))))
         (test "an empty name is refused rather than run"
               (assert-contains #:got "non-empty"
                                #:expect (error-of
                                       (lambda () (mix-release-argv "")))))))

;; ====== mix cmd ======

(define cmd-suite
  (suite "Running a command in a directory or for an umbrella app"
         #:concurrent? #f
         #:shuffle? #f
         (test "a command is handed to mix cmd as it is"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "cmd" "make" "test")
                               #:got (mix-cmd-argv '("make" "test")))))
         (test "a directory is the documented --cd"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "cmd" "--cd" "third-party" "make")
                               #:got (mix-cmd-argv '("make") #:cd "third-party"))))
         (test "shell expansion is asked for by name"
               (parameterize ((mix-command "mix"))
                 (assert-equal #:expect '("mix" "cmd" "--shell" "echo" "$HOME")
                               #:got (mix-cmd-argv '("echo" "$HOME")
                                                   #:shell? #t))))
         (test "several apps are one --app each, through mix do"
               (parameterize ((mix-command "mix"))
                 (assert-equal
                  #:expect '("mix" "do" "--app" "web" "--app" "api"
                             "cmd" "mix" "test")
                  #:got (mix-cmd-argv '("mix" "test")
                                      #:apps '("web" "api")))))
         (test "a directory and several apps are two different runs"
               (assert-contains
                #:got "not both"
                #:expect (error-of (lambda ()
                                  (mix-cmd-argv '("make")
                                                #:cd "third-party"
                                                #:apps '("web"))))))
         (test "an empty command is refused"
               (assert-error (lambda () (mix-cmd-argv '()))))))

;; ====== The wrappers, and what the installed Elixir says ======

(define running-suite
  (suite "The wrappers and the version queries"
         #:concurrent? #f
         #:shuffle? #f
         (test "each wrapper prints the command it would run"
               (parameterize ((mix-command "mix"))
                 (let ((out (open-output-string)))
                   (parameterize ((current-output-port out)
                                  (dry-run? #t)
                                  (quiet? #f))
                     (mix-deps-get "--only" "prod")
                     (mix-compile)
                     (mix-test "--trace")
                     (mix-format "--check-formatted")
                     (mix-release "example")
                     (mix-run "--no-start")
                     (mix-clean))
                   (let ((text (get-output-string out)))
                     (assert-equal
                      #:expect '(#t #t #t #t #t #t #t)
                      #:got (list (and (string-contains text "mix deps.get --only prod") #t)
                                  (and (string-contains text "mix compile") #t)
                                  (and (string-contains text "mix test --trace") #t)
                                  (and (string-contains text "mix format --check-formatted") #t)
                                  (and (string-contains text "mix release example") #t)
                                  (and (string-contains text "mix run --no-start") #t)
                                  (and (string-contains text "mix clean") #t)))))))
         (test "the Mix version is the number the program reports"
               (with-scratch "mix-version"
                (lambda (dir)
                  (parameterize
                      ((mix-command
                        (fake-program dir "mix"
                                      "echo 'Erlang/OTP 27 [erts-15.2.7.4] [source]'\necho\necho 'Mix 1.19.3 (compiled with Erlang/OTP 27)'\n")))
                    (assert-equal #:expect "1.19.3"
                                  #:got (mix-version))))))
         (test "the Elixir version is the number its own line carries"
               (with-scratch "elixir-version"
                (lambda (dir)
                  (parameterize
                      ((elixir-command
                        (fake-program dir "elixir"
                                      "echo 'Erlang/OTP 27 [erts-15.2.7.4] [source]'\necho\necho 'Elixir 1.19.3 (compiled with Erlang/OTP 27)'\n")))
                    (assert-equal #:expect "1.19.3"
                                  #:got (elixir-version))))))
         (test "a version line that is not there is refused by name"
               (with-scratch "mix-version-missing"
                (lambda (dir)
                  (parameterize ((mix-command
                                  (fake-program dir "mix" "echo 'not mix'\n")))
                    (assert-contains #:got "Mix "
                                     #:expect (error-of (lambda () (mix-version))))))))
         (test "a failing query names the exit status"
               (with-scratch "mix-output-failure"
                (lambda (dir)
                  (parameterize ((mix-command
                                  (fake-program dir "mix" "exit 3\n")))
                    (assert-contains
                     #:got "exit code 3"
                     #:expect (error-of
                            (lambda () (mix-output (mix-argv "compile")))))))))))

;; ====== mix.exs ======

(define project-suite
  (suite "Reading mix.exs"
         #:concurrent? #f
         #:shuffle? #f
         (test "the fields are read in file order, comments left out"
               (with-scratch "project-fields"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.exs")))
                    (write-text path mix-exs)
                    (assert-equal
                     #:expect '(("app" . "example")
                                ("version" . "0.3.1")
                                ("elixir" . "~> 1.15"))
                     #:got (mix-project path))))))
         (test "the application name loses the atom's colon"
               (with-scratch "project-app"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.exs")))
                    (write-text path mix-exs)
                    (assert-equal #:expect "example"
                                  #:got (mix-project-name path))))))
         (test "the version is the string the project declares"
               (with-scratch "project-version"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.exs")))
                    (write-text path mix-exs)
                    (assert-equal #:expect "0.3.1"
                                  #:got (mix-project-version path))))))
         (test "a dependency commented out is not a field"
               (with-scratch "project-comment"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.exs")))
                    (write-text path mix-exs)
                    (assert-false (assoc "tag" (mix-project path)))))))
         (test "the Elixir requirement is what mix.exs asks for"
               (with-scratch "project-elixir"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.exs")))
                    (write-text path mix-exs)
                    (assert-equal #:expect "~> 1.15"
                                  #:got (mix-project-elixir path))))))
         (test "an umbrella names its applications directory"
               (with-scratch "project-umbrella"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.exs")))
                    (write-text path umbrella-exs)
                    (assert-equal #:expect '(#t "apps")
                                  #:got (list (mix-project-umbrella? path)
                                              (cdr (assoc "apps_path"
                                                          (mix-project path)))))))))
         (test "a project that is not an umbrella says so"
               (with-scratch "project-not-umbrella"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.exs")))
                    (write-text path mix-exs)
                    (assert-false (mix-project-umbrella? path))))))
         (test "a version a function computes is refused rather than guessed"
               (with-scratch "project-computed"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.exs")))
                    (write-text path computed-exs)
                    (assert-contains
                     #:got "computes"
                     #:expect (error-of
                            (lambda () (mix-project-version path))))))))
         (test "a field the file does not declare is named"
               (with-scratch "project-missing-field"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.exs")))
                    (write-text path umbrella-exs)
                    (assert-contains
                     #:got "app"
                     #:expect (error-of (lambda () (mix-project-name path))))))))
         (test "a directory without mix.exs is refused by name"
               (with-scratch "project-absent"
                (lambda (dir)
                  (assert-contains
                   #:got "mix.exs"
                   #:expect (error-of
                          (lambda ()
                            (mix-project (in-vicinity dir "mix.exs"))))))))))

;; ====== mix.lock ======

(define lock-suite
  (suite "Reading mix.lock"
         #:concurrent? #f
         #:shuffle? #f
         (test "every hex dependency answers with its version"
               (with-scratch "lock-versions"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.lock")))
                    (write-text path mix-lock)
                    (assert-equal #:expect '(("decimal" . "2.1.1")
                                             ("phoenix" . "1.7.10"))
                                  #:got (mix-lock-versions path))))))
         (test "one dependency is asked for by name"
               (with-scratch "lock-one"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.lock")))
                    (write-text path mix-lock)
                    (assert-equal #:expect "1.7.10"
                                  #:got (mix-lock-version "phoenix" path))))))
         (test "a dependency that is not pinned is refused as such"
               (with-scratch "lock-unknown"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.lock")))
                    (write-text path mix-lock)
                    (assert-contains
                     #:got "no dependency called"
                     #:expect (error-of
                            (lambda () (mix-lock-version "ecto" path))))))))
         (test "a git dependency carries a revision, not a version"
               (with-scratch "lock-git"
                (lambda (dir)
                  (let ((path (in-vicinity dir "mix.lock")))
                    (write-text path mix-lock)
                    (assert-contains
                     #:got "revision rather than a version"
                     #:expect (error-of
                            (lambda ()
                              (mix-lock-version "local_dep" path))))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               test-argv-suite
               format-argv-suite
               release-argv-suite
               cmd-suite
               running-suite
               project-suite
               lock-suite))
