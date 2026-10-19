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
;; Unit tests for `(maak niri)'.  No compositor is needed: every test runs the
;; helper in dry-run mode and asserts on the command line, which is the same
;; line `maak --dry-run` prints.  The query wrappers are checked together, one
;; assertion over a list, because the interesting part is that each reaches the
;; right subcommand rather than the particular line that follows it.

;;; Code:

(define-module (veritas unit niri)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:use-module (maak niri)
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
  (let ((dir (format #f "~a/maak-test-niri-~a-~a"
                     (tmp-dir) label (getpid))))
    (mkdir-p dir)
    dir))

(define (remove-directory dir)
  "Remove DIR quietly, whatever the tests left in it."
  (parameterize ((quiet? #t))
    (delete-file-recursively dir)))

(define (dry-run-line thunk)
  "Return just the command line of a dry run, without the prefix."
  (let* ((text (dry-run-command thunk))
         (line (cadr (string-split text #\newline)))
         (prefix "[DRY-RUN]: "))
    (string-trim (string-drop line (string-length prefix)))))

(define argv-suite
  (suite "niri argument lists"
         #:concurrent? #f
         #:shuffle? #f
         (test "the program and subcommand come first"
               (assert-equal #:expect '("niri" "msg" "outputs")
                             #:got (niri-argv (list "outputs"))))
         (test "JSON is a flag of msg, before the subcommand"
               (assert-equal #:expect '("niri" "msg" "--json" "windows")
                             #:got (niri-argv (list "windows") #:json? #t)))
         (test "an action is its own subcommand"
               (assert-equal #:expect '("niri" "msg" "action" "close-window")
                             #:got (niri-action-argv (list "close-window"))))
         (test "an output names the output before the action"
               (assert-equal
                #:expect '("niri" "msg" "output" "DP-1" "mode" "1920x1080@60")
                #:got (niri-output-argv "DP-1" "mode" "1920x1080@60")))
         (test "the generic runner prints the command"
               (assert-equal #:expect "niri msg action close-window"
                             #:got (dry-run-line
                                    (lambda () (niri (list "action"
                                                           "close-window"))))))
         (test "capture returns nothing in a dry run"
               (assert-equal #:expect ""
                             #:got (parameterize ((dry-run? #t) (quiet? #t))
                                     (niri-capture (list "outputs")))))))

(define queries-suite
  (suite "niri queries"
         #:concurrent? #f
         #:shuffle? #f
         (test "every query reaches its subcommand"
               (assert-equal
                #:expect '("niri msg version"
                           "niri msg outputs"
                           "niri msg workspaces"
                           "niri msg windows"
                           "niri msg layers"
                           "niri msg keyboard-layouts"
                           "niri msg focused-output"
                           "niri msg focused-window"
                           "niri msg pick-window"
                           "niri msg pick-color"
                           "niri msg overview-state"
                           "niri msg casts"
                           "niri msg request-error")
                #:got (map (lambda (thunk) (dry-run-line thunk))
                           (list (lambda () (niri-version))
                                 (lambda () (niri-outputs))
                                 (lambda () (niri-workspaces))
                                 (lambda () (niri-windows))
                                 (lambda () (niri-layers))
                                 (lambda () (niri-keyboard-layouts))
                                 (lambda () (niri-focused-output))
                                 (lambda () (niri-focused-window))
                                 (lambda () (niri-pick-window))
                                 (lambda () (niri-pick-color))
                                 (lambda () (niri-overview-state))
                                 (lambda () (niri-casts))
                                 (lambda () (niri-request-error))))))
         (test "a query can ask for JSON"
               (assert-equal #:expect "niri msg --json workspaces"
                             #:got (dry-run-line
                                    (lambda () (niri-workspaces #:json? #t)))))
         (test "niri-available? finds niri on PATH"
               (let* ((dir (scratch-directory "niri-available"))
                      (program (string-append dir "/niri"))
                      (found? (begin
                                (write-file program "#!/bin/sh\nexit 0\n")
                                (parameterize ((quiet? #t))
                                  (chmod-file "755" program))
                                (with-env "PATH" dir
                                  (lambda () (niri-available?))))))
                 (remove-directory dir)
                 (assert-true found?)))
         (test "niri-available? is false when niri is not on PATH"
               (let ((dir (scratch-directory "niri-unavailable")))
                 (let ((found? (with-env "PATH" dir
                                 (lambda () (niri-available?)))))
                   (remove-directory dir)
                   (assert-false found?))))))

(define actions-suite
  (suite "niri actions"
         #:concurrent? #f
         #:shuffle? #f
         (test "the argument-free actions are plain subcommands"
               (assert-equal
                #:expect '("niri msg action close-window"
                           "niri msg action fullscreen-window"
                           "niri msg action toggle-windowed-fullscreen"
                           "niri msg action toggle-overview"
                           "niri msg action focus-workspace-previous"
                           "niri msg action focus-window-previous"
                           "niri msg action screenshot"
                           "niri msg action screenshot-screen"
                           "niri msg action screenshot-window"
                           "niri msg action power-off-monitors"
                           "niri msg action power-on-monitors"
                           "niri msg action unset-workspace-name")
                #:got (map (lambda (thunk) (dry-run-line thunk))
                           (list (lambda () (niri-close-window))
                                 (lambda () (niri-fullscreen-window))
                                 (lambda () (niri-toggle-windowed-fullscreen))
                                 (lambda () (niri-toggle-overview))
                                 (lambda () (niri-focus-workspace-previous))
                                 (lambda () (niri-focus-window-previous))
                                 (lambda () (niri-screenshot))
                                 (lambda () (niri-screenshot-screen))
                                 (lambda () (niri-screenshot-window))
                                 (lambda () (niri-power-off-monitors))
                                 (lambda () (niri-power-on-monitors))
                                 (lambda () (niri-unset-workspace-name))))))
         (test "a workspace can be focused by reference"
               (assert-equal #:expect "niri msg action focus-workspace 2"
                             #:got (dry-run-line
                                    (lambda () (niri-focus-workspace 2)))))
         (test "a workspace name stays one argument"
               (assert-equal #:expect "niri msg action focus-workspace 'my space'"
                             #:got (dry-run-line
                                    (lambda () (niri-focus-workspace "my space")))))
         (test "a column and a window take their index"
               (assert-equal #:expect '("niri msg action focus-column 1"
                                       "niri msg action focus-window 42")
                             #:got (map (lambda (thunk) (dry-run-line thunk))
                                        (list (lambda () (niri-focus-column 1))
                                              (lambda () (niri-focus-window 42))))))
         (test "a monitor is named as it is"
               (assert-equal #:expect "niri msg action focus-monitor DP-1"
                             #:got (dry-run-line
                                    (lambda () (niri-focus-monitor "DP-1")))))
         (test "moving a window carries its id and focus decision"
               (assert-equal
                #:expect "niri msg action move-window-to-workspace --window-id 7 --focus false 3"
                #:got (dry-run-line
                       (lambda ()
                         (niri-move-window-to-workspace
                          "3" #:window-id 7 #:focus #f)))))
         (test "moving a column names the workspace"
               (assert-equal
                #:expect "niri msg action move-column-to-workspace 4"
                #:got (dry-run-line
                       (lambda () (niri-move-column-to-workspace 4)))))
         (test "windows and columns move to a monitor"
               (assert-equal
                #:expect '("niri msg action move-window-to-monitor HDMI-A-1"
                           "niri msg action move-column-to-monitor HDMI-A-1")
                #:got (map (lambda (thunk) (dry-run-line thunk))
                           (list (lambda () (niri-move-window-to-monitor
                                             "HDMI-A-1"))
                                 (lambda () (niri-move-column-to-monitor
                                             "HDMI-A-1"))))))
         (test "width changes carry their optional id"
               (assert-equal
                #:expect '("niri msg action set-window-width --id 5 +10%"
                           "niri msg action set-column-width 50%")
                #:got (map (lambda (thunk) (dry-run-line thunk))
                           (list (lambda () (niri-set-window-width "+10%"
                                                                    #:id 5))
                                 (lambda () (niri-set-column-width "50%"))))))
         (test "a workspace can be named and unnamed"
               (assert-equal #:expect "niri msg action set-workspace-name web"
                             #:got (dry-run-line
                                    (lambda () (niri-set-workspace-name "web")))))
         (test "spawn separates niri's flags from the program"
               (assert-equal
                #:expect "niri msg action spawn -- foot -e htop"
                #:got (dry-run-line
                       (lambda () (niri-spawn (list "foot" "-e" "htop"))))))
         (test "spawn-sh keeps the line for the shell"
               (assert-equal #:expect "niri msg action spawn-sh -- 'echo hi'"
                             #:got (dry-run-line
                                    (lambda () (niri-spawn-sh "echo hi")))))
         (test "quit skips the confirmation by default"
               (assert-equal #:expect "niri msg action quit --skip-confirmation"
                             #:got (dry-run-line (lambda () (niri-quit)))))
         (test "spawn refuses an empty program list"
               (assert-error (lambda () (niri-spawn '()))))
         (test "spawn refuses arguments that are not strings"
               (assert-error (lambda () (niri-spawn (list "foot" 1)))))))

(define outputs-suite
  (suite "niri outputs"
         #:concurrent? #f
         #:shuffle? #f
         (test "turning an output on and off"
               (assert-equal
                #:expect '("niri msg output DP-1 on"
                           "niri msg output DP-1 off")
                #:got (map (lambda (thunk) (dry-run-line thunk))
                           (list (lambda () (niri-output-on "DP-1"))
                                 (lambda () (niri-output-off "DP-1"))))))
         (test "an output mode is passed through"
               (assert-equal
                #:expect "niri msg output DP-1 mode 1920x1080@60"
                #:got (dry-run-line
                       (lambda () (niri-output-mode "DP-1" "1920x1080@60")))))
         (test "scale, transform and position reach the right actions"
               (assert-equal
                #:expect '("niri msg output DP-1 scale 1.5"
                           "niri msg output DP-1 transform 90"
                           "niri msg output DP-1 position 0 1080")
                #:got (map (lambda (thunk) (dry-run-line thunk))
                           (list (lambda () (niri-output-scale "DP-1" "1.5"))
                                 (lambda () (niri-output-transform "DP-1" 90))
                                 (lambda () (niri-output-position
                                             "DP-1" 0 1080))))))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               argv-suite
               queries-suite
               actions-suite
               outputs-suite))
