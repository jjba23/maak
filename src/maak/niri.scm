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
;; Helpers for talking to a running niri compositor through `niri msg'.  A
;; task that changes the session -- focus a workspace, spawn a program, put a
;; window on another output -- can do it in a line rather than in a shell
;; string, and a task that reads the session can ask for JSON and parse it.
;;
;; Two families are exported.  The queries (`niri-outputs`, `niri-workspaces`,
;; `niri-windows', ...) are ordinary commands; `niri-capture` runs any of them
;; and returns what niri printed, which is what a task needs for a decision.
;; The actions (`niri-focus-workspace`, `niri-spawn`, ...) are thin wrappers
;; over `niri msg action'.
;;
;; Every helper has an `-argv` builder where the command has options worth
;; asserting on.  The builders are what `--dry-run` prints and what the tests
;; exercise; no compositor is needed to check the command line.
;;
;; `niri msg' is deliberately not started when this module loads.  A task that
;; needs niri checks `niri-available?' first, or lets the missing program be
;; reported by `run' as exit code 127.

;;; Code:

(define-module (maak niri)
  #:declarative? #t
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (maak dsl)
  #:use-module (maak shell)
  #:export (niri
            niri-available?
            niri-argv
            niri-capture
            niri-action
            niri-action-argv
            niri-version
            niri-outputs
            niri-workspaces
            niri-windows
            niri-layers
            niri-keyboard-layouts
            niri-focused-output
            niri-focused-window
            niri-pick-window
            niri-pick-color
            niri-overview-state
            niri-casts
            niri-request-error
            niri-focus-workspace
            niri-focus-workspace-previous
            niri-focus-column
            niri-focus-monitor
            niri-focus-window
            niri-focus-window-previous
            niri-close-window
            niri-fullscreen-window
            niri-toggle-windowed-fullscreen
            niri-toggle-overview
            niri-spawn
            niri-spawn-sh
            niri-quit
            niri-screenshot
            niri-screenshot-screen
            niri-screenshot-window
            niri-power-off-monitors
            niri-power-on-monitors
            niri-move-window-to-workspace
            niri-move-column-to-workspace
            niri-move-window-to-monitor
            niri-move-column-to-monitor
            niri-set-window-width
            niri-set-column-width
            niri-set-workspace-name
            niri-unset-workspace-name
            niri-output
            niri-output-argv
            niri-output-on
            niri-output-off
            niri-output-mode
            niri-output-scale
            niri-output-transform
            niri-output-position))

;; ====== The runner ======

(define* (niri-argv arguments #:key (json? #f))
  "Return the argv that sends ARGUMENTS to `niri msg`.

With JSON? the global @code{--json} flag is inserted before the subcommand,
which is where niri expects it.  ARGUMENTS is the subcommand and its flags:
@code{(\"outputs\")}, @code{(\"action\" \"close-window\")}, and so on."
  (append (list "niri" "msg")
          (if json? (list "--json") '())
          arguments))

(define* (niri arguments #:key (json? #f) (allow-failure? #f) (verbose? #f))
  "Send ARGUMENTS to `niri msg'.

This is the escape hatch for subcommands and flags without a wrapper.  Nothing
is handed to a shell, so a program name or a workspace name with a space in it
reaches niri as one argument.

A command that exits non-zero raises a maak error, unless ALLOW-FAILURE? is #t,
in which case the call answers #f -- the usual `run' contract."
  (run (niri-argv arguments #:json? json?)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

(define* (niri-capture arguments #:key (json? #f) (allow-failure? #f))
  "Send ARGUMENTS to `niri msg' and return what it printed.

This is the query form: the output is a string a task can trim, split or hand
to a JSON parser, rather than a line on the terminal.  In dry-run mode nothing
runs, the command is printed, and the empty string comes back."
  (capture (string-join (map shell-quote (niri-argv arguments #:json? json?))
                        " ")
           #:allow-failure? allow-failure?))

(define (niri-available?)
  "Return #t when the niri program can be found on PATH."
  (command-on-path? "niri" (getenv "PATH")))

(define* (niri-action-argv arguments #:key (json? #f))
  "Return the argv that performs an action with its ARGUMENTS.

The action name comes first and its own flags follow, so
@code{(\"focus-workspace\" \"2\")} is the argv for `focus-workspace 2'."
  (niri-argv (cons "action" arguments) #:json? json?))

(define* (niri-action arguments #:key (json? #f) (allow-failure? #f)
                     (verbose? #f))
  "Perform an action with ARGUMENTS, through `niri msg action'.

A bare action name is enough for the ones that take no argument, such as
@code{close-window}; the others take their reference after it.

@example
(niri-action (list \"focus-workspace\" \"2\"))
(niri-action (list \"spawn\" \"--\" \"foot\" \"-e\" \"htop\"))
@end example"
  (run (niri-action-argv arguments #:json? json?)
       #:allow-failure? allow-failure?
       #:verbose? verbose?))

;; ====== Queries ======

(define* (niri-version #:key (json? #f) (allow-failure? #f))
  "Print the version of the running niri instance."
  (niri (list "version") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-outputs #:key (json? #f) (allow-failure? #f))
  "List the connected outputs."
  (niri (list "outputs") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-workspaces #:key (json? #f) (allow-failure? #f))
  "List the workspaces."
  (niri (list "workspaces") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-windows #:key (json? #f) (allow-failure? #f))
  "List the open windows."
  (niri (list "windows") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-layers #:key (json? #f) (allow-failure? #f))
  "List the open layer-shell surfaces."
  (niri (list "layers") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-keyboard-layouts #:key (json? #f) (allow-failure? #f))
  "Print the configured keyboard layouts."
  (niri (list "keyboard-layouts") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-focused-output #:key (json? #f) (allow-failure? #f))
  "Print information about the focused output."
  (niri (list "focused-output") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-focused-window #:key (json? #f) (allow-failure? #f))
  "Print information about the focused window."
  (niri (list "focused-window") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-pick-window #:key (json? #f) (allow-failure? #f))
  "Pick a window with the mouse and print information about it."
  (niri (list "pick-window") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-pick-color #:key (json? #f) (allow-failure? #f))
  "Pick a color from the screen with the mouse."
  (niri (list "pick-color") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-overview-state #:key (json? #f) (allow-failure? #f))
  "Print the overview state."
  (niri (list "overview-state") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-casts #:key (json? #f) (allow-failure? #f))
  "List the screencasts."
  (niri (list "casts") #:json? json? #:allow-failure? allow-failure?))

(define* (niri-request-error #:key (json? #f) (allow-failure? #f))
  "Ask niri to return an error, which is useful for checking a client's
error handling against a live compositor."
  (niri (list "request-error") #:json? json? #:allow-failure? allow-failure?))

;; ====== Actions ======
;;
;; These wrap the common `niri msg action' calls.  A reference is a workspace
;; index or name, a window id, or a monitor name, and is passed through as one
;; argument whatever it contains.

(define* (niri-focus-workspace reference #:key (json? #f) (allow-failure? #f))
  "Focus the workspace REFERENCE, an index or a name."
  (niri-action (list "focus-workspace" (format #f "~a" reference))
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-focus-workspace-previous #:key (json? #f) (allow-failure? #f))
  "Focus the previous workspace."
  (niri-action (list "focus-workspace-previous")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-focus-column index #:key (json? #f) (allow-failure? #f))
  "Focus the column at INDEX."
  (niri-action (list "focus-column" (format #f "~a" index))
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-focus-monitor name #:key (json? #f) (allow-failure? #f))
  "Focus the monitor NAME."
  (niri-action (list "focus-monitor" name)
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-focus-window id #:key (json? #f) (allow-failure? #f))
  "Focus the window with ID."
  (niri-action (list "focus-window" (format #f "~a" id))
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-focus-window-previous #:key (json? #f) (allow-failure? #f))
  "Focus the previously focused window."
  (niri-action (list "focus-window-previous")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-close-window #:key (json? #f) (allow-failure? #f))
  "Close the focused window."
  (niri-action (list "close-window")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-fullscreen-window #:key (json? #f) (allow-failure? #f))
  "Toggle fullscreen on the focused window."
  (niri-action (list "fullscreen-window")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-toggle-windowed-fullscreen #:key (json? #f) (allow-failure? #f))
  "Toggle windowed (fake) fullscreen on the focused window."
  (niri-action (list "toggle-windowed-fullscreen")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-toggle-overview #:key (json? #f) (allow-failure? #f))
  "Toggle the overview."
  (niri-action (list "toggle-overview")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-spawn command #:key (json? #f) (allow-failure? #f))
  "Spawn COMMAND, a list of strings, without a shell.

The @code{--} that separates niri's own flags from the program is added here,
so a program with a flag that looks like niri's is still passed through."
  (when (or (not (list? command)) (null? command) (not (every string? command)))
    (raise-maak-error
     "niri-spawn expects a non-empty list of strings, starting with the program"
     #:hint "Write (niri-spawn (list \"foot\"))."))
  (niri-action (append (list "spawn" "--") command)
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-spawn-sh command #:key (json? #f) (allow-failure? #f))
  "Spawn COMMAND through the shell.

Use `niri-spawn' when the program and its arguments are already separate
strings; this one is for a line that needs a shell."
  (niri-action (list "spawn-sh" "--" command)
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-quit #:key (skip-confirmation? #t) (json? #f)
                    (allow-failure? #f))
  "Exit niri.

SKIP-CONFIRMATION? is the default because a task that asks niri to quit is not
sitting at a keyboard to press Enter."
  (niri-action (append (list "quit")
                       (if skip-confirmation? (list "--skip-confirmation") '()))
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-screenshot #:key (json? #f) (allow-failure? #f))
  "Open the screenshot UI."
  (niri-action (list "screenshot")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-screenshot-screen #:key (json? #f) (allow-failure? #f))
  "Screenshot the focused screen."
  (niri-action (list "screenshot-screen")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-screenshot-window #:key (json? #f) (allow-failure? #f))
  "Screenshot the focused window."
  (niri-action (list "screenshot-window")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-power-off-monitors #:key (json? #f) (allow-failure? #f))
  "Power off all monitors via DPMS."
  (niri-action (list "power-off-monitors")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-power-on-monitors #:key (json? #f) (allow-failure? #f))
  "Power on all monitors via DPMS."
  (niri-action (list "power-on-monitors")
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-move-window-to-workspace reference
                                        #:key (window-id #f) (focus 'default)
                                        (json? #f) (allow-failure? #f))
  "Move the focused window, or WINDOW-ID, to the workspace REFERENCE.

FOCUS is @code{default} unless given: with #t the focus follows the window,
and with #f it stays on the original workspace, which niri spells as
@code{--focus true} and @code{--focus false}."
  (niri-action
   (append (list "move-window-to-workspace")
           (if window-id (list "--window-id" (format #f "~a" window-id)) '())
           (if (eq? focus 'default)
               '()
               (list "--focus" (if focus "true" "false")))
           (list (format #f "~a" reference)))
   #:json? json? #:allow-failure? allow-failure?))

(define* (niri-move-column-to-workspace reference
                                        #:key (json? #f) (allow-failure? #f))
  "Move the focused column to the workspace REFERENCE."
  (niri-action (list "move-column-to-workspace" (format #f "~a" reference))
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-move-window-to-monitor name #:key (json? #f)
                                      (allow-failure? #f))
  "Move the focused window to the monitor NAME."
  (niri-action (list "move-window-to-monitor" name)
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-move-column-to-monitor name #:key (json? #f)
                                      (allow-failure? #f))
  "Move the focused column to the monitor NAME."
  (niri-action (list "move-column-to-monitor" name)
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-set-window-width change #:key (id #f) (json? #f)
                                (allow-failure? #f))
  "Change the width of the focused window, or of window ID, by CHANGE.

CHANGE is a string such as @code{+10%} or @code{500}, exactly as niri's command
line takes it."
  (niri-action (append (list "set-window-width")
                       (if id (list "--id" (format #f "~a" id)) '())
                       (list change))
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-set-column-width change #:key (json? #f) (allow-failure? #f))
  "Change the width of the focused column by CHANGE."
  (niri-action (list "set-column-width" change)
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-set-workspace-name name #:key (json? #f) (allow-failure? #f))
  "Set the name of the focused workspace to NAME."
  (niri-action (list "set-workspace-name" name)
               #:json? json? #:allow-failure? allow-failure?))

(define* (niri-unset-workspace-name #:key (json? #f) (allow-failure? #f))
  "Unset the name of the focused workspace."
  (niri-action (list "unset-workspace-name")
               #:json? json? #:allow-failure? allow-failure?))

;; ====== Outputs ======

(define* (niri-output-argv output action #:rest arguments)
  "Return the argv that changes OUTPUT with ACTION and ARGUMENTS.

The output name comes first and the action after it, which is the order
`niri msg output' requires: @code{niri msg output DP-1 mode 1920x1080@@60}.

@example
(niri-output-argv \"DP-1\" \"scale\" \"1.5\")
@end example"
  (niri-argv (append (list "output" output action) arguments)))

(define* (niri-output output action #:rest arguments)
  "Change OUTPUT with ACTION and ARGUMENTS; see `niri-output-argv'.

The change is temporary and is not written to niri's configuration file."
  (run (apply niri-output-argv output action arguments)))

(define* (niri-output-on output #:key (allow-failure? #f))
  "Turn OUTPUT on."
  (run (niri-output-argv output "on") #:allow-failure? allow-failure?))

(define* (niri-output-off output #:key (allow-failure? #f))
  "Turn OUTPUT off."
  (run (niri-output-argv output "off") #:allow-failure? allow-failure?))

(define* (niri-output-mode output mode #:key (allow-failure? #f))
  "Set OUTPUT's mode, such as @code{1920x1080@@60}."
  (run (niri-output-argv output "mode" mode)
       #:allow-failure? allow-failure?))

(define* (niri-output-scale output scale #:key (allow-failure? #f))
  "Set OUTPUT's scale, such as @code{1.5}."
  (run (niri-output-argv output "scale" (format #f "~a" scale))
       #:allow-failure? allow-failure?))

(define* (niri-output-transform output transform #:key (allow-failure? #f))
  "Set OUTPUT's transform, such as @code{90} or @code{flipped-180}."
  (run (niri-output-argv output "transform" (format #f "~a" transform))
       #:allow-failure? allow-failure?))

(define* (niri-output-position output x y #:key (allow-failure? #f))
  "Set OUTPUT's position on the desktop to X and Y."
  (run (niri-output-argv output "position" (format #f "~a" x) (format #f "~a" y))
       #:allow-failure? allow-failure?))
