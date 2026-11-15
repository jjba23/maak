"""Maak integration for Sublime Text.

The tasks are read by contrib/bin/maak-tasks, the helper every maak editor
integration shares, so the Scheme parsing lives in one place.  This file is the
Sublime half: quick panels for the pickers, input panels for the argument
prompts, and `exec` for the run, so a task's output lands in a build panel.

SPDX-License-Identifier: MIT
"""

import json
import os
import shlex
import subprocess

import sublime
import sublime_plugin


def settings():
    return sublime.load_settings("Maak.sublime-settings")


def helper():
    return settings().get("helper", "maak-tasks")


def executable():
    return settings().get("executable", "maak")


def find_root(window):
    """The nearest directory at or above the view that holds a maak.scm."""
    variables = window.extract_variables()
    start = (
        variables.get("file_path")
        or variables.get("folder")
        or variables.get("project_path")
    )
    if not start:
        return None
    directory = start if os.path.isdir(start) else os.path.dirname(start)
    while True:
        if os.path.isfile(os.path.join(directory, "maak.scm")):
            return directory
        parent = os.path.dirname(directory)
        if parent == directory:
            return None
        directory = parent


def tasks(root):
    """The tasks of ROOT's Maak file, as the helper prints them."""
    process = subprocess.Popen(
        [helper(), "--directory", root],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    out, err = process.communicate()
    if process.returncode != 0:
        raise RuntimeError(err.decode("utf-8").strip() or "maak-tasks failed")
    return json.loads(out.decode("utf-8"))["tasks"]


def signature(task):
    """The command-line spelling of the task's arguments."""
    words = []
    for argument in task["arguments"]:
        kind = argument["kind"]
        if kind == "required":
            words.append(argument["name"])
        elif kind == "optional":
            if argument["hasDefault"]:
                words.append("[{0}={1}]".format(argument["name"], argument["default"]))
            else:
                words.append("[{0}]".format(argument["name"]))
        elif kind == "key":
            if argument["hasDefault"]:
                words.append("{0}={1}".format(argument["keyword"], argument["default"]))
            else:
                words.append(argument["keyword"])
        elif kind == "rest":
            words.append("...{0}".format(argument["name"]))
    return " ".join(words)


def label(task):
    """One line naming the task, its arguments and its docstring's first line."""
    parts = [task["name"]]
    words = signature(task)
    if words:
        parts.append(words)
    summary = (task["doc"] or "").split("\n")[0]
    if summary:
        parts.append(u"\u2014 " + summary)
    return "  ".join(parts)


def prose(argument):
    """The docstring's prose for an argument, ready to follow its name."""
    doc = argument.get("doc") or ""
    return u" \u2014 " + doc if doc else ""


def detail(task):
    """What the task says beyond its name, for the second line of a panel."""
    parts = []
    summary = " ".join((task["doc"] or "").strip().split())
    if summary:
        parts.append(summary)
    if task.get("depends"):
        parts.append("needs: " + ", ".join(task["depends"]))
    if task.get("example"):
        parts.append("example: " + task["example"])
    return "  ".join(parts)


def row(task):
    """A quick panel entry: the label, then its detail when it has one."""
    extra = detail(task)
    return [label(task), extra] if extra else [label(task)]


def error(message):
    sublime.error_message("Maak: {0}".format(message))


class MaakRunCommand(sublime_plugin.WindowCommand):
    """Pick a task, answer its arguments, and run it."""

    dry_run = False

    def run(self):
        root = find_root(self.window)
        if root is None:
            return error("no maak.scm in this directory or its parents")
        try:
            listing = tasks(root)
        except Exception as failure:
            return error(str(failure))
        if not listing:
            return error("no tasks in {0}/maak.scm".format(root))
        listing.sort(key=lambda task: task["name"])
        self.window.show_quick_panel(
            [row(task) for task in listing],
            lambda index: self.chosen(root, listing, index),
        )

    def chosen(self, root, listing, index):
        if index < 0:
            return
        self.ask(root, listing[index], 0, [], self.dry_run)

    def ask(self, root, task, index, words, dry_run):
        """Prompt for the arguments from INDEX on, then run the task."""
        arguments = task["arguments"]
        # A keyword argument is never asked for: maak's command line passes
        # positional strings, so its default is all a task can see.
        while index < len(arguments) and arguments[index]["kind"] == "key":
            if not arguments[index]["hasDefault"]:
                return error(
                    "{0} needs keyword {1}, which maak cannot pass; give it a "
                    "default in the Maak file".format(
                        task["name"], arguments[index]["keyword"]
                    )
                )
            index += 1
        if index >= len(arguments):
            self.execute(root, task, words, dry_run)
            return

        argument = arguments[index]
        kind = argument["kind"]
        if kind == "optional":
            prompt = "Argument <{0}>{1} ({2}, blank to omit)".format(
                argument["name"],
                prose(argument),
                argument["default"] or "optional",
            )
        elif kind == "rest":
            prompt = "Arguments <{0}>{1} (space separated, blank for none)".format(
                argument["name"], prose(argument)
            )
        else:
            prompt = "Argument <{0}>{1}".format(argument["name"], prose(argument))

        def on_done(value):
            kept = list(words)
            if kind == "optional":
                # Leaving it out leaves out everything after it too, because
                # maak binds positional words in order.
                if not value.strip():
                    self.execute(root, task, kept, dry_run)
                    return
                kept.append(value)
            elif kind == "rest":
                kept.extend(value.split())
            else:
                if not value.strip():
                    return error(
                        "{0} needs a value for <{1}>".format(
                            task["name"], argument["name"]
                        )
                    )
                kept.append(value)
            self.ask(root, task, index + 1, kept, dry_run)

        self.window.show_input_panel(prompt, "", on_done, None, None)

    def execute(self, root, task, words, dry_run):
        command = [executable()]
        if dry_run:
            command.append("-n")
        command.append(task["name"])
        if any(word.startswith("-") for word in words):
            command.append("--")
        command.extend(words)
        self.window.run_command(
            "exec",
            {
                "shell_cmd": " ".join(shlex.quote(part) for part in command),
                "working_dir": root,
            },
        )


class MaakRunDryCommand(MaakRunCommand):
    """Pick a task and print the commands it would run, instead of running them."""

    dry_run = True


class MaakListCommand(sublime_plugin.WindowCommand):
    """Write every task, with its arguments, to a Maak output panel."""

    def run(self):
        root = find_root(self.window)
        if root is None:
            return error("no maak.scm in this directory or its parents")
        try:
            listing = tasks(root)
        except Exception as failure:
            return error(str(failure))
        panel = self.window.create_output_panel("maak")
        self.window.run_command("show_panel", {"panel": "output.maak"})
        lines = []
        for task in listing:
            lines.append(label(task))
            extra = detail(task)
            if extra:
                lines.append("    " + extra)
        panel.run_command(
            "append",
            {"characters": "{0}\n".format("\n".join(lines))},
        )
        panel.run_command(
            "append",
            {"characters": "\n{0} tasks in {1}/maak.scm\n".format(len(listing), root)},
        )


class MaakGotoCommand(sublime_plugin.WindowCommand):
    """Pick a task and open the procedure that defines it."""

    def run(self):
        root = find_root(self.window)
        if root is None:
            return error("no maak.scm in this directory or its parents")
        try:
            listing = tasks(root)
        except Exception as failure:
            return error(str(failure))
        listing.sort(key=lambda task: task["name"])
        self.window.show_quick_panel(
            [row(task) for task in listing],
            lambda index: self.open(root, listing, index),
        )

    def open(self, root, listing, index):
        if index < 0:
            return
        path = os.path.join(root, "maak.scm")
        self.window.open_file(path)
        view = self.window.find_open_file(path)
        if view is not None:
            view.run_command("goto_line", {"line": max(listing[index]["line"], 1)})
