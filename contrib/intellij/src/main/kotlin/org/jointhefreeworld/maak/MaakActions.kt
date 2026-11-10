// The actions: run a task in the Run tool window, dry-run one, and open the
// procedure that defines one.
//
// SPDX-License-Identifier: MIT

package org.jointhefreeworld.maak

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.filters.TextConsoleBuilderFactory
import com.intellij.execution.process.OSProcessHandler
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.Messages
import com.intellij.openapi.ui.popup.BaseListPopupStep
import com.intellij.openapi.ui.popup.JBPopupFactory
import com.intellij.openapi.ui.popup.PopupStep
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.openapi.wm.ToolWindowManager
import com.intellij.ui.content.ContentFactory
import java.io.File

/** The task picked, and the project it belongs to. */
typealias TaskChoice = (File, MaakTask) -> Unit

object MaakRunner {
    /** Pick a task of PROJECT's nearest Maak file and hand it to ON_CHOSEN. */
    fun choose(project: Project, title: String, onChosen: TaskChoice) {
        val root = Maak.root(project)
        if (root == null) {
            Messages.showErrorDialog(project, "No maak.scm in this project or its parents.", "Maak")
            return
        }
        val listing = try {
            Maak.tasks(root)
        } catch (error: Exception) {
            Messages.showErrorDialog(project, error.message ?: "The task list could not be read.", "Maak")
            return
        }
        if (listing.tasks.isEmpty()) {
            Messages.showErrorDialog(project, "No tasks in ${listing.root}/maak.scm.", "Maak")
            return
        }
        val step = object : BaseListPopupStep<MaakTask>(title, listing.tasks) {
            override fun getTextFor(task: MaakTask): String = task.label

            override fun onChosen(task: MaakTask, finalChoice: Boolean): PopupStep<*>? {
                ApplicationManager.getApplication().invokeLater { onChosen(listing.root, task) }
                return PopupStep.FINAL_CHOICE
            }
        }
        JBPopupFactory.getInstance().createListPopup(step).showInFocusCenter()
    }

    /** Ask for TASK's arguments, run it, and read its output in the Run tool window. */
    fun run(project: Project, root: File, task: MaakTask, dryRun: Boolean) {
        val words = readArguments(project, task) ?: return
        val commandLine = GeneralCommandLine(Maak.command(task, words, dryRun)).withWorkDirectory(root.path)
        val console = TextConsoleBuilderFactory.getInstance().createBuilder(project).console
        val toolWindow = ToolWindowManager.getInstance(project).getToolWindow("Run") ?: return
        val content = ContentFactory.getInstance().createContent(
            console.component,
            if (dryRun) "maak -n: ${task.name}" else "maak: ${task.name}",
            false,
        )
        content.isCloseable = true
        content.setDisposer(console)
        toolWindow.contentManager.addContent(content)
        toolWindow.activate { toolWindow.contentManager.setSelectedContent(content) }
        val handler = OSProcessHandler(commandLine)
        console.attachToProcess(handler)
        handler.startNotify()
    }

    /**
     * Ask for the task's arguments in order, or null when it cannot run.
     *
     * A keyword argument is never asked for: maak's command line passes
     * positional strings, so its default is all a task can see.  An optional
     * argument left blank stops the prompts, because a later word would
     * otherwise fill its slot.
     */
    private fun readArguments(project: Project, task: MaakTask): List<String>? {
        val words = mutableListOf<String>()
        var stopped = false
        for (argument in task.arguments) {
            when (argument.kind) {
                "key" -> if (!argument.hasDefault) {
                    Messages.showErrorDialog(
                        project,
                        "${task.name} needs keyword ${argument.keyword}, which maak cannot pass; give it a default in the Maak file.",
                        "Maak",
                    )
                    return null
                }

                "required" -> {
                    val answer = ask(project, task, "Argument <${argument.name}>:") ?: return null
                    if (answer.isBlank()) {
                        Messages.showErrorDialog(project, "${task.name} needs a value for <${argument.name}>.", "Maak")
                        return null
                    }
                    words += answer
                }

                "optional" -> if (!stopped) {
                    val answer = ask(project, task, "Argument <${argument.name}> (blank to omit):") ?: return null
                    if (answer.isBlank()) stopped = true else words += answer
                }

                "rest" -> if (!stopped) {
                    val answer = ask(
                        project,
                        task,
                        "Arguments <${argument.name}> (space separated, blank for none):",
                    ) ?: return null
                    words += answer.split(Regex("\\s+")).filter { it.isNotEmpty() }
                }
            }
        }
        return words
    }

    private fun ask(project: Project, task: MaakTask, prompt: String): String? =
        Messages.showInputDialog(project, prompt, "maak ${task.name}", Messages.getQuestionIcon())
}

class MaakRunAction : AnAction() {
    override fun actionPerformed(event: AnActionEvent) {
        val project = event.project ?: return
        MaakRunner.choose(project, "Run maak task") { root, task ->
            MaakRunner.run(project, root, task, dryRun = false)
        }
    }
}

class MaakDryRunAction : AnAction() {
    override fun actionPerformed(event: AnActionEvent) {
        val project = event.project ?: return
        MaakRunner.choose(project, "Dry-run maak task") { root, task ->
            MaakRunner.run(project, root, task, dryRun = true)
        }
    }
}

class MaakGoToTaskAction : AnAction() {
    override fun actionPerformed(event: AnActionEvent) {
        val project = event.project ?: return
        MaakRunner.choose(project, "Go to maak task") { root, task ->
            val file = LocalFileSystem.getInstance()
                .refreshAndFindFileByIoFile(File(root, "maak.scm"))
                ?: return@choose
            OpenFileDescriptor(project, file, maxOf(task.line - 1, 0), 0).navigate(true)
        }
    }
}
