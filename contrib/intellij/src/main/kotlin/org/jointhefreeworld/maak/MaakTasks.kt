// Reading a project's Maak file through the shared maak-tasks helper.
//
// SPDX-License-Identifier: MIT

package org.jointhefreeworld.maak

import com.google.gson.JsonParser
import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import java.io.File
import java.util.concurrent.TimeUnit

/** The settings, kept where a plugin keeps them rather than in a file of ours. */
object MaakSettings {
    private const val HELPER = "maak.helper"
    private const val EXECUTABLE = "maak.executable"

    fun helper(): String = PropertiesComponent.getInstance().getValue(HELPER, "maak-tasks")

    fun executable(): String = PropertiesComponent.getInstance().getValue(EXECUTABLE, "maak")
}

/** One argument of a task, as its lambda list spells it. */
data class MaakArgument(
    val name: String,
    val kind: String,
    val default: String,
    val hasDefault: Boolean,
    val keyword: String,
) {
    /** The command-line spelling of the argument. */
    val display: String
        get() = when (kind) {
            "required" -> name
            "optional" -> if (hasDefault) "[$name=$default]" else "[$name]"
            "key" -> if (hasDefault) "$keyword=$default" else keyword
            "rest" -> "...$name"
            else -> name
        }
}

/** One task of a Maak file. */
data class MaakTask(
    val name: String,
    val doc: String,
    val line: Int,
    val arguments: List<MaakArgument>,
) {
    val signature: String get() = arguments.joinToString(" ") { it.display }

    val summary: String get() = doc.lineSequence().firstOrNull() ?: ""

    val label: String
        get() = buildString {
            append(name)
            if (signature.isNotEmpty()) append("  ").append(signature)
            if (summary.isNotEmpty()) append("  — ").append(summary)
        }
}

/** The tasks of one project. */
data class MaakListing(val root: File, val tasks: List<MaakTask>)

object Maak {
    /** The nearest directory at or above FILE that holds a maak.scm. */
    fun root(project: Project, file: VirtualFile? = null): File? {
        var directory = file?.path?.let(::File) ?: project.basePath?.let(::File)
        while (directory != null) {
            if (File(directory, "maak.scm").isFile) {
                return directory
            }
            directory = directory.parentFile
        }
        return null
    }

    /** Read ROOT's Maak file, or throw with the helper's own message. */
    fun tasks(root: File): MaakListing {
        val process = ProcessBuilder(MaakSettings.helper(), "--directory", root.path).start()
        val output = process.inputStream.bufferedReader().use { it.readText() }
        val errors = process.errorStream.bufferedReader().use { it.readText() }
        if (!process.waitFor(20, TimeUnit.SECONDS)) {
            process.destroy()
            throw IllegalStateException("maak-tasks did not answer within 20 seconds")
        }
        if (process.exitValue() != 0) {
            throw IllegalStateException(errors.trim().ifEmpty { "maak-tasks failed" })
        }
        val tasks = JsonParser.parseString(output).asJsonObject.getAsJsonArray("tasks").map { element ->
            val task = element.asJsonObject
            MaakTask(
                name = task.get("name").asString,
                doc = task.get("doc").asString,
                line = task.get("line").asInt,
                arguments = task.getAsJsonArray("arguments").map { argumentElement ->
                    val argument = argumentElement.asJsonObject
                    MaakArgument(
                        name = argument.get("name").asString,
                        kind = argument.get("kind").asString,
                        default = argument.get("default").asString,
                        hasDefault = argument.get("hasDefault").asBoolean,
                        keyword = argument.get("keyword").asString,
                    )
                },
            )
        }
        return MaakListing(root, tasks)
    }

    /**
     * The command line that runs TASK, with `--' where a dash needs it, and
     * `-n' when the run is a dry one.
     */
    fun command(task: MaakTask, words: List<String>, dryRun: Boolean = false): List<String> {
        val command = mutableListOf(MaakSettings.executable())
        if (dryRun) {
            command += "-n"
        }
        command += task.name
        if (words.any { it.startsWith("-") }) {
            command += "--"
        }
        command += words
        return command
    }
}
