// Maak for VS Code: the tasks of the nearest maak.scm, in the command palette,
// in a terminal, and as VS Code tasks.
//
// The tasks are read by contrib/bin/maak-tasks, the helper every maak editor
// integration shares, so the Scheme parsing lives in one place.
//
// SPDX-License-Identifier: MIT

import * as cp from "child_process";
import * as fs from "fs";
import * as path from "path";
import * as vscode from "vscode";

interface MaakArgument {
  name: string;
  kind: "required" | "optional" | "key" | "rest";
  default: string;
  hasDefault: boolean;
  keyword: string;
}

interface MaakTask {
  name: string;
  doc: string;
  line: number;
  arguments: MaakArgument[];
}

interface MaakListing {
  file: string;
  root: string;
  tasks: MaakTask[];
}

interface MaakTaskDefinition extends vscode.TaskDefinition {
  task: string;
  args?: string[];
}

/** The task last run, so `Maak: Run Last Task` can repeat it exactly. */
interface MaakRun {
  name: string;
  words: string[];
  root: string;
  dryRun: boolean;
}

const LAST_RUN = "maak.lastRun";

let output: vscode.OutputChannel;

function setting<T>(key: string, fallback: T): T {
  return vscode.workspace.getConfiguration("maak").get<T>(key, fallback);
}

function isDirectory(candidate: string): boolean {
  try {
    return fs.statSync(candidate).isDirectory();
  } catch {
    return false;
  }
}

/** Walk up from START until a directory holds a maak.scm. */
function findRoot(start: string): string | undefined {
  let directory = isDirectory(start) ? start : path.dirname(start);
  for (;;) {
    if (fs.existsSync(path.join(directory, "maak.scm"))) {
      return directory;
    }
    const parent = path.dirname(directory);
    if (parent === directory) {
      return undefined;
    }
    directory = parent;
  }
}

/** Every project with a Maak file: the active editor's, then the folders. */
function roots(): string[] {
  const found: string[] = [];
  const add = (start: string | undefined) => {
    if (!start) {
      return;
    }
    const root = findRoot(start);
    if (root && !found.includes(root)) {
      found.push(root);
    }
  };
  add(vscode.window.activeTextEditor?.document.uri.fsPath);
  for (const folder of vscode.workspace.workspaceFolders ?? []) {
    add(folder.uri.fsPath);
  }
  return found;
}

/** Read the tasks of the Maak file in ROOT with the shared helper. */
function load(root: string): Promise<MaakListing> {
  const helper = setting("helper", "maak-tasks");
  return new Promise((resolve, reject) => {
    cp.execFile(helper, ["--directory", root], { cwd: root }, (error, stdout, stderr) => {
      if (error) {
        reject(new Error((stderr || error.message).trim()));
        return;
      }
      try {
        resolve(JSON.parse(stdout) as MaakListing);
      } catch (parseError) {
        reject(parseError as Error);
      }
    });
  });
}

/** The command-line spelling of the task's arguments. */
function signature(task: MaakTask): string {
  return task.arguments
    .map((argument) => {
      switch (argument.kind) {
        case "required":
          return argument.name;
        case "optional":
          return argument.hasDefault
            ? `[${argument.name}=${argument.default}]`
            : `[${argument.name}]`;
        case "key":
          return argument.hasDefault
            ? `${argument.keyword}=${argument.default}`
            : argument.keyword;
        case "rest":
          return `...${argument.name}`;
        default:
          return argument.name;
      }
    })
    .join(" ");
}

function firstLine(doc: string): string {
  return (doc ?? "").split("\n")[0];
}

/**
 * Ask for the values of the task's arguments, in order.
 *
 * A keyword argument is never asked for: maak's command line passes positional
 * strings, so its default is all a task can see.  An optional argument left
 * blank stops the prompts, because a later word would otherwise fill its slot.
 * Returns undefined when the task cannot be run as answered.
 */
async function readArguments(task: MaakTask): Promise<string[] | undefined> {
  const words: string[] = [];
  for (const argument of task.arguments) {
    if (argument.kind === "key") {
      if (!argument.hasDefault) {
        vscode.window.showErrorMessage(
          `Maak: ${task.name} needs keyword ${argument.keyword}, which maak cannot pass; give it a default in the Maak file`
        );
        return undefined;
      }
    } else if (argument.kind === "required") {
      const answer = await vscode.window.showInputBox({
        title: `maak ${task.name}`,
        prompt: `Argument <${argument.name}>`,
        ignoreFocusOut: true,
      });
      if (answer === undefined) {
        return undefined;
      }
      if (answer.trim() === "") {
        vscode.window.showErrorMessage(
          `Maak: ${task.name} needs a value for <${argument.name}>`
        );
        return undefined;
      }
      words.push(answer);
    } else if (argument.kind === "optional") {
      const answer = await vscode.window.showInputBox({
        title: `maak ${task.name}`,
        prompt: `Argument <${argument.name}>${
          argument.hasDefault ? `, default ${argument.default}` : ""
        } — leave blank to omit`,
        ignoreFocusOut: true,
      });
      if (answer === undefined) {
        return undefined;
      }
      if (answer.trim() === "") {
        break;
      }
      words.push(answer);
    } else if (argument.kind === "rest") {
      const answer = await vscode.window.showInputBox({
        title: `maak ${task.name}`,
        prompt: `Arguments <${argument.name}> — space separated, blank for none`,
        ignoreFocusOut: true,
      });
      if (answer === undefined) {
        return undefined;
      }
      words.push(...answer.split(/\s+/).filter((word) => word !== ""));
    }
  }
  return words;
}

function quote(word: string): string {
  return `'${word.replace(/'/g, "'\\''")}'`;
}

/** The command line that runs the task, with `--' where a dash needs it. */
function commandLine(task: MaakTask, words: string[], dryRun: boolean): string {
  const parts = [setting("executable", "maak")];
  if (dryRun) {
    parts.push("-n");
  }
  parts.push(task.name);
  if (words.some((word) => word.startsWith("-"))) {
    parts.push("--");
  }
  return [...parts, ...words].map(quote).join(" ");
}

function run(root: string, task: MaakTask, words: string[], dryRun: boolean): void {
  const terminal = vscode.window.createTerminal({
    name: dryRun ? `maak -n: ${task.name}` : `maak: ${task.name}`,
    cwd: root,
  });
  terminal.show();
  terminal.sendText(commandLine(task, words, dryRun));
}

function pick(prompt: string, tasks: MaakTask[]): Thenable<MaakTask | undefined> {
  const items = tasks
    .slice()
    .sort((a, b) => a.name.localeCompare(b.name))
    .map((task) => ({
      label: task.name,
      description: signature(task),
      detail: firstLine(task.doc),
      task,
    }));
  return vscode.window
    .showQuickPick(items, {
      title: prompt,
      matchOnDescription: true,
      matchOnDetail: true,
    })
    .then((item) => item?.task);
}

/** The project and its tasks, or undefined with the reason already shown. */
async function tasksOf(start?: string): Promise<{ root: string; tasks: MaakTask[] } | undefined> {
  const root = start ?? roots()[0];
  if (!root) {
    vscode.window.showErrorMessage("Maak: no maak.scm in this workspace or its parents");
    return undefined;
  }
  try {
    const listing = await load(root);
    if (listing.tasks.length === 0) {
      vscode.window.showErrorMessage(`Maak: no tasks in ${listing.file}`);
      return undefined;
    }
    return { root, tasks: listing.tasks };
  } catch (error) {
    vscode.window.showErrorMessage(`Maak: ${(error as Error).message}`);
    return undefined;
  }
}

async function runTask(context: vscode.ExtensionContext, dryRun: boolean): Promise<void> {
  const project = await tasksOf();
  if (!project) {
    return;
  }
  const task = await pick(dryRun ? "Dry-run maak task" : "Run maak task", project.tasks);
  if (!task) {
    return;
  }
  const words = await readArguments(task);
  if (!words) {
    return;
  }
  run(project.root, task, words, dryRun);
  await context.workspaceState.update(LAST_RUN, {
    name: task.name,
    words,
    root: project.root,
    dryRun,
  } as MaakRun);
}

async function runLastTask(context: vscode.ExtensionContext): Promise<void> {
  const last = context.workspaceState.get<MaakRun>(LAST_RUN);
  if (!last) {
    vscode.window.showInformationMessage("Maak: no task has been run from this workspace yet");
    return;
  }
  const project = await tasksOf(last.root);
  const task = project?.tasks.find((candidate) => candidate.name === last.name);
  if (!task) {
    vscode.window.showErrorMessage(`Maak: the Maak file no longer defines ${last.name}`);
    return;
  }
  run(last.root, task, last.words, last.dryRun);
}

async function goToTask(): Promise<void> {
  const project = await tasksOf();
  if (!project) {
    return;
  }
  const task = await pick("Go to maak task", project.tasks);
  if (!task) {
    return;
  }
  const document = await vscode.workspace.openTextDocument(
    vscode.Uri.file(path.join(project.root, "maak.scm"))
  );
  const line = Math.max(task.line, 1) - 1;
  const position = new vscode.Position(line, 0);
  await vscode.window.showTextDocument(document, {
    selection: new vscode.Range(position, position),
  });
}

async function listTasks(): Promise<void> {
  const project = await tasksOf();
  if (!project) {
    return;
  }
  output.clear();
  for (const task of project.tasks) {
    const words = signature(task);
    output.appendLine(
      `${task.name}${words === "" ? "" : `  ${words}`}${
        firstLine(task.doc) === "" ? "" : `  — ${firstLine(task.doc)}`
      }`
    );
  }
  output.appendLine("");
  output.appendLine(`${project.tasks.length} tasks in ${project.root}/maak.scm`);
  output.show(true);
}

/** The tasks of every Maak file, for `Tasks: Run Task`. */
class MaakTaskProvider implements vscode.TaskProvider {
  async provideTasks(): Promise<vscode.Task[]> {
    const provided: vscode.Task[] = [];
    for (const root of roots()) {
      let listing: MaakListing;
      try {
        listing = await load(root);
      } catch {
        continue;
      }
      const folder = vscode.workspace.getWorkspaceFolder(vscode.Uri.file(root));
      for (const task of listing.tasks) {
        const definition: MaakTaskDefinition = { type: "maak", task: task.name };
        const execution = new vscode.ShellExecution(setting("executable", "maak"), [task.name], {
          cwd: root,
        });
        const vscodeTask = new vscode.Task(
          definition,
          folder ?? vscode.TaskScope.Workspace,
          task.name,
          "maak",
          execution
        );
        vscodeTask.detail = signature(task);
        provided.push(vscodeTask);
      }
    }
    return provided;
  }

  resolveTask(task: vscode.Task): vscode.Task | undefined {
    const definition = task.definition as MaakTaskDefinition;
    if (!definition.task) {
      return undefined;
    }
    const folder =
      typeof task.scope === "object" ? (task.scope as vscode.WorkspaceFolder) : undefined;
    const root = folder ? folder.uri.fsPath : roots()[0];
    if (!root) {
      return undefined;
    }
    return new vscode.Task(
      definition,
      task.scope ?? vscode.TaskScope.Workspace,
      task.name || definition.task,
      "maak",
      new vscode.ShellExecution(
        setting("executable", "maak"),
        [definition.task, ...(definition.args ?? [])],
        { cwd: root }
      )
    );
  }
}

export function activate(context: vscode.ExtensionContext): void {
  output = vscode.window.createOutputChannel("Maak");
  context.subscriptions.push(
    output,
    vscode.commands.registerCommand("maak.runTask", () => runTask(context, false)),
    vscode.commands.registerCommand("maak.dryRunTask", () => runTask(context, true)),
    vscode.commands.registerCommand("maak.runLastTask", () => runLastTask(context)),
    vscode.commands.registerCommand("maak.goToTask", goToTask),
    vscode.commands.registerCommand("maak.listTasks", listTasks),
    vscode.tasks.registerTaskProvider("maak", new MaakTaskProvider())
  );
}

export function deactivate(): void {
  // Nothing to stop: a task runs in a terminal the user owns.
}
