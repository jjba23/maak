--- Maak integration: the tasks of the nearest maak.scm, in a picker, run in a
--- terminal.
---
--- The tasks are read by contrib/bin/maak-tasks, the helper every maak editor
--- integration shares, so the Scheme parsing lives in one place.  This file is
--- the Neovim half: a picker (vim.ui.select, so telescope or dressing is used
--- when installed), the argument prompts maak's grammar allows, and the
--- terminal the task runs in.
---
--- SPDX-License-Identifier: MIT

local M = {}

M.config = {
  -- The helper, on $PATH or given as a path.  See contrib/README.org.
  helper = "maak-tasks",
  -- The maak executable a task runs with.
  executable = "maak",
}

function M.setup(options)
  M.config = vim.tbl_extend("force", M.config, options or {})
end

-- The task last run from this editor, so `:MaakLast` can run it again.
local last_run = nil

--- Walk up from PATH until a directory holds a maak.scm.
local function find_root(path)
  local directory = vim.fn.fnamemodify(path == "" and vim.fn.getcwd() or path, ":p")
  if vim.fn.isdirectory(directory) == 0 then
    directory = vim.fn.fnamemodify(directory, ":h")
  end
  while true do
    if vim.fn.filereadable(directory .. "/maak.scm") == 1 then
      return directory
    end
    local parent = vim.fn.fnamemodify(directory, ":h")
    if parent == directory then
      return nil
    end
    directory = parent
  end
end

--- The project the current buffer belongs to.
function M.root()
  local root = find_root(vim.api.nvim_buf_get_name(0)) or find_root(vim.fn.getcwd())
  if not root then
    error("maak: no maak.scm in this directory or its parents", 0)
  end
  return root
end

-- Reading the file costs one guile start, so the answer is kept until the file
-- changes.
local cache = {}

--- The tasks of the Maak file in ROOT.
function M.tasks(root)
  local file = root .. "/maak.scm"
  local stamp = vim.fn.getftime(file)
  if cache[file] and cache[file].stamp == stamp then
    return cache[file].tasks
  end
  local lines = vim.fn.systemlist({ M.config.helper, "--directory", root })
  if vim.v.shell_error ~= 0 then
    error("maak: " .. table.concat(lines, " "), 0)
  end
  local tasks = vim.json.decode(table.concat(lines, "\n")).tasks
  cache[file] = { stamp = stamp, tasks = tasks }
  return tasks
end

--- The task names of the nearest Maak file, for `:MaakRun`'s completion.
function M.complete(lead)
  local root = find_root(vim.api.nvim_buf_get_name(0)) or find_root(vim.fn.getcwd())
  if not root then
    return {}
  end
  local ok, tasks = pcall(M.tasks, root)
  if not ok then
    return {}
  end
  local names = {}
  for _, task in ipairs(tasks) do
    if task.name:sub(1, #lead) == lead then
      table.insert(names, task.name)
    end
  end
  table.sort(names)
  return names
end

--- The command-line spelling of TASK's arguments.
function M.signature(task)
  local words = {}
  for _, argument in ipairs(task.arguments) do
    if argument.kind == "required" then
      table.insert(words, argument.name)
    elseif argument.kind == "optional" then
      table.insert(
        words,
        argument.hasDefault and ("[" .. argument.name .. "=" .. argument.default .. "]")
          or ("[" .. argument.name .. "]")
      )
    elseif argument.kind == "key" then
      table.insert(
        words,
        argument.hasDefault and (argument.keyword .. "=" .. argument.default) or argument.keyword
      )
    elseif argument.kind == "rest" then
      table.insert(words, "..." .. argument.name)
    end
  end
  return table.concat(words, " ")
end

--- One line naming TASK, its arguments and the first line of its docstring.
function M.label(task)
  local line = task.name
  local signature = M.signature(task)
  if signature ~= "" then
    line = line .. "  " .. signature
  end
  local summary = (task.doc or ""):match("^[^\n]*")
  if summary and summary ~= "" then
    line = line .. "  — " .. summary
  end
  return line
end

--- Ask for the values of TASK's arguments, in order.
---
--- A keyword argument is never asked for: maak's command line passes positional
--- strings, so its default is all a task can see.  An optional argument left
--- blank stops the prompts, because a later word would otherwise fill its slot.
--- Returns nil when the task cannot be run as answered.
function M.read_arguments(task)
  local arguments = {}
  for _, argument in ipairs(task.arguments) do
    if argument.kind == "key" then
      if not argument.hasDefault then
        vim.notify(
          ("maak: %s needs keyword %s, which maak cannot pass; give it a default in the Maak file"):format(
            task.name,
            argument.keyword
          ),
          vim.log.levels.ERROR
        )
        return nil
      end
    elseif argument.kind == "required" then
      local answer = vim.fn.input(("Argument <%s>: "):format(argument.name))
      if answer:find("%S") == nil then
        vim.notify(
          ("maak: %s needs a value for <%s>"):format(task.name, argument.name),
          vim.log.levels.ERROR
        )
        return nil
      end
      table.insert(arguments, answer)
    elseif argument.kind == "optional" then
      local answer = vim.fn.input(
        ("Argument <%s> (%s, RET to omit): "):format(
          argument.name,
          argument.hasDefault and argument.default or "optional"
        )
      )
      if answer:find("%S") == nil then
        break
      end
      table.insert(arguments, answer)
    elseif argument.kind == "rest" then
      local answer =
        vim.fn.input(("Arguments <%s> (space separated, RET for none): "):format(argument.name))
      for word in answer:gmatch("%S+") do
        table.insert(arguments, word)
      end
    end
  end
  return arguments
end

--- Run TASK with ARGUMENTS in a new tab's terminal, in ROOT.
---
--- OPTIONS may hold `dry_run', which runs maak with `-n' so the task prints the
--- commands it would run instead of running them.
function M.run(name, arguments, root, options)
  options = options or {}
  root = root or M.root()
  arguments = arguments or {}
  local command = { M.config.executable }
  if options.dry_run then
    table.insert(command, "-n")
  end
  table.insert(command, name)
  for _, argument in ipairs(arguments) do
    -- An argument that begins with a dash is maak's, not the task's, unless a
    -- `--' sets it apart.
    if argument:sub(1, 1) == "-" then
      table.insert(command, "--")
      break
    end
  end
  vim.list_extend(command, arguments)
  last_run = { name = name, arguments = arguments, root = root, options = options }
  vim.cmd("tabnew")
  vim.fn.termopen(command, { cwd = root })
  vim.cmd("startinsert")
end

--- Run the task last run from this editor again, in the same way.
function M.last()
  if not last_run then
    vim.notify("maak: no task has been run from this editor yet", vim.log.levels.WARN)
    return
  end
  M.run(last_run.name, last_run.arguments, last_run.root, last_run.options)
end

--- Pick a task of the nearest Maak file and hand it to CALLBACK.
function M.select(prompt, callback)
  local root = M.root()
  local tasks = M.tasks(root)
  if #tasks == 0 then
    error("maak: no tasks in " .. root .. "/maak.scm", 0)
  end
  table.sort(tasks, function(a, b)
    return a.name < b.name
  end)
  vim.ui.select(tasks, {
    prompt = prompt,
    format_item = M.label,
  }, function(task)
    if task then
      callback(task, root)
    end
  end)
end

--- Pick a task, prompt for its arguments and run it.
function M.pick(options)
  options = options or {}
  M.select(options.dry_run and "Dry-run maak task: " or "Run maak task: ", function(task, root)
    local arguments = M.read_arguments(task)
    if arguments then
      M.run(task.name, arguments, root, options)
    end
  end)
end

--- Put every task in the quickfix list, where `:cnext' walks them.
function M.list()
  local root = M.root()
  local items = {}
  for _, task in ipairs(M.tasks(root)) do
    table.insert(items, {
      filename = root .. "/maak.scm",
      lnum = math.max(task.line, 1),
      text = M.label(task),
    })
  end
  vim.fn.setqflist({}, " ", { title = "maak tasks", items = items })
  vim.cmd("copen")
end

--- Pick a task and visit its definition.
function M.goto_task()
  M.select("Go to maak task: ", function(task, root)
    vim.cmd("edit " .. vim.fn.fnameescape(root .. "/maak.scm"))
    vim.api.nvim_win_set_cursor(0, { math.max(task.line, 1), 0 })
    vim.cmd("normal! zz")
  end)
end

return M
