-- Maak's commands.  The work is in lua/maak/init.lua.
--
-- SPDX-License-Identifier: MIT

if vim.g.loaded_maak then
  return
end
vim.g.loaded_maak = true

local function tasks_complete(lead)
  local names = require("maak").complete(lead)
  return names
end

vim.api.nvim_create_user_command("Maak", function()
  require("maak").pick()
end, { desc = "Pick a maak task, prompt for its arguments and run it" })

vim.api.nvim_create_user_command("MaakDryRun", function()
  require("maak").pick({ dry_run = true })
end, { desc = "Pick a maak task and print the commands it would run" })

vim.api.nvim_create_user_command("MaakRun", function(opts)
  require("maak").run(opts.fargs[1], vim.list_slice(opts.fargs, 2))
end, {
  nargs = "+",
  complete = tasks_complete,
  desc = "Run a maak task with the words that follow",
})

vim.api.nvim_create_user_command("MaakLast", function()
  require("maak").last()
end, { desc = "Run the maak task last run from this editor again" })

vim.api.nvim_create_user_command("MaakList", function()
  require("maak").list()
end, { desc = "List the tasks of the nearest maak.scm in the quickfix list" })

vim.api.nvim_create_user_command("MaakGoto", function()
  require("maak").goto_task()
end, { desc = "Jump to the definition of a maak task" })
