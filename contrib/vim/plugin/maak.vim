" Maak's commands and defaults.  The work is in autoload/maak.vim.
"
" SPDX-License-Identifier: MIT

if exists('g:loaded_maak')
  finish
endif
let g:loaded_maak = 1

if !exists('g:maak_helper')
  " The helper every maak editor integration shares; see contrib/README.org.
  let g:maak_helper = 'maak-tasks'
endif

if !exists('g:maak_executable')
  let g:maak_executable = 'maak'
endif

if !exists('*json_decode')
  echohl WarningMsg
  echomsg 'maak: this Vim cannot read JSON (json_decode); Vim 8.0 or newer is needed'
  echohl None
  finish
endif

command! -nargs=0 Maak call maak#pick()
command! -nargs=0 MaakDryRun call maak#pick(1)
command! -nargs=+ -complete=customlist,maak#complete MaakRun call maak#run_command(<f-args>)
command! -nargs=0 MaakLast call maak#last()
command! -nargs=0 MaakList call maak#list()
command! -nargs=0 MaakGoto call maak#goto()
