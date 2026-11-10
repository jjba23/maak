" maak.vim --- the tasks of the nearest maak.scm, in Vim
"
" The tasks are read by contrib/bin/maak-tasks, the helper every maak editor
" integration shares, so the Scheme parsing lives in one place.  This file is
" the Vim half: :Maak to pick and run, :MaakRun to run without prompts,
" :MaakList for the quickfix list and :MaakGoto to visit a definition.
"
" SPDX-License-Identifier: MIT

if exists('g:autoloaded_maak')
  finish
endif
let g:autoloaded_maak = 1

let s:cpo_save = &cpo
set cpo&vim

" The task last run from this editor, so :MaakLast can run it again.
let s:last_run = {}

" Walk up from the current directory until a directory holds a maak.scm.
function! maak#root() abort
  let l:found = findfile('maak.scm', '.;')
  return l:found ==# '' ? v:null : fnamemodify(l:found, ':p:h')
endfunction

" The tasks of the Maak file in ROOT, one dict each.
function! maak#tasks(...) abort
  let l:root = a:0 ? a:1 : maak#root()
  if l:root is v:null
    throw 'maak: no maak.scm in this directory or its parents'
  endif
  let l:lines = systemlist([g:maak_helper, '--directory', l:root])
  if v:shell_error
    throw 'maak: ' . (empty(l:lines) ? 'the task list could not be read' : join(l:lines, ' '))
  endif
  return json_decode(join(l:lines, "\n")).tasks
endfunction

" Task names, for :MaakRun's completion.
function! maak#complete(lead, line, position) abort
  let l:root = maak#root()
  if l:root is v:null
    return []
  endif
  try
    let l:tasks = maak#tasks(l:root)
  catch
    return []
  endtry
  return map(filter(l:tasks, {_, t -> stridx(t.name, a:lead) == 0}), {_, t -> t.name})
endfunction

" The command-line spelling of the task's arguments.
function! maak#signature(task) abort
  let l:words = []
  for l:argument in a:task.arguments
    if l:argument.kind ==# 'required'
      call add(l:words, l:argument.name)
    elseif l:argument.kind ==# 'optional'
      call add(l:words, l:argument.hasDefault
            \ ? '[' . l:argument.name . '=' . l:argument.default . ']'
            \ : '[' . l:argument.name . ']')
    elseif l:argument.kind ==# 'key'
      call add(l:words, l:argument.hasDefault
            \ ? l:argument.keyword . '=' . l:argument.default
            \ : l:argument.keyword)
    elseif l:argument.kind ==# 'rest'
      call add(l:words, '...' . l:argument.name)
    endif
  endfor
  return join(l:words, ' ')
endfunction

" One line naming the task, its arguments and the first line of its docstring.
function! maak#label(task) abort
  let l:signature = maak#signature(a:task)
  let l:summary = get(split(a:task.doc, "\n"), 0, '')
  return a:task.name
        \ . (l:signature ==# '' ? '' : '  ' . l:signature)
        \ . (l:summary ==# '' ? '' : '  — ' . l:summary)
endfunction

function! s:error(message) abort
  echohl ErrorMsg
  echomsg 'maak: ' . a:message
  echohl None
endfunction

" Ask for the values of the task's arguments, in order.
"
" A keyword argument is never asked for: maak's command line passes positional
" strings, so its default is all a task can see.  An optional argument left
" blank stops the prompts, because a later word would otherwise fill its slot.
" Returns v:null when the task cannot be run as answered.
function! maak#read_arguments(task) abort
  let l:arguments = []
  for l:argument in a:task.arguments
    if l:argument.kind ==# 'key'
      if !l:argument.hasDefault
        call s:error(printf('%s needs keyword %s, which maak cannot pass; give it a default in the Maak file',
              \ a:task.name, l:argument.keyword))
        return v:null
      endif
    elseif l:argument.kind ==# 'required'
      let l:answer = input('Argument <' . l:argument.name . '>: ')
      if l:answer =~# '^\s*$'
        call s:error(printf('%s needs a value for <%s>', a:task.name, l:argument.name))
        return v:null
      endif
      call add(l:arguments, l:answer)
    elseif l:argument.kind ==# 'optional'
      let l:answer = input(printf('Argument <%s> (%s, RET to omit): ',
            \ l:argument.name,
            \ l:argument.hasDefault ? l:argument.default : 'optional'))
      if l:answer =~# '^\s*$'
        break
      endif
      call add(l:arguments, l:answer)
    elseif l:argument.kind ==# 'rest'
      let l:answer = input(printf('Arguments <%s> (space separated, RET for none): ',
            \ l:argument.name))
      call extend(l:arguments, split(l:answer))
    endif
  endfor
  return l:arguments
endfunction

" Run NAME with ARGUMENTS in a new tab's terminal, in ROOT, or with `-n' when
" DRY_RUN is true.
function! s:run(name, arguments, root, dry_run) abort
  if a:root is v:null
    call s:error('no maak.scm in this directory or its parents')
    return
  endif
  if !has('terminal')
    call s:error('this Vim has no :terminal, so a task has nowhere to run')
    return
  endif
  let l:command = [g:maak_executable]
  if a:dry_run
    call add(l:command, '-n')
  endif
  call add(l:command, a:name)
  " An argument that begins with a dash is maak's, not the task's, unless a
  " `--' sets it apart.
  for l:argument in a:arguments
    if l:argument =~# '^-'
      call add(l:command, '--')
      break
    endif
  endfor
  call extend(l:command, a:arguments)
  let s:last_run = {'name': a:name, 'arguments': a:arguments, 'root': a:root, 'dry_run': a:dry_run}
  tabnew
  call term_start(l:command, {'cwd': a:root, 'term_finish': 'close'})
  startinsert
endfunction

" Run NAME with ARGUMENTS, in the nearest Maak file's directory.
function! maak#run(name, arguments, ...) abort
  call s:run(a:name, a:arguments, a:0 ? a:1 : maak#root(), 0)
endfunction

" Run the task last run from this editor again, with the same words.
function! maak#last() abort
  if empty(s:last_run)
    call s:error('no task has been run from this editor yet')
    return
  endif
  call s:run(s:last_run.name, s:last_run.arguments, s:last_run.root, s:last_run.dry_run)
endfunction

" The words of `:MaakRun', which are the task's name and then its arguments.
function! maak#run_command(name, ...) abort
  call maak#run(a:name, a:000)
endfunction

" Put every task in the quickfix list, where :cnext walks them.
function! maak#list() abort
  let l:root = maak#root()
  if l:root is v:null
    call s:error('no maak.scm in this directory or its parents')
    return
  endif
  let l:items = []
  for l:task in maak#tasks(l:root)
    call add(l:items, {
          \ 'filename': l:root . '/maak.scm',
          \ 'lnum': max([l:task.line, 1]),
          \ 'text': maak#label(l:task)})
  endfor
  call setqflist([], ' ', {'title': 'maak tasks', 'items': l:items})
  copen
endfunction

" Pick a task from a numbered list, or return {} when the pick is cancelled.
function! s:pick(prompt) abort
  let l:root = maak#root()
  if l:root is v:null
    call s:error('no maak.scm in this directory or its parents')
    return {}
  endif
  let l:tasks = sort(maak#tasks(l:root), {a, b -> a.name ==# b.name ? 0 : a.name ># b.name ? 1 : -1})
  if empty(l:tasks)
    call s:error('no tasks in ' . l:root . '/maak.scm')
    return {}
  endif
  let l:labels = []
  let l:index = 1
  for l:task in l:tasks
    call add(l:labels, l:index . '. ' . maak#label(l:task))
    let l:index += 1
  endfor
  let l:choice = inputlist(l:labels)
  if l:choice < 1 || l:choice > len(l:tasks)
    return {}
  endif
  return {'root': l:root, 'task': l:tasks[l:choice - 1]}
endfunction

" Pick a task, prompt for its arguments and run it.
function! maak#pick(...) abort
  let l:dry_run = a:0 ? a:1 : 0
  let l:chosen = s:pick(l:dry_run ? 'Dry-run maak task: ' : 'Run maak task: ')
  if empty(l:chosen)
    return
  endif
  let l:arguments = maak#read_arguments(l:chosen.task)
  if l:arguments is v:null
    return
  endif
  call s:run(l:chosen.task.name, l:arguments, l:chosen.root, l:dry_run)
endfunction

" Pick a task and visit the procedure that defines it.
function! maak#goto() abort
  let l:chosen = s:pick('Go to maak task: ')
  if empty(l:chosen)
    return
  endif
  execute 'edit ' . fnameescape(l:chosen.root . '/maak.scm')
  call cursor(max([l:chosen.task.line, 1]), 1)
  normal! zz
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save
