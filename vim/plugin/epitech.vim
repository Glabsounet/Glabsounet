" Gaby Czegany <gaby.czegany@epitech.eu>

if exists("g:loaded_epitech") || &cp
  finish
endif
let g:loaded_epitech = 1

autocmd BufWrite,FileWritePre	*.[ch],Makefile    call Epitech_UpdateHeader()
nnoremap <leader>eh		:call Epitech_CreateHeader(input("Type project name: "))<cr>

let s:name  = "gaby czegany"
let s:login = "czegan_g"

let s:date_format = "%a %b %e %H:%M:%S %Y"

function! Epitech_GetComments()
  if &ft == "c"
    let comments = ["/*", "** ", "*/"]
  elseif &ft == "make"
    let comments = ['##', '## ', '##']
  else
    let comments = ["", "", ""]
  endif
  return comments
endfunction

function! Epitech_CreateHeader(project_name)
  let comments = Epitech_GetComments()
  let save_pos = getpos(".")
  let save_opt = [&autoindent, &smartindent, &cindent, &fo]
  setlocal noautoindent nosmartindent nocindent fo-=c fo-=r fo-=o
  execute "normal! ggO" .
	\ comments[0] . "\n" .
	\ comments[1] . expand("%:t") . " for " . a:project_name . " in " . getcwd() . "\n" .
	\ comments[1] . "\n" .
	\ comments[1] . "Made by " . s:name . "\n" .
	\ comments[1] . "Login <" . s:login . "@epitech.net>\n" .
	\ comments[1] . "\n" .
	\ comments[1] . "Started on  " . strftime(s:date_format) . " " . s:name . "\n" .
	\ comments[1] . "Last Update " . strftime(s:date_format) . " " . s:name . "\n" .
	\ comments[2] . "\n\<esc>"
  let [&autoindent, &smartindent, &cindent, &fo] = save_opt
  let save_pos[1] += 10
  call setpos('.', save_pos)
endfunction

function! Epitech_UpdateHeader()
  let comments = Epitech_GetComments()
  let pat = comments[1] . "Last Update "
  if comments[0] ==? getline(1) && comments[2] ==? getline(9) && match(getline(8), pat) != -1
    let save_view = winsaveview()
    execute "normal! :8\<cr>3W\"_d$a" . strftime(s:date_format) . " " . s:name . "\<esc>"
    call winrestview(save_view)
  endif
endfunction
