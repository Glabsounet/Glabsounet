let mapleader = " "
let maplocalleader = ","

" Normal {{{
nnoremap    <leader>ev	    :tabe $MYVIMRC<cr>
nnoremap    <leader>sv	    :source $MYVIMRC<cr>
nnoremap    <leader>em	    :tabe ~/.vim/mappings.vim<cr>
nnoremap    <leader>sm	    :source ~/.vim/mappings.vim<cr>

nnoremap    <leader>bn	    :bnext<cr>
nnoremap    <leader>bp	    :bprev<cr>
nnoremap    <leader>bb	    :buffer #<cr>
nnoremap    <leader>bc	    :ls<cr>:buffer 

nnoremap    <leader>cc	    :cc<cr>
nnoremap    <leader>cn	    :cnext<cr>
nnoremap    <leader>cp	    :cprevious<cr>
" }}}

" Insert {{{
inoremap    jk		    <esc>
" }}}

" Command line {{{
cnoremap    <C-a>	    <Home>
cnoremap    <C-e>	    <End>
cnoremap    <C-p>	    <Up>
cnoremap    <C-n>	    <Down>
cnoremap    <C-b>	    <Left>
cnoremap    <C-f>	    <Right>
cnoremap    <C-Left>	    <S-Left>
cnoremap    <C-Right>	    <S-Right>
" }}}
