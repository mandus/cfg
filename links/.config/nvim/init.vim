set   autowrite
set   backspace=2
set nobackup
set   cinkeys=0{,0},:,!^F,o,O,e
set   cursorline
set nocompatible             
set   expandtab
set   formatoptions=cjroql2
"set   guioptions=aed "not supported
set   hidden
set   ignorecase
set   incsearch
set   lazyredraw
set   listchars=tab:»·,trail:~,eol:·
set   number
set   numberwidth=4
set   relativenumber
set   ruler
set   shiftwidth=4
set   softtabstop=4
set nosplitbelow
set   splitright
set   tabstop=4                                                                                                                                                               
set   tags=./.git/tags,./tags,.git/tags,../.git/tags,.ctags,./TAGS,tags,TAGS
set   viminfo='75,\"500,f1,:250,n~/.nviminfo
set wildignore=*.fasl
set nowrap
set nowritebackup

augroup CustomColors
	" overrides for colors independent of colorscheme can be set here.
	autocmd!
	autocmd ColorScheme * highlight LineNr ctermfg=5 guifg=gray
					  \ | highlight CursorLineNr ctermfg=5 guifg=gray
augroup End

colorscheme desert

fun! <SID>StripTrailingWhitespaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfun

filetype plugin indent on    " required
syntax on

nnoremap <leader>a <C-W><C-H>
nnoremap -a <C-W><C-H>
nnoremap <leader>s <C-W><C-K>
nnoremap -s <C-W><C-K>
nnoremap <leader>d <C-W><C-J>
nnoremap -d <C-W><C-J>
nnoremap <leader>f <C-W><C-L>
nnoremap -f <C-W><C-L>
nnoremap <leader>A <C-W>L
nnoremap <leader>F <C-W>R
nnoremap <leader>S 3<C-W>h<C-W>l<C-W>H<C-W>l
nnoremap <leader>D 3<C-W>l<C-W>h<C-W>L<C-W>h
nnoremap <F1> :set invpaste<CR>
nnoremap <F2> :set hls!<CR>
nnoremap <F4> :NERDTreeFind<CR>
nnoremap <F5> :NERDTreeToggle<CR>
nnoremap <F7> :set invlist<CR>
nnoremap ,gg :exe 'Ggrep ' . expand('<cword>')<CR>
nnoremap ,sc <C-W>c
nnoremap ,sr :%s/\<<C-r><C-w>\>//g<Left><Left>
nnoremap ,ss <C-W>s
nnoremap ,sv <C-W>v
nnoremap ,, <C-^>
nnoremap -- <C-^>
" swap colon and semicolon in normal mode:
nnoremap ; :
nnoremap : ; 
" with NO keyboard, just map ø to : to make life a bit easier
nnoremap ø :
nnoremap ¤ $

" lisp settings
let g:lisp_rainbow=1 
let g:slimv_repl_split=2
let g:slimv_repl_split_size=24

" AutoCommands

" move cursor to last know position in file when opening
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif
" Strip trailing white space in python-files
autocmd BufWritePre *.py :call <SID>StripTrailingWhitespaces()
" cursorline only in active window and in normal mode
au WinLeave,InsertEnter * set nocursorline
au WinEnter,InsertLeave * set   cursorline
