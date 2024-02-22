set   autowrite
set   backspace=2
set nobackup
set   cinkeys=0{,0},:,!^F,o,O,e
set   cursorline
set nocompatible             
set   expandtab
set   formatoptions=cjroql2
set   guioptions=aed
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
set   viminfo='75,\"500,f1,:250,n~/.viminfo
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

" set the runtime path to include Vundle and initialize
filetype off               
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

fun! <SID>StripTrailingWhitespaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfun

" Need to stay on ruby CommandT as long as I'm on plain vim (and/or new
" version become vim 9 compliant)
let g:CommandTPreferredImplementation='ruby'

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'fatih/vim-go'
Plugin 'majutsushi/tagbar'
Plugin 'mileszs/ack.vim'
Plugin 'preservim/nerdtree'
Plugin 'sjl/gundo.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-commentary'
Plugin 'wincent/command-t'
Plugin 'nvie/vim-flake8'
Plugin 'davidhalter/jedi-vim'
Plugin 'kovisoft/slimv'
Plugin 'psf/black'
Plugin 'fisadev/vim-isort'
Plugin 'zoomlogo/vim-apl'
Plugin 'github/copilot.vim'
"vim status in tmux - doesn't work with powerline?
"Plugin 'vimpostor/vim-tpipeline'

"Consider adding this if debugger in vim is needed:
"see https://puremourning.github.io/vimspector-web/
"Plugin 'puremourning/vimspector'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
syntax on

" gundo defaults to python2, which may be gone by now
if has('python3')
    let g:gundo_prefer_python3 = 1
endif

nnoremap <leader><space> :silent :exe "!X-open-silent " . expand("<cWORD>")<CR><C-L>
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
nnoremap <F6> :GundoToggle<CR>
nnoremap <F7> :set invlist<CR>
nnoremap <F8> :TagbarToggle<CR>
nnoremap ,a :Ack!<CR>
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

set guifont=Source\ Code\ Pro\ Light\ 10

" go-specifics
nnoremap <leader>ga :GoAlternate!<cr>                                                                                                                                                                                                                                                                                          
set   comments=b:#,:%,f:\",fb:-,n:>,sl1:/*,mb:*,ex:*/
set noexpandtab
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 0
let g:go_fmt_experimental = 0
nnoremap <F10> :GoFmt<CR>
nnoremap <F11> :GoImports<CR>

" ocaml
if executable('ocamlmerlin') && has('python')
  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
  execute "set rtp+=" . g:opamshare . "/merlin/vim"
endif
if executable('ocp-indent')
  set rtp^="/home/aasmundo/.opam/default/share/ocp-indent/vim"
endif

let g:CommandTEncoding = 'UTF-8'

" silversearcher
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" powerline
if executable('powerline') && has('python3')
  py3 from powerline.vim import setup as powerline_setup
  py3 powerline_setup()
  py3 del powerline_setup
endif

" lisp settings
let g:lisp_rainbow=1 
let g:slimv_repl_split=2
let g:slimv_repl_split_size=24

" AutoCommands

" move cursor to last know position in file when opening
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif
" Enable auto-flake8 on save:
au BufWritePost *.py call Flake8()
" Strip trailing white space in python-files
autocmd BufWritePre *.py :call <SID>StripTrailingWhitespaces()
" " Black format python files on save:
" augroup black_on_save
"   autocmd!
"   autocmd BufWritePre *.py Black
" augroup end
" " Isort python imports on save:
" augroup isort_on_save
"   autocmd!
"   autocmd BufWritePre *.py Isort
" augroup end
" Strip trailing white space in lisp-files
autocmd BufWritePre *.lisp :call <SID>StripTrailingWhitespaces()
" cursorline only in active window and in normal mode
au WinLeave,InsertEnter * set nocursorline
au WinEnter,InsertLeave * set   cursorline
