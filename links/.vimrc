set   autowrite
set   backspace=2
set nobackup
set   cinkeys=0{,0},:,!^F,o,O,e
set   cursorline
set nocompatible             
set   expandtab
set   guioptions=aed
set   ignorecase
set   incsearch
set   lazyredraw
set   listchars=tab:»·,trail:~,eol:·
set   shiftwidth=4
set   softtabstop=4
set nosplitbelow
set   splitright
set   tabstop=4                                                                                                                                                               
set   viminfo='75,\"500,f1,:250,n~/.viminfo
set nowrap
set nowritebackup


" move cursor to last know position in file when opening
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g`\"" | endif

colorscheme desert

" set the runtime path to include Vundle and initialize
filetype off               
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

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
Plugin 'wincent/command-t'
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
syntax on

" gundo defaults to python2, which may be gone by now
if has('python3')
    let g:gundo_prefer_python3 = 1
endif

nnoremap <leader>a <C-W><C-H>
nnoremap <leader>s <C-W><C-K>
nnoremap <leader>d <C-W><C-J>
nnoremap <leader>f <C-W><C-L>
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
nnoremap ,gg :exe 'Ggrep ' . expand('<cword>')<CR>
nnoremap ,sc <C-W>c
nnoremap ,sr :%s/\<<C-r><C-w>\>//g<Left><Left>
nnoremap ,ss <C-W>s
nnoremap ,sv <C-W>v
nnoremap ,, <C-^>
nnoremap ; :

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

" silversearcher
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

