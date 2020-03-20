#!/bin/bash

# All files in the links subdirs should be linked in $HOME 
# following the same structure.

CURDIR=$(pwd)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
LDIR=${DIR}/links
cd ${LDIR}

function compile_command_t () {
	cd $HOME/.vim/bundle/command-t/ruby/command-t/ext/command-t
	ruby extconf.rb
	make
	cd -
}

#for fn in $(find . -name '??*') ; do
for fn in $(find -type f -printf '%P\n') ; do
	echo ${fn}
	fdir=$(dirname ${fn})
	[[ $fdir != '.' ]] && mkdir -p $HOME/${fdir}
	[[ $fdir = '.' ]] && fdir='' || fdir=${fdir}/
	ln -sf ${LDIR}/${fn} $HOME/${fdir}.
done

cd ${CURDIR}

# configure vim modules
sudo apt install vim vim-gtk3
[[ ! -x $HOME/.vim/bundle/Vundle.vim ]] && git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# if command-t is in the plugins list, the extension must be compiled
grep -q -i command-t ~/.vimrc && compile_command_t
grep -q -i tagbar ~/.vimrc && sudo apt install exuberant-ctags
