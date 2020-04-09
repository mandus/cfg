#!/bin/bash

# All files in the links subdirs should be linked in $HOME 
# following the same structure.

CURDIR=$(pwd)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
LDIR=${DIR}/links

function compile_command_t () {
	cd $HOME/.vim/bundle/command-t/ruby/command-t/ext/command-t
	sudo apt -y install ruby ruby-dev
	ruby extconf.rb
	make
	cd -
}

# link all rc files in the links directory
# we force symlink, so destination is just overwritten
cd ${LDIR}
for fn in $(find -type f -printf '%P\n') ; do
	echo ${fn}
	fdir=$(dirname ${fn})
	[[ $fdir != '.' ]] && mkdir -p $HOME/${fdir}
	[[ $fdir = '.' ]] && fdir='' || fdir=${fdir}/
	ln -sf ${LDIR}/${fn} $HOME/${fdir}.
done

cd ${CURDIR}

# install packages from the package list
for p in $(cat ${DIR}/packages) ; do 
	sudo apt -y install $p
done

# configure vim modules
sudo apt -y install vim vim-gtk3
[[ ! -x $HOME/.vim/bundle/Vundle.vim ]] && git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall! +qall

# if command-t is in the plugins list, the extension must be compiled
grep -q -i command-t ~/.vimrc && compile_command_t

# install dependencies for tagbar, if requested 
grep -q -i tagbar ~/.vimrc && sudo apt -y install exuberant-ctags

# install dependencies for ack, if requested
grep -q -i ack ~/.vimrc && sudo apt -y install silversearcher-ag

# install i3 if we have config
[[ -f $HOME/.config/i3/config ]] && sudo apt -y install i3 i3status i3blocks j4-dmenu-desktop

# dropbox needs to watch more files than the default
sudo sed -i '/fs.inotify.max_user_watches.*/d' /etc/sysctl.conf
echo fs.inotify.max_user_watches=1000000 | sudo tee -a /etc/sysctl.conf; sudo sysctl -p

# install the ly display manager (fails unless other display mgr than lightdm hasn't been disabled first)
${DIR}/ly.sh
