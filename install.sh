#!/bin/bash

# All files in the links subdirs should be linked in $HOME 
# following the same structure.

CURDIR=$(pwd)
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
LDIR=${DIR}/links
BDIR=${DIR}/bins
VDIR=${DIR}/venvs
REPODIR=${DIR}/repos
VENVDIR=${HOME}/software/venvs

function compile_command_t () {
	cd $HOME/.vim/bundle/command-t/ruby/command-t/ext/command-t
	sudo apt -y install ruby ruby-dev
	ruby extconf.rb
	make
	cd -
}

function create_venv() {
	vname=$1
	srcfile=$2
	[[ ! -d $VENVDIR ]] && mkdir -p $VENVDIR
	cd $VENVDIR
	[[ ! -d $vname ]] && virtualenv $vname
	. ${vname}/bin/activate
	pip install -r $srcfile
	deactivate
	cd -
}


function add_package_repo() {
	repo=$1
	# The "repo" name is the unique package provided by repo that we check for
	# If not available, we run the script to add repo
	if ! $(apt-cache show $repo &>/dev/null) ; then
		bash ${REPODIR}/${repo}
	fi
}

# Install additional repos if needed
cd ${REPODIR}
for repo in * ; do
	add_package_repo $repo
done

# install packages from the package list
sudo apt update
cat ${DIR}/packages | xargs sudo apt -y install

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

# link all scripts in the bins directory
# we force symlink, so destination is overwritten if exists
cd ${BDIR}
for fn in $(find -type f -printf '%P\n') ; do
	echo ${fn}
	ln -sf ${BDIR}/${fn} $HOME/bin/.
done

# For all files in the venvs directory, create a virtualenv
# and use the file as source of packages to install
cd ${VDIR}
for fn in $(find -type f -printf '%P\n')  ; do
	echo ${fn}
	create_venv ${fn} ${VDIR}/${fn}
done

cd ${CURDIR}

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
