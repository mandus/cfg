currentos=$(uname -s)

# set -x

[ -f /etc/bash_completion ] && source /etc/bash_completion

# Git prompt
if [ -f ~/.git-prompt.sh ] ; then
   col_off="\033[0m"
   bluecol="\033[0;36m"
   redcol="\033[0;31m"
   greencol="\033[0;32m"
   yellowcol="\033[0;33m"
   magentacol="\033[0;35m"

   source ~/.git-prompt.sh
   export GIT_PS1_SHOWDIRTYSTATE=1
   export GIT_PS1_SHOWSTASHSTATE=1
   export GIT_PS1_SHOWUPSTREAM="auto"
   #PS1='\[]2;\h: \w \]\[\e[31m\]\!\[\e[35m\]$(__git_ps1 " (%s)")\[\e[33;1m\]>\[\e[m\]'
   inssh=$([[ $(ps -ocomm= -p $(ps -o ppid= $$)) =~ (sshd|mosh) ]] && hostname) 
   set_bash_prompt() {
     #PS1='\[]2;\h: \w \]\[\e[31m\]\!\[\e[35m\]$(__git_ps1 " (%s)")\[\e[33;1m\]>\[\e[m\]'
     #PS1='\[]2;\w \]$(date +%H:%M)|\[\e[31m\]\!\[\e[35m\]$(__git_ps1 " (%s)")\[\e[33;1m\]>\[\e[m\]'
     #PS1="\[]2;\w\]$(date +%H:%M)|\[\e[31m\]\!\[\e[35m\]$(__git_ps1 "(%s)")\[\e[33;1m\]>\[\e[m\]"
	 venv=$([[ -n $VIRTUAL_ENV ]] && basename $VIRTUAL_ENV)
     PS1="\[]2;\w\]\[$yellowcol\]${inssh}\[$col_off\]${inssh:+|}\[$bluecol\]${venv:-$(date +%H:%M)}\[$col_off\]|\[$redcol\]\!\[$magentacol\]$(__git_ps1 "(%s)")\[$yellowcol\]>\[$col_off\]"
   }
   PROMPT_COMMAND=set_bash_prompt
else
   PS1='\[]2;\h: \w \]\[\e[31m\]\!\[\e[33;1m\]>\[\e[m\]'
fi

# Enable color in OS X Terminal
export CLICOLOR=1 # Value can be anything, must just set variable

export EDITOR=/usr/bin/vim
export FCEDIT=/usr/bin/vim
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export MOZ_USE_XINPUT2=1

# some history stuff
HISTCONTROL=ignoredups
HISTFILESIZE=50000
HISTSIZE=50000
command_oriented_history=1
shopt -s histappend

# pwd sier det som er sant... (ikke symlink..)
set -o physical

function addinpath() { 
	declare exitcode=1
	if [ -x $* ] ; then 
		declare -i count=$(echo $PATH | sed 's/:/\n/g' | grep -c "$*")
		[ $count -lt 1 ] && PATH=$*:$PATH
		exitcode=0
	fi
	return ${exitcode}
}

addinpath $HOME/bin
addinpath $HOME/software/bin
addinpath $HOME/cfg/lisp/bin
addinpath $HOME/go/bin
addinpath $HOME/.local/bin
addinpath $HOME/.dotnet
addinpath $HOME/.dotnet/tools
addinpath $HOME/.vsdbg
# Add most up-to-date go in PATH
addinpath /usr/lib/go-1.21/bin
type go &>/dev/null || addinpath /usr/lib/go-1.16/bin
type go &>/dev/null || addinpath /usr/lib/go-1.15/bin
type go &>/dev/null || addinpath /usr/lib/go-1.14/bin
type go &>/dev/null || addinpath /usr/lib/go-1.13/bin
addinpath $HOME/software/installs/q/l32 && export QHOME=$HOME/software/installs/q
#addinpath $HOME/software/starship

# ssh-agent stuff:
[ -f ~/.bash_ssh_settings ] && source ~/.bash_ssh_settings
# check that the info is still valid:
if ! ps -p $(echo $SSH_AGENT_PID) | grep -q 'ssh-agent' ; then
   ssh-agent | grep -v echo > ~/.bash_ssh_settings
   chmod 600 ~/.bash_ssh_settings
   source ~/.bash_ssh_settings
fi

[ -f ~/.git_completion ] && source ~/.git_completion 

# Add fzf file finder...
# Need to set vi mode before loading fzf
set -o vi
export FZF_COMPLETION_TRIGGER='\\'
[ -f ~/.fzf.completion.bash ] && source ~/.fzf.completion.bash
[ -f ~/.fzf.key-bindings.bash ] && source ~/.fzf.key-bindings.bash

# set up zoxide if available
type zoxide &>/dev/null && eval "$(zoxide init bash)"

# opam configuration
addinpath $HOME/software/ocaml-4.09.1/bin
test -r /home/aasmundo/.opam/opam-init/init.sh && . /home/aasmundo/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

#echo "node version mgr disabled"
#export NVM_DIR="$HOME/.nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# kubectl completion
type kubectl >/dev/null 2>&1 && . <(kubectl completion bash)

#gh completion
type gh &>/dev/null && . <(gh completion -s bash)

# manage python with pyenv
if [[ -d ~/.pyenv ]] ; then
	export PYENV_ROOT="$HOME/.pyenv"
	PATH="${PYENV_ROOT}/bin:$PATH"
	eval "$(pyenv init --path)"
	eval "$(pyenv init -)"
	eval "$(pyenv virtualenv-init -)"
fi

# perl-stuff
[ -f ~/.perl.bash ] && source ~/.perl.bash

# rust
[ -f ~/.cargo/env ] && source ~/.cargo/env

# starship
# type starship &>/dev/null && eval "$(starship init bash)"

# broot / br (https://dystroy.org/broot)
[ -f ~/.config/broot/launcher/bash/br ] && source /home/aasmundo/.config/broot/launcher/bash/br

# Don't send more info than necessary, for Azure function tools
export FUNCTIONS_CORE_TOOLS_TELEMETRY_OPTOUT=1

export PATH
source ~/.bash_alias
