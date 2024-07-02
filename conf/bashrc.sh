#
# ~/.bashrc
#

export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# bash completion
if [[ `uname` == 'FreeBSD' ]]; then
    BASH_COMPLETION=/usr/local/share/bash-completion/bash_completion.sh
else
    BASH_COMPLETION=/usr/share/bash-completion/bash_completion
fi
if [ -f $BASH_COMPLETION ]; then
    source $BASH_COMPLETION
fi

# git completion
GIT_COMPLETE=~/.xmonad/bash/git-completion.sh
if [ -f $GIT_COMPLETE ]; then
    source $GIT_COMPLETE
fi

# kubectl completion
KUBE_COMPLETE=~/.xmonad/bash/kubectl.sh
if [ -f $KUBE_COMPLETE ]; then
    source $KUBE_COMPLETE
fi

# git prompt
GIT_PROMPT=~/.xmonad/bash/git-prompt.sh
if [ -f $GIT_PROMPT ]; then
    source $GIT_PROMPT
fi

# extend rc
EXTRC=~/.bash_extrc
if [ -f $EXTRC ]; then
    source $EXTRC
fi

# QT5 look and feel
export QT_STYLE_OVERRIDE='Arc-Darker'

# unlimited history
export HISTFILESIZE=
export HISTSIZE=

# PATH & GOPATH
export GOPATH=$HOME/go
export PATH="/opt/homebrew/opt/openjdk/bin:$HOME/.pulumi/bin:$HOME/.krew/bin:$HOME/.cargo/bin:$HOME/.pyenv/bin:$HOME/bin:$HOME/go/bin:/snap/bin:/usr/local/go/bin:$HOME/flutter/bin:$HOME/.pub-cache/bin:$PATH"

# pyenv
which pyenv 2>&1 > /dev/null
if [ "$?" -eq "0" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
fi

# zoxide
which zoxide 2>&1 > /dev/null
if [ "$?" -eq "0" ]; then
    eval "$(zoxide init bash)"
    alias j='z'
fi

# set default enviroment
export TERM=xterm-256color
export TERMINAL=/usr/bin/sakura
export EDITOR=vim

# get current branch in git repo
function parse_git_branch() {
	BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
	if [ ! "${BRANCH}" == "" ]
	then
		STAT=`parse_git_dirty`
		echo "[${BRANCH}${STAT}]"
	else
		echo ""
	fi
}

# get current status of git repo
function parse_git_dirty {
	status=`git status 2>&1 | tee`
	dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
	untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
	ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
	newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
	renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
	deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
	bits=''
	if [ "${renamed}" == "0" ]; then
		bits=">${bits}"
	fi
	if [ "${ahead}" == "0" ]; then
		bits="*${bits}"
	fi
	if [ "${newfile}" == "0" ]; then
		bits="+${bits}"
	fi
	if [ "${untracked}" == "0" ]; then
		bits="?${bits}"
	fi
	if [ "${deleted}" == "0" ]; then
		bits="x${bits}"
	fi
	if [ "${dirty}" == "0" ]; then
		bits="!${bits}"
	fi
	if [ ! "${bits}" == "" ]; then
		echo " ${bits}"
	else
		echo ""
	fi
}

export PS1="\[\e[33m\]\u\[\e[m\]@\[\e[32m\]\h\[\e[m\] \`parse_git_branch\` \W \\$ "

# aliases

# neovim
alias vim='nvim'

#ls
if [[ `uname` == 'FreeBSD' ]]; then
    alias ls='ls -G'
else
    alias ls='ls --color'
fi
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

#grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

#git
if [ -n `which git` ]; then
    alias gpu='git push --set-upstream origin `git rev-parse --abbrev-ref HEAD`'
    alias gf='git diff'
    alias gs='git status'
    alias gc='git commit'
    alias gp='git push'
    alias gck='git checkout'
    alias gcp='git cherry-pick'
    alias grb='git rebase'
    if [ -f $GIT_COMPLETE ]; then
        __git_complete gf _git_diff
        __git_complete gc _git_commit
        __git_complete gp _git_push
        __git_complete gck _git_checkout
        __git_complete gcp _git_cherry_pick
    fi
fi

#proxychains
alias pc='proxychains'
alias pcq='proxychains -q'

# rm
alias rm='rm -I'

# cd
alias ..='cd ../'

# free
alias free='free -h'

# work
alias gweeklog='git log origin/master --author `git config --get user.name` --since "5 days ago" --oneline | tac | sed "s/^\w*\ *//''" | cut -d " " -f2-'

# ydcv
alias y='ydcv'

# vim
alias e='nvim'  # e means edit

# tmux
alias tt='tmux attach || tmux new'

# vim
alias vimdiff='nvim -d'

# proxy
function proxyon() {
    export http_proxy="http://127.0.0.1:8123/"
    export https_proxy=$http_proxy HTTP_PROXY=$http_proxy HTTPS_PROXY=$http_proxy
    export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com"
}

function proxyoff() {
    unset http_proxy https_proxy no_proxy HTTP_PROXY HTTPS_PROXY
}

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
