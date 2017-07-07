#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# bash completion
BASH_COMPLETION=/usr/share/bash-completion/bash_completion
if [ -f $BASH_COMPLETION ]; then
    source $BASH_COMPLETION
fi

# autojump
AUTO_JUMP=/usr/share/autojump/autojump.sh
if [ -f $AUTO_JUMP ]; then
    source $AUTO_JUMP
fi

# git completion
GIT_COMPLETE=~/.xmonad/bash/git-completion.sh
if [ -f $GIT_COMPLETE ]; then
    source $GIT_COMPLETE
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

# golang
export GOPATH=$HOME/golang
export PATH=$GOPATH/bin:/usr/local/openresty/bin:$PATH

# set default enviroment
export TERM=xterm-256color
export EDITOR=vim
export PS1='\[\033[38;5;2m\]\u\[$(tput sgr0)\]\[\033[38;5;15m\]@\[$(tput sgr0)\]\[\033[38;5;11m\]\h\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]\[\033[38;5;6m\]\W\[$(tput sgr0)\]\[\033[38;5;15m\]$(__git_ps1 " (%s)"): \[$(tput sgr0)\]'

# aliases

#ls
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

#grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

#git
if [ -n `which git` ]; then
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

# pandoc
alias topdf='pandoc --latex-engine=xelatex --template=$HOME/.xmonad/latex/cn_tpl.tex'

# ydcv
alias y='ydcv'

# vim
alias e='nvim'  # e means edit

# tmux
alias tt='tmux attach || tmux new'

# go use proxy by default
alias go='http_proxy=http://127.0.0.1:8123 https_proxy=http://127.0.0.1:8123 go'

# rsync
function syncto {
    if [ $# -eq 0 ]
    then
        echo "Usage: syncto [from] [to]"
        echo "Example: syncto . root@my_remote_host:/tmp/"
    else
        rsync -a --delete $@
    fi
}

# python
alias python=python3
alias bp=bpython

# pandoc
function tohtml() {
    pandoc -s -S --toc --highlight-style pygments --css common.css -t html $@ > /data/doc/$@.html
}

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
