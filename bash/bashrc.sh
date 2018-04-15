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
if [ -f "/etc/arch-release" ]; then
    AUTO_JUMP=/etc/profile.d/autojump.bash
else
    AUTO_JUMP=/usr/share/autojump/autojump.sh
fi
if [ -f $AUTO_JUMP ]; then
    source $AUTO_JUMP
fi

# git completion
GIT_COMPLETE=~/.i3/bash/git-completion.sh
if [ -f $GIT_COMPLETE ]; then
    source $GIT_COMPLETE
fi

# kubectl completion
KUBE_COMPLETE=~/.i3/bash/kubectl.sh
if [ -f $KUBE_COMPLETE ]; then
    source $KUBE_COMPLETE
fi

# git prompt
GIT_PROMPT=~/.i3/bash/git-prompt.sh
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
export PATH=$HOME/.py3k/bin:$GOPATH/bin:$HOME/bin:/usr/local/openresty/bin:$PATH

# set default enviroment
export TERM=xterm-256color
export TERMINAL=/usr/bin/sakura
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

# go use proxy by default
PROXY='http_proxy=http://127.0.0.1:8123 https_proxy=http://127.0.0.1:8123'
alias go="$PROXY go"
alias dep="$PROXY dep"

# python
alias python=python3
alias bp=bpython

# antlr4
export CLASSPATH=".:/usr/share/java/antlr-complete.jar:/usr/local/lib/antlr-complete.jar:$CLASSPATH"
alias antlr4='java -Xmx500M -cp "/usr/local/lib/antlr-complete.jar:$CLASSPATH" org.antlr.v4.Tool'
alias grun='java org.antlr.v4.gui.TestRig'

# vim
alias vimdiff='nvim -d'

# lastpass
alias lpass='http_proxy=http://127.0.0.1:8123 https_proxy=http://127.0.0.1:8123 lpass'

# readline wrapper
alias luajit='rlwrap luajit'

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

# pandoc
function tohtml() {
    pandoc -s -S --toc --highlight-style pygments --css common.css -t html $@ -o /data/doc/$@.html
}
function topdf() {
    pandoc --pdf-engine=xelatex -V CJKmainfont="Source Han Serif CN" $@ -o /data/doc/$@.pdf
}

# proxy
function proxyon() {
    export http_proxy="http://127.0.0.1:8123/"
    export https_proxy=$http_proxy HTTP_PROXY=$http_proxy HTTPS_PROXY=$http_proxy
    export no_proxy="localhost,127.0.0.1,localaddress,.localdomain.com"
    echo "proxy on: " $http_proxy
}

function proxyoff() {
    unset http_proxy https_proxy no_proxy HTTP_PROXY HTTPS_PROXY
    echo "done"
}

# logbook
function lb() {
    # learn from: https://routley.io/tech/2017/11/23/logbook.html
    vim ~/logbook/$(date '+%Y-%m-%d').md
}

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
