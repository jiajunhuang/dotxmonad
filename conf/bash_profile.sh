#
# ~/.bash_profile
#

# start tmux
if [[ -z "$TMUX"  ]] && [ "$SSH_CONNECTION" != "" ] && [ "$TERM_PROGRAM" == "" ]; then
    exec ~/.xmonad/conf/tmux.py
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc
