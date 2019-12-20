#
# ~/.bash_profile
#

# start tmux
if [[ -z "$TMUX"  ]] && [ "$SSH_CONNECTION" != ""  ]; then
    exec ~/.xmonad/bash/tmux.py
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc
