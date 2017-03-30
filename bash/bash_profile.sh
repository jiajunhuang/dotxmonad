#
# ~/.bash_profile
#

[[ -f ~/.xmonad/bash/bashrc.sh ]] && . ~/.xmonad/bash/bashrc.sh
[[ -f ~/.bashrc ]] && . ~/.bashrc
if [ -x `command -v byobu` ]; then
    _byobu_sourced=1 . /usr/bin/byobu-launch 2>/dev/null || true
fi
