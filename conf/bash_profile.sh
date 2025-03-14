#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if command -v byobu > /dev/null; then
    _byobu_sourced=1 . /usr/bin/byobu-launch 2>/dev/null || true
fi
