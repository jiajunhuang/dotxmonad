#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [ -d ~/.byobu ]; then
    _byobu_sourced=1 . /usr/bin/byobu-launch 2>/dev/null || true
fi
