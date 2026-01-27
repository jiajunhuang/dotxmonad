#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [[ -n "$SSH_CONNECTION" || -n "$SSH_CLIENT" ]]; then
    if command -v byobu >/dev/null 2>&1; then
        if [[ "$(uname)" == "Darwin" ]]; then
            _byobu_sourced=1 . /opt/homebrew/bin/byobu-launch 2>/dev/null || true
        else
            _byobu_sourced=1 . /usr/bin/byobu-launch 2>/dev/null || true
        fi
    fi
fi
