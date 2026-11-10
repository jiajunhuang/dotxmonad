#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

if [[ $- == *i* && -t 0 && -t 1 && -z "$TMUX" &&
      ( -n "$SSH_CONNECTION" || -n "$SSH_CLIENT" ) ]]; then
    if command -v tmux >/dev/null 2>&1; then
        # Reuse the same session on each SSH login, creating it if needed.
        exec tmux new-session -A -s main
    fi
fi
