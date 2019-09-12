import os

PREFIX = "~/.xmonad/"

MAPPER = {
    "bash/bashrc.sh": "~/.bashrc",
    "bash/bash_profile.sh": "~/.bash_profile",
    "ack/ackrc": "~/.ackrc",
    "vim/vimrc": "~/.config/nvim/init.vim",
    "tmux/tmux.conf": "~/.tmux.conf",
}

for k, v in MAPPER.items():
    print("unlink", v)
    os.system("rm -f {}".format(v))
    print("symbol link {} to {}".format(v, k))
    os.system("ln -s {} {}".format(PREFIX + k, v))
