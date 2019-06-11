import os

PREFIX = "~/.i3/"

MAPPER = {
    "bash/bashrc.sh": "~/.bashrc",
    "bash/bash_profile.sh": "~/.bash_profile",
    "ack/ackrc": "~/.ackrc",
    "vim/vimrc": "~/.config/nvim/init.vim",
    "vim/coc-settings.json", "~/.config/nvim/coc-settings.json",
}

for k, v in MAPPER.items():
    print("unlink", v)
    os.system("rm -f {}".format(v))
    print("symbol link {} to {}".format(v, k))
    os.system("ln -s {} {}".format(PREFIX + k, v))
