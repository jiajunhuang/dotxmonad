import os

PREFIX = "~/.xmonad/"

MAPPER = {
    "conf/bashrc.sh": "~/.bashrc",
    "conf/bash_profile.sh": "~/.bash_profile",
    "conf/ackrc": "~/.ackrc",
    "conf/vimrc": "~/.config/nvim/init.vim",
    "conf/tmux.conf": "~/.tmux.conf",
    "conf/zathurarc": "~/.config/zathura/zathurarc",
    "conf/sakura.conf": "~/.config/sakura/sakura.conf",
}

os.system(
    "curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs "
    "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
)

for k, v in MAPPER.items():
    print("unlink", v)
    os.system("rm -f {}".format(v))
    print("symbol link {} to {}".format(v, k))
    os.system("ln -s {} {}".format(PREFIX + k, v))
