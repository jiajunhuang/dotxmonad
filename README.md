## This is my configuration files

xmonad, neovim, sakura, zathura with all custom to solarized dark colorscheme.

### Usage

```bash
$ git clone https://github.com:jiajunhuang/dotxmonad $HOME/.xmonad
$ curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
$ ln -s ~/.xmonad/nvim/init.vim ~/.config/nvim/init.vim
$ ln -s ~/.xmonad/bash/bashrc $HOME/.bashrc # same as above
```

you can have your own bashrc set in `.xmonad/bash/bash_extendrc`

Okay, then logout and login(or simply `source .bashrc`).

### About configuration file for xmonad

if you would like to use my xmonad configuration, what you need to do is just
follow the steps below:

```bash
$ xmonad --recompile
```

of course, you need to install some packages for xmonad to get ready to use
e.g. dmenu, xcompmgr, xbacklight, etc. have a look at xsession.

finally, link it to `.xsession`

```bash
cd ~
ln -s .xmonad/xsession .xsession
```

enjoy! :)
