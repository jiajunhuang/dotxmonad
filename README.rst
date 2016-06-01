My configuration files
=======================

This repo is named ``dotxmonad`` because I'm a XMonad_ user.

Items:

- Neovim_ : my editor

- Aria2_ : for download

- Bash_ : shell, you know

.. _XMonad: http://xmonad.org/
.. _Neovim: https://neovim.io/
.. _Aria2: https://aria2.github.io/
.. _Bash: https://www.gnu.org/software/bash/

Usage
--------

XMonad, Bash, etc...
~~~~~~~~~~~~~~~~~~~~~~

# clone::

    git clone git@github.com:jiajunhung/dotxmonad ~/.xmonad

# Install::

    # pacman -S xmonad xmonad-contrib xmobar dmenu trayer xcompmgr feh

# Link::

    $ ln -s ~/.xmonad/bash/bashrc ~/.bashrc # remove it first if it exists
    $ ln -s ~/.xmonad/bash/bash_aliases ~/.bash_aliases # same as above
    $ ln -s ~/.xmonad/aria2 ~/.config/aria2
    $ ln -s ~/.xmonad/zathura ~/.config/zathura

Vim, Neovim
~~~~~~~~~~~~~

if you're using Vim::

    $ pacman -S vim
    $ curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    $ ln -s ~/.xmonad/vim/vimrc ~/.vimrc

or Neovim::

    $ pacman -S neovim
    $ curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    $ ln -s ~/.xmonad/vim/vimrc ~/.config/nvim/init.vim

then spawn vim and enter ``:PlugInstall``!

Enjoy!

