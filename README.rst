My configuration files for bash and vim
========================================

This repo is named ``dotxmonad`` because I was a ``XMonad`` user.
Now I'm using Windows, with Linux in a vm.

Usage
------

1. install bash and vim::

    $ sudo apt-get install vim bash

#. clone this repository to your gnu/linux and rename it to ``.xmonad``::

    $ git clone https://github.com/jiajunhuang/dotxmonad ~/.xmonad

#. install plug.vim for neovim::

    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

#. make a soft link with your ``.bashrc``, ``.vimrc``

    $ ln -s ~/.xmonad/bash/bashrc ~/.bashrc
    $ ln -s ~/.xmonad/vim/vimrc ~/.vimrc

Note
----

- you can set your own bash script in ``.xmonad/bash/bash_extendrc``, and then type ``source ~/.bashrc``, it will be included
  automatically.
