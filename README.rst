My configuration files for bash and vim
========================================

This repo is named ``dotxmonad`` because I was a ``XMonad`` user.
Now I'm using Windows, with Linux in a vm.

Usage
------

1. install bash and vim::

    $ sudo apt-get install vim bash  # Unix user only

    if you're a Windows user, please install ``gvim for Windows`` and ``Git for Windows``

#. clone this repository to your gnu/linux and rename it to ``.xmonad``::

    $ git clone https://github.com/jiajunhuang/dotxmonad ~/.xmonad

#. install plug.vim for neovim::

    $ curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim  # Unix user only

    $ # if you're a Windows user, please follow these steps in PowerShell::

    md ~\vimfiles\autoload
    $uri = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    (New-Object Net.WebClient).DownloadFile($uri, $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath("~\vimfiles\autoload\plug.vim"))

#. make a soft link with your ``.bashrc``, ``.vimrc``::

    $ ln -s ~/.xmonad/bash/bashrc ~/.bashrc
    $ ln -s ~/.xmonad/vim/vimrc ~/.vimrc

    $ # if you're a Windows user, please add a soft-link::

    $ ln -s ~/vimfiles ~/.vim

Note
----

- you can set your own bash script in ``.xmonad/bash/bash_extendrc``, and then type ``source ~/.bashrc``, it will be included
  automatically.
