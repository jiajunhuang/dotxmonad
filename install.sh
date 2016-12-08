#!/bin/bash

# NOTE:
# it's for personal use! so this script will remove .bashrc etc.

case $1 in
    "neovim")
        curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        ln -s ~/.xmonad/vim/vimrc ~/.config/nvim/init.vim
    ;;

    "vim")
        curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        ln -s ~/.xmonad/vim/vimrc ~/.vimrc
    ;;

    *)
        echo "Usage: ./install.sh neovim|vim"
        exit 1
    ;;
esac
