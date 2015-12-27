======================
My configuration files for xmonad, zathura, neovim
======================

------
Usage
------

1. install xmonad and xmobar(or dzen2 if you like)::

   $ sudo pacman -S xmonad xmonad-contrib xmobar dmenu  # ArchLinux
   $ sudo apt-get install xmonad xmobar suckless-tools  # Ubuntu, Debian

#. clone this repository to your gnu/linux and rename it to ``.xmonad``::

   $ git clone https://github.com/jiajunhuang/dotxmonad ~/.xmonad

#. install plug.vim for neovim::

   $ curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

#. make a soft link with your ``.bashrc``, ``init.vim`` and maybe ``zathura`` if you like::

   $ ln -s ~/.xmonad/bash/bashrc ~/.bashrc
   $ ln -s ~/.xmonad/nvim/init.vim ~/.config/nvim/init.vim
   $ ln -s ~/.xmonad/zathura ~/.config/zathura  # this should be execute only if you use zathura


#. make a soft link with ``.xsession``::

   $ ln -s ~/.xmonad/xsession ~/.xsession

#. press ``Alt+q`` to recompile xmonad, and try to relogin. enjoy!

# (optional) install other packages you need::

    $ sudo pacman -S xcompmgr xorg-xbacklight trayer redshift volumeico\
        fcitx network-manager-applet neovim gnome-terminal slock feh

----
Note
----

1, you can set your own bash script in ``.xmonad/bash/bash_extendrc``, and then type ``source ~/.bashrc``, it will be included automatically.
