#!/bin/bash

xcompmgr &

# set backlight
xbacklight -set 60 &

# spawn trayer
trayer --edge top --align right --widthtype percent --width 11 --tint 0x353945 --height 21 --transparent true --alpha 0 &

# volumeicon
volumeicon &

# input method
fcitx &
sogou-qimpanel &

# dropbox
dropbox &

# udiskie
udiskie -aN &

# touchpad
xinput disable `xinput list | grep SynPS | sed 's/.*id\=\([0-9]\+\).*/\1/g'` &

# aria2
aria2c -D &

# nm-applet
nm-applet &
