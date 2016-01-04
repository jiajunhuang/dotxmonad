#!/bin/bash

xcompmgr &

# set backlight
xbacklight -set 60 &

# spawn trayer
trayer --edge top --align right --widthtype percent --width 11 --tint 0x353945 --height 21 --transparent true --alpha 0 &

#redshift (it's Shanghai, works if you're in China East part)
redshift -l 31.2000:121.5000 &

# volumeicon
volumeicon &

# input method
fcitx &

# nm-applet
nm-applet &
