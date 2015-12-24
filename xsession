#!/bin/bash

xcompmgr &

# set backlight
xbacklight -set 60 &

# spawn trayer
trayer --edge top --align right --widthtype percent --width 10 --tint 0x002b36 --height 20 --transparent true --alpha 0 &

#redshift (it's Shanghai, works if you're in China East part)
redshift -l 31.2000:121.5000 &

# volumeicon
volumeicon &

# nm-applet
nm-applet &
