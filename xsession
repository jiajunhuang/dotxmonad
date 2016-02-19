#!/bin/bash

xcompmgr &

# set backlight
xbacklight -set 60 &

# volumeicon
volumeicon &

# input method
fcitx &

# disable touchpad after login
touchpad_disable.sh &
