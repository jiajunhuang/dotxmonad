#!/bin/bash

trayer --edge top --align right --widthtype percent --width 11 --tint 0x353945 --height 21 --transparent true --alpha 0 &

dropbox &
volumeicon &
fcitx &
nm-applet &

exec xmonad
