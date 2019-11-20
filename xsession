#!/bin/bash

xsettingsd &
xcompmgr &
xrandr --auto --output DP-3 --primary --right-of DP-2

volumeicon &
/usr/share/goobuntu-indicator/goobuntu_indicator.py &

if [ "z$DISPLAY" = "z:0" ]; then
  xscreensaver &
else
  . $HOME/.profile
fi

xmodmap $HOME/.xmodmap &

exec xmonad
