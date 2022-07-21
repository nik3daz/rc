#!/bin/bash

xsettingsd &
xcompmgr &
xrandr --auto --output DP-3 --primary --right-of DP-2
xrdb -merge ~/.Xresources
/usr/lib/notification-daemon/notification-daemon &

volumeicon &
/usr/share/goobuntu-indicator/goobuntu_indicator.py &

if [ -n "$DISPLAY" ]; then
  xscreensaver &
else
  . $HOME/.profile
fi

xmodmap $HOME/.xmodmap &

exec xmonad
