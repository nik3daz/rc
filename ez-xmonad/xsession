#!/bin/bash

xsettingsd &
xcompmgr &
xscreensaver &

volumeicon &
/usr/share/goobuntu-indicator/goobuntu_indicator.py &

if [ "z$DISPLAY" = "z:0" ]; then
  xscreensaver &
else
  . $HOME/.profile
fi

exec xmonad
