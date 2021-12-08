#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cpbak() {
  if [ -f $2 ]; then
    mv $2 $2.bak
  elif [ -d $2 ]; then
    mv $2 $2.bak
  fi
  cp -R $1 $2
}

sudo apt-get install xmonad libghc-xmonad-contrib-dev libghc-xmonad-dev gmrun libghc-x11-dev xscreensaver xsettingsd xcompmgr volumeicon-alsa trayer

# To change toolbar monitor, change monitor 1 to 0 and barX to 0 in bar.hs.

cpbak $DIR/xmonad $HOME/.xmonad
cd $HOME/.xmonad/bar
make
cd -
cpbak $DIR/xsession $HOME/.xsession

