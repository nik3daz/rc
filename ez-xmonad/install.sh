#!/bin/bash -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cpbak() {
  mv $2 $2.bak 2>/dev/null || true
  cp -R $1 $2
}

sudo apt install ghc libghc-parsec3-dev xmonad libghc-xmonad-contrib-dev libghc-xmonad-dev gmrun libghc-x11-dev xscreensaver xsettingsd xcompmgr volumeicon-alsa trayer dmenu gmrun xscreensaver xmodmap dzen2

dump_xsettings || true;

# To change toolbar monitor, change monitor 1 to 0 and barX to 0 in bar.hs.

