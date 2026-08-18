#!/bin/bash

alias vi='vim'
#
# Paths etc
#

# Mac gets crappy hostname sometimes.
export EDITOR="vim"

export CHROME_DEVEL_SANDBOX=/usr/local/sbin/chrome-devel-sandbox

export PATH="$HOME/local/bin:$PATH"
export PATH="$HOME/local/rc_scripts:$PATH"
export PATH="$HOME/depot_tools:$PATH"
export PATH="$HOME/local/depot_tools:$PATH"
export PATH="$GOROOT/bin:$PATH"

export CHROME_REMOTE_DESKTOP_DEFAULT_DESKTOP_SIZES=3440x1440,2560x1440,1920x1080

umask 022
