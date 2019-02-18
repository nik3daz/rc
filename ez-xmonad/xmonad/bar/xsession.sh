#!/bin/bash

$(dirname $0)/tray.sh &
exec xmonad
