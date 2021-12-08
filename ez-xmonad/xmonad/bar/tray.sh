#!/bin/bash -x

run() {
  (exec "$@") &
}

run gnome-screensaver
run gnome-settings-daemon
run gnome-power-manager
run nm-applet --sm-disable &
run kmix --keepvisibility
run bluetooth-applet
#sleep 5 
#exec trayer --expand false --edge top --align right --widthtype request --height 22 --margin 267

