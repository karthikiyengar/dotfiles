#!/bin/sh
# Allow screenshot to be selected from cursor, then display shot image.
# Author: Vic Fryzel
# http://github.com/vicfryzel/xmonad-config

SCREENSHOT_DIR=$HOME/Screenshots
mkdir -p $SCREENSHOT_DIR
sleep 0.2; scrot -s "$SCREENSHOT_DIR/%Y-%m-%d-%H%M%S_\$wx\$h.png" -e "xdg-open \$f"
