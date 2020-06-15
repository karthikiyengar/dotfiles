#!/bin/sh
if [ $(pgrep -x xidlehook) ]; then
    pkill xidlehook
else
    sh ~/.wm-scripts/xidlehook.sh
fi