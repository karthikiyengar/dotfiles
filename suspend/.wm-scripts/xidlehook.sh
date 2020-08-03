#!/bin/sh

xidlehook --not-when-fullscreen --not-when-audio \
  --timer 60 'xrandr --output "eDP-1" --brightness .3' 'xrandr --output "eDP-1" --brightness 1' \
  --timer 10 'xrandr --output "eDP-1" --brightness 1; i3lock -c 444444' '' \
  --timer 300 'systemctl suspend' '' &
