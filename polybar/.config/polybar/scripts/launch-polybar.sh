#!/usr/bin/env bash

killall -q polybar;


# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null
do
    sleep 1
done


if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar --reload xmother >> /tmp/polybar-${m}.log 2>&1 &
    disown
  done
else
  polybar --reload xmother &
fi
