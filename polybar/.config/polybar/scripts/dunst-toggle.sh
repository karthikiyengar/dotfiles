#!/bin/sh
set -xe
if [ "$(pgrep -xf /run/current-system/sw/bin/dunst)" ]; then
    pkill dunst
else
		echo "HERE";
    dunst &
fi
