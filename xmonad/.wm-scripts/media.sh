#!/bin/bash

GET_VOLUME="amixer sget Master | grep -oP '\[(\d+)%\]' | head -n 1 | grep -oP '\d+'"
if [ $1 == "volume-inc" ]; then
    amixer set 'Master' 5%+; rumno -v $(eval $GET_VOLUME) 
fi

if [ $1 == "volume-dec" ]; then
    amixer set 'Master' 5%-; rumno -v $(eval $GET_VOLUME) 
fi

if [ $1 == "mute" ]; then
    STATUS=$(amixer set 'Master' toggle | grep -oP '\[(on|off)\]' | head -n 1 | grep -oP '\w+')
    if [ $STATUS == "off" ]; then
        rumno -m 
    else
        rumno -v $(eval $GET_VOLUME)
    fi
fi


if [ $1 == "brightness-inc" ]; then
    light -A 5; rumno -b $(light -G) 
fi

if [ $1 == "brightness-dec" ]; then
    light -U 5; rumno -b $(light -G) 
fi