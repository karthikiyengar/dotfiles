#!/usr/bin/env bash

GET_VOLUME="amixer sget Master | grep -oP '\[(\d+)%\]' | head -n 1 | grep -oP '\d+'"
volMsgId="20893"
brightMsgId="15323"

if [ $1 == "volume-inc" ]; then
    volume=$(eval $GET_VOLUME)
    amixer set 'Master' 5%+; rumno -v $(eval $GET_VOLUME) 
    dunstify -a "changeVolume" -u low -i audio-volume-high -r "$volMsgId" -h int:value:"$volume" "Volume: ${volume}%"
fi

if [ $1 == "volume-dec" ]; then
    volume=$(eval $GET_VOLUME)
    amixer set 'Master' 5%-;
    dunstify -a "changeVolume" -u low -i audio-volume-high -r "$volMsgId" -h int:value:"$volume" "Volume: ${volume}%"
fi

if [ $1 == "mute" ]; then
    STATUS=$(amixer set 'Master' toggle | grep -oP '\[(on|off)\]' | head -n 1 | grep -oP '\w+')
    volume=$(eval $GET_VOLUME)
    if [ $STATUS == "off" ]; then
        dunstify -a "changeVolume" -u low -i audio-volume-muted -r "$volMsgId" "Volume Muted" 
    else
        dunstify -a "changeVolume" -u low -i audio-volume-high -r "$volMsgId" -h int:value:"$volume" "Volume: ${volume}%"
    fi
fi


if [ $1 == "brightness-inc" ]; then
    light -A 5; 
    brightness=$(light -G)
    dunstify -a "changeBrightness" -u low -r "$brightMsgId" -h int:value:"$brightness" "Brightness: ${brightness}%"
fi

if [ $1 == "brightness-dec" ]; then
    light -U 5;
    brightness=$(light -G)
    dunstify -a "changeBrightness" -u low -r "$brightMsgId" -h int:value:"$brightness" "Brightness: ${brightness}%"
fi 