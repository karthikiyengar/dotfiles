if [ -x "$(command -v syndaemon)" ]; then
    # disable touchpad clicks when typing
    syndaemon -i .5 -K -t -R -d 
fi

if [ -x "$(command -v synclient)" ]; then
    synclient VertScrollDelta=-113
    synclient HorizScrollDelta=-113
else 
    TOUCHPAD=$(xinput | grep -i touchpad | grep -Po 'id=(\d+)' | cut -b 4-)
    if [ $TOUCHPAD ]; then 
        xinput set-prop $TOUCHPAD 'libinput Natural Scrolling Enabled' 1
        xinput set-prop $TOUCHPAD 'libinput Tapping Enabled' 1
    fi
fi
