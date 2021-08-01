#!/usr/bin/env bash
if [[ "$DESKTOP_SESSION" == *"xmonad"* ]] 
then
    lxqt-policykit-agent &!
    gnome-keyring-daemon &!
    # Applets
    blueman-applet &!
    greenclip daemon &!
    flameshot &!
    redshift-gtk &!
    (sleep 5 && nm-applet --sm-disable --indicator) &!

    # Volume and brightness notifications
    # ~/.wm-scripts/brightness-watcher.py | xob &!
    # ~/.wm-scripts/pulse-volume-watcher.py | xob &!

    # Set cursor to pointer
    xsetroot -cursor_name left_ptr &!

    # Load custom fonts
    xset +fp $HOME/.fonts
    xset fp rehash &!

    # Notification Daemon
    dunst &!

    # Set the correct xrandr settings
    autorandr -c

    # Enable keyboard layouts
    recollindex -w 60 -m &!


    # Set Wallpaper
    if [ -f ~/.wallpapers/current-wallpaper.jpg ]; then
        feh --bg-scale ~/.wallpapers/current-wallpaper.jpg
    else
        feh --bg-scale ~/.wallpapers/dark-space.jpg
    fi

    if [ -x "$(command -v nextcloud)" ]; then
        nextcloud --background &!
    fi

    # Set touchpad settings
    ~/.wm-scripts/touchpad-settings.sh  

    export _JAVA_AWT_WM_NONREPARENTING=1
    # Run xidlehook
    ~/.wm-scripts/suspend.sh
    
fi
