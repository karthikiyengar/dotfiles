#!/bin/sh

ADAPTER="Adapter1"
ADDRESS="hci0"

IS_BLUETOOTH_ON=$(dbus-send --system --dest=org.bluez --print-reply /org/bluez/${ADDRESS} org.freedesktop.DBus.Properties.Get string:org.bluez.${ADAPTER} string:Powered )

DESIRED_STATE="false"
case $IS_BLUETOOTH_ON in 
  *false*)
    DESIRED_STATE="true"
    ;;
esac

dbus-send --system --dest=org.bluez --print-reply /org/bluez/${ADDRESS} org.freedesktop.DBus.Properties.Set string:org.bluez.${ADAPTER} string:Powered variant:boolean:${DESIRED_STATE}

exit 0