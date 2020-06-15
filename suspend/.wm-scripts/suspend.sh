# Disconnect Sony Heaphones
bluetoothctl disconnect CC:98:8B:34:ED:2B

# Run xidlehook
xidlehook --not-when-fullscreen --not-when-audio \
  --timer 60 'xrandr --output "eDP-1" --brightness .3' 'xrandr --output "eDP-1" --brightness 1' \
  --timer 10 'xrandr --output "eDP-1" --brightness 1; slock' '' \
  --timer 300 'systemctl suspend' '' &
