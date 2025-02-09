#!/bin/bash
# switch_audio.sh
# This script uses pactl and wofi to switch the default PulseAudio output device
# by selecting it from its Description with case insensitive search.
#
# Ensure that the necessary environment variables are set.

# Set up a full PATH and DISPLAY variable for GUI applications
export PATH="/usr/local/bin:/usr/bin:/bin"
export DISPLAY=:0

# Use absolute paths if needed (adjust if wofi or pactl are located elsewhere)
WOFI=/usr/bin/wofi
PACTL=/usr/bin/pactl

# Build a list of sinks with index and description.
# Each line will be in the format: "sink_index - Device Description"
sink_list=$($PACTL list sinks | awk '
    /^Sink #/ {
        # Extract the sink index by removing the leading "#" from the second field.
        sink_index = substr($2, 2);
        next
    }
    /Description:/ {
        # Extract the description after "Description:".
        desc = substr($0, index($0, "Description:") + length("Description:") + 1);
        # Print in the format: "sink_index - description"
        print sink_index " - " desc;
    }
')

# Check if any sinks were found.
if [ -z "$sink_list" ]; then
    echo "No audio sinks found."
    exit 1
fi

# Display a menu using wofi in dmenu mode with case-insensitive search enabled,
# and capture the selection.
selected_sink=$(echo "$sink_list" | $WOFI --dmenu --prompt "Select Audio Output:" --insensitive)

# If the user made a selection, extract the sink index and change the audio output.
if [ -n "$selected_sink" ]; then
    # The selected line is in the format "sink_index - description"
    sink_index=$(echo "$selected_sink" | cut -d' ' -f1)

    # Set the chosen sink as the default audio output device.
    $PACTL set-default-sink "$sink_index"

    # Move all active sink inputs (audio streams) to the new default sink.
    for input in $($PACTL list short sink-inputs | awk '{print $1}'); do
        $PACTL move-sink-input "$input" "$sink_index"
    done
fi
