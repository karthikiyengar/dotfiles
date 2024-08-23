# Check if tmux is installed
if [ -z "$TMUX" ]; then
    exec tmux new-session -A -s workspace
fi
