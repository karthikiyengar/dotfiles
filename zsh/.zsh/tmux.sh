# Auto-attach to tmux session
if [ -z "$TMUX" ]; then
    exec tmux new-session -A -s workspace
fi
