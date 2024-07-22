# Check if tmux is installed
if command -v tmux >/dev/null 2>&1; then
    # If not inside a tmux session, start or attach to a session
    [ -z "$TMUX" ] && (tmux attach || tmux new-session)
fi
