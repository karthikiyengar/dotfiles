#!/usr/bin/env bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_DIR="$(dirname "$SCRIPT_DIR")"
HOME_DIR="$HOME"

echo "Installing tmux with Oh My Tmux..."

# Check if tmux is installed
if ! command -v tmux &> /dev/null; then
    echo "tmux is not installed. Please install it first:"
    echo "  macOS: brew install tmux"
    echo "  Ubuntu/Debian: sudo apt install tmux"
    echo "  Fedora: sudo dnf install tmux"
    exit 1
fi

# Clone Oh My Tmux if not already present
if [ ! -d "$HOME_DIR/.tmux" ]; then
    echo "Cloning Oh My Tmux..."
    git clone https://github.com/gpakosz/.tmux.git "$HOME_DIR/.tmux"
else
    echo "Oh My Tmux already installed, updating..."
    cd "$HOME_DIR/.tmux"
    git pull
    cd "$SCRIPT_DIR"
fi

# Symlink the main .tmux.conf from Oh My Tmux
echo "Symlinking .tmux.conf..."
ln -sf "$HOME_DIR/.tmux/.tmux.conf" "$HOME_DIR/.tmux.conf"

# Symlink our custom local configuration
echo "Symlinking custom .tmux.conf.local..."
ln -sf "$SCRIPT_DIR/.tmux.conf.local" "$HOME_DIR/.tmux.conf.local"

echo ""
echo "✓ tmux setup complete!"
echo ""
echo "Features enabled:"
echo "  • Vim keybindings"
echo "  • Mouse support (scroll, select panes, resize)"
echo "  • Seamless navigation with vim/ssh"
echo "  • Copy mode with vim bindings"
echo "  • Search with / and ?"
echo "  • Pane zooming with prefix + z"
echo "  • Better defaults for splits, status bar, etc."
echo ""
echo "Useful keybindings:"
echo "  Prefix: Ctrl+a (instead of Ctrl+b)"
echo "  Split horizontal: prefix + -"
echo "  Split vertical: prefix + _"
echo "  Navigate panes: prefix + h/j/k/l"
echo "  Zoom pane: prefix + z"
echo "  Copy mode: prefix + ["
echo "  Search: prefix + / (in copy mode)"
echo "  Reload config: prefix + r"
echo ""
echo "Start tmux with: tmux"
