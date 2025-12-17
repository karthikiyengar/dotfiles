#!/usr/bin/env bash

# Bootstrap script for dotfiles
# Usage: ./install.sh

set -e

DOTFILES_DIR="$HOME/.dotfiles"
BACKUP_DIR="$HOME/.dotfiles_backup_$(date +%Y%m%d_%H%M%S)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}==>${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}==>${NC} $1"
}

log_error() {
    echo -e "${RED}==>${NC} $1"
}

# Check if running on macOS
if [[ "$OSTYPE" != "darwin"* ]]; then
    log_warn "This script is optimized for macOS. Some steps may need adjustment for other platforms."
fi

log_info "Starting dotfiles installation..."

# Install Homebrew if not installed
if ! command -v brew &> /dev/null; then
    log_info "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Add Homebrew to PATH for Apple Silicon Macs
    if [[ $(uname -m) == "arm64" ]]; then
        echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> "$HOME/.zprofile"
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
else
    log_info "Homebrew already installed"
fi

# Update Homebrew
log_info "Updating Homebrew..."
brew update

# Install packages from Brewfile
if [ -f "$DOTFILES_DIR/Brewfile" ]; then
    log_info "Installing packages from Brewfile..."
    cd "$DOTFILES_DIR"
    brew bundle
else
    log_error "Brewfile not found at $DOTFILES_DIR/Brewfile"
    exit 1
fi

# Install zinit (zsh plugin manager)
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
if [ ! -d "$ZINIT_HOME" ]; then
    log_info "Installing zinit..."
    mkdir -p "$(dirname $ZINIT_HOME)"
    git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
else
    log_info "zinit already installed"
fi

# Backup existing dotfiles
log_info "Backing up existing dotfiles to $BACKUP_DIR..."
mkdir -p "$BACKUP_DIR"

for dir in git npm nvim tmux zsh; do
    if [ -d "$DOTFILES_DIR/$dir" ]; then
        # Find config files that would be symlinked
        find "$DOTFILES_DIR/$dir" -type f | while read -r file; do
            rel_path="${file#$DOTFILES_DIR/$dir/}"
            target="$HOME/$rel_path"

            if [ -e "$target" ] && [ ! -L "$target" ]; then
                mkdir -p "$(dirname "$BACKUP_DIR/$dir/$rel_path")"
                mv "$target" "$BACKUP_DIR/$dir/$rel_path"
                log_warn "Backed up: $target"
            fi
        done
    fi
done

# Use stow to symlink dotfiles
log_info "Symlinking dotfiles with stow..."
cd "$DOTFILES_DIR"

for dir in git npm nvim tmux zsh; do
    if [ -d "$dir" ]; then
        log_info "Stowing $dir..."
        stow -v "$dir"
    fi
done

# Run tmux installation script
if [ -f "$DOTFILES_DIR/tmux/install.sh" ]; then
    log_info "Running tmux setup..."
    bash "$DOTFILES_DIR/tmux/install.sh"
fi

# Install pyenv if not installed
if ! command -v pyenv &> /dev/null; then
    log_info "Installing pyenv..."
    curl https://pyenv.run | bash
else
    log_info "pyenv already installed"
fi

# Install rbenv if not installed
if [ ! -d "$HOME/.rbenv" ]; then
    log_info "Installing rbenv..."
    git clone https://github.com/rbenv/rbenv.git "$HOME/.rbenv"
    git clone https://github.com/rbenv/ruby-build.git "$HOME/.rbenv/plugins/ruby-build"
else
    log_info "rbenv already installed"
fi

# Install rust if not installed
if ! command -v rustc &> /dev/null; then
    log_info "Installing Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
else
    log_info "Rust already installed"
fi

# Install SDKMAN if not installed
if [ ! -d "$HOME/.sdkman" ]; then
    log_info "Installing SDKMAN..."
    curl -s "https://get.sdkman.io" | bash
else
    log_info "SDKMAN already installed"
fi

log_info ""
log_info "Installation complete!"
log_info ""
log_info "Next steps:"
log_info "  1. Restart your terminal or run: exec zsh"
log_info "  2. Install language versions as needed:"
log_info "     - Node: fnm install <version>"
log_info "     - Python: pyenv install <version>"
log_info "     - Ruby: rbenv install <version>"
log_info "  3. Generate SSH key if needed: ssh-keygen -t ed25519 -C 'your@email.com'"
log_info ""
log_info "Backup of old dotfiles: $BACKUP_DIR"
