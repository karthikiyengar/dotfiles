# Dotfiles

Personal dotfiles managed with GNU Stow.

## Quick Start

On a new machine, clone this repo and run the install script:

```bash
git clone https://github.com/yourusername/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
./install.sh
```

That's it! The script will:
- Install Homebrew (if not installed)
- Install all dependencies from the Brewfile
- Install zinit, pyenv, rbenv, rust, SDKMAN
- Symlink all configs using stow
- Run additional setup (like tmux configuration)
- Backup any existing dotfiles

## Manual Setup

If you prefer to install things manually:

### Install Dependencies

```bash
# Install Homebrew packages
brew bundle

# Install zinit
mkdir -p "${XDG_DATA_HOME:-${HOME}/.local/share}/zinit"
git clone https://github.com/zdharma-continuum/zinit.git "${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
```

### Symlink Configs

```bash
cd ~/.dotfiles

# Symlink all configs
stow git npm nvim tmux zsh

# Or symlink individually
stow zsh    # Symlinks zsh/.zshrc to ~/.zshrc
stow nvim   # Symlinks nvim config
stow tmux   # Symlinks tmux config
```

### Additional Setup

```bash
# Setup tmux with Oh My Tmux
./tmux/install.sh
```

## Structure

```
.dotfiles/
├── git/          # Git configuration
├── npm/          # npm configuration
├── nvim/         # Neovim configuration
├── tmux/         # Tmux configuration
├── zsh/          # Zsh configuration
├── Brewfile      # Homebrew dependencies
└── install.sh    # Bootstrap script
```

## Updating

To update your dotfiles on an existing machine:

```bash
cd ~/.dotfiles
git pull
brew bundle  # Install any new dependencies
```

To add new configs:

```bash
cd ~/.dotfiles
# Add files in the appropriate directory structure
# then restow
stow -R zsh  # Restow zsh (useful after adding new files)
```

## Tools Included

- **Shell**: zsh with zinit plugin manager
- **Terminal Multiplexer**: tmux with Oh My Tmux
- **Editor**: neovim
- **Fuzzy Finder**: fzf
- **Directory Navigation**: zoxide
- **Shell History**: atuin
- **Git TUI**: lazygit
- **Version Managers**: fnm (node), pyenv (python), rbenv (ruby), SDKMAN (java/kotlin/scala)
- **Environment**: direnv for per-directory env vars

## Why Stow?

GNU Stow is a symlink manager that makes it easy to:
- Keep all dotfiles in one directory
- Symlink them to the right places in your home directory
- Add/remove configs easily
- Version control your setup

It's simple, battle-tested, and doesn't require any special software beyond GNU Stow itself.
