# tmux Configuration

Brain-dead tmux setup with [Oh My Tmux](https://github.com/gpakosz/.tmux) - sane defaults out of the box.

## Features

- **Vim keybindings** - Navigate and edit like a pro
- **Mouse support** - Scroll, select panes, resize with your mouse
- **Seamless navigation** - Smooth integration with vim and SSH sessions
- **Copy mode** - Vim-style text selection and copying
- **Search** - Find text in scrollback with `/` and `?`
- **Pane zooming** - Focus on one pane with `prefix + z`
- **Beautiful status bar** - Clean, informative display
- **Smart defaults** - Everything configured for modern terminal usage

## Installation

```bash
cd ~/.dotfiles/tmux
./install.sh
```

That's it! The script will:
1. Check if tmux is installed (install it if needed)
2. Clone Oh My Tmux
3. Symlink all necessary config files
4. Set up your custom configuration

## Quick Start

Start tmux:
```bash
tmux
```

Create a new session with a name:
```bash
tmux new -s mysession
```

Attach to an existing session:
```bash
tmux attach -t mysession
# or just
tmux a
```

List sessions:
```bash
tmux ls
```

## Keybindings

### Prefix Key
The prefix is `Ctrl+a` (instead of the default `Ctrl+b`)

### Pane Management
- `prefix + |` - Split vertically
- `prefix + -` or `prefix + _` - Split horizontally
- `prefix + h/j/k/l` - Navigate panes (vim-style)
- `prefix + H/J/K/L` - Resize panes (vim-style)
- `prefix + z` - Zoom/unzoom current pane
- `Ctrl+h/j/k/l` - Navigate between tmux panes and vim splits seamlessly

### Window Management
- `prefix + c` - Create new window
- `prefix + ,` - Rename window
- `prefix + n` - Next window
- `prefix + p` - Previous window
- `prefix + Ctrl+h` - Previous window (quick)
- `prefix + Ctrl+l` - Next window (quick)
- `prefix + 0-9` - Switch to window by number

### Copy Mode
- `prefix + [` - Enter copy mode
- `v` - Start selection (in copy mode)
- `y` - Copy selection (in copy mode)
- `/` - Search forward (in copy mode)
- `?` - Search backward (in copy mode)
- `q` - Exit copy mode

### Session Management
- `prefix + d` - Detach from session
- `prefix + $` - Rename session

### Other
- `prefix + r` - Reload config
- `prefix + ?` - Show all keybindings

## Mouse Support

Mouse support is enabled by default:
- Click to select pane
- Drag border to resize panes
- Scroll to navigate history
- Click on window name to switch windows
- Double-click to select word, triple-click for line

## Seamless Vim Integration

The configuration includes smart pane navigation that works seamlessly with vim:
- Use `Ctrl+h/j/k/l` to navigate between tmux panes and vim splits
- No need to use the prefix when navigating

To enable this in vim, add the [vim-tmux-navigator](https://github.com/christoomey/vim-tmux-navigator) plugin:

```vim
" Using vim-plug
Plug 'christoomey/vim-tmux-navigator'
```

## SSH Agent Forwarding

The configuration includes setup for SSH agent forwarding. To make it work seamlessly:

Add this to your `~/.ssh/config`:
```
Host *
    ForwardAgent yes
```

Add this to your `~/.bashrc` or `~/.zshrc`:
```bash
if [ ! -S ~/.ssh/ssh_auth_sock ] && [ -S "$SSH_AUTH_SOCK" ]; then
    ln -sf $SSH_AUTH_SOCK ~/.ssh/ssh_auth_sock
fi
```

## Customization

Edit [.tmux.conf.local](.tmux.conf.local) to customize your configuration. This file is symlinked to `~/.tmux.conf.local` and contains all your personal settings.

The base configuration comes from Oh My Tmux at `~/.tmux/.tmux.conf` (don't edit this directly).

## Uninstall

```bash
rm -rf ~/.tmux
rm ~/.tmux.conf
rm ~/.tmux.conf.local
```

## Updating

Re-run the install script:
```bash
cd ~/.dotfiles/tmux
./install.sh
```

This will pull the latest version of Oh My Tmux.

## Troubleshooting

### Colors look wrong
Make sure your terminal supports 256 colors and true color. For iTerm2 and modern terminals, this should work out of the box.

### Copy doesn't work
The configuration uses `pbcopy` (macOS). For Linux, edit [.tmux.conf.local](.tmux.conf.local) and replace `pbcopy` with `xclip -selection clipboard` or `xsel --clipboard`.

### Seamless vim navigation doesn't work
Install the [vim-tmux-navigator](https://github.com/christoomey/vim-tmux-navigator) plugin in your vim/neovim configuration.

## Resources

- [Oh My Tmux Documentation](https://github.com/gpakosz/.tmux)
- [tmux Cheat Sheet](https://tmuxcheatsheet.com/)
- [vim-tmux-navigator](https://github.com/christoomey/vim-tmux-navigator)
