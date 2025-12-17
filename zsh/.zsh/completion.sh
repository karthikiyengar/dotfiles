# fzf-tab auto-enables when loaded via zinit, no manual enable needed

# Completion styling for better fzf-tab experience
zstyle ':completion:*' menu no  # Let fzf-tab handle menu
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}  # Colorize file lists
zstyle ':completion:*:descriptions' format '[%d]'  # Format group descriptions

# Prefer fzf installation using the install script on the README
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
