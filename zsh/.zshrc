source "${ZSH_CONFIG_DIR:-$HOME/.zsh}/index.sh"

# bun completions
[ -s "/Users/kiyengar/.bun/_bun" ] && source "/Users/kiyengar/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
