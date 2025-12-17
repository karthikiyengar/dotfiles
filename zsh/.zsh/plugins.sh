# Initialize zinit
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
source "${ZINIT_HOME}/zinit.zsh"

# Initialize completion system (required for fzf-tab and all completion)
autoload -Uz compinit
# Daily cache: only rebuild once per day for performance
if [ "$(date +'%j')" != "$(stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)" ]; then
  compinit
else
  compinit -C  # Skip security checks, use cache
fi

# Load fzf-tab immediately after compinit (must not be deferred)
zinit light Aloxaf/fzf-tab

# Non-critical plugins can use turbo mode
zinit wait lucid for \
    kutsan/zsh-system-clipboard

# Note: zsh-z replaced by zoxide (configured in dev.sh)
# Note: zsh-autocomplete disabled for performance