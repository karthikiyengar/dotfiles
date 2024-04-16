# Enable Rbenv
[ -f ~/.rbenv/bin/rbenv ] && eval "$(~/.rbenv/bin/rbenv init - zsh)"

# Rust/Cargo
source "$HOME/.cargo/env"

# Add ssh key to agent
command -v keychain >/dev/null 2>&1 && eval $(keychain -q --eval id_rsa)
