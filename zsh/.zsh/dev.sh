# Enable Rbenv
[ -f ~/.rbenv/bin/rbenv ] && eval "$(~/.rbenv/bin/rbenv init - zsh)"

# Rust/Cargo
source "$HOME/.cargo/env"

# Add ssh key to agent
eval $(keychain -q --eval id_rsa)