# Enable Rbenv
[ -f ~/.rbenv/bin/rbenv ] && eval "$(~/.rbenv/bin/rbenv init - zsh)"

# Rust/Cargo
if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

# Add ssh key to agent
command -v keychain >/dev/null 2>&1 && eval $(keychain -q --eval id_rsa)
