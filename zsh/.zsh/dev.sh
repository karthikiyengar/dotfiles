# Enable Rbenv
[ -f ~/.rbenv/bin/rbenv ] && eval "$(~/.rbenv/bin/rbenv init - zsh)"

# Rust/Cargo
if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"                   # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion

# Add ssh key to agent
command -v keychain >/dev/null 2>&1 && eval $(keychain -q --eval id_rsa)
ssh-add ~/.ssh/id_rsa >/dev/null 2>&1

# Editor
export EDITOR=nvim
export VISUAL=nvim
