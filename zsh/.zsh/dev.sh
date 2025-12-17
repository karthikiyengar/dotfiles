# Lazy-loaded to avoid ~50ms init cost on every shell startup
rbenv() {
  unset -f rbenv
  [ -f ~/.rbenv/bin/rbenv ] && eval "$(~/.rbenv/bin/rbenv init - zsh)"
  rbenv "$@"
}

# Lazy-loaded to avoid ~50ms init cost on every shell startup
pyenv() {
  unset -f pyenv
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(command pyenv init --path)"
  eval "$(command pyenv init -)"
  pyenv "$@"
}

# Rust/Cargo loaded immediately - minimal overhead (~5ms)
if [ -f "$HOME/.cargo/env" ]; then
    source "$HOME/.cargo/env"
fi

# Add ssh key to agent
command -v keychain >/dev/null 2>&1 && eval $(keychain -q --eval id_rsa)
ssh-add ~/.ssh/id_rsa >/dev/null 2>&1
ssh-add ~/.ssh/id_ed25519 >/dev/null 2>&1

# fnm loaded immediately for auto-switching on cd
FNM_PATH="/opt/homebrew/opt/fnm/bin"
if [ -d "$FNM_PATH" ]; then
  eval "$(fnm env --use-on-cd)"
fi

# zoxide - Smarter directory jumping (replaces zsh-z)
eval "$(zoxide init zsh)"

# direnv - Per-directory environment variables
eval "$(direnv hook zsh)"

# Editor
export EDITOR=nvim
export VISUAL=nvim

# Atuin must init early to replace zsh history mechanism
[ -f "$HOME/.atuin/bin/env" ] && . "$HOME/.atuin/bin/env"
eval "$(atuin init zsh)"

# SDKMAN requires env vars available for all Java tools (must be at end of initialization)
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
