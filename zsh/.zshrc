# Use Antigen to manage packages
# source ~/.antigen.zsh

export TERM=xterm;

# antigen bundle agkozak/zsh-z
# antigen bundle Aloxaf/fzf-tab
# antigen bundle zsh-users/zsh-autosuggestions
# antigen bundle hoffi/zsh-theme-lambda
# antigen apply

export EDITOR='nvim'

[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'

HISTSIZE=999999999

alias vim="nvim"
alias ls="ls -lah --color"
alias vn="vim ~/dotfiles/nixos/configuration.nix"
alias vz="vim ~/.zshrc"
alias sz="source ~/.zshrc"
alias dev="cd ~/development"
alias ga="git add --all"
alias gc="git commit"
alias r="sudo nixos-rebuild switch"
alias nclean="find . -name "node_modules" -exec rm -rf '{}' +"
alias adfs="sh ~/adfs.sh"
alias nixgc="sudo nix-collect-garbage -d; nix-collect-garbage -d; sudo nix-store --optimize"
alias u="sudo nixos-rebuild switch --upgrade"
alias crocks="cd ~/development/crocks"
alias mapi="cd ~/development/lyra-api"
alias wp="~/.wm-scripts/change-wallpaper.sh"
alias grm="git fetch --all; git rebase origin/master"
alias gd="git diff --staged"
alias vpn="sudo protonvpn c -f"
alias vpnin="sudo protonvpn c --cc IN"
alias vpnde="sudo protonvpn c --cc DE"
alias vpnd="sudo protonvpn d"
alias vpns="sudo protonvpn s"
alias dotf="code ~/dotfiles"
alias vx="code ~/.xmonad/"

bindkey -v
if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

# Enable thefuck
eval $(thefuck --alias f)

# Enable direnv
eval "$(direnv hook zsh)"
if [ -e /home/kiyengar/.nix-profile/etc/profile.d/nix.sh ]; then . /home/kiyengar/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export LOCALE_ARCHIVE_2_11="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
export LOCALE_ARCHIVE="/usr/bin/locale"

export PATH="$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.deno/bin:$HOME/.npm-global/bin:$PATH:$HOME/.cargo/bin"

enable-fzf-tab;

