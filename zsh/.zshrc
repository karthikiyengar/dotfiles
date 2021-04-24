# Use Antigen to manage packages
source ~/.antigen.zsh

export NVM_AUTO_USE=true
export TERM=xterm;

antigen use oh-my-zsh
antigen bundle agkozak/zsh-z
antigen bundle Aloxaf/fzf-tab
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle git-auto-fetch
antigen bundle git
antigen bundle hoffi/zsh-theme-lambda
antigen bundle vi-mode
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle akoenig/npm-run.plugin.ZSH
antigen bundle lukechilds/zsh-nvm
antigen apply

export EDITOR='nvim'

[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'

HISTSIZE=999999999

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

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

# Some git goodness
func glog() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

enable-fzf-tab;

