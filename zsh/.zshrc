# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

source ~/.zplug/init.zsh

zplug "romkatv/powerlevel10k", as:theme, depth:1
zplug "Aloxaf/fzf-tab"
zplug "agkozak/zsh-z"
zplug "kutsan/zsh-system-clipboard"
zplug "zsh-users/zsh-autosuggestions"


if ! zplug check; then
  zplug install
fi

zplug load


autoload -U compinit && compinit

HISTSIZE="999999999"
SAVEHIST="999999999"

HISTFILE="$HOME/.zsh_history"
mkdir -p "$(dirname "$HISTFILE")"

setopt HIST_FCNTL_LOCK
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
unsetopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
unsetopt EXTENDED_HISTORY
setopt autocd


if [ -f "~/.p10k.zsh" ]; then
  source ~/.p10k.zsh
fi

# Fix shitty vi mode bindings for Home/Ins/Del/Ctrl+Arrow
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line
bindkey  "^[[3~"  delete-char

eval $(thefuck --alias f) # Enable thefuck
eval "$(direnv hook zsh)" # Enable direnv
  
export PATH="$PATH:$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.deno/bin:$HOME/.npm-global/bin:$HOME/.cargo/bin"


# Aliases
alias crocks='cd ~/development/crocks'
alias dotf='code ~/dotfiles'
alias ga='git add --all'
alias gc='git commit'
alias gd='git diff --staged'
alias gst='git status'
alias gup='git fetch --all; git rebase origin/master'
alias ls='ls -lah --color'
alias mapi='cd ~/development/lyra-api'
alias nclean='find . -name " node_modules " -exec rm -rf '\''{}'\'' +'
alias nixgc='sudo nix-collect-garbage -d; nix-collect-garbage -d; sudo nix-store --optimize'
alias sz='source ~/.zshrc'
alias u='sudo dnf update'
alias vim='nvim'
alias vpn='sudo protonvpn c -f'
alias vpnd='sudo protonvpn d'
alias vpnde='sudo protonvpn c --cc DE'
alias vpnin='sudo protonvpn c --cc IN'
alias vpns='sudo protonvpn s'
alias vx='code ~/.xmonad/'
alias vz='vim ~/.zshrc'
alias wp='~/.wm-scripts/change-wallpaper.sh'

enable-fzf-tab

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
