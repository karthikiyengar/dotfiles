# Use Antigen to manage packages
source ~/.antigen.zsh

antigen use oh-my-zsh
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle git
antigen bundle hoffi/zsh-theme-lambda
antigen apply

export ZSH="$HOME/.oh-my-zsh"
source $ZSH/oh-my-zsh.sh

export EDITOR='nvim'
export FFF_HIDDEN=1

[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'

HISTSIZE=999999999

function f() {
    fff "$@"
    cd "$(cat "${XDG_CACHE_HOME:=${HOME}/.cache}/fff/.fff_d")"
}

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

alias vim="nvim"
alias ls="ls -lah --color"
alias vz="vim ~/.zshrc"
alias sz="source ~/.zshrc"
alias dev="cd ~/Development"
alias ga="git add --all"
alias gc="git commit"
alias cs="cd ~/Development/custom-solutions"
alias lia="cd ~/Development/lia"
alias whoami="echo k.iyengar"
alias vpn="cd ~/VPN && sudo openvpn --config k.iyengar@laptop.ovpn"
alias r="ranger"
alias nclean="find . -name "node_modules" -exec rm -rf '{}' +"
alias adfs="sh ~/adfs.sh"
alias u="sudo apt-get update; sudo apt-get upgrade"
alias crocks="cd ~/Development/crocks"
alias ikea="cd ~/Development/cs-ikea"
alias iam="cd ~/Development/iam-policies"
alias f="f"
alias mapi="cd ~/Development/lyra-api"
alias wp="~/.wm-scripts/change-wallpaper.sh"
alias zy="sudo zypper"
alias gd="git diff --staged"
alias ssh="kitty +kitten ssh"
export PATH="$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.deno/bin:$HOME/.npm-global/bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
