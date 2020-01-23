# Use Antigen to manage packages
source ./.antigen.zsh

antigen use oh-my-zsh
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle git
antigen apply

export ZSH="/home/kiyengar/.oh-my-zsh"
ZSH_THEME="robbyrussell"
source $ZSH/oh-my-zsh.sh

export EDITOR='nvim'

[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

HISTSIZE=999999999

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

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
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.deno/bin:$HOME/.npm-global/bin:$PATH"
