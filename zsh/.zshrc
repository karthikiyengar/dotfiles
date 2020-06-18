# Use Antigen to manage packages
source ~/.antigen.zsh

antigen use oh-my-zsh
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle git
antigen bundle hoffi/zsh-theme-lambda
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle akoenig/npm-run.plugin.ZSH
antigen apply

export ZSH="$HOME/.oh-my-zsh"
source $ZSH/oh-my-zsh.sh

export EDITOR='nvim'

[[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'

HISTSIZE=999999999

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

alias vim="nvim"
alias ls="ls -lah --color"
alias vz="vim ~/.zshrc"
alias sz="source ~/.zshrc"
alias dev="cd ~/development"
alias ga="git add --all"
alias gc="git commit"
alias app="cd ~/development/klarna-app"
alias whoami="echo k.iyengar"
alias vpn="cd ~/VPN && sudo openvpn --config k.iyengar@laptop.ovpn"
alias r="ranger"
alias nclean="find . -name "node_modules" -exec rm -rf '{}' +"
alias adfs="sh ~/adfs.sh"
alias u="sudo apt-get update; sudo apt-get upgrade"
alias crocks="cd ~/development/crocks"
alias ikea="cd ~/development/cs-ikea"
alias iam="cd ~/development/iam-policies"
alias mapi="cd ~/development/lyra-api"
alias wp="~/.wm-scripts/change-wallpaper.sh"
alias zy="sudo zypper"
alias gd="git diff --staged"
alias ssh="kitty +kitten ssh"
alias vx="code ~/.xmonad/"
export PATH="$HOME/android-sdk/platform-tools:$HOME/android-sdk/tools/bin:$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.deno/bin:$HOME/.npm-global/bin:$PATH"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


export JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"
export JRE_HOME="/usr/lib/jvm/java-8-openjdk-amd64/jre"
export ANDROID_HOME="$HOME/android-sdk"
export ANDROID_SDK_ROOT="$HOME/android-sdk"
export GRADLE_USER_HOME="$HOME/.gradle"
export M2_HOME="$HOME/.maven"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/kiyengar/.sdkman"
[[ -s "/home/kiyengar/.sdkman/bin/sdkman-init.sh" ]] && source "/home/kiyengar/.sdkman/bin/sdkman-init.sh"


# Enable thefuck
eval $(thefuck --alias f)

# Enable direnv
eval "$(direnv hook $SHELL)"
