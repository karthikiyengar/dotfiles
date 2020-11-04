# Use Antigen to manage packages
source ~/.antigen.zsh

export NVM_AUTO_USE=true

antigen use oh-my-zsh
antigen bundle zsh-users/zsh-autosuggestions
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
alias vn="sudo -e /etc/nixos/configuration.nix"
alias vz="vim ~/.zshrc"
alias sz="source ~/.zshrc"
alias dev="cd ~/development"
alias ga="git add --all"
alias gc="git commit"
alias app="cd ~/development/klapp/klarna-app"
alias appc="cd ~/development/klapp/klarna-app/clients"
alias appe="cd ~/development/klapp/klarna-app/clients/apps/extension"
alias appw="cd ~/development/klapp/klarna-app/websites"
alias appme="cd ~/development/klapp/master/clients/apps/extension"
alias appmw="cd ~/development/klapp/master/websites"
alias appmc="cd ~/development/klapp/master/clients"
alias appm="cd ~/development/klapp/master"
alias whoami="echo k.iyengar"
alias vpn="cd ~/VPN && sudo openvpn --config k.iyengar@laptop.ovpn"
alias r="sudo nixos-rebuild switch"
alias nclean="find . -name "node_modules" -exec rm -rf '{}' +"
alias adfs="sh ~/adfs.sh"
alias u="sudo apt-get update; sudo apt-get upgrade"
alias un="sudo nix-channel --update; r"
alias crocks="cd ~/development/crocks"
alias ikea="cd ~/development/cs-ikea"
alias iam="cd ~/development/iam-policies"
alias mapi="cd ~/development/lyra-api"
alias wp="~/.wm-scripts/change-wallpaper.sh"
alias zy="sudo zypper"
alias gd="git diff --staged"
alias vx="code ~/.xmonad/"
export PATH="$HOME/.android/sdk/emulator:$HOME/.android/sdk/platform-tools:$HOME/.android/sdk/tools/bin:$HOME/.cabal/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.deno/bin:$HOME/.npm-global/bin:$PATH"

bindkey -v
if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

export JAVA_HOME="$HOME/.nix-profile/lib/openjdk"
export JRE_HOME="$HOME/.nix-profile/lib/openjdk/jre"
export ANDROID_HOME="$HOME/.android/sdk"
export ANDROID_SDK_ROOT="$HOME/.android/sdk"
export ANDROID_AVD_HOME="$HOME/.android/avd"
export GRADLE_USER_HOME="$HOME/.gradle"
export M2_HOME="$HOME/.maven"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/kiyengar/.sdkman"
[[ -s "/home/kiyengar/.sdkman/bin/sdkman-init.sh" ]] && source "/home/kiyengar/.sdkman/bin/sdkman-init.sh"


# Enable thefuck
eval $(thefuck --alias f)

# Enable direnv
eval "$(direnv hook $SHELL)"
if [ -e /home/kiyengar/.nix-profile/etc/profile.d/nix.sh ]; then . /home/kiyengar/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export LOCALE_ARCHIVE_2_11="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
export LOCALE_ARCHIVE_2_27="$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive"
export LOCALE_ARCHIVE="/usr/bin/locale"
