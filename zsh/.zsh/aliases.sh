# Git
alias gst="if command -v gitui &> /dev/null; then gitui; else git status; fi"
alias gco="git checkout"
alias gcb="git checkout -b"
alias ga="git add --all"
alias gc="git commit"
alias gd="git diff"
alias gds="git diff --staged"

# Zsh
alias sz="source ~/.zshrc"
alias vz="vim ~/.dotfiles"

# dev
alias n="neovide ."
alias t="timew"
alias neovide="~/Applications/neovide_d8a7398de6f98b341232f4cfe1bdbe23.appimage"

alias u="(sudo tuxedo-tomte configure all && sudo apt update && sudo apt -y upgrade && sudo apt -y autoremove); sudo flatpak update -y"
alias ls="LC_COLLATE=C lsd -la"
alias vim="nvim"
alias grep='grep --color'
