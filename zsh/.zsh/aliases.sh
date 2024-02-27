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

alias u="(sudo apt update && sudo apt -y upgrade && sudo apt -y autoremove); sudo flatpak update -y"
alias ls="LC_COLLATE=C lsd -la"
alias vim="nvim"
alias grep='grep --color'
alias neovide="~/Applications/neovide_44930d00ac7f37a4e1562650f7bf1143.appimage"
