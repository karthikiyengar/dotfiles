# Git
alias gst="if command -v lazygit &> /dev/null; then lazygit; else git status; fi"
alias gco="git checkout"
alias gcb="git checkout -b"
alias ga="git add --all"
alias gc="git commit"
alias gd="git diff"
alias gds="git diff --staged"

# Zsh
alias sz="source ${ZSH_CONFIG_DIR:-$HOME/.zsh}/../.zshrc"
alias vz="vim ~/.dotfiles"

# dev
alias t="timew"

# System update (macOS using Homebrew)
alias u="brew update && brew upgrade && brew cleanup"

# Modern tool replacements
alias ls="eza -la --group-directories-first --icons"
alias ll="eza -l --group-directories-first --icons"
alias lt="eza --tree --level=2 --icons -a"
alias vim="nvim"
alias cat="bat --style=plain --paging=never"
alias less="bat --style=full --paging=always"
alias grep='grep --color'
