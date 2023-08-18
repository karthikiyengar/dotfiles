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
