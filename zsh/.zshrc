source ~/.zsh/index.sh

[ -f "$HOME/.atuin/bin/env" ] && . "$HOME/.atuin/bin/env"

eval "$(atuin init zsh)"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"