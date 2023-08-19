export VISUAL=nvim
export EDITOR="$VISUAL"

setopt autocd

bindkey "^[[1;5C" forward-word # Ctrl + Right
bindkey "^[[1;5D" backward-word # Ctrl + Left
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line
bindkey  "^[[3~"  delete-char
bindkey '^H' backward-kill-word
bindkey '5~' kill-word
