# Setup fzf
# ---------
if [[ ! "$PATH" == */home/kiyengar/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/kiyengar/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/kiyengar/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/kiyengar/.fzf/shell/key-bindings.bash"
