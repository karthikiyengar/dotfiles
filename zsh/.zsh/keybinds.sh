# Enable vi (Vim) keybindings for command-line editing
bindkey -v

# --- Home and End keys ---
bindkey $'\e[1~' beginning-of-line
bindkey $'\e[4~' end-of-line
bindkey $'\eOH' beginning-of-line
bindkey $'\eOF' end-of-line

# --- Ctrl + Left and Ctrl + Right for moving between words ---
bindkey $'\e[1;5D' backward-word
bindkey $'\e[1;5C' forward-word
bindkey $'\e[5D' backward-word # Alternative sequence (if needed)
bindkey $'\e[5C' forward-word  # Alternative sequence (if needed)

# --- Optional Additional Bindings ---
bindkey $'\e[3~' delete-char # Delete key
