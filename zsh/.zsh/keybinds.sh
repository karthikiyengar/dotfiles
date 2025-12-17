# Enable vi (Vim) keybindings for command-line editing
bindkey -v

# --- Home and End keys ---
bindkey $'\e[1~' beginning-of-line  # Home
bindkey $'\e[4~' end-of-line         # End
bindkey $'\eOH' beginning-of-line    # Alternative Home
bindkey $'\eOF' end-of-line          # Alternative End
bindkey "^[[H" beginning-of-line     # Home (consolidated from navigation.sh)
bindkey "^[[F" end-of-line           # End (consolidated from navigation.sh)

# --- Ctrl + Left and Ctrl + Right for moving between words ---
bindkey "^[[1;5C" forward-word       # Ctrl + Right (from navigation.sh)
bindkey "^[[1;5D" backward-word      # Ctrl + Left (from navigation.sh)
bindkey $'\e[1;5D' backward-word     # Alternative Ctrl + Left
bindkey $'\e[1;5C' forward-word      # Alternative Ctrl + Right
bindkey $'\e[5D' backward-word       # Alternative sequence
bindkey $'\e[5C' forward-word        # Alternative sequence

# --- Delete and Kill Word ---
bindkey "^[[3~" delete-char          # Delete key
bindkey $'\e[3~' delete-char         # Alternative Delete
bindkey '^H' backward-kill-word      # Ctrl + Backspace (from navigation.sh)
bindkey '5~' kill-word               # Kill word (from navigation.sh)
