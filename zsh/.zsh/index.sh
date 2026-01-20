# Central configuration directory - modify here to relocate all zsh configs
ZSH_CONFIG_DIR="${HOME}/.zsh"

# Load order: prompt → plugins → history → dev → aliases → functions → completion → navigation → path → keybinds
source ${ZSH_CONFIG_DIR}/prompt.sh
source ${ZSH_CONFIG_DIR}/plugins.sh
source ${ZSH_CONFIG_DIR}/history.sh
source ${ZSH_CONFIG_DIR}/dev.sh
source ${ZSH_CONFIG_DIR}/aliases.sh
[ -f ${ZSH_CONFIG_DIR}/aliases-private.sh ] && source ${ZSH_CONFIG_DIR}/aliases-private.sh
source ${ZSH_CONFIG_DIR}/functions.sh
source ${ZSH_CONFIG_DIR}/completion.sh
source ${ZSH_CONFIG_DIR}/navigation.sh
source ${ZSH_CONFIG_DIR}/path.sh
source ${ZSH_CONFIG_DIR}/keybinds.sh
source ${ZSH_CONFIG_DIR}/tmux.sh