# config for programs needing environmental variables as configuration

xdg_config_home=${XDG_CONFIG_HOME:-$HOME/.config}

fzfconfig="fzf/fzf-config.sh"
[[ -f $xdg_config_home/$fzfconfig ]] && . "$xdg_config_home/$fzfconfig"

[[ -f $INPUTRC ]] && bind -f "$INPUTRC"
