export FZF_DEFAULT_COMMAND='fd -uu || find'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd -uu --type d || find -type d'

fuzzyfolder="/usr/share/fzf"
fuzzycompletion="$fuzzyfolder/completion.bash"
fuzzykeybindings="$fuzzyfolder/key-bindings.bash"
[ -f "$fuzzycompletion" ] && . "$fuzzycompletion"
[ -f "$fuzzykeybindings" ] && . "$fuzzykeybindings"

