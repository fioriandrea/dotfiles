# if not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ -r $HOME/.profile ]] && source "$HOME/.profile"

shopt -s autocd
shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s extglob
shopt -s globstar
shopt -s histappend
shopt -s expand_aliases

PS1='[\u@\h \W]\$ '

unalias -a
HISTCONTROL=ignoreboth:erasedups
xdg_config_home=${XDG_CONFIG_HOME:-$HOME/.config}
# CDPATH=.:~:$xdg_config_home:$HOME/.local

# additional files to be sourced
bashconfdir=$xdg_config_home/bash/
[[ -r $bashconfdir ]] || return
for conffile in "$bashconfdir"/*; do
    [[ -f $conffile ]] || continue
    source "$conffile"
done
