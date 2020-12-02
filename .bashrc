#
# ~/.bashrc
#

#Ibus settings if you need them
#type ibus-setup in terminal to change settings and start the daemon
#delete the hashtags of the next lines and restart
#export GTK_IM_MODULE=ibus
#export XMODIFIERS=@im=dbus
#export QT_IM_MODULE=ibus

# If not running interactively, don't do anything
[[ $- != *i* ]] && return



bashConfigDir="${XDG_CONFIG_HOME:-$HOME/.config}/bash-config"

#source configuration files
for confFile in "$bashConfigDir"/*
do
    . "$confFile"
done
