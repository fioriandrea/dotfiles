#!/bin/sh

# https://wiki.archlinux.org/title/Xinit

xmodmap="xmodmap"
xrdb="xrdb"

userxinitdir="$HOME/.config/xinit"
userresources="$HOME/.Xresources"
userresources2="$HOME/.Xresources-2"
usermodmap="$HOME/.Xmodmap"

sysxinitdir="/etc/X11/xinit"
sysresources="$sysxinitdir/.Xresources"
sysmodmap="$sysxinitdir/.Xmodmap"

# merge in defaults and keymaps

merge_resources() {
    if [ -f "$1" ]; then
        if [ -x /usr/bin/cpp ] ; then
            "$xrdb" -merge "$1"
        else
            "$xrdb" -nocpp -merge "$1"
        fi
    fi
}

merge_resources "$sysresources"
if [ -f "$sysmodmap" ]; then
    "$xmodmap" "$sysmodmap"
fi

merge_resources "$userresources"
merge_resources "$userresources2"
if [ -f "$usermodmap" ]; then
    "$xmodmap" "$usermodmap"
fi

# start some nice programs

source_xinitrcd() {
    if [ -d "$1"/xinitrc.d ] ; then
	for f in "$1/xinitrc.d"/?*.sh ; do
	    if [ -f "$f" ] && [ -x "$f" ]; then
                . "$f"
            fi
	done
	unset f
    fi
}

source_xinitrcd "$sysxinitdir"
source_xinitrcd "$userxinitdir"

export _JAVA_AWT_WM_NONREPARENTING=1

exec dbus-launch ssh-agent /usr/bin/emacs --load "$HOME/.config/emacs/my-exwm.el"
