#!/usr/bin/env bash

export _JAVA_AWT_WM_NONREPARENTING=1

xrdb -load "$HOME/.Xresources"

dbus-update-activation-environment --all
exec dbus-launch ssh-agent /usr/bin/emacs --load "$HOME/.config/emacs/my-exwm.el"
