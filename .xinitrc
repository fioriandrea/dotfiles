#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

. "$HOME/.xprofile"

exec openbox-session
