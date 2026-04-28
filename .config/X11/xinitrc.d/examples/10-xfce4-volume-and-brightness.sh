#!/bin/sh

pgrep -x xfce4-volumed-pulse || xfce4-volumed-pulse &
pgrep -x xfce4-power-manager || xfce4-power-manager &
