#!/bin/sh

pgrep xfce4-volumed-pulse || xfce4-volumed-pulse &
pgrep xfce4-power-manager || xfce4-power-manager &
