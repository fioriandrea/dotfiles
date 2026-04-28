#!/bin/sh

pgrep xfce4-panel || xfce4-panel &
# pgrep xfce4-volumed-pulse || xfce4-volumed-pulse &
pgrep xfce4-power-manager || xfce4-power-manager &
pgrep xfsettingsd || xfsettingsd &
pgrep xfce4-screensaver || xfce4-screensaver &
# pgrep xfdesktop || xfdesktop &
