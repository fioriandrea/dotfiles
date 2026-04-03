#!/bin/sh

pgrep -x xfce4-panel || xfce4-panel &
pgrep -x xfce4-power-manager || xfce4-power-manager &
pgrep -x xfsettingsd || xfsettingsd &
# pgrep -x xfdesktop || xfdesktop &
