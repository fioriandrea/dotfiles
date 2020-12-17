export TERMINAL="termite"
export EDITOR="vim"
export VISUAL="vim"
export PAGER="less"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export BROWSER="firefox"
export READER="zathura"
export FILE_MANAGER="nnn-wrapper"
export CODE_EDITOR="code"
export AUDIO_CONTROL="pulsemixer"
export TASK_MANAGER="htop"
# you need to put the wallpaper here
export WALLPAPER_DIR="$HOME/.local/share/wallpapers"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"
export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/history"

# handle null terminated strings (i.e. -print0)
export PATH="$PATH:$(find "$HOME/.local/scripts" -type d -print | paste -sd ':')"

export QT_QPA_PLATFORMTHEME="gtk2"
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
