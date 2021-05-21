export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export TERMINAL="alacritty"
export EDITOR="vim"
export VISUAL="vim"
export PAGER="less"
export MANPAGER="manpager-wrapper"
export BROWSER="firefox"
export READER="zathura"
export FILE_MANAGER="nnn-wrapper"
export CODE_EDITOR="code"
export AUDIO_CONTROL="pulsemixer"
export TASK_MANAGER="htop"
# you need to put the wallpaper here
export WALLPAPER_DIR="$XDG_DATA_HOME/wallpapers"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"

export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export HISTFILE="$XDG_DATA_HOME/history"

export GOPATH="$HOME/go"

# handle null terminated strings (i.e. -print0)
export PATH="$PATH:$(find "$HOME/.local/scripts" "$HOME/.local/bin" -type d -print | paste -sd ':')"
