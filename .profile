export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export TERMINAL="xterm"
export EDITOR="vim"
export VISUAL="$EDITOR"
export PAGER="less"
export BROWSER="chromium"
export GUI_EDITOR="code"
export SCRIPTS_DIR="$HOME/.local/scripts"
export WALLPAPER_DIR="$XDG_DATA_HOME/wallpapers"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export HISTFILE="$XDG_DATA_HOME/history"
export GOPATH="$HOME/.local/go"
export npm_config_prefix="$HOME/.local"

pathappend() {
    PATH=$(
        for arg in "$@"; do
            test -d "${arg}" || continue
            case :$PATH: in
                *":${arg}:"*) continue ;;
            esac
            PATH="${arg}:$PATH"
        done
        printf '%s' "$PATH"
    )
    export PATH
}

recursive_pathappend() {
    PATH=$(
        find "$@" -type d -print 2>/dev/null | {
            while read -r dir; do
                pathappend "$dir"
            done
            printf '%s' "$PATH"
        }
    )
    export PATH
}

recursive_pathappend "$SCRIPTS_DIR" "$HOME/.local/bin" "$GOPATH/bin"
