export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export TERMINAL="xterm"
export EDITOR="vim"
export VISUAL="$EDITOR"
export PAGER="less"
export BROWSER="firefox"
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"
export HISTFILE="$XDG_DATA_HOME/history"
export GOPATH="$HOME/.local/go"
export npm_config_prefix="$HOME/.local"
export PKG_CONFIG_PATH="/usr/lib/pkgconfig"

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

pathappend "$HOME/.local/scripts" "$HOME/.local/bin" "$GOPATH/bin" "$HOME/.cargo/bin"
