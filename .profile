if [ -f /etc/profile ]; then
    . /etc/profile
fi

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export XINITRC="$XDG_CONFIG_HOME"/X11/xinitrc
export XSERVERRC="$XDG_CONFIG_HOME"/X11/xserverrc
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

pathuniq() {
    awk -v p="${1:-$PATH}" '
BEGIN {
    n = split(p, fields, ":")
    set[""] = 1
    for (i = 1; i <= n; i++) {
        f = fields[i]
        if (f in set) {
           continue
        }
        set[f] = 1
        printf("%s%s", delim, f)
        delim = ":"
    }
}'
}

pathappend() {
    for arg in "$@"; do
        test -d "$arg" || continue
        PATH="$arg:$PATH"
    done
    PATH=$(pathuniq "$PATH")
    export PATH
}

pathappend "$HOME/.local/scripts" "$HOME/.local/bin" "$HOME/.local/opt" "$GOPATH/bin" "$HOME/.cargo/bin"
