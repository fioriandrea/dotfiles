import os
import shutil
import subprocess
from pathlib import Path

from libqtile import bar, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal


mod = "mod4"
terminal = guess_terminal()
font = "monospace Bold"


def get_x_scale():
    if (
        os.environ.get("WAYLAND_DISPLAY")
        or not os.environ.get("DISPLAY")
        or shutil.which("xrdb") is None
    ):
        return 1.0
    try:
        output = subprocess.check_output(["xrdb", "-query"], text=True)
    except (OSError, subprocess.SubprocessError):
        return 1.0
    for line in output.splitlines():
        if line.lower().startswith("xft.dpi:"):
            try:
                dpi = float(line.split(":", 1)[1].strip())
            except ValueError:
                return 1.0
            return max(1.0, dpi / 96.0)
    return 1.0


def scaled(value, scale):
    return max(1, round(value * scale))


x_scale = get_x_scale()
bar_height = scaled(25, x_scale)
bar_fontsize = scaled(15, x_scale)
bar_padding = scaled(2, x_scale)
tray_icon_size = scaled(22, x_scale)


def has_command(command):
    return shutil.which(command) is not None


if os.environ.get("WAYLAND_DISPLAY"):
    menu = "wmenu-run" if has_command("wmenu-run") else "dmenu_run -i"
    menu_pipe = "wmenu -i"
else:
    menu = "dmenu_run -i"
    menu_pipe = "dmenu -i"

if has_command("j4-dmenu-desktop"):
    menu_desktop = f"j4-dmenu-desktop --dmenu='{menu_pipe}'"
elif has_command("i3-dmenu-desktop"):
    menu_desktop = f"i3-dmenu-desktop --dmenu='{menu_pipe}'"
else:
    menu_desktop = menu

colors = {
    "bg": "#222222",
    "fg": "#ffffff",
    "dim": "#888888",
    "border": "#000000",
    "blue": "#285577",
    "focus": "#ff6b6b",
    "focus_stack": "#b13f3f",
    "red": "#ff0000",
}


@lazy.function
def move_window_to_previous_group(qtile):
    if qtile.current_window is None:
        return
    target = qtile.current_group.get_previous_group()
    qtile.current_window.togroup(target.name, switch_group=True)


@lazy.function
def move_window_to_next_group(qtile):
    if qtile.current_window is None:
        return
    target = qtile.current_group.get_next_group()
    qtile.current_window.togroup(target.name, switch_group=True)


@lazy.function
def move_window_to_next_screen(qtile):
    if qtile.current_window is None or len(qtile.screens) < 2:
        return
    target = qtile.current_screen.index + 1
    if target >= len(qtile.screens):
        target = 0
    screen = qtile.screens[target]
    win = qtile.current_window
    win.togroup(screen.group.name)
    qtile.focus_screen(screen.index)
    screen.group.focus(win, True)


def directional_keys(modifiers, commands_by_direction, desc_template="{direction}"):
    direction_aliases = {
        "left": ["h", "Left"],
        "down": ["j", "Down"],
        "up": ["k", "Up"],
        "right": ["l", "Right"],
    }
    keys = []
    for direction, command in commands_by_direction.items():
        key_names = direction_aliases[direction]
        for key_name in key_names:
            keys.append(
                Key(
                    modifiers,
                    key_name,
                    command,
                    desc=desc_template.format(direction=direction, key=key_name),
                )
            )
    return keys


keys = [
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "d", lazy.spawn(menu_desktop), desc="Launch desktop menu"),
    Key([mod, "shift"], "d", lazy.spawn(menu), desc="Launch menu"),
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),
    *directional_keys(
        [mod],
        {
            "left": lazy.layout.left(),
            "down": lazy.layout.down(),
            "up": lazy.layout.up(),
            "right": lazy.layout.right(),
        },
        desc_template="Focus {direction}",
    ),
    *directional_keys(
        [mod, "shift"],
        {
            "left": lazy.layout.shuffle_left(),
            "down": lazy.layout.shuffle_down(),
            "up": lazy.layout.shuffle_up(),
            "right": lazy.layout.shuffle_right(),
        },
        desc_template="Move window {direction}",
    ),
    Key([mod], "n", lazy.layout.down(), desc="Focus down"),
    Key([mod], "p", lazy.layout.up(), desc="Focus up"),
    Key([mod, "shift"], "n", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "p", lazy.layout.shuffle_up(), desc="Move window up"),
    Key([mod], "Page_Down", lazy.screen.next_group(), desc="Next workspace"),
    Key([mod], "Page_Up", lazy.screen.prev_group(), desc="Previous workspace"),
    Key(
        [mod, "shift"],
        "Page_Down",
        move_window_to_next_group(),
        desc="Move window to next workspace",
    ),
    Key(
        [mod, "shift"],
        "Page_Up",
        move_window_to_previous_group(),
        desc="Move window to previous workspace",
    ),
    Key([mod], "w", lazy.group.setlayout("max"), desc="Max layout"),
    Key([mod], "e", lazy.group.setlayout("columns"), desc="Columns layout"),
    Key([mod], "m", lazy.layout.maximize(), desc="Maximize window in layout"),
    Key(
        [mod, "control", "shift"],
        "m",
        lazy.group.unminimize_all(),
        desc="Unminimize all windows in group",
    ),
    Key([mod], "r", lazy.layout.normalize(), desc="Normalize layout"),
    Key(
        [mod],
        "backslash",
        lazy.layout.flip().when(layout=["monadtall", "monadwide"]),
        lazy.layout.swap_column_right().when(layout=["columns"]),
        desc="Flip current layout",
    ),
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc="Toggle fullscreen"),
    Key([mod], "Tab", lazy.next_layout(), desc="Next layout"),
    Key([mod, "shift"], "Tab", lazy.prev_layout(), desc="Previous layout"),
    Key([mod, "shift"], "space", lazy.window.toggle_floating(), desc="Toggle floating"),
    Key([mod], "space", lazy.layout.next(), desc="Cycle focus in layout"),
    Key([mod], "u", lazy.next_screen(), desc="Focus next screen"),
    Key([mod], "o", move_window_to_next_screen(), desc="Move window to next screen"),
    Key([mod, "shift"], "r", lazy.restart(), desc="Restart Qtile"),
    Key(
        [mod],
        "equal",
        lazy.layout.grow_left().when(layout=["columns"]),
        lazy.layout.grow().when(layout=["monadtall", "monadwide"]),
        lazy.layout.increase_ratio().when(layout=["tile"]),
        desc="Grow in current layout",
    ),
    Key(
        [mod],
        "minus",
        lazy.layout.grow_right().when(layout=["columns"]),
        lazy.layout.shrink().when(layout=["monadtall", "monadwide"]),
        lazy.layout.decrease_ratio().when(layout=["tile"]),
        desc="Shrink in current layout",
    ),
    *directional_keys(
        [mod, "control"],
        {
            "left": lazy.layout.grow_left(),
            "down": lazy.layout.grow_down(),
            "up": lazy.layout.grow_up(),
            "right": lazy.layout.grow_right(),
        },
        desc_template="Grow window {direction}",
    ),
    Key([mod, "shift"], "c", lazy.reload_config(), desc="Reload config"),
    KeyChord(
        [mod, "shift"],
        "e",
        [
            Key([], "Return", lazy.shutdown(), desc="Exit Qtile"),
            Key([], "Escape", lazy.ungrab_chord(), desc="Cancel"),
        ],
        mode=False,
        name="Exit? [Return] yes, [Escape] no",
        desc="Exit mode",
    ),
]

for vt in range(1, 8):
    keys.append(
        Key(
            ["control", "mod1"],
            f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc=f"Switch to group {i.name}",
            ),
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc=f"Switch to & move focused window to group {i.name}",
            ),
        ]
    )


layout_theme = {
    "border_width": scaled(5, x_scale),
    "border_focus": colors["focus"],
    "border_normal": colors["border"],
    "margin": 0,
}

layouts = [
    layout.Columns(
        **layout_theme,
        border_focus_stack=[colors["focus"], colors["focus_stack"]],
        num_columns=1,
        insert_position=1,
    ),
    layout.MonadTall(**layout_theme),
    layout.MonadWide(**layout_theme),
    layout.Max(**layout_theme),
    # layout.Tile(**layout_theme),
    # layout.Stack(**layout_theme, num_stacks=2),
    # layout.Bsp(**layout_theme),
    # layout.Floating(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.TreeTab(**layout_theme),
    # layout.VerticalTile(**layout_theme),
    # layout.Zoomy(**layout_theme),
]

widget_defaults = {
    "font": font,
    "fontsize": bar_fontsize,
    "padding": bar_padding,
}
extension_defaults = widget_defaults.copy()


def has_battery():
    try:
        return any(
            name.startswith("BAT") for name in os.listdir("/sys/class/power_supply")
        )
    except FileNotFoundError:
        return False


def tray_widget():
    if os.environ.get("WAYLAND_DISPLAY"):
        return widget.StatusNotifier(icon_size=tray_icon_size, padding=8)
    else:
        return widget.Systray(icon_size=tray_icon_size, padding=8)


def build_widgets(include_tray):
    widgets = [
        widget.GroupBox(
            highlight_method="block",
            block_highlight_text_color=colors["fg"],
            this_current_screen_border=colors["blue"],
            this_screen_border=colors["blue"],
            other_current_screen_border=colors["dim"],
            other_screen_border=colors["dim"],
            active=colors["fg"],
            inactive=colors["dim"],
            urgent_alert_method="block",
            urgent_border=colors["red"],
            urgent_text=colors["fg"],
            rounded=False,
            disable_drag=True,
            use_mouse_wheel=False,
            toggle=False,
            padding_x=3,
            padding_y=3,
            margin_y=3,
            borderwidth=2,
        ),
        widget.Chord(
            chords_colors={
                "Exit? [Return] yes, [Escape] no": (colors["red"], colors["fg"]),
            },
            name_transform=lambda name: name,
            padding=7,
        ),
        widget.CurrentLayout(
            foreground=colors["fg"],
            padding=8,
        ),
        widget.Prompt(
            foreground=colors["fg"],
        ),
        widget.Spacer(),
    ]
    if has_battery():
        widgets.append(
            widget.Battery(
                format="{char} {percent:2.0%}",
                foreground=colors["fg"],
                padding=10,
            )
        )
    widgets.append(
        widget.Clock(
            format="%a %Y-%m-%d %H:%M",
            foreground=colors["fg"],
            padding=10,
        )
    )
    if include_tray:
        tray = tray_widget()
        if tray is not None:
            widgets.append(tray)
    return widgets


def build_screen(include_tray):
    return Screen(
        bottom=bar.Bar(build_widgets(include_tray), bar_height, background=colors["bg"])
    )


screens = [build_screen(include_tray=True)]
generate_screens = None
fake_screens = None

mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    border_width=5,
    border_focus=colors["focus"],
    border_normal=colors["border"],
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),
        Match(wm_class="makebranch"),
        Match(wm_class="maketag"),
        Match(wm_class="ssh-askpass"),
        Match(title="branchdialog"),
        Match(title="pinentry"),
    ],
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = False
wl_input_rules = None
wl_xcursor_theme = None
wl_xcursor_size = 24
idle_timers = []
idle_inhibitors = []
wmname = "LG3D"
