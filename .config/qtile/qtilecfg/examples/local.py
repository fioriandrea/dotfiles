import os

from libqtile import hook, qtile
from libqtile.backend.wayland import InputConfig
from libqtile.config import Key
from libqtile.lazy import lazy

from .. import base


@hook.subscribe.startup_once
def start_local_services():
    if os.environ.get("WAYLAND_DISPLAY"):
        qtile.spawn("kanshi")
        qtile.spawn("gammastep-indicator")
    else:
        qtile.spawn("redshift-gtk")
    qtile.spawn("nm-applet")
    qtile.spawn("xfce4-power-manager")
    qtile.spawn("xfce4-volumed-pulse")


wl_input_rules = {
    "type:touchpad": InputConfig(
        tap=True,
        dwt=True,
        natural_scroll=True,
        pointer_accel=-0.1,
        scroll_method="two_finger",
        tap_button_map="lrm",
    ),
}

wl_xcursor_theme = "Adwaita"
wl_xcursor_size = 35

zoomer = lazy.spawn("boomer")
if os.environ.get("WAYLAND_DISPLAY"):
    zoomer = lazy.spawn("wooz")

base.keys.extend(
    [
        Key(["mod4"], "z", zoomer, desc="Launch zoomer"),
        Key([], "XF86AudioRaiseVolume", lazy.spawn("myvolume up 5%")),
        Key([], "XF86AudioLowerVolume", lazy.spawn("myvolume down 5%")),
        Key([], "XF86AudioMute", lazy.spawn("myvolume mute")),
        Key([], "XF86AudioMicMute", lazy.spawn("myvolume mic-mute")),
        Key([], "XF86MonBrightnessDown", lazy.spawn("mybrightness down")),
        Key([], "XF86MonBrightnessUp", lazy.spawn("mybrightness up")),
    ]
)
