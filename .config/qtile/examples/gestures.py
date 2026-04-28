import subprocess

from libqtile import hook


@hook.subscribe.startup_once
def autostart_gestures():
    subprocess.Popen(["libinput-gestures-setup", "desktop", "restart", "status"])
