;; -*- lexical-binding: t; -*-

(defun my-exwm-startup-programs ()
  (dolist (cmd '("compton --dbus"
               "xfce4-power-manager"
               "xfce4-volumed-pulse"
               "nm-applet"
               "redshift-gtk"))
    (start-process-shell-command cmd nil cmd)))

(add-hook 'exwm-init-hook 'my-exwm-startup-programs)
