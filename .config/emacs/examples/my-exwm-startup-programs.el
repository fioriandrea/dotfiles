;; -*- lexical-binding: t; -*-

(defun my-exwm-startup-programs ()
  (dolist (cmd '("compton --dbus"
                 "xfce4-power-manager"
                 "xfce4-volumed-pulse"
                 "nm-applet"
                 "redshift-gtk"))
    (call-process-shell-command cmd nil 0 nil)))

(add-hook 'exwm-init-hook 'my-exwm-startup-programs)
