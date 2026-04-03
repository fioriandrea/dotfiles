;; -*- lexical-binding: t; -*-

(use-package desktop-environment
  :after exwm
  :demand t
  :vc (:url "https://github.com/DamienCassou/desktop-environment"
            :rev :newest)
  :config (desktop-environment-mode 1)
  :custom
  (desktop-environment-update-exwm-global-keys :prefix)
  (desktop-environment-brightness-small-increment "30+")
  (desktop-environment-brightness-small-decrement "30-")
  (desktop-environment-brightness-normal-increment "1%+")
  (desktop-environment-brightness-normal-decrement "1%-")
  (desktop-environment-volume-small-increment "1%+")
  (desktop-environment-volume-small-decrement "1%-")
  (desktop-environment-volume-normal-increment "2%+")
  (desktop-environment-volume-normal-decrement "2%-"))
