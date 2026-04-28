;; -*- lexical-binding: t; -*-

(setopt tab-bar-format (seq-uniq (append tab-bar-format
                                         '(tab-bar-format-align-right
                                           tab-bar-format-global))))
(setopt tab-bar-show t)
(tab-bar-mode 1)

(display-battery-mode 1)

(setopt display-time-day-and-date t)
(setopt display-time-24hr-format t)

(display-time-mode 1)
