;;; my-exwm.el -*- lexical-binding: t; -*-

(require 'exwm)
(require 'exwm-input)
(require 'exwm-systemtray)
(require 'exwm-workspace)

(defvar my-brightness-step "1%")
(defvar my-volume-step "2%")

(defvar my-workspace-number 4)

(defvar my-exwm-path-command-history nil)

(setopt exwm-workspace-number my-workspace-number
        exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))

(display-time-mode 1)
(display-battery-mode 1)

(defun my-exwm-run-sync (cmd)
  (call-process-shell-command cmd))

(defun my-exwm-command-output (cmd)
  (string-trim (shell-command-to-string cmd)))

(defun my-exwm-path-executables ()
  (let ((seen (make-hash-table :test #'equal))
        executables)
    (dolist (dir (parse-colon-path (or (getenv "PATH") "")))
      (when (file-directory-p dir)
        (dolist (entry (directory-files dir nil directory-files-no-dot-files-regexp))
          (let ((path (expand-file-name entry dir)))
            (when (and (not (file-directory-p path))
                       (file-executable-p path)
                       (not (gethash entry seen)))
              (puthash entry t seen)
              (push entry executables))))))
    (sort executables #'string-lessp)))

(defun my-exwm-run-from-path (command)
  (interactive
   (list
    (completing-read "Run: "
                     (my-exwm-path-executables)
                     nil t nil 'my-exwm-path-command-history)))
  (start-process command nil command))

(defun my-exwm-wpctl-percent (target)
  (let ((output (my-exwm-command-output
                 (format "wpctl get-volume %s" target))))
    (when (string-match "Volume: +\\([0-9.]+\\)" output)
      (round (* 100 (string-to-number (match-string 1 output)))))))

(defun my-exwm-wpctl-muted-p (target)
  (string-match-p "\\[MUTED\\]"
                  (my-exwm-command-output
                   (format "wpctl get-volume %s" target))))

(defun my-exwm-brightness-percent ()
  (my-exwm-command-output "brightnessctl -m | cut -d, -f4"))

(defvar my-workspace-indicator "")

(defun my-exwm-update-workspace-indicator ()
  (setq my-workspace-indicator
        (format " WS:%d " (1+ exwm-workspace-current-index)))
  (force-mode-line-update t))

(defun my-exwm-workspace-switch ()
  (interactive)
  (call-interactively #'exwm-workspace-switch))

(defun my-exwm-workspace-switch-create (index)
  (interactive "nWorkspace: ")
  (exwm-workspace-switch-create index))

(defun my-exwm-workspace-move-window (index)
  (interactive "nMove window to workspace: ")
  (exwm-workspace-move-window index)
  (message "Moved window to workspace: %d" (1+ index)))

(defun my-exwm-format-audio-status (label target)
  (let ((percent (my-exwm-wpctl-percent target))
        (muted (my-exwm-wpctl-muted-p target)))
    (if percent
        (if muted
            (format "%s: %d%% muted" label percent)
          (format "%s: %d%%" label percent))
      (format "%s status unavailable" label))))

(defun my-exwm-volume-status ()
  (my-exwm-format-audio-status "Volume" "@DEFAULT_AUDIO_SINK@"))

(defun my-exwm-mic-status ()
  (my-exwm-format-audio-status "Microphone" "@DEFAULT_AUDIO_SOURCE@"))

(defun my-exwm-start-systemtray ()
  (exwm-systemtray-mode 1))

(defun my-exwm-fix-initial-workspace-glitch ()
  (run-at-time
   0.2 nil
   (lambda ()
     (when (> exwm-workspace-number 1)
       (exwm-workspace-switch-create 1)
       (exwm-workspace-switch-create 0)))))

(defun my-exwm-brightness-up ()
  (interactive)
  (my-exwm-run-sync (format "brightnessctl set +%s" my-brightness-step))
  (message "Brightness: %s" (my-exwm-brightness-percent)))

(defun my-exwm-brightness-down ()
  (interactive)
  (my-exwm-run-sync (format "brightnessctl set %s-" my-brightness-step))
  (message "Brightness: %s" (my-exwm-brightness-percent)))

(defun my-exwm-volume-up ()
  (interactive)
  (my-exwm-run-sync (format "wpctl set-volume @DEFAULT_AUDIO_SINK@ %s+"
                            my-volume-step))
  (message "%s" (my-exwm-volume-status)))

(defun my-exwm-volume-down ()
  (interactive)
  (my-exwm-run-sync (format "wpctl set-volume @DEFAULT_AUDIO_SINK@ %s-"
                            my-volume-step))
  (message "%s" (my-exwm-volume-status)))

(defun my-exwm-mic-volume-up ()
  (interactive)
  (my-exwm-run-sync (format "wpctl set-volume @DEFAULT_AUDIO_SOURCE@ %s+"
                            my-volume-step))
  (message "%s" (my-exwm-mic-status)))

(defun my-exwm-mic-volume-down ()
  (interactive)
  (my-exwm-run-sync (format "wpctl set-volume @DEFAULT_AUDIO_SOURCE@ %s-"
                            my-volume-step))
  (message "%s" (my-exwm-mic-status)))

(defun my-exwm-volume-mute ()
  (interactive)
  (my-exwm-run-sync "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
  (message "%s" (my-exwm-volume-status)))

(defun my-exwm-mic-mute ()
  (interactive)
  (my-exwm-run-sync "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle")
  (message "%s" (my-exwm-mic-status)))

(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(add-hook 'exwm-init-hook #'my-exwm-fix-initial-workspace-glitch)
;; (add-hook 'exwm-init-hook #'my-exwm-start-systemtray)
(add-hook 'exwm-init-hook #'my-exwm-update-workspace-indicator)
(add-hook 'exwm-workspace-switch-hook #'my-exwm-update-workspace-indicator)

(add-to-list 'exwm-input-prefix-keys ?\C-g)
(add-to-list 'global-mode-string '(:eval my-workspace-indicator) t)

(setopt exwm-input-global-keys
      `(
        ([?\s-r] . exwm-reset)
        ([?\s-s] . my-exwm-workspace-switch)
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ([?\s-m] . my-exwm-run-from-path)

        ([XF86MonBrightnessUp] . my-exwm-brightness-up)
        ([XF86MonBrightnessDown] . my-exwm-brightness-down)

        ([XF86AudioRaiseVolume] . my-exwm-volume-up)
        ([XF86AudioLowerVolume] . my-exwm-volume-down)
        ([s-XF86AudioRaiseVolume] . my-exwm-mic-volume-up)
        ([s-XF86AudioLowerVolume] . my-exwm-mic-volume-down)
        ([XF86AudioMute] . my-exwm-volume-mute)
        ([XF86AudioMicMute] . my-exwm-mic-mute)))

(exwm-wm-mode)

(provide 'my-exwm)
;;; my-exwm.el ends here
