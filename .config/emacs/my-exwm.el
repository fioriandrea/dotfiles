;;; my-exwm.el -*- lexical-binding: t; -*-

(require 'exwm)
(require 'exwm-input)
(require 'exwm-systemtray)
(require 'exwm-workspace)

(defvar my-workspace-number 4)

(defvar my-exwm-path-command-history nil)

(setopt exwm-workspace-number my-workspace-number
        exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))

(display-time-mode 1)
(display-battery-mode 1)

(defun my-trimmed-shell-command-to-string (cmd)
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

(defun my-exwm-shell-fn (cmd)
  (lambda ()
    (interactive)
    (message "%s" (my-trimmed-shell-command-to-string cmd))))

(defun my-exwm-start-systemtray ()
  (exwm-systemtray-mode 1))

(defun my-exwm-fix-initial-workspace-glitch ()
  (run-at-time
   0.2 nil
   (lambda ()
     (when (> exwm-workspace-number 1)
       (exwm-workspace-switch-create 1)
       (exwm-workspace-switch-create 0)))))

(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

(add-hook 'exwm-init-hook #'my-exwm-fix-initial-workspace-glitch)
;; (add-hook 'exwm-init-hook #'my-exwm-start-systemtray)

(add-to-list 'exwm-input-prefix-keys ?\C-g)

(setopt exwm-input-global-keys
        `(([?\s-j] . exwm-reset)
          ([?\s-s] . exwm-workspace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-x] . my-exwm-run-from-path)

          ([XF86MonBrightnessUp] . ,(my-exwm-shell-fn "mybrightness up"))
          ([XF86MonBrightnessDown] . ,(my-exwm-shell-fn "mybrightness down"))

          ([XF86AudioRaiseVolume] . ,(my-exwm-shell-fn "myvolume up"))
          ([XF86AudioLowerVolume] . ,(my-exwm-shell-fn "myvolume down"))
          ([s-XF86AudioRaiseVolume] . ,(my-exwm-shell-fn "myvolume mic-up"))
          ([s-XF86AudioLowerVolume] . ,(my-exwm-shell-fn "myvolume mic-down"))
          ([XF86AudioMute] . ,(my-exwm-shell-fn "myvolume mute"))
          ([XF86AudioMicMute] . ,(my-exwm-shell-fn "myvolume mic-mute"))))

(exwm-wm-mode)

(provide 'my-exwm)
;;; my-exwm.el ends here
