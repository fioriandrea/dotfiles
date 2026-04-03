;;; my-exwm.el -*- lexical-binding: t; -*-

(require 'exwm)
(require 'exwm-input)
(require 'exwm-systemtray)
(require 'exwm-workspace)

(defvar my-exwm-brightness-step 0.01)
(defvar my-exwm-volume-step 0.02)
(defvar my-exwm-mic-step 0.02)

(defvar my-exwm-path-command-history nil)

(setopt exwm-workspace-number 4
        exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))

(display-time-mode 1)
(display-battery-mode 1)


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

(defun my-exwm-shell-run (cmd &rest args)
  (let* ((args (mapcar (lambda (x)
                         (if (numberp x) (number-to-string x) x))
                       args))
         (quoted-args (mapcar #'shell-quote-argument args))
         (command-line (cons cmd quoted-args))
         (final-command (string-join command-line " "))
         (result (string-trim
                  (shell-command-to-string final-command))))
    (message "%s" result)))

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
(add-hook 'exwm-init-hook #'my-exwm-start-systemtray)

(add-to-list 'exwm-input-prefix-keys ?\C-g)

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(setopt exwm-input-global-keys
        `(([?\s-j] . exwm-reset)
          ([?\s-i] . exwm-input-toggle-keyboard)
          ([?\s-b] . exwm-workspace-switch-to-buffer)
          ([?\s-s] . exwm-workspace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-x] . my-exwm-run-from-path)

          ([XF86MonBrightnessUp] . (lambda (arg)
                                     (interactive "p")
                                     (my-exwm-shell-run "mybrightness up"
                                                        (* arg my-exwm-brightness-step))))
          ([XF86MonBrightnessDown] . (lambda (arg)
                                       (interactive "p")
                                       (my-exwm-shell-run "mybrightness down"
                                                          (* arg my-exwm-brightness-step))))

          ([XF86AudioRaiseVolume] . (lambda (arg)
                                      (interactive "p")
                                      (my-exwm-shell-run "myvolume up"
                                                         (* arg my-exwm-volume-step))))
          ([XF86AudioLowerVolume] . (lambda (arg)
                                      (interactive "p")
                                      (my-exwm-shell-run "myvolume down"
                                                         (* arg my-exwm-volume-step))))

          ([s-XF86AudioRaiseVolume] . (lambda (arg)
                                        (interactive "p")
                                        (my-exwm-shell-run "myvolume mic-up"
                                                           (* arg my-exwm-mic-step))))
          ([s-XF86AudioLowerVolume] . (lambda (arg)
                                        (interactive "p")
                                        (my-exwm-shell-run "myvolume mic-down"
                                                           (* arg my-exwm-mic-step))))

          ([XF86AudioMute] . (lambda ()
                               (interactive)
                               (my-exwm-shell-run "myvolume mute")))
          ([XF86AudioMicMute] . (lambda ()
                                  (interactive)
                                  (my-exwm-shell-run "myvolume mic-mute")))))

(exwm-wm-mode)

(provide 'my-exwm)
;;; my-exwm.el ends here
