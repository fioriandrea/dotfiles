;;; my-exwm.el -*- lexical-binding: t; -*-

(require 'exwm)
(require 'exwm-input)
(require 'exwm-systemtray)
(require 'exwm-workspace)

(require 'xdg)
(require 'cl-lib)
(require 'seq)

(defvar my-exwm-brightness-step 0.01)
(defvar my-exwm-volume-step 0.02)
(defvar my-exwm-mic-step 0.02)

(setopt exwm-workspace-number 4
        exwm-workspace-index-map (lambda (i) (number-to-string (1+ i))))

(display-time-mode 1)
(display-battery-mode 1)

(defvar my-exwm-xdg-apps-dirs
  (cl-loop for dir in (cons (xdg-data-home) (xdg-data-dirs))
           collect (expand-file-name "applications" dir)))

(defvar my-exwm-desktop-app-history nil)
(defvar my-exwm-desktop-terminal-command '("xterm" "-e"))

(defun my-exwm-desktop-apps ()
  (let (apps)
    (dolist (dir my-exwm-xdg-apps-dirs)
      (when (file-directory-p dir)
        (dolist (file (directory-files-recursively dir "\\.desktop$"))
          (let* ((data (xdg-desktop-read-file file))
                 (name (gethash "Name" data))
                 (exec (gethash "Exec" data)))
            (when (and name exec
                       (not (string= (gethash "Hidden" data) "true"))
                       (not (string= (gethash "NoDisplay" data) "true")))
              (push (cons name data) apps))))))
    apps))

(defun my-exwm-run-desktop-app ()
  (interactive)
  (let* ((apps (my-exwm-desktop-apps))
         (choice (completing-read "App: " (mapcar #'car apps)
                                  nil t nil 'my-exwm-desktop-app-history))
         (data (cdr (assoc choice apps)))
         (exec (gethash "Exec" data))
         (terminal (string= (gethash "Terminal" data) "true"))
         (argv (seq-remove (lambda (arg) (string-prefix-p "%" arg))
                           (split-string-and-unquote exec))))
    (when argv
      (apply #'start-process choice nil
             (if terminal (append my-exwm-desktop-terminal-command argv) argv)))))

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

(setopt exwm-input-prefix-keys
        '([?\C-x]
          [?\C-u]
          [?\C-h]
          [?\M-x]
          [?\M-`]
          [?\M-&]
          [?\M-:]
          [?\M-!]
          [?\C-g]))

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(setopt exwm-input-global-keys
        `(([?\s-j] . exwm-reset)
          ([?\s-i] . exwm-input-toggle-keyboard)
          ([?\s-b] . exwm-workspace-switch-to-buffer)
          ([?\s-s] . exwm-workspace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-x] . my-exwm-run-desktop-app)

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

(exwm-systemtray-mode 1)

(provide 'my-exwm)
;;; my-exwm.el ends here
