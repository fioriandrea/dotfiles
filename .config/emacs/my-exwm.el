;;; my-exwm.el -*- lexical-binding: t; -*-

(require 'exwm)
(require 'exwm-input)
(require 'exwm-workspace)

(require 'xdg)
(require 'cl-lib)
(require 'seq)

;;; App launcher

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

(defun my-exwm-spawn-shell-process (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

;;; Config

(setopt exwm-workspace-number 4
        exwm-workspace-index-map (lambda (i) (number-to-string (1+ i)))
        exwm-layout-show-all-buffers t
        exwm-workspace-show-all-buffers t)

;;;; Keys

(setopt exwm-input-prefix-keys
        `(,(kbd "C-x")
          ,(kbd "C-u")
          ,(kbd "C-h")
          ,(kbd "M-x")
          ,(kbd "M-`")
          ,(kbd "M-&")
          ,(kbd "M-:")
          ,(kbd "M-!")
          ,(kbd "C-g")
          ,(kbd "C-[")
          ,@(cl-loop for i from 0 upto 9
                     collect (kbd (format "M-%d" i)))))

(setopt exwm-input-simulation-keys
        `((,(kbd "C-c C-c") . ,(kbd "C-c"))))

(keymap-set exwm-mode-map "C-q" 'exwm-input-send-next-key)

(setopt exwm-input-global-keys
        `((,(kbd "s-j") . exwm-reset)
          (,(kbd "s-i") . exwm-input-toggle-keyboard)
          (,(kbd "s-s") . exwm-workspace-switch)
          (,(kbd "s-&") . my-exwm-spawn-shell-process)
          (,(kbd "s-d") . my-exwm-run-desktop-app)))

;;;; Hooks

(defun my-exwm-rename-buffer-by-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun my-exwm-fix-initial-workspace-glitch ()
  (run-at-time
   0.3 nil
   (lambda ()
     (when (> exwm-workspace-number 1)
       (exwm-workspace-switch-create 1)
       (exwm-workspace-switch-create 0)))))

(add-hook 'exwm-update-class-hook
          'my-exwm-rename-buffer-by-class)
(add-hook 'exwm-init-hook
          'my-exwm-fix-initial-workspace-glitch)

;;;; Load additional configs

(defconst my-exwm-local-file
  (concat user-emacs-directory "my-exwm-local.el"))
(when (file-exists-p my-exwm-local-file)
  (load-file my-exwm-local-file))

;;;; Enable ewxm

(exwm-wm-mode)

(provide 'my-exwm)
;;; my-exwm.el ends here
