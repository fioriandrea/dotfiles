(defun reload-config ()
  (interactive)
  (load-file user-init-file))

(defun open-config ()
  (interactive)
  (find-file user-init-file))

(defun tick (cmd)
  (let ((output (shell-command-to-string cmd)))
    (string-trim output)))

(defconst emacs-backup-dir
  (file-name-as-directory
   (abbreviate-file-name
    (expand-file-name "backups" user-emacs-directory))))
(defconst emacs-autosave-dir
  (file-name-as-directory
   (abbreviate-file-name
    (expand-file-name "autosave" user-emacs-directory))))

(defconst proxy-file (concat user-emacs-directory "proxy.el"))
(when (file-exists-p proxy-file)
  (load proxy-file))

(use-package use-package
  :ensure nil
  :custom
  (use-package-always-ensure nil)
  (use-package-always-demand nil)
  (use-package-always-defer t))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; https://elpa.gnu.org/devel/doc/use-package.html#The-emacs-package
(use-package emacs
  :custom
  (help-window-select t)
  (global-display-line-numbers-mode t)
  (ring-bell-function 'ignore)
  (truncate-lines nil)
  (truncate-partial-width-windows nil)
  (indent-tabs-mode nil)
  (scroll-bar-mode nil)
  (menu-bar-mode t)
  (context-menu-mode t)
  (tool-bar-mode nil)
  (inhibit-startup-screen t)
  (use-short-answers t)
  (xterm-mouse-mode t)
  ;; Backup
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (auto-save-list-file-prefix (file-name-as-directory emacs-autosave-dir))
  (auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t)))
  (backup-directory-alist `(("." . ,emacs-backup-dir)))
  :init
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(use-package autorevert
  :custom
  (global-auto-revert-mode t)
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t)
  ;; https://lists.gnu.org/archive/html/emacs-devel/2014-10/msg00743.html
  ;; https://emacs.stackexchange.com/a/50134 (read comments)
  (auto-revert-interval 5)
  (global-auto-revert-ignore-modes '(Buffer-menu-mode)))

(use-package windmove
  :custom
  (windmove-default-keybindings)
  (windmove-swap-states-default-keybindings '(shift control)))

(use-package hippie-exp
  :bind
  ("M-/" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list '(try-complete-file-name
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill)))

(use-package tab-bar
  :custom
  (tab-bar-show 1))

(use-package recentf
  :custom
  (recentf-mode t))

(use-package tramp
  :custom
  (auto-revert-remote-files nil)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
  ;; https://emacs.stackexchange.com/questions/78644/how-to-tell-tramp-to-not-ask-me-about-autosave-on-local-directory
  ;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
  (tramp-auto-save-directory emacs-autosave-dir)
  ;; https://stackoverflow.com/a/47021266
  (tramp-backup-directory-alist backup-directory-alist)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  ;; https://robbmann.io/emacsd/
  ;; https://git.sr.ht/~cfeeley/doom-emacs-config/commit/1cb3f6704f38f9dbc64ff434866b5e2537d8c2ba
  (debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp)))

(use-package icomplete
  :custom
  ;; (tab-always-indent 'complete)
  ;; (completion-auto-select 'second-tab)
  (fido-mode t)
  (fido-vertical-mode t))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c z" . org-toggle-link-display))
  :custom
  (org-startup-truncated nil)
  (org-html-validation-link nil))

(use-package org-tempo
  :demand t
  :after org)

(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-alh")
  (dired-mouse-drag-files t)
  (dired-dwim-target t))

(use-package dired-x
  :demand t
  :after dired)

(use-package magit
  :if (package-installed-p 'magit)
  :bind
  ("C-x g" . magit-status)
  ("C-x p m" . magit-project-status)
  :hook
  (magit-diff-mode . (lambda () (setq truncate-lines nil)))
  (magit-status-mode . (lambda () (setq truncate-lines nil)))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-diff-refine-ignore-whitespace nil)
  (magit-auto-revert-mode nil))

(use-package dired-subtree
  :if (package-installed-p 'dired-subtree)
  :after dired
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle))
  :config
  (set-face-attribute 'dired-subtree-depth-1-face nil :background 'unspecified)
  (set-face-attribute 'dired-subtree-depth-2-face nil :background 'unspecified)
  (set-face-attribute 'dired-subtree-depth-3-face nil :background 'unspecified)
  (set-face-attribute 'dired-subtree-depth-4-face nil :background 'unspecified)
  (set-face-attribute 'dired-subtree-depth-5-face nil :background 'unspecified)
  (set-face-attribute 'dired-subtree-depth-6-face nil :background 'unspecified))

(use-package consult
  :if (package-installed-p 'consult)
  :bind (("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         :map icomplete-fido-mode-map
         ("C-k" . icomplete-consult-fido-kill))
  :config
  (consult-customize
   consult-buffer
   consult-project-buffer
   :preview-key nil
   :annotate (lambda (x) ""))
  :init
  ;; massive hack stolen from https://debbugs.gnu.org/cgi/bugreport.cgi?bug=72210
  (defun icomplete-consult-fido-kill (&optional pcat pthing)
    (interactive)
    (if (< (point) (icomplete--field-end))
        (call-interactively 'kill-line)
      (let ((cat (or pcat (icomplete--category)))
            (all (completion-all-sorted-completions)))
        (if (eq cat 'multi-category)
            (let ((mc (get-text-property 0 'multi-category (car all))))
              (icomplete-consult-fido-kill (car mc) (format "%s" (cdr mc))))
          (let* ((thing (or pthing (car all)))
                 (action
                  (cl-case cat
                    (buffer
                     (lambda ()
                       (when (yes-or-no-p (concat "Kill buffer " thing "? "))
                         (kill-buffer thing))))
                    ((project-file file)
                     (lambda ()
                       (let* ((dir (file-name-directory (icomplete--field-string)))
                              (path (expand-file-name thing dir)))
                         (when (yes-or-no-p (concat "Delete file " path "? "))
                           (delete-file path) t))))
                    (t
                     (error "Sorry, don't know how to kill things for `%s'" cat)))))
            (when (let ((enable-recursive-minibuffers t)
                        (icomplete-mode nil))
                    (funcall action))
              (completion--cache-all-sorted-completions
               (icomplete--field-beg)
               (icomplete--field-end)
               (cdr all))))
          (message nil))))))
