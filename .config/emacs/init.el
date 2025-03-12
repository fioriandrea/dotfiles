(defun reload-config ()
  (interactive)
  (load-file user-init-file))

(defun open-config ()
  (interactive)
  (find-file user-init-file))

(defun tick (cmd)
  ;; (interactive "M")
  ;; (find-file (tick "which ricegit"))
  (let ((output (shell-command-to-string cmd)))
    (string-trim output)))

(defun ctags-file-create (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   ;; -e for emacs format
   (format "%s -f TAGS -e -R %s" "ctags" (directory-file-name dir-name))))

(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun close-all-other-buffers ()
  "Kill all buffers except current buffer."
  (interactive)
  (let* ((predicate (lambda (b) (not (eq b (current-buffer)))))
	     (buffers (seq-filter predicate (buffer-list))))
    (mapc 'kill-buffer buffers)
    (delete-other-windows)))

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

(use-package package
  :defer t
  :ensure nil
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package emacs
  :defer t
  :ensure nil
  :bind
  ("M-/" . hippie-expand)
  :custom
  ;; (pop-up-windows nil)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window-Options.html#index-pop_002dup_002dwindows_002c-replacement-for
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-same-window
      display-buffer-in-previous-window
      display-buffer-use-some-window)))
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
  ;; Auto-revert
  (global-auto-revert-mode t)
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t)
  ;; https://lists.gnu.org/archive/html/emacs-devel/2014-10/msg00743.html
  ;; https://emacs.stackexchange.com/a/50134 (read comments)
  (auto-revert-interval 5)
  (global-auto-revert-ignore-modes '(Buffer-menu-mode))
  :init
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (windmove-default-keybindings)
  (windmove-swap-states-default-keybindings '(shift control)))

(use-package tab-bar
  :defer t
  :ensure nil
  :custom
  (tab-bar-show 1))

(use-package tramp
  :defer t
  :demand nil
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
  :ensure nil
  :defer t
  :custom
  ;; (tab-always-indent 'complete)
  ;; (completion-auto-select 'second-tab)
  (fido-mode t)
  (fido-vertical-mode t))

(use-package eldoc
  :defer t
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package org
  :defer t
  :ensure nil
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c z" . org-toggle-link-display))
  :custom
  (org-startup-truncated nil)
  (org-html-validation-link nil))

(use-package org-tempo
  :after org
  :ensure nil)

(use-package dired
  :defer t
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-alh")
  (dired-mouse-drag-files t)
  (dired-dwim-target t))

(use-package magit
  :ensure nil
  :defer t
  :hook
  (magit-diff-mode . (lambda () (setq truncate-lines nil)))
  (magit-status-mode . (lambda () (setq truncate-lines nil)))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-diff-refine-ignore-whitespace nil)
  (magit-auto-revert-mode nil))

(use-package dired-subtree
  :ensure nil
  :defer t
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
