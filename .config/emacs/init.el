;;; Useful functions
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
  (interactive "Directory: ")
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

;;; Packages

;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;; Setup use-package
;; https://www.gnu.org/software/emacs/manual/html_mono/use-package.html
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure nil)
(setq use-package-always-defer nil)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)


;;; General emacs config
;; https://elpa.gnu.org/devel/doc/use-package.html#The-emacs-package
(use-package emacs
  :defer t
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  ;; https://github.com/jwiegley/use-package/issues/517 (Advantages of custom?)
  :custom
  (global-auto-revert-mode t)
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling t)
  ;; https://lists.gnu.org/archive/html/emacs-devel/2014-10/msg00743.html
  ;; https://emacs.stackexchange.com/a/50134 (read comments)
  (auto-revert-interval 5)
  (indent-tabs-mode nil)
  :init
  ;; Remove scroll-bar, tool-bar and menu-bar
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  ;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  ;; Disable startup screen
  (setq inhibit-startup-screen t)

  ;; Defalut Font
  (defun font-available-p (font-name)
	(if (member font-name (font-family-list)) t nil))
  (cond
   ((font-available-p "JetBrains Mono")
	(set-frame-font "JetBrains Mono 14" nil t))
   ((font-available-p "Cascadia Code")
	(set-frame-font "Cascadia Code 14" nil t))
   ((font-available-p "Inconsolata")
	(set-frame-font "Inconsolata 16" nil t))
   (t (set-face-attribute 'default nil :height 140)))

  ;; Load Theme
  (defconst light-theme 'modus-operandi)
  (defconst dark-theme 'modus-vivendi)
  (load-theme light-theme t)

  ;; Open Emacs in fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Display Line Numbers
  (global-display-line-numbers-mode)

  ;; Line number format
  (setq linum-format "%4d ")

  ;; Don't put garbage in my config file
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
	(load custom-file))
  ;; Uncomment this to use a temp file as custom file
  ;; (setq custom-file (make-temp-file "emacs-custom"))

  ;; Useful Defaults (most from https://sanemacs.com/)
  ;; Disable bell sound
  (setq ring-bell-function 'ignore)
  ;; Line-style cursor similar to other text editors
  (setq-default cursor-type 'bar)
  ;; Make window title the buffer name
  (setq-default frame-title-format '("%b"))
  ;; y-or-n-p makes answering questions faster
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; Show closing parens by default
  (show-paren-mode 1)

  ;; Backups
  (defconst emacs-backup-dir
	(file-name-as-directory
	 (expand-file-name "backups" user-emacs-directory)))
  (defconst emacs-autosave-dir
	(file-name-as-directory
	 (expand-file-name "autosave" user-emacs-directory)))
  (setq
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   ;; https://emacs.stackexchange.com/questions/17210/how-to-place-all-auto-save-files-in-a-directory
   auto-save-list-file-prefix (file-name-as-directory emacs-autosave-dir)
   auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t))
   backup-directory-alist `((".*" . ,emacs-backup-dir)))
  ;; Disable Lockfiles
  (setq create-lockfiles nil)

  ;; Use system Clipboard
  (setq select-enable-clipboard t)

  (setq byte-compile-docstring-max-column 240)
  (setq org-startup-truncated nil)
  (setq truncate-lines nil)
  (setq org-link-descriptive nil)

  ;; http://xahlee.info/emacs/emacs/emacs_buffer_management.html
  (defalias 'list-buffers 'ibuffer))

;;; Tramp
(use-package tramp
  :defer t
  :demand nil
  :custom
  (auto-revert-remote-files nil)
  :init
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
  ;; https://emacs.stackexchange.com/questions/78644/how-to-tell-tramp-to-not-ask-me-about-autosave-on-local-directory
  ;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq tramp-auto-save-directory emacs-autosave-dir)
  (setq remote-file-name-inhibit-locks t)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (setq vc-ignore-dir-regexp
		(format "\\(%s\\)\\|\\(%s\\)"
				vc-ignore-dir-regexp
				tramp-file-name-regexp)))

;;; Evil Mode
(use-package evil
  :demand t
  :ensure t
  :bind (("<escape>" . keyboard-escape-quit)
		 ("C-c v g" . evil-mode)
		 ("C-c v l" . evil-local-mode)
		 :map evil-normal-state-map
		 ("C-n" . evil-next-line)
		 ("C-p" . evil-previous-line))
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-overriding-maps nil)
  (setq evil-want-integration t)
  (setq evil-want-minibuffer nil)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-d-scroll nil)
  (setq evil-want-C-u-scroll nil)
  ;; use emacs keys in insert mode
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))
;;; Additional Vim bindings
(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings t)
  (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

;;; Alternative to evil mode. Has less features
;;; https://www.reddit.com/r/emacs/comments/e81u80/comment/fa98l7z
;; (use-package viper
;;   :demand t
;;   :ensure t
;;   :config
;;   (setq viper-mode t)
;;   (viper-mode)
;;   :init
;;   (setq viper-inhibit-startup-message 't)
;;   (setq viper-expert-level '3)
;;   :bind
;;   ("C-c v" . toggle-viper-mode))

;;; Icomplete
(use-package icomplete
  :ensure t
  :demand t
  :custom
  ;; https://www.scss.tcd.ie/~sulimanm/posts/default-emacs-completion.html
  (completions-format 'one-column)
  (completions-header-format nil)
  (completions-max-height 20)
  (completion-auto-select 'second-tab)
  :config
  (fido-vertical-mode 1)
  :bind (:map icomplete-fido-mode-map
			  ("RET" . icomplete-fido-ret)
			  ("C-RET" . icomplete-fido-ret)
			  ("C-<return>" . icomplete-fido-ret)
			  ("M-RET" . icomplete-fido-ret)
			  ("TAB" . icomplete-force-complete)
			  :map icomplete-minibuffer-map
			  ("C-RET" . icomplete-fido-ret)
			  ("C-<return>" . icomplete-fido-ret)
			  ("C-n" . icomplete-forward-completions)
			  ("C-p" . icomplete-backward-completions)
			  :map completion-in-region-mode-map
			  ("M-n" . minibuffer-next-completion)
			  ("M-p" . minibuffer-previous-completion)
			  ("C-n" . minibuffer-next-completion)
			  ("C-p" . minibuffer-previous-completion)
			  ("C-<return>" . minibuffer-choose-completion)
			  ("C-RET" . minibuffer-choose-completion)))

(use-package eglot
  :defer t
  :ensure nil)

(use-package org-tempo
  :after org
  ;; https://edoput.it/2022/07/19/use-package.html
  :ensure nil)

;;; Git integration for Emacs (Magit)
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :custom
  (magit-auto-revert-mode nil))

(use-package dired
  :demand t
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-mouse-drag-files t)
  ;; Refresh dired automatically
  (setq-default dired-dwim-target t))

(use-package dired-x
  :demand t
  :after dired
  :ensure nil)

;;; END
