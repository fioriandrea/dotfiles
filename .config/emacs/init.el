;;; Defalut Font
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

;;; Load Theme
(defconst light-theme 'modus-operandi)
(defconst dark-theme 'modus-vivendi)
(load-theme light-theme t)

;;; Don't put garbage in my config file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
;; Uncomment this to use a temp file as custom file
;; (setq custom-file (make-temp-file "emacs-custom"))

;;; Useful functions
(defconst config-file (expand-file-name ".config/emacs/init.el" "~"))

(defun reload-config ()
  (interactive)
  (load-file config-file))

(defun open-config ()
  (interactive)
  (find-file config-file))

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
(global-set-key (kbd "C-c c t") 'ctags-file-create)

(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'close-all-buffers)

;;; Useful Defaults (most from https://sanemacs.com/)
;; http://xahlee.info/emacs/emacs/emacs_buffer_management.html
(defalias 'list-buffers 'ibuffer)
;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
(setq tramp-auto-save-directory temporary-file-directory)
;; Dired config
;; Refresh dired automatically
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq dired-listing-switches "-alh")
(setq dired-mouse-drag-files t)
(setq-default dired-dwim-target t)
;; Display Line Numbers
(global-display-line-numbers-mode)
;; Open Emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Remove scroll-bar, tool-bar and menu-bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; Disable startup screen
(setq inhibit-startup-screen t)
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
;; Line number format
(setq linum-format "%4d ")
;; Auto-update buffer if file has changed on disk
(global-auto-revert-mode t)
;; Delete trailing whitespace on save
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)
;; Disable Lockfiles
(setq create-lockfiles nil)

;;; Put Emacs auto-save and backup files to /tmp/ or C:/Temp/ (from https://sanemacs.com/)
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-list-file-prefix emacs-tmp-dir
 auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
 backup-directory-alist `((".*" . ,emacs-tmp-dir)))
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
(add-to-list 'backup-directory-alist
	(cons tramp-file-name-regexp nil))

;;; Use system Clipboard
(setq select-enable-clipboard t)

;;; Pair quotes, parentheses etc.
(electric-pair-mode 1)

;;; Packages

;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; Evil Mode
(use-package evil
  :demand t
  :ensure t
  :bind (("<escape>" . keyboard-escape-quit)
		 ("C-c v" . evil-mode)
		 ("C-c l v" . evil-local-mode)
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
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-u-scroll t)
  ;; use emacs keys in insert mode
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))
(add-hook 'term-mode-hook 'evil-emacs-state)
;;; Additional Vim bindings
(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-want-find-usages-bindings t)
  :init
  (evil-collection-init))

;;; Alternative to evil mode. Has less features
;;; https://www.reddit.com/r/emacs/comments/e81u80/comment/fa98l7z
;; (use-package viper
;;   :ensure t
;;   :init
;;   ; (setq viper-mode t)
;;   (setq viper-inhibit-startup-message 't)
;;   (setq viper-expert-level '3)
;;   :bind
;;   ("C-c v" . toggle-viper-mode))

;;; Completion in region mode mappings
(let ((map completion-in-region-mode-map))
  (define-key map (kbd "M-n") 'minibuffer-next-completion)
  (define-key map (kbd "M-p") 'minibuffer-previous-completion)
  (define-key map (kbd "C-n") 'minibuffer-next-completion)
  (define-key map (kbd "C-p") 'minibuffer-previous-completion)
  (define-key map (kbd "C-<return>") 'minibuffer-choose-completion)
  (define-key map (kbd "C-RET") 'minibuffer-choose-completion))

;;; Icomplete
(use-package icomplete
  :demand t
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
			  ("C-p" . icomplete-backward-completions)))
;;; https://www.scss.tcd.ie/~sulimanm/posts/default-emacs-completion.html
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 20)
(setq completion-auto-select nil)

;;; Git integration for Emacs (Magit)
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

;;; END
