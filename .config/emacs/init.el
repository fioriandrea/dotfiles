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

;;; Packages

;; Web Proxy Config
(defconst proxy-file (concat user-emacs-directory "proxy.el"))
(when (file-exists-p proxy-file)
  (load proxy-file))

;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))
;; https://stackoverflow.com/questions/24280325/emacs-use-package-and-package-refresh-contents
(unless package-archive-contents (package-refresh-contents))

;;; Setup use-package
;; https://www.gnu.org/software/emacs/manual/html_mono/use-package.html
;; Should not be necessary, since use-package is build-in
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(use-package use-package
  :ensure nil
  :demand nil
  :custom
  (use-package-always-ensure t)
  (use-package-always-defer nil)
  (use-package-verbose t)
  (use-package-compute-statistics t))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defconst emacs-backup-dir
  (file-name-as-directory
   (expand-file-name "backups" user-emacs-directory)))
(defconst emacs-autosave-dir
  (file-name-as-directory
   (expand-file-name "autosave" user-emacs-directory)))

;;; General emacs config
;; https://elpa.gnu.org/devel/doc/use-package.html#The-emacs-package
(use-package emacs
  :defer t
  :ensure nil
  ;; https://www.gnu.org/software/emacs/manual/html_node/autotype/Hippie-Expand.html
  :bind
  ("M-/" . hippie-expand)
  :hook (before-save . delete-trailing-whitespace)
  ;; https://github.com/jwiegley/use-package/issues/517 (Advantages of custom?)
  :custom
  ;; (pop-up-windows nil)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window-Options.html#index-pop_002dup_002dwindows_002c-replacement-for
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-same-window
      display-buffer-in-previous-window
      display-buffer-use-some-window)))

  (native-comp-async-report-warnings-errors 'silent)

  ;; https://irreal.org/blog/?p=1562
  (winner-mode t)

  (global-display-line-numbers-mode t)
  (ring-bell-function 'ignore)
  (truncate-lines nil)
  (indent-tabs-mode nil)
  (show-paren-mode t)
  (select-enable-clipboard t)
  (byte-compile-docstring-max-column 240)

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

  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (menu-bar-mode t)
  (context-menu-mode t)
  (inhibit-startup-screen t)

  (use-short-answers t)

  (create-lockfiles nil)

  (xterm-mouse-mode t)

  ;; backups
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  ;; https://emacs.stackexchange.com/questions/17210/how-to-place-all-auto-save-files-in-a-directory
  (auto-save-list-file-prefix (file-name-as-directory emacs-autosave-dir))
  (auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t)))
  (backup-directory-alist `((".*" . ,emacs-backup-dir)))
  :init
  ;; Defalut Font
  (defun font-available-p (font-name)
    (if (member font-name (font-family-list)) t nil))
  (cond
   ((font-available-p "Comic Mono")
    (set-frame-font "Comic Mono 20" nil t))
   (t (set-face-attribute 'default nil :height 180)))

  ;; Open Emacs in fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (windmove-default-keybindings)
  (windmove-swap-states-default-keybindings '(shift control)))

(use-package solarized-theme
  :config
  (load-theme 'solarized-gruvbox-light t))

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
  (enable-remote-dir-locals nil)
  (tramp-auto-save-directory emacs-autosave-dir)

  (tramp-backup-directory-alist backup-directory-alist)
  (remote-file-name-inhibit-locks t)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  ;; https://robbmann.io/emacsd/
  ;; https://git.sr.ht/~cfeeley/doom-emacs-config/commit/1cb3f6704f38f9dbc64ff434866b5e2537d8c2ba
  (debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
	   vc-ignore-dir-regexp
	   tramp-file-name-regexp)))

(use-package evil
  :demand t
  ;; https://emacs.stackexchange.com/questions/61833/how-can-i-re-enable-c-z-in-evil-mode-to-pause-emacs
  :bind (("<escape>" . keyboard-escape-quit)
         ("C-c e" . evil-mode)
         :map evil-motion-state-map
         ("TAB" . nil)
         ("RET" . nil)
         ("<backtab>" . nil)
         :map evil-normal-state-map
         ("M-" . nil)
         ("C-i" . evil-jump-forward)
         ("C-n" . evil-next-line)
         ("C-p" . evil-previous-line))
  :custom
  (evil-want-keybinding t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-overriding-maps nil)
  (evil-want-integration t)
  (evil-want-minibuffer nil)
  (evil-want-C-i-jump nil)
  (evil-want-C-d-scroll nil)
  (evil-want-C-u-scroll nil)
  (evil-symbol-word-search t)
  (evil-default-state 'insert)
  (evil-emacs-state-modes '(term-mode))
  (evil-insert-state-modes '())
  (evil-motion-state-modes '(completion-list-mode
                             special-mode
                             diff-mode
                             archive-mode
                             compilation-mode))
  :init
  ;; Don't know why, but this cannot be under customize for some reason
  (setq evil-search-module 'evil-search)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-normal-state-modes '(text-mode
                                  conf-mode
                                  Custom-mode
                                  prog-mode
                                  fundamental-mode
                                  dired-mode))
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

;;; Alternative to evil mode. Has less features. For visual mode, you
;;; can use emacs selections, and then you can operate on the region
;;; using the "r" text object (for example, cr or dr).
;;; https://www.reddit.com/r/emacs/comments/e81u80/comment/fa98l7za
;; (use-package viper
;;   :demand t
;;   :ensure nil
;;   :bind
;;   (:map viper-vi-basic-map
;;         ("C-y" . nil)
;;         ("C-b" . nil)
;;         ("C-f" . nil)
;;         ("C-e" . nil))
;;   :custom
;;   (viper-want-ctl-h-help t)
;;   (viper-want-emacs-keys-in-vi t)
;;   (viper-want-emacs-keys-in-insert t)
;;   :config
;;   (viper-mode)
;;   :init
;;   (setq viper-mode t)
;;   (setq viper-inhibit-startup-message 't)
;;   (setq viper-expert-level '5)
;;   :bind
;;   ("C-c v" . toggle-viper-mode))

(use-package icomplete
  :demand t
  :custom
  (fido-mode t)
  (fido-vertical-mode t)
  (tab-always-indent 'complete)
  ;; https://www.scss.tcd.ie/~sulimanm/posts/default-emacs-completion.html
  (completions-format 'one-column)
  (completions-header-format nil)
  (completions-max-height 20)
  (completion-auto-select 'second-tab)
  ;; (completion-auto-select t)
  :bind (:map icomplete-fido-mode-map
	          ("C-RET" . icomplete-fido-ret)
	          ("C-<return>" . icomplete-fido-ret)
	          ("M-RET" . icomplete-fido-ret)
	          ("TAB" . icomplete-force-complete)
	          :map icomplete-minibuffer-map
	          ("C-RET" . icomplete-fido-ret)
	          ("C-<return>" . icomplete-fido-ret)
	          :map completion-in-region-mode-map
	          ("C-<return>" . minibuffer-choose-completion)
	          ("C-RET" . minibuffer-choose-completion)))

(use-package eglot
  :defer t
  :ensure nil
  :init
  (remove-hook 'eldoc-display-functions 'eldoc-display-in-echo-area))

(use-package xref
  :defer t
  :ensure nil
  :config
  (evil-make-overriding-map xref--xref-buffer-mode-map 'motion))

(use-package org
  :ensure nil
  :init
  (setq org-startup-truncated nil)
  :bind (("C-c l" . org-store-link)
         :map org-mode-map
         ("C-c z" . org-toggle-link-display))
  :config
  (evil-define-key 'normal org-mode-map (kbd "<return>") 'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-cycle)
  :custom
  (org-html-validation-link nil)
  (org-link-descriptive t)
  ;; https://irreal.org/blog/?p=1562
  (org-replace-disputed-keys t))

(use-package org-tempo
  :after org
  ;; https://edoput.it/2022/07/19/use-package.html
  :ensure nil)

;;; Git integration for Emacs (Magit)
(use-package magit
  :hook
  (magit-diff-mode . (lambda () (setq truncate-lines nil)))
  (magit-status-mode . (lambda () (setq truncate-lines nil)))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-diff-refine-ignore-whitespace nil)
  (magit-auto-revert-mode nil))

(use-package dired
  :demand t
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-alh")
  (dired-mouse-drag-files t)
  (dired-dwim-target t)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "g g") 'beginning-of-buffer)
  (evil-define-key 'normal dired-mode-map (kbd "G") 'end-of-buffer)
  (evil-define-key 'normal dired-mode-map (kbd "n") 'evil-ex-search-next)
  (evil-define-key 'normal dired-mode-map (kbd "N") 'evil-ex-search-previous))

(use-package dired-x
  :demand t
  :after dired
  :ensure nil)

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle))
  :config
  (set-face-attribute 'dired-subtree-depth-1-face nil :background nil)
  (set-face-attribute 'dired-subtree-depth-2-face nil :background nil)
  (set-face-attribute 'dired-subtree-depth-3-face nil :background nil)
  (set-face-attribute 'dired-subtree-depth-4-face nil :background nil)
  (set-face-attribute 'dired-subtree-depth-5-face nil :background nil)
  (set-face-attribute 'dired-subtree-depth-6-face nil :background nil))

(use-package project
  :config
  ;; https://www.reddit.com/r/emacs/comments/lfbyq5/comment/gml2hqe/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  (defun local/project-try-explicit (dir)
    "Find a super-directory of DIR containing a root file."
    (locate-dominating-file dir ".root"))
  (cl-defmethod project-root ((project string))
    project)
  (add-hook 'project-find-functions
	        #'local/project-try-explicit))

;;; END
