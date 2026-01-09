;;; init.el --- Emacs configuration -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) Andrea Fiori

;; Author: Andrea Fiori

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

;;;; Functions

(defun my-open-config ()
  (interactive)
  (find-file user-init-file))

(defun my-kill-buffer-skip-hooks (buffer)
  (interactive "bBuffer: ")
  (with-current-buffer buffer
    (let (kill-buffer-hook kill-buffer-query-functions)
      (kill-buffer))))

;;;; my-use-package

(defun my-use-package--collect-lists (key val acc rest)
  (cond
   ((and val (listp val) (listp (car val)))
    (list (append acc val) rest))
   ((and val (listp val))
    (list (cons val acc) (cons key rest)))
   (t
    (list acc (cons val rest)))))

(defun my-use-package--collect-sexprs (key val acc rest)
  (if (keywordp val)
      (list acc (cons val rest))
    (list (cons val acc) (cons key rest))))

(defmacro my-use-package (pack &rest args)
  "Minimal `use-package' variant supporting a limited set of options.
Expands only if PACK names a provided feature or loadable library.
The expanded code catches any error during package setup."
  (declare (indent defun))
  (require 'cl-lib)
  (let ((customs nil)
        (custom-faces nil)
        (configs nil)
        (inits nil)
        (disabled nil)
        (demand nil)
        (rest args)
        (condition
         `((or (featurep ',pack)
               (locate-library ,(symbol-name pack))))))
    ;; Parse arguments
    (cl-loop
     while (and rest (cdr rest))
     for key = (pop rest)
     for val = (pop rest)
     do
     (pcase key
       (:disabled
        (setq disabled t
              rest nil)
        (cl-return))
       ((or :if :when)
        (push val condition))
       (:demand
        (setq demand val))
       (:custom-face
        (cl-destructuring-bind (new new-rest)
            (my-use-package--collect-lists key val custom-faces rest)
          (setq custom-faces new
                rest new-rest)))
       (:custom
        (cl-destructuring-bind (new new-rest)
            (my-use-package--collect-lists key val customs rest)
          (setq customs new
                rest new-rest)))
       (:init
        (cl-destructuring-bind (new new-rest)
            (my-use-package--collect-sexprs key val inits rest)
          (setq inits new
                rest new-rest)))
       (:config
        (cl-destructuring-bind (new new-rest)
            (my-use-package--collect-sexprs key val configs rest)
          (setq configs new
                rest new-rest)))
       (_
        (push val rest))))
    ;; Expansion
    (unless disabled
      `(when (condition-case-unless-debug nil
                 (and ,@condition)
               (error nil))
         ;; :custom-face
         ,(when custom-faces
            `(progn
               ,@(mapcar
                  (lambda (x)
                    `(condition-case-unless-debug err
                         (apply #'face-spec-set (backquote ,x))
                       (error
                        (warn "Failed to customize face %S because of %S"
                              ',x err))))
                  (nreverse custom-faces))))
         ;; :custom
         ,(when customs
            `(progn
               ,@(mapcar
                  (lambda (x)
                    `(condition-case-unless-debug err
                         (customize-set-variable ',(car x) ,@(cdr x))
                       (error
                        (warn "Failed to customize %S because of %S"
                              ',x err))))
                  (nreverse customs))))
         ;; :init
         ,(when inits
            `(condition-case-unless-debug err
                 (progn ,@(nreverse inits))
               (error
                (warn "Failed to :init for package %S because of %S"
                      ',pack err))))
         ;; :demand
         ,(when demand
            `(require ',pack nil nil))
         ;; :config
         ,(when configs
            `(eval-after-load ',pack
               (quote
                ,`(condition-case-unless-debug err
                      (progn ,@(nreverse configs))
                    (error
                     (warn "Failed to :config for package %S because of %S"
                           ',pack err))))))))))

(unless (fboundp 'use-package)
  (message "No use-package found, using compatibility shim")
  (defalias 'use-package 'my-use-package))

;;;; Configuration

;;;;; Built-in

(defconst my-emacs-backup-dir
  (file-name-as-directory
   (abbreviate-file-name
    (expand-file-name "backups" user-emacs-directory))))

(defconst my-emacs-autosave-dir
  (file-name-as-directory
   (expand-file-name "autosaves" user-emacs-directory)))
(make-directory my-emacs-autosave-dir 'parents)

(defconst my-before-file (concat user-emacs-directory "before.el"))
(when (file-exists-p my-before-file)
  (load-file my-before-file))

(use-package use-package
  :ensure nil
  :custom
  (use-package-always-ensure nil)
  (use-package-always-demand nil)
  (use-package-always-defer t)
  (use-package-expand-minimally nil)
  (use-package-use-theme t))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;; https://elpa.gnu.org/devel/doc/use-package.html#The-emacs-package
(use-package emacs
  :bind
  (("C-x f" . nil)
   ("C-x C-b" . ibuffer)
   ("C-M-/" . hippie-expand))
  :hook
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  (text-mode . visual-line-mode)
  :custom
  (inhibit-startup-screen t)
  (tool-bar-mode nil)
  (menu-bar-mode t)
  (confirm-kill-emacs 'y-or-n-p)
  (native-comp-async-report-warnings-errors 'silent)
  (view-read-only nil)
  (global-subword-mode t)
  (global-so-long-mode t)
  (electric-pair-mode t)
  (sentence-end-double-space t)
  (ring-bell-function 'ignore)
  (truncate-lines nil)
  (truncate-partial-width-windows nil)
  (indent-tabs-mode nil)
  (context-menu-mode t)
  (use-short-answers t)
  (frame-inhibit-implied-resize t)
  (save-interprogram-paste-before-kill t)
  (xterm-mouse-mode t)
  ;; Scrolling
  (scroll-bar-mode 'right)
  (horizontal-scroll-bar-mode nil)
  (mouse-wheel-progressive-speed nil)
  (fast-but-imprecise-scrolling t)
  (scroll-preserve-screen-position t)
  (scroll-conservatively 10)
  (scroll-margin 4)
  ;; Backup
  (backup-by-copying-when-linked t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 5)
  (auto-save-file-name-transforms `((".*" ,my-emacs-autosave-dir t)))
  (backup-directory-alist `(("." . ,my-emacs-backup-dir)))
  (lock-file-name-transforms `((".*" ,temporary-file-directory sha1)))
  :init
  (defun my-delete-autosave-current-buffer ()
    (interactive)
    (when buffer-file-name
      (let ((auto-save-file (make-auto-save-file-name)))
        (when (file-exists-p auto-save-file)
          (delete-file auto-save-file)))))
  (add-hook 'kill-buffer-hook 'my-delete-autosave-current-buffer)
  ;; hack to delete auto-save files on C-x C-c
  (add-to-list 'kill-emacs-query-functions
               (lambda () (dolist (buf (buffer-list) t)
                            (with-current-buffer buf
                              (my-delete-autosave-current-buffer)))))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(use-package help
  :custom
  (help-window-select t)
  (help-window-keep-selected t))

(use-package hl-line
  :hook
  (dired-mode . hl-line-mode)
  (proced-mode . hl-line-mode)
  (ibuffer . hl-line-mode)
  (org-agenda-mode . hl-line-mode)
  (tabulated-list-mode . hl-line-mode))

(use-package mb-depth
  :custom
  (enable-recursive-minibuffers t)
  (read-minibuffer-restore-windows nil)
  (minibuffer-depth-indicate-mode t))

(use-package whitespace
  :bind
  ("C-c s s" . whitespace-mode)
  ("C-c s d" . (lambda ()
                 (interactive)
                 (delete-trailing-whitespace)
                 (message "Trailing whitespace deleted")))
  :config
  (customize-set-variable 'whitespace-style
                          (delq 'lines whitespace-style)))

(use-package uniquify
  :custom
  (uniquify-strip-common-suffix nil)
  (uniquify-after-kill-buffer-p t)
  ;; https://emacs.stackexchange.com/questions/68499/automatically-uniquify-certain-buffers
  (uniquify-min-dir-content 3)
  (uniquify-buffer-name-style 'reverse)
  (uniquify-trailing-separator-p t))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  ;; `no-ding' makes keyboard macros never quit
  (isearch-wrap-pause t))

(use-package tab-bar
  :bind
  (:repeat-map repeat-tab-bar-history-map
               ("<left>" . tab-bar-history-back)
               ("<right>" . tab-bar-history-forward))
  :custom
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(meta))
  (tab-bar-new-tab-choice t)
  (tab-bar-new-tab-to 'right)
  (tab-bar-history-mode t)
  (tab-bar-history-limit 30)
  :config
  (customize-set-variable 'tab-bar-format
                          (delq 'tab-bar-format-history
                                tab-bar-format))
  (defun my-tab-bar-history-position-indicator ()
    (let* ((current (selected-frame))
           (hist-back-count (length (gethash current tab-bar-history-back)))
           (hist-forward-count (length (gethash current tab-bar-history-forward)))
           (total (+ hist-back-count hist-forward-count))
           (status (format "Tab history [%d / %d]" hist-back-count total))
           (tab-bar-history-command-echo-text (current-message)))
      (message
       (if tab-bar-history-command-echo-text
           (concat status " (" tab-bar-history-command-echo-text ")")
         status))))
  (advice-add 'tab-bar-history-forward
              :after
              'my-tab-bar-history-position-indicator)
  (advice-add 'tab-bar-history-back
              :after
              'my-tab-bar-history-position-indicator))

(use-package saveplace
  :custom
  (save-place-mode t)
  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-forget-unreadable-files nil))

(use-package recentf
  :bind
  ("C-c f" . recentf-open)
  :custom
  (recentf-mode t)
  (recentf-max-saved-items 200))

(use-package savehist
  :custom
  (savehist-mode t)
  (savehist-autosave-interval nil)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(search-ring
     regexp-search-ring)))

(use-package compile
  :custom
  (next-error-message-highlight t)
  (compilation-scroll-output 'first-error))

(use-package diff-mode
  :hook
  (diff-mode . whitespace-mode)
  :custom
  (diff-refine 'font-lock))

(use-package ediff
  :custom
  (ediff-auto-refine 'on)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package autorevert
  :custom
  (global-auto-revert-mode t)
  ;; The first revert gets done after auto-revert-interval, even when using notifications
  (auto-revert-interval 10)
  ;; See function auto-revert--polled-buffers
  (auto-revert-avoid-polling t)
  (auto-revert-stop-on-user-input t)
  (auto-revert-remote-files nil)
  (auto-revert-verbose nil)
  (auto-revert-check-vc-info nil)
  (global-auto-revert-non-file-buffers t)
  (global-auto-revert-ignore-modes '(Buffer-menu-mode electric-buffer-menu-mode)))

(use-package tramp
  :config
  ;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  ;; Disable autosave for some methods
  (connection-local-set-profile-variables
   'no-remote-auto-save-profile
   '((buffer-auto-save-file-name . nil)))
  (dolist (protocol '("sudo" "doas" "su" "sudoedit" "sg" "ksu" "run0"))
    (connection-local-set-profiles
     `(:application tramp :protocol ,protocol)
     'no-remote-auto-save-profile))
  :custom
  (tramp-show-ad-hoc-proxies t)
  (tramp-use-scp-direct-remote-copying t)
  ;; https://stackoverflow.com/a/47021266
  (tramp-backup-directory-alist backup-directory-alist)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (vc-handled-backends '(Git))
  (debug-ignored-errors
   (cons 'remote-file-error debug-ignored-errors)))

(use-package icomplete
  :init
  (defun my-icomplete-minibuffer-setup ()
    ;; https://lists.gnu.org/archive/html/emacs-devel/2020-05/msg03432.html
    ;; https://www.reddit.com/r/emacs/comments/13enmhl/prioritize_exact_match_in_completion_styles/
    (setq-local completion-styles
                '(flex partial-completion))
    ;; https://lists.nongnu.org/archive/html/bug-gnu-emacs/2024-10/msg00743.html
    (add-hook 'post-command-hook
              (lambda ()
                ;; https://github.com/minad/vertico/blob/2.6/vertico.el#L627
                ;; https://github.com/minad/vertico/blob/2.6/vertico.el#L597
                (setq-local truncate-lines
                            (< (point) (* 0.85 (window-width)))))
              nil 'local))
  :config
  (when (boundp 'scroll-bar-mode)
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Scroll-Bars.html
    (defun my-disable-minibuffer-scrollbar ()
      (set-window-scroll-bars
       (minibuffer-window) 0 nil 0 nil t))
    (my-disable-minibuffer-scrollbar)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my-disable-minibuffer-scrollbar)))))
  ;; don't insert icomplete completion display if there's pending input
  (advice-add 'icomplete-exhibit :before-while (lambda () (sit-for 0)))
  :hook
  (icomplete-minibuffer-setup . my-icomplete-minibuffer-setup)
  :bind
  (:map icomplete-fido-mode-map
        ("C-s" . nil)
        ("C-r" . nil)
        :map completion-in-region-mode-map
        ("M-g M-c" . switch-to-completions)
        ("M-v" . switch-to-completions))
  :custom
  (fido-mode t)
  (fido-vertical-mode t)
  (minibuffer-default-prompt-format " [%.22s]")
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (completion-auto-select nil)
  (completions-max-height 12)
  (completion-show-help nil)
  (completion-auto-help 'visible)
  (suggest-key-bindings t)
  (completions-detailed t))

(use-package repeat
  :custom
  (repeat-mode t)
  (set-mark-command-repeat-pop t)
  (repeat-exit-key "<escape>"))

(use-package comint
  :config
  (when (boundp 'comint-repeat-map)
    (define-key comint-repeat-map (kbd "C-n") nil)
    (define-key comint-repeat-map (kbd "C-p") nil)))

(use-package em-prompt
  :config
  (when (boundp 'eshell-prompt-repeat-map)
    (define-key eshell-prompt-repeat-map (kbd "C-n") nil)
    (define-key eshell-prompt-repeat-map (kbd "C-p") nil)))

(use-package python
  :hook
  ;; Disable eldoc mode in Python buffers to avoid performance issues,
  ;; as it invokes `python-shell-get-process-name', which can be
  ;; slow due to project root lookups, particularly in remote buffers.
  ;; See Bug#80045.
  (python-mode . (lambda () (eldoc-mode -1))))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (eldoc-echo-area-display-truncation-message nil)
  :config
  (advice-add 'eldoc-display-in-echo-area :around
              (lambda (orig &rest args)
                ;; allow multiline just for emacs lisp
                (let ((eldoc-echo-area-use-multiline-p
                       (if (derived-mode-p '(emacs-lisp-mode
                                             inferior-emacs-lisp-mode))
                           eldoc-echo-area-use-multiline-p
                         nil)))
                  (apply orig args)))))

(use-package eglot
  :custom
  (eglot-events-buffer-config '(:size 0 :format full)))

(use-package calendar
  :custom
  (calendar-date-style 'european)
  (calendar-week-start-day 1)
  (calendar-christian-all-holidays-flag t))

(use-package holidays
  :custom
  (holiday-bahai-holidays nil)
  (holiday-oriental-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-hebrew-holidays nil))

(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c o" . org-open-at-point-global)
   :map org-mode-map
   ("C-c z" . org-toggle-link-display))
  :custom
  (org-startup-truncated nil)
  (org-html-validation-link nil)
  (org-edit-src-content-indentation 0)
  :config
  (add-to-list 'org-link-frame-setup '(file . find-file)))

(use-package outline
  :custom
  (outline-minor-mode-cycle t))

(use-package proced
  :custom
  (proced-filter 'all)
  (proced-enable-color-flag nil)
  (proced-show-remote-processes t)
  (proced-format 'short))

(use-package project
  :custom
  (project-buffers-viewer 'project-list-buffers-ibuffer)
  (project-vc-extra-root-markers '(".project.el")))

(use-package dired
  :custom
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-clean-up-buffers-too t)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-listing-switches "-alh")
  (dired-mouse-drag-files t)
  (dired-free-space 'separate)
  (dired-dwim-target t)
  :config
  (when (boundp 'dired-jump-map)
    (define-key dired-jump-map (kbd "j") nil))
  (require 'dired-x))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  (wdired-allow-to-redirect-links t))

;;;;; External Packages

(use-package magit
  :if (locate-library "magit")
  :hook
  (magit-diff-mode . (lambda () (setq truncate-lines nil)))
  (magit-status-mode . (lambda () (setq truncate-lines nil)))
  :custom
  (magit-section-initial-visibility-alist '((stashes . hide)
                                            (tags . hide)))
  (magit-buffer-name-format "*%M%v*:%t")
  (magit-uniquify-buffer-names t)
  (magit-define-global-key-bindings 'default)
  (magit-refresh-status-buffer nil)
  (magit-branch-direct-configure nil)
  (magit-diff-refine-hunk 'all)
  (magit-diff-refine-ignore-whitespace nil)
  (magit-auto-revert-mode nil)
  :config
  (setq magit-bind-magit-project-status nil)
  (setq magit-show-long-lines-warning nil))

(use-package dired-subtree
  :if (locate-library "dired-subtree")
  :after dired
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle))
  :custom
  (dired-subtree-line-prefix "    ")
  (dired-subtree-use-backgrounds nil))

;;;;; custom-file

(defconst custom-file
  (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;;; init.el ends here
