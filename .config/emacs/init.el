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

(defmacro my-use-package (pack &rest args)
  "Fallback replacement for `use-package' on older Emacs versions.

Provides a minimal subset of `use-package' when it isn't available.
Expands only when PACK is loadable, and reports setup errors."
  (declare (indent defun))
  (let ((cur nil)
        (conds (list `(or (featurep ',pack)
                          (locate-library ,(symbol-name pack)))))
        customs inits configs)
    (while args
      (let ((x (pop args)))
        (if (keywordp x)
            (setq cur x)
          (cond ((memq cur '(:if :when)) (push x conds))
                ((eq cur :custom) (push x customs))
                ((eq cur :init) (push x inits))
                ((eq cur :config) (push x configs))))))
    (let (flat)
      (dolist (it (nreverse customs))
        (setq flat (append flat (if (and (consp it) (consp (car it))) it (list it)))))
      (setq customs flat))
    (let* ((try (lambda (kw &rest body)
                  `(condition-case-unless-debug err
                       (progn ,@body)
                     (error
                      (display-warning 'my-use-package
                                       (format "%s/%s: %s" ',pack ,kw
                                               (error-message-string err))
                                       :error)
                      nil)))))
      `(when ,(funcall try :if `(and ,@(nreverse conds)))
         ,@(mapcar (lambda (x)
                     (funcall try :custom
                              `(customize-set-variable ',(car x) ,@(cdr x))))
                   customs)
         ,@(when inits (list (apply try :init (nreverse inits))))
         ,@(when configs `((eval-after-load ',pack
                             ',(apply try :config (nreverse configs)))))))))

(unless (fboundp 'use-package)
  (message "No use-package found, using compatibility shim")
  (defalias 'use-package 'my-use-package))

;;;; Built-in Packages

(defconst my-emacs-auxiliary-files-dir
  (file-name-as-directory
   (abbreviate-file-name
    (expand-file-name "auxiliary-files" user-emacs-directory))))
(make-directory my-emacs-auxiliary-files-dir 'parents)

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
  (use-short-answers t)
  (frame-inhibit-implied-resize t)
  (save-interprogram-paste-before-kill t)
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
  (auto-save-file-name-transforms `((".*" ,my-emacs-auxiliary-files-dir t)))
  (lock-file-name-transforms `((".*" ,my-emacs-auxiliary-files-dir t)))
  (backup-directory-alist `(("." . ,my-emacs-auxiliary-files-dir)))
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

(use-package mouse
  :custom
  (xterm-mouse-mode t)
  (context-menu-mode t))

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
  (:prefix-map my-whitespace-map
               :prefix "C-c s"
               ("s" . whitespace-mode)
               ("d" . delete-trailing-whitespace))
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
  (:repeat-map my-repeat-tab-bar-history-map
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
  (extended-command-suggest-shorter nil)
  (suggest-key-bindings t)
  (completions-detailed t))

(use-package repeat
  :custom
  (repeat-mode t)
  (set-mark-command-repeat-pop t)
  (repeat-exit-key "<escape>"))

(use-package comint
  :config
  (define-key comint-repeat-map (kbd "C-n") nil)
  (define-key comint-repeat-map (kbd "C-p") nil))

(use-package em-prompt
  :config
  (define-key eshell-prompt-repeat-map (kbd "C-n") nil)
  (define-key eshell-prompt-repeat-map (kbd "C-p") nil))

(use-package python
  :hook
  ;; Disable eldoc mode in remote Python buffers to avoid performance
  ;; issues, as it invokes `python-shell-get-process-name', which can
  ;; be slow due to project root lookups, particularly in remote
  ;; buffers.  See Bug#80045.
  (python-mode . (lambda () (when (file-remote-p default-directory)
                              (eldoc-mode -1)))))

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
   ("C-c L" . org-insert-link-global)
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
  (require 'dired-x)
  (define-key dired-jump-map (kbd "j") nil))

(use-package wdired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  (wdired-allow-to-redirect-links t))

;;;; External Packages

(use-package magit
  :if (locate-library "magit")
  :bind (:map project-prefix-map
              ("m" . magit-project-status))
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

;;;; custom-file

(defconst custom-file
  (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;;; init.el ends here
