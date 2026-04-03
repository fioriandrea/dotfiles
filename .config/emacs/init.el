;;; init.el --- Emacs configuration -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) Andrea Fiori
;; This file is NOT part of GNU Emacs.
;; This file is free software.

;; Author: Andrea Fiori
;; License: GPLv3

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

(require 'cl-lib)

;;;; Functions

;;;;; Miscellanea

(defun my-open-config ()
  (interactive)
  (find-file user-init-file))

(defun my-delete-autosave-current-buffer ()
  (interactive)
  (when buffer-file-name
    (let ((auto-save-file (make-auto-save-file-name)))
      (when (file-exists-p auto-save-file)
        (delete-file auto-save-file)))))

(defun my-delete-autosave-opened-files ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (my-delete-autosave-current-buffer))))

(defun my-kill-buffer-skip-hooks (buffer)
  (interactive "bBuffer: ")
  (with-current-buffer buffer
    (let (kill-buffer-hook kill-buffer-query-functions)
      (kill-buffer))))

(defun my-read-directory-name-default (&optional prompt)
  (expand-file-name
   (read-directory-name (or prompt "Base directory: ")
		        default-directory nil t)))

(defun my-file-name-from-context ()
  (or
   (thing-at-point 'filename)
   (file-name-nondirectory
    (buffer-file-name))))

(defun my-read-regexp-default (&optional prompt)
  (read-regexp (or prompt "Search for")
               'find-tag-default-as-regexp
               'grep-regexp-history))

;;;;; Find

(defun my-find-files (directory regexp &optional dir-filter)
  (setq directory (directory-file-name directory))
  (let ((results nil)
        (full-files nil)
        (files (condition-case err
                   (directory-files
                    directory nil
                    directory-files-no-dot-files-regexp)
                 (file-error (message
                              "my-find-files: couldn't get files for dir %S because of %S"
                              directory err)
                             nil))))
    (dolist (file files)
      (let ((full-file (concat directory "/" file)))
        (if (file-directory-p full-file)
            (unless (or (file-symlink-p full-file)
                        (and dir-filter (not (funcall dir-filter directory file))))
              (setq results (nconc results (my-find-files full-file regexp dir-filter))))
          (when (string-match regexp file)
	    (push full-file full-files)))))
    (nconc results (nreverse full-files))))

(defun my-find-files-excluding-vc (directory regexp)
  (my-find-files directory regexp
                 (lambda (parent file)
                   (not (member file vc-directory-exclusion-list)))))

(defmacro my-with-project-files-fallback (&rest body)
  (declare (indent 0))
  (let ((orig (make-symbol "orig")))
    `(cl-letf* ((,orig (symbol-function 'project-files))
                ((symbol-function 'project-files)
                 (lambda (project &optional dirs)
                   (condition-case err
                       (apply ,orig (list project dirs))
                     (error
                      (message "project-files error: %S" err)
                      (mapcan (lambda (d)
                                (my-find-files-excluding-vc d "."))
                              (or dirs (list (project-root project)))))))))
       ,@body)))

(defun my-project-find-file (&optional include-all)
  (interactive "P")
  (my-with-project-files-fallback
    (project-find-file include-all)))

(defun my-project-find-dir ()
  (interactive)
  (my-with-project-files-fallback
    (project-find-dir)))

;;;;; Grep

(require 'xref)

(defun my-grep-files (files regexp)
  (cl-labels
      ((process-files-and-dirs (files)
         (cl-loop for file in files nconc
                  (condition-case err
                      (if (file-directory-p file)
                          (process-files-and-dirs (my-find-files file "."))
                        (let ((res (process-one-file file)))
                          (when res
                            (list (cons file res)))))
                    (file-error
                     (message "my-grep-files: failed to grep %S because of %S"
                              file err)
                     nil))))
       (process-one-file (file)
         (insert-file-contents file nil nil nil 'if-regular)
         (goto-char (point-min))
         (let ((lines (list (cons -1 "")))
               (matches nil))
           (while (and (not (eobp))
                       (re-search-forward regexp nil t))
             (let* ((line (line-number-at-pos))
                    (line-beg (line-beginning-position))
                    (match-beg (match-beginning 0))
                    (match-line-start (- match-beg line-beg))
                    (match-len (- (match-end 0) match-beg)))
               (when (/= (caar lines) line)
                 (push (cons line (buffer-substring-no-properties
                                   line-beg (line-end-position)))
                       lines))
               (push (cons line (list (cons :match-line-start match-line-start)
                                      (cons :match-len match-len)))
                     matches)
               (when (= match-len 0)
                 (forward-char 1))))
           (group-matches lines matches)))
       (group-matches (lines matches)
         (let ((grouped (make-hash-table)))
           (cl-loop for (linenr . match) in matches do
                    (push match
                          (gethash linenr grouped '())))
           (cl-loop with matches-by-line = ()
                    for (linenr . linetext) in lines
                    unless (= linenr -1)
                    do (push (list
                              (cons :line linenr)
                              (cons :text linetext)
                              (cons :matches (gethash linenr grouped)))
                             matches-by-line)
                    finally return matches-by-line))))
    (with-temp-buffer
      (process-files-and-dirs files))))

(defun my-grep-matches-to-xref (matches)
  (cl-loop
   for (file . file-matches) in matches
   nconc (cl-loop
          for file-match in file-matches
          for line = (alist-get :line file-match)
          for text = (alist-get :text file-match)
          for textlen = (length text)
          for line-matches = (alist-get :matches file-match)
          nconc (nreverse
                 (cl-loop
                  with prev-start = textlen
                  with first-match = (car line-matches)
                  with first-start = (alist-get :match-line-start first-match)
                  for line-match in (nreverse line-matches)
                  for start = (alist-get :match-line-start line-match)
                  for len = (alist-get :match-len line-match)
                  for end = (+ start len)
                  for sumstart = (if (= start first-start)
                                     0 start)
                  for summary = (substring text sumstart prev-start)
                  do (add-face-text-property
                      (- start sumstart) (- (+ start len) sumstart)
                      'xref-match t summary)
                  (setq prev-start start)
                  collect (xref-make-match summary
                                           (xref-make-file-location
                                            file line start)
                                           len))))))

(defun my-grep-xrefs-show (regexp files)
  (let ((fetcher (lambda (regexp files)
                   (unless files
                     (user-error "Empty file list"))
                   (let* ((matches (my-grep-files files regexp))
                          (xrefs (my-grep-matches-to-xref matches)))
                     (unless xrefs
                       (user-error "No matches for: %s" regexp))
                     xrefs))))
    (xref-show-xrefs (apply-partially fetcher regexp files) nil)))

(defvar my-grep-file-regexp-history nil)

(defun my-grep-read-file-regexp ()
  (read-regexp "File name regexp"
               "." 'my-grep-file-regexp-history))

(defun my-rgrep (regexp file-regexp dir)
  (interactive
   (list
    (my-read-regexp-default)
    (my-grep-read-file-regexp)
    (my-read-directory-name-default)))
  (my-grep-xrefs-show
   regexp (my-find-files-excluding-vc dir file-regexp)))

(defun my-project-find-regexp (regexp)
  (interactive (list (my-read-regexp-default)))
  (if current-prefix-arg
      (let ((directory (my-read-directory-name-default))
            (file-regexp (my-grep-read-file-regexp)))
        (my-rgrep regexp file-regexp directory))
    (let* ((pr (project-current t))
           (default-directory (project-root pr))
           (files (my-with-project-files-fallback
                    (project-files pr))))
      (my-grep-xrefs-show regexp files))))

(defun my-dired-do-find-regexp (regexp)
  (interactive (list (read-regexp "Find regexp"
                                  nil 'dired-regexp-history))
               dired-mode)
  (my-grep-xrefs-show regexp (dired-get-marked-files)))

(defun my-dired-do-occur (regexp &optional nlines)
  (interactive (occur-read-primary-args) dired-mode)
  (multi-occur
   (mapcar #'find-file-noselect (dired-get-marked-files))
   regexp nlines))

;;;;; my-use-package

(defmacro my-use-package (pack &rest args)
  "Minimal `use-package' variant supporting a limited set of options.
Expands only if PACK names a provided feature or loadable library.
The expanded code catches any error during package setup."
  (declare (indent defun))
  (let (customs
        disabled
        configs
        demand
        inits
        condition)
    (setq condition
          `((or
             (featurep ',pack)
             (locate-library ,(symbol-name pack)))))
    (while (and args (cdr args))
      (let (key val)
        (setq key (pop args))
        (setq val (pop args))
        (cond
         ((eq key :disabled)
          (setq disabled t)
          (setq args nil))
         ((or (eq key :if) (eq key :when))
          (push val condition))
         ((eq key :demand)
          (setq demand val))
         ((eq key :custom)
          (cond
           ((and val (listp val) (listp (car val)))
            (setq customs (append customs val)))
           ((and val (listp val))
            (push val customs)
            (push :custom args))
           (t
            (push val args))))
         ((eq key :config)
          (cond
           ((not (keywordp val))
            (push val configs)
            (push :config args))
           (t
            (push val args))))
         ((eq key :init)
          (cond
           ((not (keywordp val))
            (push val inits)
            (push :init args))
           (t
            (push val args))))
         (t (push val args)))))
    (unless disabled
      `(when (condition-case-unless-debug nil
                 (and ,@condition)
               (error nil))
         ;; :custom
         ,(when customs
            `(progn
               ,@(mapcar
                  (lambda (x)
                    (let ((variable (nth 0 x))
                          (value (nth 1 x))
                          (comment (or (nth 2 x)
                                       (format
                                        "Customized with my-use-package %s"
                                        (symbol-name pack)))))
                      `(condition-case-unless-debug err
                           (customize-set-variable
                            ',variable
                            ,value
                            ,comment)
                         (error
                          (warn "Failed to customize %S to %S because of %S"
                                ',variable ',value err)))))
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
   (expand-file-name "autosave" user-emacs-directory)))
(unless (file-directory-p my-emacs-autosave-dir)
  (make-directory my-emacs-autosave-dir 'parents))

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
  (next-error-message-highlight t)
  (debugger-stack-frame-as-list t)
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
  :init
  (add-hook 'kill-buffer-hook 'my-delete-autosave-current-buffer)
  ;; hack to delete auto-save files on C-x C-c
  (add-to-list 'kill-emacs-query-functions
               (lambda () (progn
                            (my-delete-autosave-opened-files)
                            t)))
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
  (unless (version< emacs-version "30.1")
    (connection-local-set-profile-variables
     'remote-direct-async-process
     '((tramp-direct-async-process . t)))
    (connection-local-set-profiles
     '(:application tramp :protocol "scp")
     'remote-direct-async-process))
  :custom
  (tramp-show-ad-hoc-proxies t)
  (tramp-use-scp-direct-remote-copying t)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
  ;; https://emacs.stackexchange.com/questions/78644/how-to-tell-tramp-to-not-ask-me-about-autosave-on-local-directory
  ;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
  (tramp-auto-save-directory my-emacs-autosave-dir)
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
        ;; built-in for fido-vertical-mode, but not for fido-mode
        ("C-n" . icomplete-forward-completions)
        ("C-p" . icomplete-backward-completions)
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

(use-package eldoc
  :hook
  ((emacs-lisp-mode ielm-mode) . eldoc-mode)
  :config
  (advice-add 'eldoc-display-in-echo-area :around
              (lambda (orig &rest args)
                ;; allow multiline just for emacs-lisp
                (let ((eldoc-echo-area-use-multiline-p
                       (if (derived-mode-p '(emacs-lisp-mode
                                             inferior-emacs-lisp-mode))
                           'truncate-sym-name-if-fit
                         eldoc-echo-area-use-multiline-p)))
                  (apply orig args))))
  :custom
  (global-eldoc-mode nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eglot
  :custom
  (eglot-events-buffer-config '(:size 0 :format full)))

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
