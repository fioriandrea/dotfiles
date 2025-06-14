(defun reload-config ()
  (interactive)
  (load-file user-init-file))

(defun open-config ()
  (interactive)
  (find-file user-init-file))

(defun tick (cmd)
  (let ((output (shell-command-to-string cmd)))
    (string-trim output)))

(defun delete-autosave-current-buffer ()
  (interactive)
  (when buffer-file-name
    (let ((auto-save-file (make-auto-save-file-name)))
      (when (file-exists-p auto-save-file)
        (delete-file auto-save-file)))))

(defun delete-autosave-opened-files ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (delete-autosave-current-buffer))))

;; https://www.emacswiki.org/emacs/AlarmBell
(defun subtly-flash-modeline-fg (&optional delay)
  (let ((orig-fg (face-foreground 'mode-line)))
    (set-face-foreground 'mode-line "#FF0000")
    (run-with-idle-timer (or delay 0.1) nil
                         (lambda (fg) (set-face-foreground 'mode-line fg))
                         orig-fg)))

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

(defconst custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package use-package
  :ensure nil
  :custom
  (use-package-always-ensure nil)
  (use-package-always-demand nil)
  (use-package-always-defer t))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;; https://elpa.gnu.org/devel/doc/use-package.html#The-emacs-package
(use-package emacs
  :bind
  ("C-x f" . nil)
  ("C-x C-b" . buffer-menu)
  :hook
  (text-mode . display-line-numbers-mode)
  (text-mode . visual-line-mode)
  (prog-mode . display-line-numbers-mode)
  :custom
  (global-so-long-mode t)
  (electric-pair-mode t)
  (sentence-end-double-space t)
  (ring-bell-function 'ignore)
  (truncate-lines nil)
  (truncate-partial-width-windows nil)
  (indent-tabs-mode nil)
  (menu-bar-mode t)
  (context-menu-mode t)
  (tool-bar-mode nil)
  (inhibit-startup-screen t)
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
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (auto-save-list-file-prefix (file-name-as-directory emacs-autosave-dir))
  (auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t)))
  (backup-directory-alist `(("." . ,emacs-backup-dir)))
  :init
  (setq-default confirm-kill-emacs nil)
  (add-hook 'kill-buffer-hook 'delete-autosave-current-buffer)
  ;; hack to delete auto-save files on C-x C-c
  (add-to-list 'kill-emacs-query-functions
               (lambda () (progn
                            (delete-autosave-opened-files)
                            t)))
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq-default bidi-inhibit-bpa t)
  (when scroll-bar-mode
    (set-window-scroll-bars (minibuffer-window) nil nil nil nil :persistent))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(use-package help
  :custom
  (help-window-select t)
  (help-window-keep-selected t))

(use-package uniquify
  :custom
  (uniquify-strip-common-suffix nil)
  (uniquify-after-kill-buffer-p t)
  ;; https://emacs.stackexchange.com/questions/68499/automatically-uniquify-certain-buffers
  (uniquify-min-dir-content 3)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-trailing-separator-p t))

(use-package isearch
  :init
  (defvar my-isearch-prev-ring-bell-function nil)
  :hook
  (isearch-mode . (lambda ()
                    (when (not my-isearch-prev-ring-bell-function)
                      (setq my-isearch-prev-ring-bell-function ring-bell-function)
                      (when (equal ring-bell-function 'ignore)
                        (setq ring-bell-function (lambda ()
                                                   (subtly-flash-modeline-fg 0.2)))))))
  (isearch-mode-end . (lambda ()
                        (when my-isearch-prev-ring-bell-function
                          (setq ring-bell-function my-isearch-prev-ring-bell-function)
                          (setq my-isearch-prev-ring-bell-function nil))))
  :custom
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  ;; `no-ding' makes keyboard macros never quit
  (isearch-wrap-pause 'no))

(use-package tab-bar
  :custom
  (tab-bar-show 1))

(use-package saveplace
  :custom
  (save-place-mode t)
  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-forget-unreadable-files nil))

(use-package recentf
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
   '(kill-ring
     search-ring
     regexp-search-ring)))

(use-package windmove
  :bind
  ("C-c <left>" . windmove-left)
  ("C-c <right>" . windmove-right)
  ("C-c <up>" . windmove-up)
  ("C-c <down>" . windmove-down)
  ("C-c w s" . window-swap-states)
  ("C-c w <left>" . windmove-swap-states-left)
  ("C-c w <right>" . windmove-swap-states-right)
  ("C-c w <up>" . windmove-swap-states-up)
  ("C-c w <down>" . windmove-swap-states-down))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package autorevert
  :custom
  (global-auto-revert-mode t)
  (auto-revert-verbose nil)
  (auto-revert-use-notify t)
  ;; see function auto-revert--polled-buffers
  (auto-revert-avoid-polling t)
  ;; https://lists.gnu.org/archive/html/emacs-devel/2014-10/msg00743.html
  ;; https://emacs.stackexchange.com/a/50134 (read comments)
  ;; The first revert gets done after auto-revert-interval, even when using notifications
  (auto-revert-interval 5)
  (global-auto-revert-non-file-buffers t)
  (global-auto-revert-ignore-modes '(Buffer-menu-mode electric-buffer-menu-mode)))

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
  (debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))

(use-package icomplete
  :bind (:map icomplete-fido-mode-map
              ("C-s" . nil)
              ("C-r" . nil))
  :custom
  (fido-mode t)
  (fido-vertical-mode t)
  (tab-always-indent 'complete)
  ;; (completion-auto-select 'second-tab)
  ;; (completion-auto-help 'always)
  (suggest-key-bindings t)
  (completions-detailed nil)
  :init
  ;; https://lists.gnu.org/archive/html/emacs-devel/2020-05/msg03432.html
  ;; https://www.reddit.com/r/emacs/comments/13enmhl/prioritize_exact_match_in_completion_styles/
  (defun my-icomplete-styles ()
    (setq-local completion-styles '(flex partial-completion)))
  (add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles))

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

(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-clean-up-buffers-too t)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-alh")
  (dired-mouse-drag-files t)
  (dired-dwim-target t))

(use-package dired-x
  :demand t
  :after dired)

(use-package evil
  :if (package-installed-p 'evil)
  ;; :demand t
  :bind (("C-c e" . evil-mode)
         :map evil-motion-state-map
         ("TAB" . nil)
         ("RET" . nil)
         ("<backtab>" . nil)
         ("C-f" . nil)
         ("C-o" . nil)
         ("C-b" . nil)
         ("C-y" . nil)
         ("C-e" . nil)
         ("M-n" . evil-ex-search-next)
         ("M-N" . evil-ex-search-previous)
         ("?" . evil-ex-search-backward)
         :map evil-normal-state-map
         ("M-." . nil)
         ("C-r" . nil)
         ("C-p" . nil)
         ("C-n" . nil)
         ("DEL" . nil))
  :hook
  ;; https://www.reddit.com/r/emacs/comments/gxzsjn/trying_to_have_minor_mode_key_bindings_for_edebug/
  (view-mode . evil-normalize-keymaps)
  :custom
  (evil-want-keybinding t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-want-integration t)
  (evil-want-minibuffer nil)
  (evil-want-C-i-jump nil)
  (evil-want-C-d-scroll nil)
  (evil-want-C-u-scroll nil)
  (evil-symbol-word-search t)
  (evil-default-state 'normal)
  (evil-emacs-state-modes '(term-mode))
  :init
  ;; Don't know why, but this cannot be under customize for some reason
  (setq evil-search-module 'evil-search)
  (setq evil-disable-insert-state-bindings t)
  :config
  ;; (evil-mode 1)
  (evil-set-undo-system 'undo-redo)

  (dolist (mode evil-emacs-state-modes)
    (evil-set-initial-state mode 'emacs))

  ;; https://emacs.stackexchange.com/a/13433
  (defun simulate-key-press (key)
    "Return a command that pretends KEY was presssed.
KEY must be given in `kbd' notation."
    `(lambda () (interactive)
       (setq prefix-arg current-prefix-arg)
       (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))
  (defvar-keymap my-space-map
    :parent ctl-x-map
    "c" (simulate-key-press "C-c")
    "x" 'execute-extended-command
    "SPC" 'execute-extended-command
    "f" 'find-file
    "j" 'dired-jump)
  (evil-define-key '(motion normal visual) 'global
                   (kbd "SPC") my-space-map)

  (defun my-evil-std-keys (state map)
    (evil-add-hjkl-bindings map state
                            "/"   'evil-ex-search-forward
                            "?"   'evil-ex-search-backward
                            "0"   'evil-beginning-of-line
                            "$"   'evil-end-of-line
                            (kbd "SPC") my-space-map
                            (kbd "M-n") 'evil-ex-search-next
                            (kbd "M-N") 'evil-ex-search-previous))

  ;; For minor modes, add hook
  ;; see https://www.reddit.com/r/emacs/comments/gxzsjn/trying_to_have_minor_mode_key_bindings_for_edebug/
  ;; and https://github.com/emacs-evil/evil/issues/301
  ;; and https://github.com/noctuid/evil-guide?tab=readme-ov-file#why-dont-keys-defined-with-evil-define-key-work-immediately
  (defconst my-evil-normal-overriding-modes '(completion-list-mode
                                              org-agenda-mode
                                              Info-mode
                                              occur-mode
                                              magit-mode
                                              view-mode
                                              special-mode
                                              diff-mode
                                              archive-mode
                                              Custom-mode
                                              custom-mode
                                              dired-mode
                                              compilation-mode))
  (defun my-evil-make-overriding-map (mode state)
    (let* ((map-name (format "%s-map" (symbol-name mode)))
           (map-sym (intern map-name))
           (fn-name (format "my-evil-make-overriding-map-to-%s" map-name)))
      (eval
       `(evil-with-delay
         (and
          (boundp ',map-sym)
          (keymapp ,map-sym))
         (after-load-functions t nil ,fn-name)
         (with-demoted-errors "Error in my-evil-apply-evil-std-keys-to: %S"
           (evil-make-overriding-map ,map-sym ',state))))))
  (dolist (mode my-evil-normal-overriding-modes)
    (evil-set-initial-state mode 'normal)
    (my-evil-make-overriding-map mode 'normal))

  ;; https://github.com/noctuid/evil-guide
  (defvar my-evil-intercept-mode-map (make-sparse-keymap)
    "High precedence keymap.")
  (define-minor-mode my-evil-intercept-mode
    "Global minor mode for higher precedence evil keybindings."
    :global t)
  (my-evil-intercept-mode)
  (dolist (state '(normal visual insert))
    (evil-make-intercept-map
     (evil-get-auxiliary-keymap my-evil-intercept-mode-map state t t)
     state))
  (my-evil-std-keys '(normal motion visual) my-evil-intercept-mode-map))

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
         ("C-x 4 b" . consult-buffer-other-window)
         :map icomplete-fido-mode-map
         ("C-k" . icomplete-consult-fido-kill))
  :config
  (consult-customize
   consult-buffer
   consult-buffer-other-window
   :preview-key nil
   :annotate (lambda (x) ""))
  (dolist (source '(consult--source-bookmark
                    consult--source-buffer-register
                    consult--source-file-register))
    (delete source consult-buffer-sources))
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

(defconst after-file (concat user-emacs-directory "after.el"))
(when (file-exists-p after-file)
  (load after-file))
