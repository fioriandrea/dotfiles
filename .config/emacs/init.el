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
  :bind
  ("C-x f" . nil)
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

(use-package window
  :custom
  ;; (pop-up-windows nil)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window-Options.html#index-pop_002dup_002dwindows_002c-replacement-for
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-same-window
      display-buffer-in-previous-window
      display-buffer-use-some-window))))

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package evil
  :if (package-installed-p 'evil)
  :demand t
  :bind (("<escape>" . keyboard-escape-quit)
         ("C-c e" . evil-mode)
         :map evil-motion-state-map
         ("TAB" . nil)
         ("RET" . nil)
         ("<backtab>" . nil)
         ("C-f" . nil)
         ("C-b" . nil)
         ("C-y" . nil)
         ("C-e" . nil)
         ("C-o" . nil)
         ("M-n" . evil-ex-search-next)
         ("M-N" . evil-ex-search-previous)
         ("?" . evil-ex-search-backward)
         :map evil-normal-state-map
         ("M-." . nil)
         ("C-r" . nil)
         ("C-p" . nil)
         ("C-n" . nil)
         ("DEL" . nil))
  :custom
  (evil-want-keybinding nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-overriding-maps nil)
  (evil-want-integration nil)
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
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)

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

  (dolist (mode evil-emacs-state-modes)
    (evil-set-initial-state mode 'emacs))

  (defvar my-evil-normal-overriding-modes (cl-union
                                           evil-motion-state-modes
                                           '(completion-list-mode
                                             org-agenda-mode
                                             occur-mode
                                             magit-mode
                                             help-mode
                                             Info-mode
                                             special-mode
                                             diff-mode
                                             archive-mode
                                             Custom-mode
                                             dired-mode
                                             compilation-mode)))
  (defun my-evil-std-keys (state map)
    (evil-add-hjkl-bindings map state
      "/"   'evil-ex-search-forward
      "?"   'evil-ex-search-backward
      "0"   'evil-beginning-of-line
      "$"   'evil-end-of-line
      (kbd "SPC") my-space-map
      (kbd "M-n") 'evil-ex-search-next
      (kbd "M-N") 'evil-ex-search-previous))
  (defun my-evil-apply-evil-std-keys-to-mode (mode)
    (let* ((map-name (format "%s-map" (symbol-name mode)))
           (map-sym (intern map-name))
           (fn-name (format "my-evil-apply-evil-std-keys-to-%s" map-name)))
      (eval
       `(evil-with-delay
            (and
             (boundp ',map-sym)
             (keymapp ,map-sym))
            (after-load-functions t nil ,fn-name)
          (with-demoted-errors "Error in my-evil-apply-evil-std-keys-to-mode: %S"
            (evil-make-overriding-map ,map-sym)
            (my-evil-std-keys '(normal motion visual) ,map-sym))))))
  (dolist (mode my-evil-normal-overriding-modes)
    (evil-set-initial-state mode 'normal)
    (my-evil-apply-evil-std-keys-to-mode mode)))

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
  :init
  (windmove-default-keybindings)
  (windmove-swap-states-default-keybindings '(shift control)))

(use-package hippie-exp
  :bind
  ("C-c h" . hippie-expand))

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
         ("C-c f" . consult-fd)
         ("C-c g" . consult-ripgrep)
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
