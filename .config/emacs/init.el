(defun my-reload-config ()
  (interactive)
  (load-file user-init-file))

(defun my-open-config ()
  (interactive)
  (find-file user-init-file))

(defun my-tick (cmd)
  (let ((output (shell-command-to-string cmd)))
    (string-trim output "" "[\n\r]+")))

(defun my-http-fetch-url (url &optional kwargs)
  (require 'url)
  (unless (or (string-prefix-p "http://" url)
              (string-prefix-p "https://" url))
    (error "URL must start with http:// or https://"))
  (let* ((method (cdr (assoc 'method kwargs)))
         (data (cdr (assoc 'data kwargs)))
         (headers (cdr (assoc 'headers kwargs)))
         (proxy (cdr (assoc 'proxy kwargs)))
         (url-request-method method)
         (url-request-data data)
         (url-request-extra-headers headers)
         (url-proxy-services (or proxy url-proxy-services)))
    (with-current-buffer (url-retrieve-synchronously url)
      (let ((response-headers
             (mapcar (lambda (line)
                       (let ((parts (split-string line ": ")))
                         (cons (car parts) (cadr parts))))
                     (split-string
                      (buffer-substring-no-properties (point-min) url-http-end-of-headers)
                      "\n" t)))
            (body (buffer-substring-no-properties url-http-end-of-headers (point-max))))
        (kill-buffer)
        `((headers . ,response-headers)
          (body . ,(string-trim body)))))))

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

;; https://emacs.stackexchange.com/a/13433
(defun my-simulate-key-press (key)
  "Return a command that pretends KEY was presssed.
KEY must be given in `kbd' notation."
  `(lambda () (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key)))))

;; https://www.emacswiki.org/emacs/AlarmBell
(defun my-subtly-flash-modeline-fg (&optional delay)
  (let ((orig-fg (face-foreground 'mode-line)))
    (set-face-foreground 'mode-line "#FF0000")
    (run-with-idle-timer (or delay 0.1) nil
                         (lambda (fg) (set-face-foreground 'mode-line fg))
                         orig-fg)))

(defun my-unpop-local-mark ()
  "Unpop off local mark ring."
  (interactive)
  (or (not (null (mark t)))
      (user-error "No mark set in this buffer"))
  (let* ((full-mark-ring (cons
                          (copy-marker (mark-marker))
                          mark-ring))
         (tl (last full-mark-ring 2))
         (trunc-ring (nbutlast full-mark-ring 2)))
    (setq full-mark-ring (nconc tl trunc-ring))
    (setq mark-ring (cdr full-mark-ring))
    (set-marker (mark-marker) (car full-mark-ring) (current-buffer))
    (pop-to-mark-command)))

(defun my-unpop-global-mark ()
  "Unpop off global mark ring."
  (interactive)
  (or global-mark-ring
      (error "No global mark set"))
  (let* ((tl (last global-mark-ring 2))
         (trunc-ring (nbutlast global-mark-ring 2)))
    (setq global-mark-ring (nconc tl trunc-ring))
    (pop-global-mark)))

(defun my-set-mark-command (arg)
  "Enhanced version of `set-mark-command' with wider support for prefix arguments."
  (interactive "P")
  (let ((arg-num (prefix-numeric-value arg)))
    (cond
     ((and
       (null arg)
       set-mark-command-repeat-pop
       (eq last-command 'my-unpop-local-mark))
      (setq this-command 'my-unpop-local-mark)
      (my-unpop-local-mark))
     ((and
       (null arg)
       set-mark-command-repeat-pop
       (eq last-command 'my-unpop-global-mark))
      (setq this-command 'my-unpop-global-mark)
      (my-unpop-global-mark))
     ((or
       (null arg)
       (equal arg '(4))
       (equal arg '(16)))
      (setq this-command 'set-mark-command)
      (set-mark-command arg))
     ((< arg-num 0)
      (setq this-command 'my-unpop-local-mark)
      (dotimes (_ (- arg-num))
        (my-unpop-local-mark)))
     ((>= arg-num 0)
      (setq this-command 'pop-to-mark-command)
      (dotimes (_ arg-num)
        (pop-to-mark-command))))))

(defun my-pop-global-mark (arg)
  "Enhanced version of `pop-global-mark' with support for prefix arguments."
  (interactive "P")
  (let ((arg-num (prefix-numeric-value arg)))
    (cond
     ((< arg-num 0)
      (setq this-command 'my-unpop-global-mark)
      (dotimes (_ (- arg-num))
        (my-unpop-global-mark)))
     ((>= arg-num 0)
      (setq this-command 'pop-global-mark)
      (dotimes (_ arg-num)
        (pop-global-mark))))))

(defmacro my-use-package (pack &rest args)
  "Minimal replacement for `use-package' with restricted
functionality.  Supports only the keywords: :custom, :config, :init,
:demand, :if, and :when.  Defaults are: :ensure nil, :demand nil, and
:defer t.  Unlike standard `use-package', this macro expands to code
that is evaluated only if the specified feature PACK is already
provided or corresponds to a loadable library."
  (declare (indent defun))
  (let (customs configs demand inits condition)
    (setq condition `((or
                       (featurep ',pack)
                       (locate-library ,(symbol-name pack)))))
    (while (and args (cdr args))
      (let (key val)
        (setq key (pop args))
        (setq val (pop args))
        (cond
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
    (let (body)
      (when configs
        (push
         `(eval-after-load ',pack
            (quote ,(cons 'progn (nreverse configs))))
         body))
      (when demand
        (push
         `(require ',pack nil nil) body))
      (when inits
        (push `(progn ,@(nreverse inits)) body))
      (when customs
        (push
         ;; see use-package-handler/:custom
         `(let ((custom--inhibit-theme-enable nil))
            (unless (memq 'my-use-package custom-known-themes)
              (deftheme my-use-package) (enable-theme 'my-use-package)
              (setq custom-enabled-themes
                    (remq 'my-use-package custom-enabled-themes)))
            (custom-theme-set-variables
             'my-use-package
             ,@(mapcar (lambda (p)
                         (let ((variable (nth 0 p))
                               (value (nth 1 p))
                               (comment (or (nth 2 p)
                                            (format
                                             "Customized with my-use-package %s"
                                             (symbol-name pack)))))
                           `'(,variable ,value nil nil ,comment)))
                       (nreverse customs))))
         body))
      `(when (and ,@condition) ,@body))))

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

(unless (fboundp 'advice-add)
  (message "WARNING: no advice-add found, using compatibility shim")
  (defmacro advice-add (&rest body)))

(unless (fboundp 'use-package)
  (message "WARNING: no use-package found, using compatibility shim")
  (defalias 'use-package 'my-use-package))

(use-package use-package
  :ensure nil
  :custom
  (use-package-always-ensure nil)
  (use-package-always-demand nil)
  (use-package-always-defer t)
  (use-package-expand-minimally t)
  (use-package-use-theme t))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;; https://elpa.gnu.org/devel/doc/use-package.html#The-emacs-package
(use-package emacs
  :bind
  ("C-x f" . nil)
  ("C-x C-b" . buffer-menu)
  ([remap pop-global-mark] . my-pop-global-mark)
  ([remap set-mark-command] . my-set-mark-command)
  :hook
  (text-mode . visual-line-mode)
  :custom
  (inhibit-startup-screen t)
  (tool-bar-mode nil)
  (menu-bar-mode t)
  (next-error-message-highlight t)
  (debugger-stack-frame-as-list t)
  (confirm-kill-emacs nil)
  (native-comp-async-report-warnings-errors 'silent)
  (set-mark-command-repeat-pop t)
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
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (auto-save-list-file-prefix (file-name-as-directory emacs-autosave-dir))
  (auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t)))
  (backup-directory-alist `(("." . ,emacs-backup-dir)))
  :init
  (add-hook 'kill-buffer-hook 'my-delete-autosave-current-buffer)
  ;; hack to delete auto-save files on C-x C-c
  (add-to-list 'kill-emacs-query-functions
               (lambda () (progn
                            (my-delete-autosave-opened-files)
                            t)))
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq-default bidi-inhibit-bpa t)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(use-package help
  :custom
  (help-window-select t)
  (help-window-keep-selected t))

(use-package tabulated-list
  :hook (tabulated-list-mode . hl-line-mode))

(use-package whitespace
  :bind
  ("C-c s m" . whitespace-mode)
  ("C-c s d" . (lambda ()
                 (interactive)
                 (delete-trailing-whitespace)
                 (message "Trailing whitespace deleted"))))

(use-package window
  :bind ("C-c x" . window-swap-states)
  :custom
  (split-height-threshold nil)
  (split-width-threshold nil))

(use-package uniquify
  :custom
  (uniquify-strip-common-suffix nil)
  (uniquify-after-kill-buffer-p t)
  ;; https://emacs.stackexchange.com/questions/68499/automatically-uniquify-certain-buffers
  (uniquify-min-dir-content 3)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-trailing-separator-p t))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  ;; `no-ding' makes keyboard macros never quit
  (isearch-wrap-pause t))

(use-package tab-bar
  :custom
  (tab-bar-show 1)
  (tab-bar-format '(tab-bar-format-tabs
                    tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width t)
  (tab-bar-new-tab-choice t)
  (tab-bar-history-mode t)
  (tab-bar-history-limit 30)
  :config
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
  (advice-add 'tab-bar-history-forward :after 'my-tab-bar-history-position-indicator)
  (advice-add 'tab-bar-history-back :after 'my-tab-bar-history-position-indicator))

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
  :hook (diff-mode . whitespace-mode)
  :custom
  (diff-refine 'font-lock))

(use-package ediff
  :custom
  (ediff-auto-refine 'on)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package autorevert
  :init
  (if (version< emacs-version "27.1")
      (message "WARNING: unsupported auto-revert on window change")
    (defun my-auto-revert-buffer-h ()
      "Automatically revert the current buffer if not already managed by
auto-revert. Taken from doomemacs."
      (require 'autorevert)
      (unless (or (active-minibuffer-window)
                  (auto-revert-active-p)
                  (memq major-mode global-auto-revert-ignore-modes)
                  (and (not buffer-file-name)
                       (or
                        (not global-auto-revert-non-file-buffers)
                        (string-prefix-p " " (buffer-name))
                        (eq buffer-stale-function
                            #'buffer-stale--default-function)))
                  (if (functionp global-auto-revert-ignore-buffer)
                      (not (funcall global-auto-revert-ignore-buffer
                                    (current-buffer)))
                    global-auto-revert-ignore-buffer)
                  (and buffer-file-name
                       auto-revert-remote-files
                       (file-remote-p buffer-file-name nil t)))
        (let ((auto-revert-mode t))
          (auto-revert-handler))))
    (add-hook 'window-selection-change-functions (lambda (_)
                                                   (my-auto-revert-buffer-h)))
    (add-hook 'window-buffer-change-functions (lambda (_)
                                                (my-auto-revert-buffer-h)))
    ;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
    (add-hook 'server-visit-hook 'my-auto-revert-buffer-h))
  :custom
  (global-auto-revert-mode nil)
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
  :custom
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
  ;; https://emacs.stackexchange.com/questions/78644/how-to-tell-tramp-to-not-ask-me-about-autosave-on-local-directory
  ;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
  (tramp-auto-save-directory emacs-autosave-dir)
  ;; https://stackoverflow.com/a/47021266
  (tramp-backup-directory-alist backup-directory-alist)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))

(use-package icomplete
  :init
  ;; https://github.com/minad/vertico/blob/2.3/vertico.el#L590
  (defun my-icomplete-minibuffer-truncate-lines-hook ()
    (setq-local truncate-lines (< (point) (* 0.8 (window-width)))))
  (defun my-icomplete-minibuffer-setup ()
    ;; https://lists.gnu.org/archive/html/emacs-devel/2020-05/msg03432.html
    ;; https://www.reddit.com/r/emacs/comments/13enmhl/prioritize_exact_match_in_completion_styles/
    (setq-local completion-styles '(flex partial-completion))
    ;; https://lists.nongnu.org/archive/html/bug-gnu-emacs/2024-10/msg00743.html
    (add-hook 'post-command-hook #'my-icomplete-minibuffer-truncate-lines-hook nil 'local))
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Scroll-Bars.html
  (defun my-disable-minibuffer-scrollbar ()
    (set-window-scroll-bars
     (minibuffer-window) 0 nil 0 nil t))
  :config
  (when (boundp 'scroll-bar-mode)
    (my-disable-minibuffer-scrollbar)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my-disable-minibuffer-scrollbar)))))
  :hook (icomplete-minibuffer-setup . my-icomplete-minibuffer-setup)
  :bind (:map icomplete-fido-mode-map
              ("C-s" . nil)
              ("C-r" . nil)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              :map completion-in-region-mode-map
              ("M-g M-c" . switch-to-completions)
              ("M-v" . switch-to-completions))
  :custom
  (fido-mode t)
  (fido-vertical-mode t)
  (minibuffer-default-prompt-format "")
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (completion-auto-select nil)
  (completions-max-height 12)
  (completion-show-help nil)
  (completion-auto-help t)
  (suggest-key-bindings t)
  (completions-detailed nil))

(use-package ffap
  :custom
  (ffap-require-prefix t)
  (dired-at-point-require-prefix t)
  :init
  (ffap-bindings))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package eglot
  :custom
  (eglot-events-buffer-size 0)
  (eglot-events-buffer-config '(:size 0 :format full)))

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

(use-package magit
  :if (locate-library "magit")
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

(defconst after-file (concat user-emacs-directory "after.el"))
(when (file-exists-p after-file)
  (load after-file))
