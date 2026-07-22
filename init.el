;;; init.el      -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t)

(use-package emacs
  :init
  (progn
    (setq inhibit-startup-screen t)
    (setq initial-scratch-message nil)
    (setq enable-recursive-minibuffers t)
    (setq use-short-answers t)
    (setq scroll-conservatively 101)
    (setq scroll-preserve-screen-position t)
    (setq gc-cons-threshold (* 128 1024 1024))
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)
    (setq bidi-inhibit-bpa t)
    (setq redisplay-skip-fontification-on-input t)
    (setq read-process-output-max (* 4 1024 1024))
    (setq ffap-machine-p-known 'reject)
    (setq-default truncate-lines t)
    (when (eq system-type 'darwin)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'meta))
    (when (and (display-graphic-p)
	       (null (assq 'font default-frame-alist))
	       (find-font (font-spec :name "JetBrains Mono NL")))
      (set-face-attribute 'default nil :font "JetBrains Mono NL-13")
      (add-to-list 'default-frame-alist '(font . "JetBrains Mono NL-13")))))

(use-package custom
  :config
  (progn
    (setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
    (when (file-exists-p custom-file)
      (load custom-file))))

(defun beginning-of-line-or-indentation ()
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    (when (= (point) start)
      (move-beginning-of-line nil))))

(use-package simple
  :init
  (progn
    (setq indent-tabs-mode nil)
    (setq save-interprogram-paste-before-kill t)
    (setq kill-do-not-save-duplicates t))
  :config
  (progn
    (keymap-global-set
     "<remap> <move-beginning-of-line>"
     #'beginning-of-line-or-indentation)
    (column-number-mode)))

(use-package files
  :init
  (progn
    (let ((backup-dir (expand-file-name "~/.emacs.d/var/backups/"))
          (auto-save-dir (expand-file-name "~/.emacs.d/var/auto-save/"))
          (auto-save-list-dir
           (expand-file-name "~/.emacs.d/var/auto-save-list/")))
      (make-directory backup-dir t)
      (make-directory auto-save-dir t)
      (make-directory auto-save-list-dir t)
      (setq backup-directory-alist `((".*" . ,backup-dir)))
      (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
      (setq auto-save-list-file-prefix (concat auto-save-list-dir ".saves-"))
      (setq backup-by-copying t)
      (setq version-control t)
      (setq delete-old-versions t)
      (setq kept-new-versions 6)
      (setq kept-old-versions 2)
      (setq create-lockfiles nil)
      ;; Buck2 things... pretend Starlark is Python
      (add-to-list 'auto-mode-alist '("BUCK\\'" . python-mode))
      (add-to-list 'auto-mode-alist '("PACKAGE\\'" . python-mode)))))

(use-package whitespace
  :init
  (progn
    (setq whitespace-style '(face trailing lines-tail tab-mark))
    (setq whitespace-line-column 88))
  :config (whitespace-mode))

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode))

(use-package frame
  :config (blink-cursor-mode -1))

(use-package help
  :init (setq help-window-select t))

(use-package autorevert
  :init (setq global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode))

(use-package savehist
  :init (setq savehist-file (expand-file-name "~/.emacs.d/var/history"))
  :config (savehist-mode))

(use-package saveplace
  :init (setq save-place-file (expand-file-name "~/.emacs.d/var/places"))
  :config (save-place-mode))

(use-package recentf
  :init (setq recentf-save-file (expand-file-name "~/.emacs.d/var/recentf"))
  :config (recentf-mode))

(use-package delsel
  :config (delete-selection-mode))

(use-package windmove
  :config (windmove-default-keybindings))

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

(use-package compat
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package direnv
  :ensure t
  :config (direnv-mode))

(use-package project
  :init (setq project-list-file (expand-file-name "~/.emacs.d/var/projects")))

(use-package diminish
  :ensure t)

(use-package eldoc
  :diminish eldoc-mode)

(use-package helpful
  :ensure t
  :bind (("C-h f"   . helpful-callable)
         ("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-h x"   . helpful-command)
         ("C-c C-d" . helpful-at-point)))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package doom-themes
  :ensure t
  ;; :init (load-theme 'doom-one t)
  )

(use-package tokyo-night
  :ensure t
  :init (load-theme 'tokyo-night-moon t))

(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

(use-package haskell-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package eglot
  :ensure t
  :hook ((haskell-mode . eglot-ensure))
  :init (customize-set-variable
         'eglot-ignored-server-capabilities
         '(:inlayHintProvider))
  :config (add-to-list 'eglot-server-programs '(haskell-mode . ("static-ls")))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)))

(use-package apheleia
  :ensure t
  :config
  (progn
    (add-to-list 'apheleia-formatters '(treefmt "treefmt" "--stdin" filepath))
    (apheleia-global-mode))
  :diminish apheleia-mode)

(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-strict-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-strict-mode)
    (sp-local-pair 'minibuffer-mode "'" nil :actions nil)
    (sp-local-pair 'minibuffer-mode "`" nil :actions nil)
    (sp-use-paredit-bindings)
    (define-key smartparens-mode-map (kbd "M-e") #'sp-forward-sexp)
    (define-key smartparens-mode-map (kbd "M-a") #'sp-backward-sexp)
    (define-key smartparens-mode-map (kbd "M-k") #'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd "M-k") #'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd "ESC M-DEL") #'sp-backward-kill-sexp)
    (define-key smartparens-mode-map (kbd "C-M-u") #'sp-backward-up-sexp)
    (define-key smartparens-mode-map (kbd "M-q") #'sp-indent-defun)
    ;; Don't shadow xref-find-references
    (define-key smartparens-mode-map (kbd "M-?") nil)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :init (setq mc/list-file (expand-file-name "~/.emacs.d/var/mc-lists.el"))
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (progn
    (customize-set-variable 'completion-styles '(orderless basic))
    (customize-set-variable
     'completion-category-overrides
     '((file (styles basic partial-completion))))))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package prescient
  :ensure t
  :config (prescient-persist-mode))

(use-package vertico-prescient
  :ensure t
  :after (vertico prescient)
  :init (customize-set-variable 'vertico-prescient-enable-filtering nil)
  :config (vertico-prescient-mode))

(use-package consult
  :ensure t
  :bind (("C-s"     . consult-line)
         ("M-y"     . consult-yank-pop)
         ("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-g f"   . consult-flymake)
         ("M-s r"   . consult-ripgrep)
         ("M-s f"   . consult-find)))

(use-package embark
  :ensure t
  :bind (("C-."   . embark-act)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :init (customize-set-variable 'corfu-auto t)
  :config
  (progn
    (keymap-set corfu-map "RET" nil)
    (keymap-set corfu-map "<remap> <next-line>" nil)
    (keymap-set corfu-map "<remap> <previous-line>" nil)
    (global-corfu-mode)))

(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :init (customize-set-variable 'corfu-prescient-enable-filtering nil)
  :config (corfu-prescient-mode))

(use-package cape
  :ensure t
  :init
  (progn
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file)))

(use-package avy
  :ensure t
  :bind (("C-;"   . avy-goto-char-timer)
         ("M-g w" . avy-goto-word-1)))

(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)))
