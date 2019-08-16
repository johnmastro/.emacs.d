;;; init.el      -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-

;; If this Emacs is pre-27, load early-init.el and run `package-initialize'
(when (version< emacs-version "27")
  (let* ((this (file-chase-links (or load-file-name buffer-file-name)))
         (here (file-name-directory this))
         (file (expand-file-name "early-init.el" here)))
    (when (file-exists-p file) (load file))
    (package-initialize)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finish up early initialization (picking up from early-init.el)

(defun basis/emacs-dir (name)
  "Return directory NAME expanded in `basis/emacs-dir'.
Create the directory if it does not exist and CREATE is non-nil."
  (if (string-suffix-p "/" name)
      (expand-file-name name basis/emacs-dir)
    ;; This isn't actually necessary
    (error "Directory name should end with a slash")))

(defun basis/emacs-file (name)
  "Return file NAME expanded in `basis/emacs-dir'."
  (if (not (string-suffix-p "/" name))
      (expand-file-name name basis/emacs-dir)
    (error "File name should not end with a slash")))

(dolist (dir '("var/" "var/autosaves/" "tmp/"))
  (make-directory (basis/emacs-dir dir) t))

(let ((dir (basis/emacs-dir "site-lisp/")))
  (make-directory dir t)
  (add-to-list 'load-path dir))

;; Opt out of automatically saving a list of installed packages
(when (fboundp 'package--save-selected-packages)
  (advice-add 'package--save-selected-packages :override #'ignore))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (and (eq system-type 'darwin)
           (null (getenv "PWD"))
           (equal default-directory "/"))
  (cd "~/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load some code

(setq use-package-enable-imenu-support t)

(require 'use-package)

(use-package auto-compile
  :ensure t
  :config (progn (setq auto-compile-mode-line-counter t)
                 (setq auto-compile-source-recreate-deletes-dest t)
                 (setq auto-compile-toggle-deletes-nonlib-dest t)
                 (setq auto-compile-update-autoloads t)
                 (auto-compile-on-load-mode)
                 (auto-compile-on-save-mode)))

(use-package bind-key
  :ensure t)

(use-package diminish
  :ensure t)

(use-package pcase)

(use-package subr-x
  :config
  (progn
    (unless (fboundp 'if-let*) (defalias 'if-let* 'if-let))
    (unless (fboundp 'when-let*) (defalias 'when-let* 'when-let))))

(use-package seq)

(use-package stream
  :ensure t
  :defer t)

(use-package async
  :ensure t
  :defer t)

(load (basis/emacs-file "defuns") nil nil nil 'must-suffix)
(load (basis/emacs-file "local") 'noerror nil nil 'must-suffix)

;; Specify the default font, but only if one of the local init file(s) didn't
(when-let* ((font (and (display-graphic-p)
                       (null (assq 'font default-frame-alist))
                       (basis/select-default-font))))
  (add-to-list 'default-frame-alist (cons 'font font))
  (set-face-font 'fixed-pitch font))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operating system-specific configuration

(use-package exec-path-from-shell
  :ensure t
  :if (eq window-system 'ns)
  :config (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various settings

(setq visible-bell t)
(setq inhibit-default-init t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'emacs-lisp-mode)
(setq sentence-end-double-space nil)
(setq recenter-positions '(top middle bottom))
(setq scroll-preserve-screen-position t)
(setq delete-by-moving-to-trash t)
(setq gc-cons-threshold (* 20 1024 1024))
(setq history-delete-duplicates t)
(setq temporary-file-directory (basis/emacs-dir "tmp/"))
(setq switch-to-buffer-preserve-window-point t)
(setq enable-recursive-minibuffers t)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq echo-keystrokes 0.5)
(setq user-mail-address "jbm@jbm.io")
(setq user-full-name "John Mastro")
(setq mail-host-address "jbm.io")
(setq print-gensym t)
(setq print-circle t)
(setq select-active-regions nil)

(defun basis/default-major-mode ()
  (let ((case-fold-search (eq system-type 'darwin)))
    ;; Ignore the more complicated case where the element of `auto-mode-alist'
    ;; is (REGEXP FUNCTION NON-NIL)
    (pcase (assoc-default (buffer-name) auto-mode-alist #'string-match)
      ((and mode (pred functionp))
       (funcall mode))
      (_
       (text-mode)))))

(setq-default major-mode #'basis/default-major-mode)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq-default truncate-lines t)

(add-hook 'kill-buffer-query-functions #'basis/kill-scratch-query-function)
(remove-hook 'kill-buffer-query-functions #'process-kill-buffer-query-function)

(fset 'display-startup-echo-area-message (symbol-function 'ignore))
(fset 'yes-or-no-p (symbol-function 'y-or-n-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic packages

(use-package custom
  :config
  (when (file-exists-p (setq custom-file (basis/emacs-file "custom.el")))
    (load custom-file)))

(defun basis/init-minibuffer ()
  (setq-local next-line-add-newlines nil))

(defun basis/init-eval-expression-minibuffer ()
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local tab-always-indent 'complete)
  (local-set-key (kbd "TAB") #'indent-for-tab-command))

(use-package simple
  :config
  (progn
    (setq shift-select-mode nil)
    (setq next-line-add-newlines t)
    (line-number-mode)
    (column-number-mode)
    (size-indication-mode)
    (advice-add 'pop-to-mark-command :around
                #'basis/pop-to-mark-ensure-new-pos)
    (add-hook 'minibuffer-setup-hook #'basis/init-minibuffer)
    (add-hook 'eval-expression-minibuffer-setup-hook
              #'basis/init-eval-expression-minibuffer)))

(use-package mule
  :config (prefer-coding-system 'utf-8))

(use-package files
  :config
  (progn
    (setq-default require-final-newline t)
    (setq confirm-nonexistent-file-or-buffer nil)
    (setq backup-by-copying t)
    (setq version-control t)
    (setq delete-old-versions t)
    (setq backup-directory-alist
          `(("." . ,(basis/emacs-dir "var/backups/"))))
    (setq auto-save-file-name-transforms
          `((".*" ,(basis/emacs-dir "var/autosaves/") t)))
    (setq auto-save-list-file-prefix
          (concat (basis/emacs-dir "var/auto-save-list/") ".saves-"))
    (put 'not-modified 'disabled t)))

(use-package windmove
  :defer t
  :config (progn (setq windmove-wrap-around t)
                 (windmove-default-keybindings)))

(use-package cursor-sensor
  :config
  (progn
    (unless (memq 'cursor-intangible minibuffer-prompt-properties)
      (setq minibuffer-prompt-properties
            (append minibuffer-prompt-properties '(cursor-intangible t))))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

(use-package mouse
  ;; Move `mouse-yank-*' so I'm less likely to hit them accidentally
  :config (progn (setq mouse-yank-at-point t)
                 (global-set-key (kbd "<mouse-2>") nil)
                 (global-set-key (kbd "<M-mouse-2>") nil)
                 (global-set-key (kbd "<mouse-4>") #'mouse-yank-primary)
                 (global-set-key (kbd "<M-mouse-4>") #'mouse-yank-secondary)))

(use-package mwheel
  :config
  (progn (setq mouse-wheel-progressive-speed nil)
         (pcase mouse-wheel-scroll-amount
           ((and `(,n . ,more) (guard (integerp n)))
            (setq mouse-wheel-scroll-amount (cons (min n 3) more))))))

(use-package url
  :defer t
  :config
  (progn (setq url-privacy-level 'high)
         (setq url-configuration-directory (basis/emacs-dir "var/url/"))
         (setq url-cookie-file (basis/emacs-file "var/url/cookies"))
         (url-setup-privacy-info)
         (add-hook 'kill-emacs-hook #'basis/delete-cookies)))

(use-package bookmark
  :defer t
  :config (progn (setq bookmark-default-file (basis/emacs-file "var/bookmarks"))
                 (setq bookmark-save-flag 1)))

(use-package ffap
  :defer t
  :init (global-set-key (kbd "C-M-j") #'find-file-at-point)
  :config (setq ffap-machine-p-known 'reject))

(use-package advice
  :defer t
  :config (setq ad-redefinition-action 'accept))

(use-package server
  :config
  (progn (setq server-auth-dir (basis/emacs-dir "var/server/"))
         (unless (and server-name (server-running-p server-name))
           (server-start))))

(use-package hl-line
  :config
  (progn (setq global-hl-line-sticky-flag t)
         (global-hl-line-mode)))

(use-package paren
  :config (show-paren-mode))

(use-package jka-cmpr-hook
  :config (auto-compression-mode))

(use-package delsel
  :config (delete-selection-mode))

(use-package subword
  :defer t
  :diminish subword-mode)

(use-package superword
  :defer t
  :diminish superword-mode)

(use-package autorevert
  :config
  (progn (setq auto-revert-verbose nil)
         (global-auto-revert-mode)))

(use-package frame
  :config (blink-cursor-mode -1))

(use-package saveplace
  :config
  (progn (setq save-place-file (basis/emacs-file "var/places"))
         (add-hook 'kill-emacs-hook #'basis/clean-up-save-place-alist)
         (save-place-mode)))

(use-package savehist
  :config
  (progn (setq savehist-additional-variables '(search-ring regexp-search-ring))
         (setq savehist-file (basis/emacs-file "var/history"))
         (savehist-mode)))

(use-package uniquify
  :config (progn (setq uniquify-buffer-name-style 'forward)
                 (setq uniquify-ignore-buffers-re "\\`\\*")))

(use-package whitespace
  :config (progn (setq whitespace-style '(face trailing lines-tail tab-mark))
                 (setq whitespace-line-column 80)
                 (put 'whitespace-line-column 'safe-local-variable #'integerp))
  :diminish whitespace-mode)

(use-package recentf
  :config
  (progn (setq recentf-max-saved-items 50)
         (setq recentf-save-file (basis/emacs-file "var/recentf"))
         (add-to-list 'recentf-exclude #'file-remote-p)
         (recentf-mode)))

(use-package tramp
  :defer t
  :config
  (progn
    (setq tramp-persistency-file-name (basis/emacs-file "var/tramp"))
    (setq tramp-default-method "scp")))

(use-package time
  :defer t
  :config (when (or (eq display-time-world-list zoneinfo-style-world-list)
                    (and (eq display-time-world-list t)
                         (fboundp 'time--display-world-list)
                         (eq (time--display-world-list)
                             zoneinfo-style-world-list)))
            (setq display-time-world-list
                  '(("America/Los_Angeles" "Los Angeles")
                    ("America/Denver"      "Denver")
                    ("America/Chicago"     "Chicago")
                    ("America/New_York"    "New York")
                    ("Europe/London"       "London")
                    ("Europe/Paris"        "Paris")
                    ("Europe/Moscow"       "Moscow")
                    ("Asia/Jakarta"        "Jakarta")
                    ("Asia/Shanghai"       "Shanghai")
                    ("Asia/Tokyo"          "Tokyo")))))

(use-package minibuffer
  :config
  (define-key minibuffer-inactive-mode-map
    [mouse-1]
    #'basis/toggle-echo-area-messages))

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode))

(use-package abbrev
  :defer t
  :config
  (advice-add 'abbrev-insert :before #'basis/abbrev-insert-undo-boundary))

(use-package enriched
  :defer t
  :config
  ;; Avoid a remote code execution vulnerability
  (when (version< emacs-version "25.3")
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface

(use-package solarized-theme
  :ensure color-theme-solarized
  :init (let ((val (if (and (display-graphic-p)
                            (getenv "DISPLAY")
                            (getenv "SSH_CLIENT"))
                       'light
                     'dark)))
          (set-frame-parameter nil 'background-mode val)
          (set-terminal-parameter nil 'background-mode val)
          (setq solarized-termcolors 256)
          (setq solarized-italic nil)
          (add-to-list 'custom-theme-load-path
                       (basis/emacs-dir "themes/solarized-moar/")))
  :config (progn (load-theme 'solarized t)
                 (load-theme 'solarized-moar t)))

(setq frame-title-format
      (list "%b " '(:eval (and buffer-file-name '(" | " buffer-file-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation modes

(use-package help-mode
  :defer t
  :config (progn (define-key help-mode-map "n" #'next-line)
                 (define-key help-mode-map "p" #'previous-line)))

(use-package help-at-pt
  :config (progn (setq help-at-pt-display-when-idle t)
                 (help-at-pt-set-timer)))

(defun basis/init-helpful-mode ()
  (global-set-key (kbd "C-h f")   #'helpful-callable)
  (global-set-key (kbd "C-h v")   #'helpful-variable)
  (global-set-key (kbd "C-h k")   #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F")   #'helpful-function)
  (global-set-key (kbd "C-h C")   #'helpful-command))

(use-package helpful
  :ensure t
  :defer t)

(use-package info
  :defer t
  :config
  (let ((info (basis/emacs-dir "doc/info/"))
        (font (face-attribute 'default :font)))
    (add-to-list 'Info-additional-directory-list info)
    (set-face-attribute 'Info-quoted nil :font font :slant 'italic)))

(use-package apropos
  :defer t
  :config (setq apropos-do-all t))

(use-package man
  :defer t
  :init (global-set-key (kbd "C-h C-k") #'basis/ido-man)
  :config (progn (setq Man-width 80)
                 (setq Man-notify-method 'aggressive)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(use-package which-key
  :ensure t
  :config (progn
            (setq which-key-idle-delay 0.5)
            (which-key-mode))
  :diminish which-key-mode)

(global-set-key (kbd "M-o") #'other-window)

(global-set-key (kbd "C-x k") #'basis/kill-buffer)

(use-package winner
  :config (winner-mode))

(global-set-key [remap next-line] #'basis/next-line)

(global-set-key [remap delete-horizontal-space] #'basis/delete-some-whitespace)

(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)

(global-set-key [remap move-beginning-of-line] #'basis/beginning-of-line)

(global-set-key [remap open-line]  #'basis/open-line-maybe-reindent)
(global-set-key (kbd "<S-return>") #'basis/open-line-above)
(global-set-key (kbd "<C-return>") #'basis/open-line-below)

(global-set-key [remap forward-sentence]  #'forward-sexp)
(global-set-key [remap backward-sentence] #'backward-sexp)
(global-set-key [remap kill-sentence]     #'kill-sexp)

(global-set-key [remap kill-region]        #'basis/kill-something)
(global-set-key [remap backward-kill-word] #'basis/kill-something)
(global-set-key (kbd "<s-backspace>")      #'basis/kill-something)
(global-set-key (kbd "<M-delete>")         #'basis/smart-kill-whole-line)

(global-set-key [remap kill-ring-save] #'basis/kill-ring-save-something)
(global-set-key (kbd "ESC M-w") #'basis/kill-ring-save-as-fenced-code-block)
(global-set-key (kbd "<f2>")    #'basis/clipboard-save-something)

(global-set-key (kbd "s-^")                #'basis/delete-indentation)
(global-set-key [remap delete-indentation] #'basis/delete-indentation)
(global-set-key [remap fill-paragraph]     #'basis/fill-or-unfill-paragraph)

(basis/define-prefix-command 'basis/insertion-map (kbd "C-c i"))
(define-key basis/insertion-map "f" #'basis/insert-file-name)
(define-key basis/insertion-map "F" #'basis/insert-files)

(basis/define-prefix-command 'basis/meta-t-map (kbd "M-t"))
(define-key basis/meta-t-map (kbd "l")   #'transpose-lines)
(define-key basis/meta-t-map (kbd "w")   #'transpose-words)
(define-key basis/meta-t-map (kbd "s")   #'transpose-sexps)
(define-key basis/meta-t-map (kbd "c")   #'transpose-chars)
(define-key basis/meta-t-map (kbd "M-w") #'basis/transpose-windows)
(define-key basis/meta-t-map (kbd "M-s") #'basis/toggle-window-split)
(define-key basis/meta-t-map (kbd "n")   #'display-line-numbers-mode)

(global-set-key (kbd "C-c ;") #'basis/comment-or-uncomment)
(global-set-key (kbd "C-x ;") #'basis/comment-region-lines)
(global-set-key (kbd "C-M-;") #'basis/comment-or-uncomment-sexp)

(global-set-key (kbd "s-%") #'query-replace)
(global-set-key (kbd "M-s-%") #'query-replace-regexp)

(global-set-key (kbd "C-c M-e") #'basis/eval-and-replace)

(global-set-key (kbd "M-i") #'basis/imenu-dwim)

;; I use M-SPC for `avy-goto-word-1'
(global-set-key (kbd "C-c SPC") #'just-one-space)

(global-set-key [remap goto-line] #'basis/goto-line-with-numbers)

(global-set-key (kbd "C-c <C-return>") #'shell)
(global-set-key (kbd "C-c C-^") #'shell)

(global-set-key [remap upcase-word]     #'upcase-dwim)
(global-set-key [remap downcase-word]   #'downcase-dwim)
(global-set-key [remap capitalize-word] #'capitalize-dwim)

(global-set-key [remap save-buffers-kill-terminal]
                #'basis/kill-frame-or-terminal)

(global-set-key (kbd "<f9>") #'basis/google)

(global-set-key (kbd "C-x C-r") #'basis/find-file-recentf)

(basis/define-prefix-command 'basis/file-map (kbd "C-c f"))
(define-key basis/file-map "d" #'basis/diff-buffer-with-file)
(define-key basis/file-map "r" #'basis/rename-buffer-file)
(define-key basis/file-map "D" #'basis/delete-buffer-file)
(define-key basis/file-map "f" #'find-name-dired)
(define-key basis/file-map "F" #'find-dired)
(define-key basis/file-map "m" #'make-directory)
(define-key basis/file-map "n" #'basis/kill-ring-save-buffer-file-name)
(define-key basis/file-map "v" #'revert-buffer)

(global-set-key (kbd "C-c e") #'basis/open-file-externally)

;; Wrap text in various styles of quotes
(global-set-key (kbd "C-c q") #'basis/quote-thing)

(basis/define-prefix-command 'basis/region-map (kbd "C-c r"))
(define-key basis/region-map "a" #'align)
(define-key basis/region-map "d" #'basis/delete-empty-lines)
(define-key basis/region-map "c" #'basis/count-words)
(define-key basis/region-map "l" #'basis/count-sloc)
(define-key basis/region-map "s" #'sort-lines)
(define-key basis/region-map "n" #'basis/narrow-or-widen-dwim)

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(basis/define-prefix-command 'basis/find-lisp-map (kbd "C-h e"))
(define-key basis/find-lisp-map "c" #'finder-commentary)
(define-key basis/find-lisp-map "e" #'view-echo-area-messages)
(define-key basis/find-lisp-map "f" #'find-function)
(define-key basis/find-lisp-map "F" #'find-face-definition)
(define-key basis/find-lisp-map "i" #'info-apropos)
(define-key basis/find-lisp-map "k" #'find-function-on-key)
(define-key basis/find-lisp-map "l" #'find-library)
(define-key basis/find-lisp-map "m" #'info-display-manual)
(define-key basis/find-lisp-map "v" #'find-variable)
(define-key basis/find-lisp-map "V" #'apropos-value)
(define-key basis/find-lisp-map "a" #'counsel-apropos)
(define-key basis/find-lisp-map "s" #'basis/find-source-directory)

(use-package other-frame-window
  :ensure t
  :config (other-frame-window-mode))

(global-set-key (kbd "C-h C-m") #'basis/toggle-echo-area-messages)

;; Make it harder to accidentally `suspend-frame'
(basis/define-prefix-command 'basis/ctl-z-map (kbd "C-z"))
(define-key basis/ctl-z-map (kbd "C-z") #'suspend-frame)

(global-set-key (kbd "C-x C-z") #'repeat)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing

(use-package undo-tree
  :ensure t
  :config (progn (global-undo-tree-mode)
                 (define-key undo-tree-map (kbd "C-M-_") #'undo-tree-redo))
  :diminish undo-tree-mode)

(use-package expand-region
  :ensure t
  :defer t
  :init (progn (global-set-key (kbd "M-=")      #'er/expand-region)
               (global-set-key (kbd "<s-up>")   #'er/expand-region)
               (global-set-key (kbd "<s-down>") #'er/contract-region)))

(use-package multiple-cursors
  :ensure t
  :defer t
  :init
  (progn (global-set-key (kbd "M-]") #'mc/mark-next-like-this)
         (global-set-key (kbd "C->") #'mc/mark-next-like-this)
         (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
         (global-set-key (kbd "<M-down-mouse-1>") nil)
         (global-set-key (kbd "<M-mouse-1>") #'mc/add-cursor-on-click)
         (basis/define-prefix-command 'basis/mc-map (kbd "C-c m"))
         (let ((map basis/mc-map))
           (define-key map (kbd "e")   #'mc/edit-lines)
           (define-key map (kbd "C-a") #'mc/edit-beginnings-of-lines)
           (define-key map (kbd "C-e") #'mc/edit-ends-of-lines)
           (define-key map (kbd "d")   #'mc/mark-all-like-this-dwim)
           (define-key map (kbd "D")   #'mc/mark-all-dwim)
           (define-key map (kbd "m")   #'mc/mark-more-like-this-extended)
           (define-key map (kbd "s")   #'mc/mark-all-symbols-like-this-in-defun)
           (define-key map (kbd "w")   #'mc/mark-all-words-like-this-in-defun)
           (define-key map (kbd "n")   #'mc/insert-numbers)
           (define-key map (kbd "l")   #'mc/insert-letters))))

(use-package multiple-cursors-core
  :defer t
  :init (setq mc/list-file (basis/emacs-file "var/mc-lists.el"))
  :config (define-key mc/keymap (kbd "RET") #'multiple-cursors-mode))

(use-package move-text
  :ensure t
  :defer t
  :init (progn (global-set-key (kbd "<M-s-up>") #'move-text-up)
               (global-set-key (kbd "<M-s-down>") #'move-text-down)))

(use-package browse-kill-ring
  :ensure t
  :defer t
  :init (global-set-key (kbd "M-Y") #'browse-kill-ring))

(use-package easy-kill
  :ensure t
  :defer t)

(use-package visual-regexp
  :ensure t
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Brackets

(defun basis/init-paredit-mode ()
  (unless (or (minibufferp) (memq major-mode '(inferior-emacs-lisp-mode
                                               inferior-lisp-mode
                                               inferior-scheme-mode
                                               geiser-repl-mode
                                               cider-repl-mode)))
    (local-set-key (kbd "RET") #'paredit-newline)))

(use-package paredit
  :ensure t
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  :config
  (progn
    (let ((map paredit-mode-map))
      (define-key map (kbd "M-?")   nil) ; Make room for `xref-find-references'
      (define-key map (kbd "M-)")   #'basis/paredit-wrap-round-from-behind)
      (define-key map (kbd "M-e")   #'paredit-forward)
      (define-key map (kbd "M-a")   #'paredit-backward)
      (define-key map (kbd "M-k")   #'kill-sexp)
      (define-key map (kbd "C-w")   #'basis/paredit-kill-something)
      (define-key map (kbd "M-DEL") #'basis/paredit-kill-something)
      (define-key map (kbd "<s-backspace>")
        #'basis/paredit-kill-something))
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'basis/paredit-doublequote-space-p)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'basis/paredit-splicing-unquote-p)
    ;; Show `eldoc' messages after Paredit motion commands
    (with-eval-after-load 'eldoc
      (eldoc-add-command 'paredit-forward
                         'paredit-forward-up
                         'paredit-forward-down
                         'paredit-backward
                         'paredit-backward-up
                         'paredit-backward-down
                         'paredit-newline))
    (add-hook 'paredit-mode-hook #'basis/init-paredit-mode)
    (pcase-dolist (`(,sym . ,act) '((paredit-kill            . supersede)
                                    (paredit-forward-delete  . supersede)
                                    (paredit-backward-delete . supersede)
                                    (paredit-newline         . t)))
      (put sym 'delete-selection act))))

(defvar basis/sp-ignore-modes
  '(magit-key-mode
    lisp-mode
    emacs-lisp-mode
    inferior-emacs-lisp-mode
    lisp-interaction-mode
    cider-repl-mode
    clojure-mode
    clojure-mode-mode
    inferior-scheme-mode
    geiser-repl-mode
    inferior-lisp-mode
    scheme-mode
    slime-repl-mode)
  "List of modes in which not to active `smartparens'.")

(use-package smartparens
  :ensure t
  :config
  (progn
    (smartparens-global-strict-mode)
    (dolist (mode basis/sp-ignore-modes)
      (add-to-list 'sp-ignore-modes-list mode))
    (sp-use-paredit-bindings)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-autoescape-string-quote nil)
    (setq sp-use-subword t)
    (setq-default sp-autoskip-closing-pair 'always)
    (sp-pair "'" "'"
             :unless '(sp-in-string-quotes-p basis/sp-point-after-word-p)
             :actions (sp-get-pair-definition "\"" t :actions))
    (pcase-dolist (`(,mode ,open ,close ,actions)
                   '((org-mode  "=" "=" (wrap))
                     (rust-mode "'" nil (:rem insert autoskip))
                     (c-mode    "{" "}" (:rem insert autoskip))
                     (c++-mode  "{" "}" (:rem insert autoskip))
                     (java-mode "{" "}" (:rem insert autoskip))))
      (sp-local-pair mode open close :actions actions))
    (define-key sp-keymap (kbd "M-DEL")         #'basis/sp-kill-something)
    (define-key sp-keymap (kbd "C-DEL")         #'basis/sp-kill-something)
    (define-key sp-keymap (kbd "<C-backspace>") #'basis/sp-kill-something)
    (define-key sp-keymap (kbd "C-w")           #'basis/sp-kill-something)
    (define-key sp-keymap (kbd "<s-backspace>") #'basis/sp-kill-something)
    (define-key sp-keymap (kbd "M-k")           #'basis/sp-kill-sexp)
    (define-key sp-keymap (kbd "M-e")           #'sp-forward-sexp)
    (define-key sp-keymap (kbd "M-a")           #'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-u")         #'basis/sp-backward-up)
    (define-key sp-keymap (kbd "M-J")           #'sp-join-sexp)
    (define-key sp-keymap (kbd "M-j")           nil)
    (advice-add 'sp--cleanup-after-kill :before-until
                #'basis/sp-cleanup-maybe-not)
    (advice-add 'sp--unwrap-sexp :filter-args #'basis/sp-unwrap-no-cleanup)
    (advice-add 'sp-backward-delete-char :filter-args
                #'basis/sp-backward-delete-no-prefix)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement

(use-package imenu
  :defer t
  :config (progn (setq imenu-auto-rescan t)
                 (setq imenu-auto-rescan-maxout 120000)))

(use-package imenu-list
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :defer t
  :init
  (progn (global-set-key (kbd "M-SPC") #'avy-goto-word-1)
         (global-set-key (kbd "M-g g") #'avy-goto-line))
  :config
  (progn (setq avy-keys '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?u ?i))
         (setq avy-style 'pre)
         (advice-add 'avy-push-mark :after #'basis/push-mark-noactivate)))

(use-package ace-window
  :ensure t
  :defer t
  :init (global-set-key (kbd "C-x o") #'ace-window)
  :config (progn (setq aw-keys '(?h ?j ?k ?l ?n ?m))
                 (setq aw-scope 'frame)
                 (advice-add 'ace-window :around #'basis/ace-window-kludge)))

(use-package ace-link
  :ensure t
  :config (ace-link-setup-default))

(use-package jump-char
  :ensure t
  :defer t
  :init (progn (global-set-key (kbd "M-m") #'jump-char-forward)
               (global-set-key (kbd "M-M") #'jump-char-backward)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search

(setq search-default-mode #'char-fold-to-regexp)
(setq isearch-regexp-lax-whitespace t)
(setq isearch-allow-scroll t)

(global-set-key (kbd "C-s")     #'isearch-forward-regexp)
(global-set-key (kbd "C-r")     #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   #'isearch-forward)
(global-set-key (kbd "C-M-r")   #'isearch-backward)
(global-set-key (kbd "ESC M-s") search-map)

(let ((map isearch-mode-map))
  (define-key map (kbd "M-DEL")         #'basis/isearch-meta-del-char)
  (define-key map (kbd "<M-backspace>") #'basis/isearch-meta-del-char)
  (define-key map (kbd "C-t")           #'basis/isearch-yank-something)
  (define-key map (kbd "C-g")           #'basis/isearch-cancel)
  (define-key map (kbd "<up>")          #'isearch-ring-retreat)
  (define-key map (kbd "<down>")        #'isearch-ring-advance)
  (define-key map (kbd "<left>")        #'isearch-repeat-backward)
  (define-key map (kbd "<right>")       #'isearch-repeat-forward))

(use-package swiper
  :ensure t
  :defer t
  :commands (swiper-from-isearch)
  :init (define-key isearch-mode-map (kbd "M-i") #'swiper-from-isearch)
  :config
  (progn
    (setq swiper-min-highlight 1)
    (define-key swiper-map (kbd "M-%")   #'swiper-query-replace)
    (define-key swiper-map (kbd "M-SPC") #'swiper-avy)))

(defun basis/init-occur-mode ()
  (setq basis/beginning-of-buffer-function #'basis/occur-beginning-of-buffer)
  (setq basis/end-of-buffer-function #'basis/occur-end-of-buffer))

(progn ;; replace.el
  (global-set-key (kbd "ESC M-%") #'query-replace-regexp)
  (define-key occur-mode-map "n" #'occur-next)
  (define-key occur-mode-map "p" #'occur-prev)
  (define-key occur-mode-map [remap beginning-of-buffer]
    #'basis/beginning-of-buffer)
  (define-key occur-mode-map [remap end-of-buffer] #'basis/end-of-buffer)
  (add-hook 'occur-mode-hook #'basis/init-occur-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External search

(basis/define-prefix-command 'basis/grep-map (kbd "C-c g"))
(define-key basis/grep-map "a"  #'ag-regexp)
(define-key basis/grep-map "g"  #'grep)
(define-key basis/grep-map "s"  #'lgrep)
(define-key basis/grep-map "r"  #'rgrep)
(define-key basis/grep-map "z"  #'zrgrep)
(define-key basis/grep-map "v"  #'projectile-grep)
(define-key basis/grep-map "V"  #'vc-git-grep)
(define-key basis/grep-map "f"  #'find-grep)
(define-key basis/grep-map "d"  #'find-grep-dired)
(define-key basis/grep-map "o"  #'basis/occur-dwim)
(define-key basis/grep-map "mo" #'multi-occur)
(define-key basis/grep-map "mm" #'multi-occur-in-matching-buffers)

(use-package grep
  :defer t
  :config
  (progn
    (grep-compute-defaults)
    (grep-apply-setting
     'grep-command
     (if (bound-and-true-p grep-use-null-filename-separator)
         "grep --null --color=always -inHE -e "
       "grep --color=always -inHE -e "))
    (grep-apply-setting
     'grep-find-command
     (if (bound-and-true-p grep-use-null-filename-separator)
         '("find . -type f -exec grep -inH --null -e  \\{\\} +" . 42)
       '("find . -type f -exec grep -inH -e  \\{\\} +" . 35)))
    (when (string-match-p "zsh" shell-file-name)
      (advice-add 'lgrep :around #'basis/grep-use-bash))
    (pcase-dolist (`(,alias . ,files)
                   '(("clj" . "*.clj *.cljs *.cljc")
                     ("cl"  . "*.lisp *.cl")
                     ("txt" . "*.txt *.org *.rst *.md *.mkd *.markdown")))
      (unless (assoc alias grep-files-aliases)
        (add-to-list 'grep-files-aliases (cons alias files))))))

(use-package locate
  :defer t
  :config
  (when (eq system-type 'darwin)
    (setq locate-make-command-line (lambda (s) (list "mdfind" "-name" s)))))

(use-package ag
  :ensure t
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion

(defun basis/init-ido-keys ()
  (let ((map ido-file-completion-map))
    (define-key map (kbd "C-w")   nil)
    (define-key map (kbd "M-w")   #'ido-copy-current-file-name)
    (define-key map (kbd "C-x g") #'ido-enter-magit-status)
    (define-key map (kbd "C-c e") #'basis/ido-open-file-externally)))

(use-package ido
  :config
  (progn (setq ido-enable-prefix nil)
         (setq ido-enable-flex-matching t)
         (setq ido-auto-merge-work-directories-length -1)
         (setq ido-create-new-buffer 'always)
         (setq ido-use-filename-at-point nil)
         (setq ido-use-virtual-buffers t)
         (setq ido-use-faces nil)
         (setq ido-ignore-extensions t)
         (setq ido-save-directory-list-file (basis/emacs-file "var/ido.last"))
         (setq ido-enable-tramp-completion t)
         (add-hook 'ido-setup-hook #'basis/init-ido-keys)
         (ido-mode)
         (ido-everywhere)))

(use-package ido-completing-read+
  :ensure t
  :init (progn (setq ido-cr+-auto-update-blacklist t)
               (setq ido-cr+-max-items nil)
               (ido-ubiquitous-mode)))

(use-package flx-ido
  :ensure t
  :config (progn (setq flx-ido-threshhold 10000)
                 (flx-ido-mode)))

(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode))

(use-package idomenu
  :ensure t
  :defer t)

(use-package smex
  :ensure t
  :config (progn (setq smex-save-file (basis/emacs-file "var/smex-items"))
                 (global-set-key (kbd "M-x") #'smex)
                 (global-set-key (kbd "M-X") #'smex-major-mode-commands)))

(use-package ivy
  :ensure t
  :defer t
  :config
  (progn (setq ivy-format-function #'ivy-format-function-arrow)
         (setq ivy-use-selectable-prompt t)
         (let ((map ivy-minibuffer-map))
           (define-key map (kbd "C-r")     #'ivy-previous-line-or-history)
           (define-key map (kbd "<up>")    #'ivy-previous-line-or-history)
           (define-key map (kbd "<left>")  #'ivy-previous-line-or-history)
           (define-key map (kbd "<down>")  #'ivy-next-line-or-history)
           (define-key map (kbd "<right>") #'ivy-next-line-or-history))))

(use-package counsel
  :ensure t
  :commands (counsel-mark-ring)
  :init (global-set-key (kbd "M-`") #'counsel-mark-ring)
  :defer t)

(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook #'global-company-mode)
    (define-key company-active-map (kbd "TAB") #'company-complete)
    (define-key company-active-map [tab] #'company-complete)
    (define-key company-active-map (kbd "<C-f1>") #'company-show-location)
    (define-key company-active-map (kbd "<M-f1>") #'company-show-location)
    (define-key company-active-map (kbd "C-w") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map [return] nil)
    (set-default
     (make-variable-buffer-local 'company-backends)
     '(company-capf
       (company-dabbrev-code company-gtags company-etags company-keywords)
       company-files
       company-dabbrev))
    (setq company-minimum-prefix-length 2)
    (setq company-tooltip-flip-when-above t)
    (setq company-show-numbers t)
    (setq company-selection-wrap-around t)
    (advice-add 'company-auto-begin :before-until
                #'basis/company-maybe-block-completion))
  :diminish company-mode)

(use-package company-statistics
  :ensure t
  :config (let ((file (basis/emacs-file "var/company-statistics-cache.el")))
            (setq company-statistics-file file)
            (add-hook 'after-init-hook #'company-statistics-mode t)))

(use-package company-emoji
  :ensure t
  :defer t)

(use-package hippie-exp
  :init (global-set-key (kbd "M-/") #'hippie-expand)
  :config (setq hippie-expand-try-functions-list
                (seq-difference hippie-expand-try-functions-list
                                '(try-expand-line
                                  try-expand-list
                                  try-expand-all-abbrevs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming modes

(defun basis/init-prog-mode ()
  (basis/maybe-enable-whitespace-mode)
  (basis/maybe-enable-flyspell-prog-mode)
  (basis/maybe-enable-bug-reference-mode)
  (setq indicate-empty-lines t)
  (unless (eq major-mode 'sql-mode)
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode)))

(use-package prog-mode
  :config (progn (define-key prog-mode-map (kbd "RET") #'basis/electric-return)
                 (add-hook 'prog-mode-hook #'basis/init-prog-mode)))

(use-package lisp-mode
  :defer t
  :mode "\\.sbclrc\\'"
  :config (add-hook 'lisp-mode-hook #'basis/init-lisp-generic))

(use-package inf-lisp
  :defer t
  :config
  (progn (setq inferior-lisp-program (or (executable-find "sbcl")
                                         (executable-find "ccl")
                                         "lisp"))
         (add-hook 'inferior-lisp-mode-hook #'basis/init-lisp-generic)))

(use-package cl-indent
  :config (progn (setq lisp-lambda-list-keyword-alignment t)
                 (setq lisp-lambda-list-keyword-parameter-alignment t)
                 (setq lisp-loop-keyword-indentation 6)
                 (setq lisp-loop-forms-indentation 6)))

(defun basis/init-lisp-generic ()
  "Enable features useful in all Lisp modes."
  (paredit-mode))

(defun basis/init-emacs-lisp-modes ()
  "Enable features useful when working with Emacs Lisp."
  (let ((functions (make-local-variable 'hippie-expand-try-functions-list)))
    (add-to-list functions #'try-complete-lisp-symbol t)
    (add-to-list functions #'try-complete-lisp-symbol-partially t)))

(defun basis/init-emacs-lisp-mode ()
  (unless no-byte-compile
    (basis/maybe-enable-flycheck)))

(use-package elisp-mode
  :mode ("\\.eld\\'" . emacs-lisp-mode)
  :config
  (progn
    (let ((map emacs-lisp-mode-map))
      (define-key map (kbd "C-x C-e") #'eval-last-sexp)
      (define-key map (kbd "C-M-x")   #'eval-defun)
      (define-key map (kbd "C-c C-r") #'eval-region)
      (define-key map (kbd "C-c C-b") #'eval-buffer)
      (define-key map (kbd "C-c C-c") #'basis/eval-something)
      (define-key map (kbd "C-c C-f") #'load-file)
      (define-key map (kbd "C-c C-e") #'macrostep-expand))
    (let ((map lisp-interaction-mode-map))
      (define-key map (kbd "C-x C-e") #'basis/eval-last-sexp)
      (define-key map (kbd "C-M-x")   #'eval-defun)
      (define-key map (kbd "C-c C-r") #'eval-region)
      (define-key map (kbd "C-c C-b") #'eval-buffer)
      (define-key map (kbd "C-c C-c") #'basis/eval-something)
      (define-key map (kbd "C-c C-f") #'load-file)
      (define-key map (kbd "C-c C-e") #'macrostep-expand))
    (add-hook 'emacs-lisp-mode-hook #'basis/init-lisp-generic)
    (add-hook 'emacs-lisp-mode-hook #'basis/init-emacs-lisp-modes)
    (add-hook 'emacs-lisp-mode-hook #'basis/init-emacs-lisp-mode)
    (add-hook 'lisp-interaction-mode-hook #'basis/init-lisp-generic)
    (add-hook 'lisp-interaction-mode-hook #'basis/init-emacs-lisp-modes)))

(use-package ielm
  :defer t
  :config
  (progn
    (add-hook 'inferior-emacs-lisp-mode-hook #'basis/init-lisp-generic)
    (add-hook 'inferior-emacs-lisp-mode-hook #'basis/init-emacs-lisp-modes)))

(use-package eldoc
  :config (progn (setq eldoc-idle-delay 0.1)
                 (setq eldoc-echo-area-use-multiline-p t)
                 (global-eldoc-mode))
  :diminish eldoc-mode)

(use-package macrostep
  :ensure t
  :defer t)

(use-package elisp-refs
  :ensure t
  :defer t)

(use-package el-search
  :ensure t
  :defer t)

(use-package pcre2el
  :ensure t
  :defer t)

(use-package redshank
  :ensure t
  :defer t
  :config (redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t)
  :diminish redshank-mode)

(use-package slime
  :ensure t
  :defer t
  :config
  (progn
    (setq slime-lisp-implementations
          '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
            (ccl ("ccl"))))
    (setq slime-default-lisp 'sbcl)
    (setq slime-contribs '(slime-fancy))
    (setq slime-autodoc-use-multiline-p t)
    (let ((map slime-mode-map))
      (define-key map (kbd "C-x C-e") #'slime-eval-last-expression)
      (define-key map (kbd "C-M-x")   #'slime-eval-defun)
      (define-key map (kbd "C-c C-r") #'slime-eval-region)
      (define-key map (kbd "C-c C-b") #'slime-eval-buffer)
      (define-key map (kbd "C-c C-c") #'basis/slime-eval-something)
      (define-key map (kbd "C-c C-f") #'slime-compile-and-load-file)
      (define-key map (kbd "C-c C-e") #'slime-expand-1))))

(use-package slime-repl
  :defer t
  :config (add-hook 'slime-repl-mode-hook #'basis/init-lisp-generic))

(use-package slime-company
  :ensure t
  :defer t)

(defun basis/init-clojure-mode ()
  (subword-mode)
  (clj-refactor-mode))

(defun basis/init-cider-repl-mode ()
  (subword-mode))

(defun basis/init-cider-mode ()
  nil)

(defvar basis/clojure-indent-specs
  '((-> 1)
    (->> 1)
    (cond-> 1)
    (cond->> 1)
    (some-> 1)
    (some->> 1)
    (add-watch 2)
    (match 1)
    (query 1)
    (symbol-macrolet 1)
    (defsymbolmacro defun)
    (with-symbol-macros defun)
    (run 2)
    (run* 1)
    (fresh 1))
  "Additional form indentation settings for `clojure-mode'.")

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'clojure-mode-hook #'basis/init-lisp-generic)
    (add-hook 'clojure-mode-hook #'basis/init-clojure-mode)
    (pcase-dolist (`(,sym ,n) basis/clojure-indent-specs)
      (put-clojure-indent sym n))
    (put 'macrolet 'clojure-backtracking-indent '((2) 2))))

(use-package cider
  :ensure t
  :defer t
  :config (progn
            (setq cider-prompt-for-symbol nil)
            (setq cider-font-lock-dynamically '(macros deprecated))
            (add-hook 'cider-mode-hook #'basis/init-cider-mode)
            (let ((map cider-mode-map))
              (define-key map (kbd "C-x C-e") #'cider-eval-last-sexp)
              (define-key map (kbd "C-M-x")   #'cider-eval-defun-at-point)
              (define-key map (kbd "C-c C-r") #'cider-eval-region)
              (define-key map (kbd "C-c C-b") #'cider-eval-buffer)
              (define-key map (kbd "C-c C-c") #'basis/cider-eval-something)
              (define-key map (kbd "C-c C-f") #'cider-load-current-buffer)
              (define-key map (kbd "C-c C-e") #'cider-macroexpand-1))))

(use-package nrepl-client
  :defer t
  :config (setq nrepl-log-messages t))

(use-package cider-repl
  :defer t
  :config (progn
            (setq cider-repl-use-pretty-printing t)
            (add-hook 'cider-repl-mode-hook #'basis/init-lisp-generic)
            (add-hook 'cider-repl-mode-hook #'basis/init-cider-repl-mode)))

(use-package clj-refactor
  :ensure t
  :defer t
  :config (progn (setq cljr-thread-all-but-last t)
                 (setq cljr-favor-prefix-notation nil)
                 (setq cljr-favor-private-functions nil)
                 (cljr-add-keybindings-with-prefix "C-c m"))
  :diminish clj-refactor-mode)

(use-package clojure-cheatsheet
  :ensure t
  :defer t)

(use-package scheme
  :defer t
  :config (progn
            (let ((map scheme-mode-map))
              (define-key map (kbd "C-x C-e") #'scheme-send-last-sexp)
              (define-key map (kbd "C-M-x")   #'scheme-send-definition)
              (define-key map (kbd "C-c C-r") #'scheme-send-region)
              (define-key map (kbd "C-c C-c") #'basis/scheme-send-something)
              (define-key map (kbd "C-c C-f") #'scheme-load-file)
              (define-key map (kbd "C-c C-e") #'scheme-expand-current-form))
            (add-hook 'scheme-mode-hook #'basis/init-lisp-generic)))

(use-package quack
  :after scheme
  :config (progn (setq quack-default-program "guile")
                 (setq quack-fontify-style 'emacs)))

(use-package cmuscheme
  :defer t
  :config (add-hook 'inferior-scheme-mode-hook #'basis/init-lisp-generic))

(use-package geiser-mode
  :ensure geiser
  :defer t
  :config (let ((map geiser-mode-map))
            (define-key map (kbd "C-x C-e") #'geiser-eval-last-sexp)
            (define-key map (kbd "C-M-x")   #'geiser-eval-definition)
            (define-key map (kbd "C-c C-r") #'geiser-eval-region)
            (define-key map (kbd "C-c C-b") #'geiser-eval-buffer)
            (define-key map (kbd "C-c C-f") #'geiser-load-file)
            (define-key map (kbd "C-c C-c") #'basis/geiser-eval-something)
            (define-key map (kbd "C-c C-e") #'basis/geiser-expand-something)))

(use-package geiser-repl
  :defer t
  :config (add-hook 'geiser-repl-mode-hook #'basis/init-lisp-generic))

(use-package racket-mode
  :ensure t
  :defer t)

(defun basis/init-python-mode ()
  (subword-mode)
  (basis/maybe-enable-flycheck)
  (setq fill-column 79)
  (setq tab-width 4)
  (setq-local whitespace-line-column 79)
  (setq-local electric-indent-chars (remove ?: electric-indent-chars)))

(defun basis/init-inferior-python-mode ()
  (subword-mode)
  (setq tab-width 4))

(use-package python
  :defer t
  :config
  (progn
    (setq python-indent-guess-indent-offset-verbose nil)
    (setq python-fill-docstring-style 'pep-257-nn)
    (let ((map python-mode-map))
      (define-key map (kbd "DEL")     #'basis/sp-python-backspace)
      (define-key map (kbd "C-c C-D") #'python-eldoc-at-point)
      (define-key map (kbd "C-c C-p") #'basis/run-python)
      (define-key map (kbd "C-c '")   #'basis/python-insert-triple-quotes)
      (define-key map (kbd "C-c \"")  #'basis/python-insert-triple-quotes)
      (define-key map (kbd "C-M-SPC") #'sp-mark-sexp)
      (define-key map (kbd "C-M-@")   #'sp-mark-sexp)
      (define-key map (kbd "C-M-x")   #'python-shell-send-defun)
      (define-key map (kbd "C-c C-b") #'python-shell-send-buffer)
      (define-key map (kbd "C-c C-r") #'python-shell-send-region)
      (define-key map (kbd "C-c C-c") #'basis/python-send-something)
      (define-key map (kbd "C-c C-f") #'python-shell-send-file))
    (add-hook 'python-mode-hook #'basis/init-python-mode)
    (add-hook 'inferior-python-mode-hook #'basis/init-inferior-python-mode)
    (when (basis/jedi-installed-p)
      (add-hook 'python-mode-hook #'jedi:setup))))

(use-package pyvenv
  :ensure t
  :defer t)

(use-package jedi
  :ensure t
  :defer t
  :config (progn (setq jedi:setup-keys t)
                 (setq jedi:tooltip-method nil)))

(use-package php-mode
  :ensure t
  :defer t)

(defun basis/init-haskell-mode ()
  (subword-mode)
  (turn-on-haskell-indentation)
  (haskell-decl-scan-mode)
  (interactive-haskell-mode))

(use-package haskell-mode
  :ensure t
  :defer t
  :config (progn (setq haskell-process-log t)
                 (setq haskell-doc-prettify-types (display-graphic-p))
                 (add-hook 'haskell-mode-hook #'basis/init-haskell-mode)))

(use-package interactive-haskell-mode
  :defer t
  :config (let ((map interactive-haskell-mode-map))
            (define-key map (kbd "C-c C-z") #'haskell-interactive-bring)
            (define-key map (kbd "C-c C-l") #'haskell-process-load-or-reload)
            (define-key map (kbd "C-c C-k") #'haskell-interactive-mode-clear)
            (define-key map (kbd "C-c C-c") #'haskell-process-cabal-build)
            (define-key map (kbd "C-c c")   #'haskell-process-cabal)
            (define-key map (kbd "M-.")     #'haskell-mode-goto-loc)
            (define-key map (kbd "M-?")     #'haskell-mode-find-uses)
            (define-key map (kbd "C-c C-t") #'haskell-mode-show-type-at)))

(use-package ghci-script-mode
  :defer t
  :mode "\\.ghci\\'")

(defun basis/init-rust-mode ()
  (subword-mode)
  (when (and (equal compile-command (default-value 'compile-command))
             (not (or (file-exists-p "Makefile")
                      (file-exists-p "makefile"))))
    (let* ((name nil)
           (cmd (cond ((file-exists-p "Cargo.toml")
                       "cargo build")
                      ((setq name (buffer-file-name))
                       (format "rustc %s" (shell-quote-argument name))))))
      (when cmd (setq-local compile-command cmd)))))

(use-package rust-mode
  :ensure t
  :defer t
  :config (add-hook 'rust-mode-hook #'basis/init-rust-mode))

(defun basis/init-js-mode ()
  (when (locate-dominating-file default-directory ".eslintrc.js")
    (flycheck-mode)))

(use-package js
  :defer t
  :config (add-hook 'js-mode-hook #'basis/init-js-mode))

(defun basis/init-js2-mode ()
  (setq tab-width 4)
  (subword-mode)
  (js2-imenu-extras-setup)
  (js2-refactor-mode))

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'"
  :config (progn
            (setq-default js2-basic-offset 2)
            (setq-default js2-show-parse-errors nil)
            (setq-default js2-allow-rhino-new-expr-initializer nil)
            (setq-default js2-strict-inconsistent-return-warning nil)
            (setq-default js2-strict-missing-semi-warning nil)
            (setq-default js2-strict-trailing-comma-warning t)
            (define-key js2-mode-map (kbd "C-;") #'basis/eol-maybe-semicolon)
            (add-hook 'js2-mode-hook #'basis/init-js2-mode)))

(use-package rjsx-mode
  :ensure t
  :defer t)

(defun basis/enable-xref-js2 ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend))

(use-package xref-js2
  :ensure t
  :defer t
  :init (with-eval-after-load 'js2-mode
          (add-hook 'js2-mode-hook #'basis/enable-xref-js2)))

(defvar basis/tern-program nil)

(defun basis/maybe-enable-company-tern ()
  (when (or basis/tern-program
            (setq basis/tern-program (executable-find "tern")))
    (add-to-list 'company-backends 'company-tern)
    (tern-mode)))

(use-package tern
  :ensure t
  :defer t)

(use-package company-tern
  :ensure t
  :defer t
  :init (with-eval-after-load 'js2-mode
          (add-hook 'js2-mode-hook #'basis/maybe-enable-company-tern)))

(use-package js-comint
  :ensure t
  :defer t)

(use-package js2-refactor
  :ensure t
  :defer t
  :config (js2r-add-keybindings-with-prefix "C-c m"))

(use-package indium
  :ensure t
  :defer t)

(use-package skewer-mode
  :ensure t
  :after (js2-mode sgml-mode css-mode)
  :config (progn
            (skewer-setup)
            (let ((map skewer-mode-map))
              (define-key map (kbd "C-x C-e") #'skewer-eval-last-sexp)
              (define-key map (kbd "C-M-x")   #'skewer-eval-defun)
              (define-key map (kbd "C-c C-b") #'skewer-load-buffer))))

(use-package skewer-repl
  :defer t
  :config (define-key skewer-repl-mode-map (kbd "TAB") #'hippie-expand))

(use-package skewer-css
  :defer t
  :config
  (let ((map skewer-css-mode-map))
    (define-key map (kbd "C-x C-e") #'skewer-css-eval-current-declaration)
    (define-key map (kbd "C-M-x")   #'skewer-css-eval-current-rule)
    (define-key map (kbd "C-c C-b") #'skewer-css-eval-buffer)))

(defun basis/init-sql-mode ()
  (setq tab-width 4)
  (sql-set-product 'postgres))

(defun basis/init-sql-interactive-mode ()
  ;; Smartparens needs to be enabled here specifically, despite
  ;; `smartparens-global-strict-mode', because `sql-interactive-mode' does not
  ;; derive from `comint-mode' but does have a `mode-class' of `special'
  (smartparens-strict-mode))

(use-package sql
  :defer t
  ;; When using Emacs as $PSQL_EDITOR, open the files in `sql-mode'
  :mode ("/psql.edit.[0-9]+\\'" . sql-mode)
  :config (progn
            (let ((more-keywords '("unload" "elsif" "endif" "while")))
              (add-to-list
               'sql-mode-postgres-font-lock-keywords
               (apply #'sql-font-lock-keywords-builder
                      'font-lock-keyword-face nil more-keywords)))
            (let ((map sql-mode-map))
              (define-key map (kbd "TAB")   #'basis/sql-indent)
              (define-key map (kbd "DEL")   #'basis/sql-backspace-dedent)
              (define-key map (kbd "M-n")   #'basis/sql-forward-clause)
              (define-key map (kbd "M-p")   #'basis/sql-backward-clause)
              (define-key map (kbd "C-M-a") #'basis/sql-beginning-of-defun)
              (define-key map (kbd "C-M-e") #'basis/sql-end-of-defun))
            (add-hook 'sql-mode-hook #'basis/init-sql-mode)
            (add-hook 'sql-interactive-mode-hook
                      #'basis/init-sql-interactive-mode)
            ;; Put the advice on `sql-highlight-product' rather than
            ;; `sql-set-product' because the former is potentially re-run later,
            ;; as part of `hack-local-variables-hook', and would undo our
            ;; changes.
            (advice-add 'sql-highlight-product :after
                        #'basis/sql-after-highlight-product)))

(defun basis/init-c-base ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq-local comment-style 'extra-line)
  (dolist (cleanup '(brace-else-brace
                     brace-elseif-brace
                     defun-close-semi
                     empty-defun-braces))
    (add-to-list 'c-cleanup-list cleanup))
  (subword-mode))

(defun basis/init-c-mode ()
  (c-set-style "linux")
  (basis/init-c-base))

(defun basis/init-c++-mode ()
  (c-set-style "stroustrup")
  (basis/init-c-base)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'member-init-intro 2)
  (c-set-offset 'member-init-cont 0)
  (dolist (cleanup '(brace-catch-brace scope-operator))
    (add-to-list 'c-cleanup-list cleanup)))

(defun basis/init-java-mode ()
  (c-set-style "java")
  (basis/init-c-base))

(use-package cc-mode
  :defer t
  :config
  (progn
    (setq c-delete-function #'basis/c-delete)
    (setq c-backspace-function #'basis/c-backspace)
    (define-key c-mode-base-map [deletechar] #'c-electric-delete-forward)
    (define-key c-mode-base-map (kbd "C-j") #'c-context-line-break)
    (define-key c-mode-map (kbd "C-c C-v") #'ff-find-other-file)
    (add-hook 'c-mode-hook    #'basis/init-c-mode)
    (add-hook 'c++-mode-hook  #'basis/init-c++-mode)
    (add-hook 'java-mode-hook #'basis/init-java-mode)))

(defun basis/init-gud-mode ()
  ;; I haven't had a chance to look into it, but company-mode seems to trigger a
  ;; problem for me in GUD buffers on text terminals specifically.
  (unless (display-graphic-p)
    (company-mode -1)))

(use-package gud
  :defer t
  :config (add-hook 'gud-mode-hook #'basis/init-gud-mode))

(defun basis/init-go-mode ()
  (subword-mode))

(use-package go-mode
  :ensure t
  :defer t
  :config (add-hook 'go-mode-hook #'basis/init-go-mode))

(use-package lua-mode
  :ensure t
  :defer t)

(use-package swift-mode
  :ensure t
  :defer t)

(use-package nasm-mode
  :ensure t
  :defer t)

(defun basis/init-sh-mode ()
  (setq tab-width 4)
  (let* ((file buffer-file-name)
         (name (and file (file-name-nondirectory file)))
         (extn (and file (file-name-extension file))))
    (when (and name (null extn))
      (cond ((string-match-p "\\`\\.bash" name)
             (sh-set-shell "bash"))
            ((string-match-p "\\`\\.zsh" name)
             (sh-set-shell "zsh"))))))

(use-package sh-script
  :defer t
  :config (add-hook 'sh-mode-hook #'basis/init-sh-mode))

(use-package gforth
  :defer t
  :mode ("\\.f\\'" "\\.fs\\'" "\\.fth\\'")
  :config (progn (define-key forth-mode-map (kbd "M-o") nil)
                 (define-key forth-mode-map (kbd "M-SPC") nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text, markup, and configuration modes

(defun basis/init-text-mode ()
  (auto-fill-mode)
  (basis/maybe-enable-flyspell)
  (whitespace-mode)
  (setq indicate-empty-lines t)
  (when (and ispell-alternate-dictionary
             (not (derived-mode-p 'sgml-mode 'nxml-mode)))
    (add-to-list 'company-backends 'company-ispell)))

(add-hook 'text-mode-hook #'basis/init-text-mode)

(defun basis/init-log-view-mode ()
  (basis/maybe-enable-bug-reference-mode))

(use-package log-view
  :defer t
  :config (add-hook 'log-view-mode-hook #'basis/init-log-view-mode))

(use-package org
  :ensure t
  :defer t
  :init
  (progn (global-set-key (kbd "C-c a") #'org-agenda)
         (global-set-key (kbd "C-c c") #'org-capture)
         (global-set-key (kbd "C-c l") #'org-store-link))
  :config
  (progn
    (setq org-directory "~/Dropbox/org/")
    (setq org-default-notes-file (expand-file-name "refile.org" org-directory))
    (setq org-archive-location "%s.archive::")
    (setq org-agenda-files (mapcar (lambda (name)
                                     (expand-file-name name org-directory))
                                   '("todo.org" "work.org")))
    (setq org-completion-use-ido t)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-reverse-note-order t)
    (setq org-log-done t)
    (setq org-special-ctrl-a/e t)
    (setq org-M-RET-may-split-line '((default . nil)))
    (let ((ellipsis ?…))
      (when (char-displayable-p ellipsis)
        (setq org-ellipsis (string ellipsis))))
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-src-fontify-natively t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-capture-templates
          `(("t" "Todo" entry (file+headline ,org-default-notes-file "Tasks")
             "* TODO %?\n %i\n")
            ("w" "Work todo" entry (file+headline "~/Dropbox/org/work.org"
                                                  "Tasks")
             "* TODO %?\n %i\n")
            ("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
             "* %u %?")))
    (setq org-refile-use-outline-path 'file)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-targets '((nil :maxlevel . 2)
                               (org-agenda-files :maxlevel . 2)))
    (setq org-todo-keywords
          '((sequence
             "TODO(t)" "STARTED(s@)" "WAITING(w@/!)" "DELEGATED(l@)" "|"
             "DONE(d!)" "DEFERRED(f@)" "CANCELED(c@)")))
    (let ((names (delq nil (mapcar (lambda (elt)
                                     (and (string-match "\\`\\([A-Z]+\\)" elt)
                                          (match-string 1 elt)))
                                   (cdar org-todo-keywords)))))
      (setq basis/org-todo-keyword-regexp (regexp-opt names 'symbols)))
    (advice-add 'org-beginning-of-line :around
                #'basis/org-maybe-beginning-of-todo-keyword)
    (define-key org-mode-map (kbd "RET") #'org-return-indent)
    (org-babel-do-load-languages
     'org-babel-load-languages
     (mapcar (lambda (sym) (cons sym t))
             (cons (if (locate-file "ob-shell" load-path '(".el" ".el.gz"))
                       'shell
                     'sh)
                   '(C calc clojure emacs-lisp haskell python scheme))))))

(use-package ob-clojure
  :defer t
  :config (setq org-babel-clojure-backend 'cider))

(defun basis/init-html-mode ()
  (setq tab-width 4)
  (tagedit-mode))

(use-package sgml-mode
  :defer t
  :config
  (progn
    (let ((map html-mode-map))
      (define-key map [remap forward-paragraph]  #'basis/next-blank-line)
      (define-key map [remap backward-paragraph] #'basis/previous-blank-line)
      (define-key map (kbd "RET")     #'basis/html-newline-and-indent)
      (define-key map (kbd "M-RET")   #'basis/html-multiline-expand)
      (define-key map (kbd "C-c C-w") #'basis/html-wrap-in-tag)
      (define-key map (kbd "C-c w")   #'basis/html-wrap-in-tag))
    (add-hook 'html-mode-hook #'basis/init-html-mode)
    (advice-add 'sgml-delete-tag :after #'basis/sgml-delete-tag-reindent)))

(use-package tagedit
  :ensure t
  :commands (tagedit-mode)
  :config (progn (tagedit-add-paredit-like-keybindings)
                 (tagedit-add-experimental-features)
                 (advice-add 'tagedit-toggle-multiline-tag :around
                             #'basis/tagedit-toggle-multiline-maybe-forward)))

(defun basis/init-markdown-mode ()
  (setq tab-width 4)
  (whitespace-mode)
  (when (eq major-mode 'gfm-mode)
    (setq-local whitespace-style '(face trailing tabs))
    (auto-fill-mode -1)
    (visual-line-mode)))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")
  :config
  (progn
    (let ((map markdown-mode-map))
      (define-key map (kbd "DEL")     #'basis/sp-markdown-backspace)
      (define-key map (kbd "M-n")     #'forward-paragraph)
      (define-key map (kbd "M-p")     #'backward-paragraph)
      (define-key map (kbd "C-c r")   #'markdown-insert-reference-link-dwim)
      (define-key map (kbd "C-c C-r") #'markdown-insert-reference-link-dwim))
    (add-hook 'markdown-mode-hook #'basis/init-markdown-mode)))

(use-package writegood-mode
  :ensure t
  :defer t)

(defun basis/init-yaml-mode ()
  (basis/maybe-enable-flyspell-prog-mode))

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (progn (define-key yaml-mode-map (kbd "DEL") #'basis/sp-yaml-backspace)
         (add-hook 'yaml-mode-hook #'basis/init-yaml-mode)))

(use-package deft
  :ensure t
  :defer t
  :config
  (progn
    (setq deft-extensions '("mkd" "md" "markdown" "rst" "org" "txt"))
    (setq deft-directory (if (file-directory-p "~/Dropbox/")
                             "~/Dropbox/Notes/"
                           "~/.deft/"))
    (setq deft-archive-directory (expand-file-name "archive/" deft-directory))
    (setq deft-text-mode 'markdown-mode)
    (setq deft-file-naming-rules '((noslash . "-")
                                   (nospace . "-")
                                   (case-fn . downcase)))
    (define-key deft-mode-map (kbd "C-w") #'deft-filter-decrement-word)
    (define-key deft-mode-map (kbd "C-k") #'deft-filter-clear)))

(use-package csv-mode
  :ensure t
  :defer t
  :init (setq auto-mode-alist
              (seq-remove (lambda (elt)
                            (or (eq (cdr elt) 'csv-mode)
                                (eq (car-safe (cdr elt)) 'csv-mode)))
                          auto-mode-alist))
  :config (setq auto-mode-alist
                (seq-remove (lambda (elt)
                              (or (eq (cdr elt) 'csv-mode)
                                  (eq (car-safe (cdr elt)) 'csv-mode)))
                            auto-mode-alist)))

(defun basis/init-ssh-config-mode ()
  (setq-local tab-stop-list nil)
  (setq-local tab-width 4))

(use-package ssh-config-mode
  :ensure t
  :defer t
  :init (autoload 'ssh-authorized-keys-mode "ssh-config-mode" nil t)
  :config
  (progn (add-hook 'ssh-config-mode-hook #'basis/init-ssh-config-mode)
         (define-key ssh-config-mode-map (kbd "TAB") #'tab-to-tab-stop)))

(use-package css-mode
  :defer t
  :init (put 'css-indent-offset 'safe-local-variable #'integerp))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package terraform-mode
  :ensure t
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error checking

(use-package ispell
  :defer t
  ;; M-$ is Command-$ in my MacOS configuration, which is shadowed by a system
  ;; screenshot command
  :init (global-set-key (kbd "s-$") #'ispell-word)
  :config
  (progn (setq ispell-program-name (executable-find "aspell"))
         (setq ispell-personal-dictionary "~/.aspell.en.pws")
         (setq ispell-extra-args '("--sug-mode=ultra"))
         (let ((file "~/Dropbox/dict/words"))
           (when (and (not ispell-alternate-dictionary)
                      (file-readable-p file)
                      (not (ignore-errors (lookup-words "whatever"))))
             (setq ispell-alternate-dictionary file)))))

(use-package flyspell
  :defer t
  :config
  (let ((map flyspell-mode-map))
    (define-key map (kbd "C-,")  nil)
    (define-key map (kbd "C-.")  nil)
    (define-key map (kbd "C-;")  nil)
    (define-key map (kbd "C-:")  #'flyspell-goto-next-error)
    (define-key map (kbd "C-\"") #'flyspell-auto-correct-previous-word)))

(use-package flycheck
  :ensure t
  :defer t
  :init
  (progn
    (basis/define-prefix-command 'basis/flycheck-map (kbd "<f8>"))
    (let ((map basis/flycheck-map))
      (define-key map (kbd "c")    #'flycheck-buffer)
      (define-key map (kbd "n")    #'flycheck-next-error)
      (define-key map (kbd "p")    #'flycheck-previous-error)
      (define-key map (kbd "l")    #'flycheck-list-errors)
      (define-key map (kbd "s")    #'flycheck-select-checker)
      (define-key map (kbd "C")    #'flycheck-clear)
      (define-key map (kbd "<f8>") #'basis/flycheck-check-and-list-errors)))
  :config (progn
            (setq flycheck-check-syntax-automatically nil)
            ;; Check buffers with errors more frequently than ones without
            (make-variable-buffer-local 'flycheck-idle-change-delay)
            (add-hook 'flycheck-after-syntax-check-hook
                      #'basis/adjust-flycheck-idle-change-delay)))

(use-package flycheck-pyflakes
  :ensure t
  :after flycheck)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version control

(defun basis/init-diff-mode ()
  (basis/maybe-enable-bug-reference-mode))

(use-package diff-mode
  :defer t
  :config
  (progn
    (setq diff-default-read-only t)
    ;; `diff-goto-source' is still available on C-c C-c.
    (define-key diff-mode-map (kbd "M-o") nil)
    (add-hook 'diff-mode-hook #'basis/init-diff-mode)))

(defun basis/init-ediff ()
  (ediff-setup-keymap))

(use-package ediff
  :defer t
  :config (progn
            (setq ediff-window-setup-function #'ediff-setup-windows-plain)
            (setq ediff-split-window-function #'split-window-horizontally)
            (advice-add 'ediff-setup :before #'basis/ediff-save-window-config)
            (advice-add 'ediff-quit :after #'basis/ediff-quit-restore)
            (add-hook 'ediff-mode-hook #'basis/init-ediff)))

(use-package vc
  :defer t
  :init (global-set-key (kbd "C-x v h") #'vc-region-history)
  :config (setq vc-follow-symlinks t))

(use-package magit
  :ensure t
  :defer t
  :init (progn
          (global-set-key (kbd "C-x g")   #'magit-status)
          (global-set-key (kbd "C-x M-g") #'magit-dispatch)
          (global-set-key (kbd "C-c M-g") #'magit-file-dispatch))
  :config
  (progn
    (setq magit-save-repository-buffers 'dontask)
    (setq magit-clone-set-remote.pushDefault t)
    (setq magit-branch-popup-show-variables nil)
    (setq magit-completing-read-function #'magit-ido-completing-read)
    (setq magit-revision-show-gravatars nil)
    (setq magit-use-sticky-arguments 'current)
    (setq magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1)
    (when (require 'projectile nil t)
      (setq magit-repository-directories
            (seq-reduce (lambda (result file)
                          (let ((.git (expand-file-name ".git" file)))
                            (if (and (not (file-remote-p file))
                                     (file-exists-p .git))
                                (cons (cons (directory-file-name file) 0)
                                      result)
                              result)))
                        projectile-known-projects
                        nil)))
    (define-key basis/file-map "g" #'magit-find-file)
    (define-key ctl-x-4-map "g" #'magit-find-file-other-window)
    (define-key magit-status-mode-map
      (kbd "C-c C-v") #'basis/magit-browse-pull-request-url)
    ;; Don't (sometimes) show recent commits in magit-status
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-unpulled-from-upstream
                            'magit-insert-unpulled-from-upstream-or-recent
                            'replace)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-unpushed-to-upstream
                            'magit-insert-unpushed-to-upstream-or-recent
                            'replace)))

(use-package transient
  :ensure t
  :defer t
  :config (progn (setq transient-history-limit 20)
                 (setq transient-detect-key-conflicts t)))

(use-package with-editor
  :ensure t
  :defer t)

(use-package gist
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(defun basis/init-git-commit-mode ()
  (setq-local fill-column 72))

(use-package git-commit
  :ensure t
  :config
  (progn
    (global-git-commit-mode)
    (add-to-list 'git-commit-setup-hook #'basis/init-git-commit-mode)
    (add-to-list 'git-commit-style-convention-checks 'overlong-summary-line)))

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package diff-hl
  :ensure t
  :defer t)

(use-package browse-at-remote
  :ensure t
  :defer t
  :init (global-set-key (kbd "M-g r") #'browse-at-remote))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project management

(defun basis/init-ibuffer-mode ()
  (setq basis/beginning-of-buffer-function #'basis/ibuffer-beginning-of-buffer)
  (setq basis/end-of-buffer-function #'basis/ibuffer-end-of-buffer))

(use-package ibuffer
  :defer t
  :init (progn (defalias 'ls #'ibuffer)
               (global-set-key [remap list-buffers] #'ibuffer))
  :config
  (progn
    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (vc-status 16 16 :left)
                  " "
                  filename-and-process)
            (mark " "
                  (name 18 18 :left :elide)
                  " "
                  filename)))
    (setq ibuffer-show-empty-filter-groups nil)
    (let ((map ibuffer-mode-map))
      (define-key map (kbd "M-o")   nil)
      (define-key map (kbd "C-M-o") #'ibuffer-visit-buffer-1-window)
      (define-key map [remap beginning-of-buffer] #'basis/beginning-of-buffer)
      (define-key map [remap end-of-buffer]       #'basis/end-of-buffer))
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond ((> (buffer-size) 1000000)
             (format "%7.1fM" (/ (buffer-size) 1000000.0)))
            ((> (buffer-size) 1000)
             (format "%7.1fk" (/ (buffer-size) 1000.0)))
            (t
             (format "%8d" (buffer-size)))))
    (add-hook 'ibuffer-mode-hook #'basis/init-ibuffer-mode)))

(use-package ibuffer-vc
  :ensure t
  :after ibuffer
  :config
  (progn
    (define-key ibuffer-mode-map "\\" #'basis/ibuffer-toggle-vc-grouping)
    (advice-add 'ibuffer-vc-root :around #'basis/ibuffer-vc-root-files-only)))

(use-package etags
  :defer t
  :config (setq tags-revert-without-query t))

(use-package xref
  :defer t
  :config (let ((map xref--xref-buffer-mode-map))
            (define-key map (kbd "M-}") #'basis/xref-next-group)
            (define-key map (kbd "M-{") #'basis/xref-prev-group)))

(use-package ztree
  :ensure t
  :defer t)

(basis/define-prefix-command 'basis/projectile-map)

(let ((map basis/projectile-map))
  (define-key map (kbd "b") #'projectile-switch-to-buffer)
  (define-key map (kbd "d") #'projectile-find-dir)
  (define-key map (kbd "D") #'projectile-find-file-in-directory)
  (define-key map (kbd "f") #'projectile-find-file)
  (define-key map (kbd "F") #'projectile-find-file-dwim)
  (define-key map (kbd "g") #'projectile-grep)
  (define-key map (kbd "i") #'projectile-ibuffer)
  (define-key map (kbd "K") #'projectile-kill-buffers)
  (define-key map (kbd "o") #'projectile-multi-occur)
  (define-key map (kbd "p") #'projectile-switch-project)
  (define-key map (kbd "r") #'projectile-recentf)
  (define-key map (kbd "x") #'projectile-remove-known-project)
  (define-key map (kbd "X") #'projectile-cleanup-known-projects)
  (define-key map (kbd "z") #'projectile-cache-current-file))

(use-package projectile
  :ensure t
  :init
  (when (eq system-type 'darwin)
    (global-set-key (kbd "s-b") #'projectile-switch-to-buffer)
    (global-set-key (kbd "s-f") #'projectile-find-file)
    (global-set-key (kbd "s-g") #'projectile-grep))
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'ido)
    (setq projectile-known-projects-file
          (basis/emacs-file "var/projectile-bookmarks.eld"))
    (setq projectile-cache-file (basis/emacs-file "var/projectile.cache"))
    (setq projectile-use-git-grep t)
    (projectile-global-mode)
    (global-set-key projectile-keymap-prefix
                    'basis/projectile-map)
    (advice-add 'projectile-regenerate-tags :before
                #'basis/projectile-save-some-buffers))
  :diminish projectile-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Processes, shells, and the filesystem

(use-package compile
  :defer t
  :init (progn
          (global-set-key (kbd "C-c b c") #'compile)
          (global-set-key (kbd "C-c b b") #'recompile)
          (global-set-key (kbd "C-c b t") #'basis/make-tags))
  :config (progn
            (setq compilation-ask-about-save nil)
            (setq compilation-always-kill t)
            (setq compilation-scroll-output 'first-error)
            (setq compilation-context-lines 2)
            (require 'ansi-color)
            (add-hook 'compilation-filter-hook #'basis/colorize-compilation)))

(defun basis/init-dired-mode ()
  (dired-omit-mode)
  (when (and (eq system-type 'darwin)
             (not (file-remote-p default-directory))
             (executable-find "gls"))
    (setq-local insert-directory-program "gls"))
  (setq basis/beginning-of-buffer-function #'basis/dired-beginning-of-buffer)
  (setq basis/end-of-buffer-function #'basis/dired-end-of-buffer))

(use-package dired
  :defer t
  :init (global-set-key (kbd "C-x C-d") #'dired)
  :config
  (progn
    (let ((map dired-mode-map))
      (define-key map (kbd "RET")   #'dired-find-alternate-file)
      (define-key map (kbd "M-RET") #'dired-find-file)
      (define-key map (kbd "e")     #'basis/open-file-externally)
      (define-key map (kbd "-")     #'diredp-up-directory-reuse-dir-buffer)
      (define-key map (kbd "^")     #'diredp-up-directory-reuse-dir-buffer)
      (define-key map (kbd "Y")     #'basis/dired-rsync)
      (define-key map (kbd "M-^")   #'diredp-up-directory)
      (define-key map (kbd "M-m")   #'dired-omit-mode)
      (define-key map (kbd "M-n")   #'diredp-next-subdir)
      (define-key map (kbd "M-p")   #'diredp-prev-subdir)
      (define-key map (kbd "M-e")   #'dired-next-dirline)
      (define-key map (kbd "M-a")   #'dired-prev-dirline)
      (define-key map (kbd "M-o")   nil)
      (define-key map [remap beginning-of-buffer] #'basis/beginning-of-buffer)
      (define-key map [remap end-of-buffer]       #'basis/end-of-buffer))
    (setq dired-recursive-deletes 'top)
    (setq dired-listing-switches "-alht")
    (put 'dired-find-alternate-file 'disabled nil)
    (add-hook 'dired-mode-hook #'basis/init-dired-mode)))

(use-package dired-x
  :commands (dired-omit-mode)
  :config
  (progn (setq dired-omit-verbose nil)
         (setq dired-omit-extensions (remove ".bak" dired-omit-extensions))))

(use-package dired-aux
  :after dired)

(use-package find-dired
  :after dired
  :config (setq find-ls-option '("-exec ls -ldh {} +" . "-ldh")))

(use-package image-dired
  :defer t
  ;; Set `image-dired-dir' before loading the package because several other
  ;; options' default values are defined relative to it.
  :init (setq image-dired-dir (basis/emacs-dir "var/image-dired/")))

(use-package dired+
  :after dired
  :config (progn (define-key dired-mode-map (kbd "M-b") nil)
                 (define-key dired-mode-map (kbd "M-B") #'diredp-do-bookmark)))

(defun basis/init-comint-mode ()
  (setq comint-scroll-to-bottom-on-input 'this))

(use-package comint
  :defer t
  :init (setenv "PAGER" "cat")
  :config
  (progn
    (let ((map comint-mode-map))
      (define-key map (kbd "C-d") #'basis/sp-comint-delchar-or-maybe-eof)
      (define-key map (kbd "M-p") #'comint-previous-matching-input-from-input)
      (define-key map (kbd "M-n") #'comint-next-matching-input-from-input)
      (define-key map (kbd "C-M-r") #'comint-history-isearch-backward-regexp))
    (dolist (cmd '(comint-previous-input
                   comint-next-input
                   comint-previous-matching-input-from-input
                   comint-next-matching-input-from-input))
      (advice-add cmd :before #'basis/comint-input-goto-bottom-if-necessary))
    (add-hook 'comint-mode-hook #'basis/init-comint-mode)))

(defun basis/init-shell-mode ()
  (setq comint-process-echoes t)
  (setq-local scroll-conservatively 101)
  (let* ((shell (thread-first (current-buffer)
                  get-buffer-process
                  process-command
                  car
                  file-name-nondirectory))
         (query (cond ((string-match-p "bash" shell)
                       "command dirs")
                      ((string-match-p "zsh" shell)
                       "dirs -l"))))
    (when query (setq shell-dirstack-query query)))
  (shell-dirtrack-mode -1)
  (dirtrack-mode)
  (add-hook 'comint-preoutput-filter-functions
            #'basis/dirtrack-filter-pwd t t))

(use-package shell
  :defer t
  :config (add-hook 'shell-mode-hook #'basis/init-shell-mode))

(use-package dirtrack
  :defer t
  :config
  (setq-default dirtrack-list '("^\r*|_P_W_D_:|\\([^|]*\\)|" 1)))

(use-package esh-mode
  :defer t
  :config (setq eshell-directory-name (basis/emacs-dir "var/eshell/")))

(use-package proced
  :defer t
  :init (global-set-key (kbd "C-x p") #'proced))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Applications

(use-package elfeed
  :ensure t
  :defer t
  :config (progn (setq elfeed-db-directory (basis/emacs-dir "var/elfeed/"))
                 (when-let* ((curl (executable-find "curl")))
                   (setq elfeed-curl-program-name curl)
                   (setq elfeed-use-curl t))
                 (let ((feeds (basis/emacs-file "feeds.eld")))
                   (when (file-exists-p feeds)
                     (setq elfeed-feeds (basis/elfeed-load-feeds feeds))))))

(use-package shr
  :defer t
  :config (progn (setq shr-use-colors nil)
                 (setq shr-use-fonts nil)))

(use-package eww
  :defer t
  :config
  (progn (define-key eww-mode-map (kbd "<backtab>") #'shr-previous-link)
         (when-let* ((directory
                      (or (basis/xdg-user-dir 'download)
                          (seq-find #'file-directory-p
                                    '("~/downloads/" "~/Downloads/")))))
           (setq eww-download-directory directory))))

(use-package browse-url
  :defer t
  :init
  (unless (display-graphic-p)
    (setq browse-url-browser-function #'eww-browse-url))
  :config
  (advice-add 'browse-url-url-at-point :around
              #'basis/browse-url-url-at-point-https))

(defun basis/init-sx-question-mode ()
  (toggle-truncate-lines -1))

(use-package sx
  :ensure t
  :defer t
  :config (setq sx-cache-directory (basis/emacs-dir "var/sx/")))

(use-package sx-question-list
  :defer t
  :config
  (define-key sx-question-list-mode-map
    (kbd "M-RET")
    #'basis/sx-display-full-screen))

(use-package sx-question-mode
  :defer t
  :config
  (progn
    (define-key sx-question-mode-map (kbd "<up>") nil)
    (define-key sx-question-mode-map (kbd "<down>") nil)
    (add-hook 'sx-question-mode-hook #'basis/init-sx-question-mode)))

(use-package debbugs
  :ensure t
  :defer t
  :config (setq debbugs-gnu-persistency-file (basis/emacs-file "var/debbugs")))

(use-package define-word
  :ensure t
  :defer t
  :init (global-set-key (kbd "C-c d") #'basis/define-word))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Email & news

(defun basis/init-message-mode ()
  (setq fill-column 72)
  (setq-local org-footnote-tag-for-non-org-mode-files nil))

(use-package message
  :defer t
  :init (global-set-key (kbd "C-x M") #'basis/compose-message)
  :config
  (progn
    (setq message-user-fqdn "jbm.io")
    (setq message-auto-save-directory (basis/emacs-dir "tmp/"))
    (setq message-subject-trailing-was-query nil)
    (setq message-signature "jbm")
    (setq message-kill-buffer-on-exit t)
    (setq message-dont-reply-to-names nil)
    (setq message-send-mail-function #'smtpmail-send-it)
    (setq message-citation-line-function
          #'message-insert-formatted-citation-line)
    (setq message-citation-line-format "%N wrote:\n")
    (define-key message-mode-map (kbd "C-c n") #'org-footnote-action)
    (add-hook 'message-mode-hook #'basis/init-message-mode)
    (advice-add 'message-insert-signature :after
                #'basis/message-maybe-delete-sig-dashes)))

(use-package sendmail
  :defer t
  :config (setq send-mail-function #'smtpmail-send-it))

(use-package smtpmail
  :defer t
  :config (progn (setq smtpmail-smtp-server "smtp.fastmail.com")
                 (setq smtpmail-smtp-user "jbm@fastmail.com")
                 (setq smtpmail-smtp-service 465)
                 (setq smtpmail-stream-type 'ssl)))

(use-package mu4e
  :defer t
  :init
  (progn
    (global-set-key (kbd "C-x M-m") #'mu4e)
    (let ((dir "/usr/local/share/emacs/site-lisp/mu4e/"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)
        (autoload 'mu4e "mu4e" "Launch mu4e." t))))
  :config
  (progn
    (setq mail-user-agent #'mu4e-user-agent)
    (setq mu4e-get-mail-command "offlineimap")
    (setq mu4e-maildir (expand-file-name "~/.mail/fastmail"))
    (setq mu4e-sent-folder "/sent")
    (setq mu4e-drafts-folder "/drafts")
    (setq mu4e-trash-folder "/trash")
    (setq mu4e-compose-signature "jbm")
    (setq mu4e-maildir-shortcuts '(("/archive" . ?a)
                                   ("/inbox"   . ?i)
                                   ("/sent"    . ?s)))
    (setq mu4e-user-mail-address-list '("jbm@jbm.io"
                                        "jbm@deft.li"
                                        "jbm@fastmail.com"
                                        "jbm@fastmail.fm"
                                        "jbm@mailforce.net"))
    (setq mu4e-html2text-command (if (executable-find "html2text")
                                     "html2text -utf8 -width 72"
                                   #'basis/shr-html2text))
    (let ((dirs '("~/downloads" "~/Downloads" "~/")))
      (setq mu4e-attachment-dir (seq-find #'file-directory-p dirs)))
    (setq mu4e-reply-to-address "jbm@jbm.io")
    (setq mu4e-sent-messages-behavior 'delete)
    (require 'mu4e-actions)
    (dolist (action '(("open in browser" . mu4e-action-view-in-browser)
                      ("open in eww"     . mu4e-action-view-in-eww)))
      (add-to-list 'mu4e-view-actions action t))))

(use-package auth-source
  :defer t
  :config
  (setq auth-sources
        (cond ((and (featurep 'dbusbind)
                    (require 'secrets nil t)
                    (bound-and-true-p secrets-enabled))
               ;; Probably only `default' and ~/.authinfo.gpg are needed (?)
               '(default "secrets:session" "secrets:Login" "~/.authinfo.gpg"))
              ((eq system-type 'darwin)
               '(macos-keychain-internet))
              (t
               '("~/.authinfo.gpg")))))

(use-package gnus
  :defer t
  :init (setq gnus-init-file (basis/emacs-file ".gnus"))
  :config (progn (setq gnus-use-dribble-file nil)
                 (setq gnus-always-read-dribble-file nil)
                 (setq gnus-read-newsrc-file nil)
                 (setq gnus-save-newsrc-file nil)))

(use-package mml
  :defer t
  :config (dolist (cmd '(mml-attach-buffer mml-attach-file mml-attach-external))
            (advice-add cmd :around #'basis/mml-attach-at-eob)))

(use-package mm-decode
  :defer t
  :config (add-to-list 'mm-discouraged-alternatives "text/html"))

(use-package erc
  :defer t
  :config (when (null erc-accidental-paste-threshold-seconds)
            (setq erc-accidental-paste-threshold-seconds 0.2)))


;;; init.el ends here
