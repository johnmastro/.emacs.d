;;; init.el      -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Early configuration

;; Disable superfluous UI immediately to prevent momentary display
(let ((modes '(menu-bar-mode
               tool-bar-mode
               scroll-bar-mode
               horizontal-scroll-bar-mode)))
  (dolist (mode (if (eq window-system 'ns) (cdr modes) modes))
    (when (fboundp mode)
      (funcall mode -1))))

(defconst basis/emacs-dir
  (file-name-directory (file-chase-links (or load-file-name buffer-file-name)))
  "This Emacs's configuration directory.")

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

;; So `find-function' works for C functions in Emacsen I didn't build myself
(unless (file-directory-p source-directory)
  (let ((dir (format "~/src/emacs-%s/" emacs-version)))
    (when (file-directory-p dir)
      (setq source-directory dir))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set up package.el

(setq load-prefer-newer t)

(let ((dir (format "elpa/%d/" emacs-major-version)))
  (setq package-user-dir (basis/emacs-dir dir)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Opt out of automatically saving a list of installed packages
(when (fboundp 'package--save-selected-packages)
  (advice-add 'package--save-selected-packages :override #'ignore))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


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

(use-package async-bytecomp
  :ensure async
  :defer t
  :config
  (when (eq system-type 'windows-nt)
    (with-eval-after-load 'async-bytecomp
      (setq async-bytecomp-allowed-packages nil))))

(use-package map)

(use-package dash
  :ensure t
  :defer t)

(use-package dash-functional
  :ensure t
  :defer t)

(use-package s
  :ensure t
  :defer t)

(use-package persistent-soft
  :ensure t
  :defer t)

(use-package request
  :ensure t
  :defer t)

(load (basis/emacs-file "defuns") nil nil nil 'must-suffix)
(load (basis/emacs-file "local") 'noerror nil nil 'must-suffix)
(load (basis/emacs-file (concat "init-" (system-name)))
      'noerror nil nil 'must-suffix)

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

(use-package xclip
  :ensure t
  :config
  (when (and (not (display-graphic-p))
             (or (and (memq system-type '(gnu/linux berkeley-unix))
                      (getenv "DISPLAY")
                      (executable-find "xclip"))
                 (and (eq system-type 'darwin)
                      (executable-find "pbcopy"))))
    (xclip-mode)))

(defvar basis/system-type system-type
  "Like `system-type' but with the additional option `windows+cygwin'.")

(defvar basis/cygwin-path-directories
  '("/bin" "/usr/bin" "/usr/local/bin"
    "/Windows" "/ProgramData/Oracle/Java/javapath")
  "Directories to add to PATH on Cygwin.")

(defvar basis/pre-cygwin-process-environment nil
  "Value of `process-environment' before initializing Cygwin.")

(defun basis/init-for-cygwin ()
  (unless basis/pre-cygwin-process-environment
    (setq basis/pre-cygwin-process-environment
          (mapcar #'copy-sequence process-environment)))
  (let* ((home (basis/cygwinize-file-name (or (getenv "HOME")
                                              (error "HOME not defined"))))
         (home/bin (concat (basis/cygwinize-file-name home)
                           (unless (string-suffix-p "/" home) "/")
                           "bin"))
         (path (cons home/bin basis/cygwin-path-directories)))
    (when (and (file-directory-p home) (not after-init-time))
      (cd home))
    (setenv "PATH" (mapconcat #'identity path ":"))
    (setq exec-path (mapcar (lambda (dir) (concat "c:" dir)) path))
    (let ((shell (or (executable-find "zsh")
                     (executable-find "bash"))))
      (setq shell-file-name shell)
      (setq explicit-shell-file-name shell)
      (setq ediff-shell shell)
      (setq null-device "/dev/null")
      (setenv "SHELL" shell))
    (advice-add 'shell-quote-argument :filter-args
                #'basis/cygwin-shell-quote-argument)
    (setq basis/system-type 'windows+cygwin)))

(when (and (eq basis/system-type 'windows-nt)
           (file-executable-p "c:/bin/bash.exe"))
  (basis/init-for-cygwin))


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
  (let ((case-fold-search (memq system-type '(windows-nt cygwin darwin))))
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

(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

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

(defun basis/maybe-set-coding ()
  (when (and (eq system-type 'windows-nt)
             (when-let* ((method (file-remote-p buffer-file-name 'method)))
               (string-match-p "ssh\\|scp\\|plink" method)))
    (set-buffer-file-coding-system 'utf-8-unix)))

(defvar basis/external-temporary-file-directory nil)

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
    (setq basis/external-temporary-file-directory
          (if (file-in-directory-p temporary-file-directory basis/emacs-dir)
              (seq-find #'file-directory-p
                        (if (eq system-type 'windows-nt)
                            '("e:/tmp/" "d:/tmp/" "c:/tmp/")
                          '("/tmp/")))
            temporary-file-directory))
    (when (eq system-type 'windows-nt)
      (add-hook 'before-save-hook #'basis/maybe-set-coding))
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
  :config (progn (setq ffap-machine-p-known 'reject)
                 (when (eq system-type 'windows-nt)
                   (advice-add 'ffap-file-at-point :around
                               #'basis/ffap-more-file-at-point))))

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
         (setq recentf-exclude (list #'file-remote-p))
         (recentf-mode)))

(use-package tramp
  :defer t
  :config
  (progn
    (setq tramp-persistency-file-name (basis/emacs-file "var/tramp"))
    (setq tramp-default-method
          (pcase basis/system-type
            (`windows+cygwin "scpx")
            (`windows-nt     "pscp")
            (_               "scp")))
    (when (eq basis/system-type 'windows+cygwin)
      (setq tramp-encoding-shell (executable-find "sh"))
      (setq tramp-encoding-command-switch "-c")
      (setq tramp-encoding-command-interactive "-i"))))

(use-package time
  :defer t
  :config (when (eq display-time-world-list zoneinfo-style-world-list)
            (setq display-time-world-list
                  '(("America/Los_Angeles" "Los Angeles")
                    ("America/Denver"      "Denver")
                    ("America/Chicago"     "Chicago")
                    ("America/New_York"    "New York")
                    ("Europe/London"       "London")
                    ("Europe/Paris"        "Paris")
                    ("Europe/Moscow"       "Moscow")
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
  :init (let ((val (if (and (not (eq system-type 'windows-nt))
                            (display-graphic-p)
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
      (list (concat "%b | " invocation-name "@" (system-name))
            '(:eval (and buffer-file-name '(" | " buffer-file-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation modes

(use-package help-mode
  :defer t
  :config (progn (define-key help-mode-map "n" #'next-line)
                 (define-key help-mode-map "p" #'previous-line)))

(use-package help-at-pt
  :config (progn (setq help-at-pt-display-when-idle t)
                 (help-at-pt-set-timer)))

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

(pcase system-type
  (`darwin
   (setq mac-command-modifier 'meta)
   (setq mac-option-modifier 'super))
  (`windows-nt
   (setq w32-pass-apps-to-system nil)
   (setq w32-lwindow-modifier 'super)
   (setq w32-rwindow-modifier 'super)))

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

(global-set-key [remap move-beginning-of-line] #'basis/beginning-of-line)

(global-set-key (kbd "<S-return>") #'basis/open-line-above)
(global-set-key (kbd "<C-return>") #'basis/open-line-below)

(global-set-key [remap forward-sentence]  #'forward-sexp)
(global-set-key [remap backward-sentence] #'backward-sexp)
(global-set-key [remap kill-sentence]     #'kill-sexp)

(global-set-key [remap kill-region]        #'basis/kill-something)
(global-set-key [remap backward-kill-word] #'basis/kill-something)
(global-set-key (kbd "<M-delete>")         #'basis/smart-kill-whole-line)

(global-set-key [remap kill-ring-save] #'basis/kill-ring-save-something)
(global-set-key (kbd "ESC M-w") #'basis/kill-ring-save-as-fenced-code-block)
(global-set-key (kbd "<f2>")    #'basis/clipboard-save-something)

(global-set-key [remap delete-indentation] #'basis/delete-indentation)
(global-set-key [remap fill-paragraph]     #'basis/fill-or-unfill-paragraph)

(basis/define-prefix-command 'basis/insertion-map (kbd "C-c i"))
(define-key basis/insertion-map "f" #'basis/insert-file-name)
(define-key basis/insertion-map "F" #'basis/insert-files)
(define-key basis/insertion-map "p" #'basis/insert-file-preamble)

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
(define-key basis/file-map "c" #'helm-locate)
(define-key basis/file-map "d" #'basis/diff-buffer-with-file)
(define-key basis/file-map "r" #'basis/rename-buffer-file)
(define-key basis/file-map "D" #'basis/delete-buffer-file)
(define-key basis/file-map "f" #'find-name-dired)
(define-key basis/file-map "F" #'find-dired)
(define-key basis/file-map "m" #'make-directory)
(define-key basis/file-map "n" #'basis/kill-ring-save-buffer-file-name)
(define-key basis/file-map "v" #'revert-buffer)

(global-set-key (kbd "C-c x") #'basis/open-file-externally)

;; Wrap text in various styles of quotes
(global-set-key (kbd "C-c q") #'basis/quote-thing)

(basis/define-prefix-command 'basis/region-map (kbd "C-c r"))
(define-key basis/region-map "a" #'align)
(define-key basis/region-map "b" #'basis/delete-empty-lines)
(define-key basis/region-map "c" #'basis/count-words)
(define-key basis/region-map "l" #'basis/count-sloc)
(define-key basis/region-map "s" #'sort-lines)
(define-key basis/region-map "n" #'basis/narrow-or-widen-dwim)

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; In Emacs 26, describe-\(function\|variable\) are incompatible with ido and
;; can load libraries as part of completion, so override them with simpler
;; definitions
(global-set-key [remap describe-function] #'basis/describe-function)
(global-set-key [remap describe-variable] #'basis/describe-variable)

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
(define-key basis/find-lisp-map "a" #'helm-apropos)
(define-key basis/find-lisp-map "s" #'basis/find-source-directory)

(use-package other-frame-window
  :ensure t
  :config (other-frame-window-mode))

(global-set-key (kbd "C-h C-m") #'basis/toggle-echo-area-messages)

;; Make it harder to accidentally `suspend-frame'
(basis/define-prefix-command 'basis/ctl-z-map (kbd "C-z"))
(define-key basis/ctl-z-map (kbd "C-z") #'suspend-frame)

(global-set-key (kbd "C-x C-z") #'repeat)

(defvar basis/tmux-key-translations
  (when (getenv "TMUX")
    (append
     '(("M-[ 1 ; 5 k" "C-=")
       ("M-[ 1 ; 5 l" "C-,")
       ("M-[ 1 ; 5 n" "C-.")
       ("M-[ 1 ; 6 k" "C-+")
       ("M-[ 1 ; 6 l" "C-<")
       ("M-[ 1 ; 6 n" "C->")
       ("M-[ 1 ; 6 y" "C-(")
       ("M-[ 1 ; 7 k" "C-M-=")
       ("M-[ 1 ; 7 n" "C-M-.")
       ("M-[ 1 ; 7 l" "C-M-,"))
     (seq-mapcat (pcase-lambda (`(,n ,k))
                   `((,(format "M-[ 1 ; %d A" n)  ,(format "%s<up>" k))
                     (,(format "M-[ 1 ; %d B" n)  ,(format "%s<down>" k))
                     (,(format "M-[ 1 ; %d C" n)  ,(format "%s<right>" k))
                     (,(format "M-[ 1 ; %d D" n)  ,(format "%s<left>" k))
                     (,(format "M-[ 1 ; %d H" n)  ,(format "%s<home>" k))
                     (,(format "M-[ 1 ; %d F" n)  ,(format "%s<end>" k))
                     (,(format "M-[ 5 ; %d ~" n)  ,(format "%s<prior>" k))
                     (,(format "M-[ 6 ; %d ~" n)  ,(format "%s<next>" k))
                     (,(format "M-[ 2 ; %d ~" n)  ,(format "%s<delete>" k))
                     (,(format "M-[ 3 ; %d ~" n)  ,(format "%s<delete>" k))
                     (,(format "M-[ 1 ; %d P" n)  ,(format "%s<f1>" k))
                     (,(format "M-[ 1 ; %d Q" n)  ,(format "%s<f2>" k))
                     (,(format "M-[ 1 ; %d R" n)  ,(format "%s<f3>" k))
                     (,(format "M-[ 1 ; %d S" n)  ,(format "%s<f4>" k))
                     (,(format "M-[ 15 ; %d ~" n) ,(format "%s<f5>" k))
                     (,(format "M-[ 17 ; %d ~" n) ,(format "%s<f6>" k))
                     (,(format "M-[ 18 ; %d ~" n) ,(format "%s<f7>" k))
                     (,(format "M-[ 19 ; %d ~" n) ,(format "%s<f8>" k))
                     (,(format "M-[ 20 ; %d ~" n) ,(format "%s<f9>" k))
                     (,(format "M-[ 21 ; %d ~" n) ,(format "%s<f10>" k))
                     (,(format "M-[ 23 ; %d ~" n) ,(format "%s<f11>" k))
                     (,(format "M-[ 24 ; %d ~" n) ,(format "%s<f12>" k))
                     (,(format "M-[ 25 ; %d ~" n) ,(format "%s<f13>" k))
                     (,(format "M-[ 26 ; %d ~" n) ,(format "%s<f14>" k))
                     (,(format "M-[ 28 ; %d ~" n) ,(format "%s<f15>" k))
                     (,(format "M-[ 29 ; %d ~" n) ,(format "%s<f16>" k))
                     (,(format "M-[ 31 ; %d ~" n) ,(format "%s<f17>" k))
                     (,(format "M-[ 32 ; %d ~" n) ,(format "%s<f18>" k))
                     (,(format "M-[ 33 ; %d ~" n) ,(format "%s<f19>" k))
                     (,(format "M-[ 34 ; %d ~" n) ,(format "%s<f20>" k))))
                 '((2 "S-")
                   (3 "M-")
                   (4 "M-S-")
                   (5 "C-")
                   (6 "C-S-")
                   (7 "C-M-")
                   (8 "C-M-S-")))))
  "Keys to add to `key-translation-map' when running in tmux.

A number of non-alphanumeric keys don't work by default when
Emacs is running in tmux. These keys are added to
`key-translation-map' in an attempt to fix that. The list is
based on code from ArchWiki's Emacs page.

`setw -g xterm-keys on` must be set in ~/.tmux.conf for this to
work.

TODO: <home> and <end> still don't work.")

(defun basis/init-for-tmux ()
  (pcase-dolist (`(,k1 ,k2) basis/tmux-key-translations)
    (define-key key-translation-map (kbd k1) (kbd k2))))

(when (getenv "TMUX")
  (basis/init-for-tmux))


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
  :init (global-set-key (kbd "M-=") #'er/expand-region))

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
  :defer t)

(use-package easy-kill
  :ensure t
  :defer t)

(use-package visual-regexp
  :ensure t
  :defer t
  :init (defalias 'vqr #'vr/query-replace))


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
      (define-key map (kbd "M-DEL") #'basis/paredit-kill-something))
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
    (define-key sp-keymap (kbd "M-k")           #'basis/sp-kill-sexp)
    (define-key sp-keymap (kbd "M-e")           #'sp-forward-sexp)
    (define-key sp-keymap (kbd "M-a")           #'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-u")         #'basis/sp-backward-up)
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

(use-package swiper-helm
  :ensure t
  :defer t)

(defun basis/init-occur-mode ()
  (setq basis/beginning-of-buffer-function #'basis/occur-beginning-of-buffer)
  (setq basis/end-of-buffer-function #'basis/occur-end-of-buffer))

(progn ;; replace.el
  (defalias 'qrr #'query-replace-regexp)
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
    (grep-apply-setting 'grep-command
                        (if (bound-and-true-p grep-use-null-filename-separator)
                            "grep --null --color=always -inHE -e "
                          "grep --color=always -inHE -e "))
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
    (define-key map (kbd "C-c x") #'basis/ido-open-file-externally)))

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
         (setq ido-enable-tramp-completion (not (eq system-type 'windows-nt)))
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

(use-package helm
  :ensure t
  :defer t
  :init (progn
          (basis/define-prefix-command 'basis/helm-map (kbd "C-c h"))
          (define-key basis/helm-map "a" #'helm-apropos)
          (define-key basis/helm-map "b" #'helm-mini)
          (define-key basis/helm-map "c" #'helm-colors)
          (define-key basis/helm-map "e" #'helm-register)
          (define-key basis/helm-map "f" #'helm-find-files)
          (define-key basis/helm-map "g" #'helm-do-grep)
          (define-key basis/helm-map "i" #'helm-info-at-point)
          (define-key basis/helm-map "k" #'helm-man-woman)
          (define-key basis/helm-map "l" #'helm-bookmarks)
          (define-key basis/helm-map "m" #'helm-all-mark-rings)
          (define-key basis/helm-map "o" #'helm-occur)
          (define-key basis/helm-map "p" #'helm-list-emacs-process)
          (define-key basis/helm-map "r" #'helm-regexp)
          (define-key basis/helm-map "R" #'helm-resume)
          (define-key basis/helm-map "s" #'helm-swoop)
          (define-key basis/helm-map "t" #'helm-top)
          (define-key basis/helm-map "x" #'helm-M-x)
          (define-key basis/helm-map "y" #'helm-show-kill-ring)
          (define-key basis/helm-map "/" #'helm-find)
          (define-key basis/helm-map ":" #'helm-eval-expression-with-eldoc))
  :config (progn
            (setq helm-split-window-default-side 'other)
            (setq helm-split-window-in-side-p t)
            (setq helm-quick-update t)
            (setq helm-truncate-lines t)
            (setq helm-display-header-line nil)
            (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
            (define-key helm-map (kbd "M-s") #'helm-select-action)
            (define-key helm-map (kbd "DEL") #'basis/helm-backspace)
            (define-key helm-map (kbd "M-y") #'helm-yank-text-at-point)
            (define-key helm-map (kbd "C-w") nil)
            (add-to-list 'display-buffer-alist
                         '("\\`\\*helm.*\\*\\'"
                           (display-buffer-in-side-window)
                           (inhibit-same-window . t)
                           (window-height . 0.4)))
            (set-face-attribute 'helm-source-header nil :height 1.0)))

(use-package helm-config
  :defer t
  :config (global-unset-key (kbd helm-command-prefix-key)))

(use-package helm-mode
  :defer t
  :config (progn
            (setq helm-mode-handle-completion-in-region nil)
            (add-to-list 'helm-completing-read-handlers-alist
                         '(multi-occur . ido-completing-read)))
  :diminish helm-mode)

(use-package helm-files
  :defer t
  :config
  (progn
    (require 'dired-x) ; For `dired-omit-extensions'
    (setq helm-ff-newfile-prompt-p nil)
    (setq helm-ff-file-name-history-use-recentf t)
    (setq helm-ff-search-library-in-sexp t)
    (setq helm-ff-skip-boring-files t)
    (setq helm-recentf-fuzzy-match t)
    (setq helm-boring-file-regexp-list
          (mapcar (lambda (e) (concat (regexp-quote e) "$"))
                  dired-omit-extensions))
    (let ((map helm-find-files-map))
      (define-key map (kbd "TAB")     #'helm-execute-persistent-action)
      (define-key map (kbd "M-s")     #'helm-select-action)
      (define-key map (kbd "DEL")     #'basis/helm-backspace)
      (define-key map (kbd "C-c C-b") #'basis/helm-run-bookmarks)
      (define-key map (kbd "C-x g")   #'basis/helm-ff-run-magit-status))
    ;; Disable `ffap' behavior
    (advice-add 'helm-find-files-input :override #'ignore)))

(use-package helm-flx
  :ensure t
  :after helm
  :config (progn (require 'helm)
                 (require 'helm-files)
                 (helm-flx-mode)))

(use-package helm-locate
  :defer t
  :config (setq helm-locate-fuzzy-match nil))

(use-package helm-buffers
  :defer t
  :config
  (progn
    (setq helm-buffers-fuzzy-matching t)
    (define-key helm-buffer-map (kbd "C-c C-b") #'basis/helm-run-bookmarks)))

(use-package helm-command
  :after helm
  :config (setq helm-M-x-fuzzy-match t))

(use-package helm-imenu
  :defer t
  :config (progn (setq helm-imenu-execute-action-at-once-if-one nil)
                 (setq helm-imenu-fuzzy-match t)))

(use-package helm-ring
  :defer t
  :init (progn (global-set-key (kbd "M-y") #'helm-show-kill-ring)
               (global-set-key (kbd "M-`") #'helm-mark-ring)
               (global-set-key (kbd "M-~") #'helm-all-mark-rings)))

(use-package helm-elisp
  :defer t
  :init (global-set-key (kbd "C-h SPC") #'helm-apropos)
  :config (progn (setq helm-apropos-fuzzy-match t)
                 (setq helm-lisp-fuzzy-completion t)))

(use-package helm-man
  :defer t
  :config (or (executable-find "man")
              (setq helm-man-or-woman-function #'woman)))

(use-package helm-descbinds
  :ensure t
  :defer t
  :init (global-set-key (kbd "C-h b") #'helm-descbinds))

(use-package helm-projectile
  :ensure t
  :defer t
  :config (setq helm-projectile-fuzzy-match t))

(use-package helm-ag
  :ensure t
  :defer t)

(use-package helm-swoop
  :ensure t
  :defer t
  :config (progn
            (setq helm-swoop-use-line-number-face t)
            (define-key helm-swoop-map (kbd "C-s") #'helm-next-line)
            (define-key helm-swoop-map (kbd "C-r") #'helm-previous-line)))

(use-package helm-external
  :defer t
  :config (when (eq system-type 'windows-nt)
            (advice-add 'helm-open-file-externally :override
                        #'basis/helm-open-file-w32)))

(use-package helm-grep
  :defer t)

(use-package helm-make
  :ensure t
  :defer t)

(use-package helm-flycheck
  :ensure t
  :defer t)

(use-package helm-open-github
  :ensure t
  :defer t)

(use-package helm-unicode
  :ensure t
  :defer t
  :init (define-key ctl-x-map (kbd "8 M-RET") #'helm-unicode))

(use-package helm-pages
  :ensure t
  :defer t
  :config (advice-add 'helm-pages-get-next-header :override
                      #'basis/helm-pages-get-next-header))

(use-package ivy
  :ensure t
  :defer t
  :config
  (progn (setq ivy-format-function #'ivy-format-function-arrow)
         (let ((map ivy-minibuffer-map))
           (define-key map (kbd "C-r")     #'ivy-previous-line-or-history)
           (define-key map (kbd "<up>")    #'ivy-previous-line-or-history)
           (define-key map (kbd "<left>")  #'ivy-previous-line-or-history)
           (define-key map (kbd "<down>")  #'ivy-next-line-or-history)
           (define-key map (kbd "<right>") #'ivy-next-line-or-history))))

(use-package counsel
  :ensure t
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
                #'basis/company-maybe-block-completion)
    (when (string= (system-name) "sierra")
      (advice-add 'company-update-candidates :filter-args
                  #'basis/company-no-srv-candidates))
    (unless (eq system-type 'windows-nt)
      (with-eval-after-load 'cc-mode
        (when-let* ((prog (basis/find-clang-program))
                    (args (basis/build-clang-args 'c)))
          (require 'company-clang)
          (setq company-clang-executable prog)
          (setq company-clang-arguments args)))))
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

(use-package yasnippet
  :ensure t
  :defer t
  :mode ("\\.yasnippet\\'" . snippet-mode)
  :config
  (progn
    ;; Steal C-t for expanding snippets. `transpose-chars' is still
    ;; available on M-t c
    (define-key yas-minor-mode-map (kbd "C-t") #'basis/yas-expand-or-insert)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map [(tab)] nil)
    (define-key yas-keymap (kbd "RET") #'yas-exit-all-snippets)
    (setq yas-snippet-dirs (list (basis/emacs-dir "snippets/")))
    (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
    (setq yas-wrap-around-region t)))


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
  (when (member (buffer-name) '("*scratch*" "*ielm*"))
    (setq lexical-binding t))
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

(defun basis/set-lein-command-for-mac ()
  (when-let* ((lein (executable-find "lein")))
    (setq cider-lein-command lein)))

(defun basis/set-lein-command-for-cygwin ()
  (let ((lein "~/bin/lein"))
    (when (file-exists-p lein)
      (setq cider-lein-command (expand-file-name lein)))))

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
            (pcase basis/system-type
              (`darwin
               (basis/set-lein-command-for-mac))
              (`windows+cygwin
               (basis/set-lein-command-for-cygwin)))
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
  :ensure t
  :after scheme
  :config (progn (setq quack-default-program
                       (if (eq system-type 'windows-nt)
                           "racket"
                         "guile"))
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
    (setq python-shell-unbuffered (not (eq system-type 'windows-nt)))
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
      (add-hook 'python-mode-hook #'jedi:setup))
    (when (eq system-type 'windows-nt)
      (when-let* ((python (cond ((executable-find "py")
                                 "py")
                                ((file-executable-p "c:/Python27/python.exe")
                                 "c:/Python27/python.exe"))))
        (setq python-shell-interpreter python))
      ;; Hopefully-temporary workaround
      (when (boundp 'python-shell-completion-native-enable)
        (setq python-shell-completion-native-enable nil)))))

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

(use-package js-comint
  :ensure t
  :defer t)

(use-package js2-refactor
  :ensure t
  :defer t
  :config (js2r-add-keybindings-with-prefix "C-c m"))

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

(use-package batch-mode
  :ensure t
  :defer t
  :mode ("\\.bat\\'" "\\.cmd\\'"))

(use-package ahk-mode
  :ensure t
  :defer t)


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
    (setq org-structure-template-alist
          (mapcar (pcase-lambda (`(,key ,val)) (list key (downcase val)))
                  org-structure-template-alist))
    (org-babel-do-load-languages
     'org-babel-load-languages
     (mapcar (lambda (sym) (cons sym t))
             (cons (if (locate-file "ob-shell" load-path '(".el" ".el.gz"))
                       'shell
                     'sh)
                   '(C clojure emacs-lisp haskell python scheme))))))

(use-package ob-clojure
  :defer t
  :config (setq org-babel-clojure-backend 'cider))

(defun basis/init-simplezen ()
  (setq-local yas-fallback-behavior
              '(apply simplezen-expand-or-indent-for-tab)))

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
    (add-hook 'sgml-mode-hook #'basis/init-simplezen)
    (add-hook 'html-mode-hook #'basis/init-html-mode)
    (advice-add 'sgml-delete-tag :after #'basis/sgml-delete-tag-reindent)))

(use-package tagedit
  :ensure t
  :commands (tagedit-mode)
  :config (progn (tagedit-add-paredit-like-keybindings)
                 (tagedit-add-experimental-features)
                 (advice-add 'tagedit-toggle-multiline-tag :around
                             #'basis/tagedit-toggle-multiline-maybe-forward)))

(use-package simplezen
  :ensure t
  :after sgml-mode)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error checking

(use-package ispell
  :defer t
  :config
  (progn (setq ispell-program-name (executable-find "aspell"))
         (setq ispell-personal-dictionary "~/.aspell.en.pws")
         (setq ispell-extra-args '("--sug-mode=ultra"))
         (let ((file "~/Dropbox/dict/words"))
           (when (and (not ispell-alternate-dictionary)
                      (file-readable-p file)
                      (not (ignore-errors (lookup-words "whatever"))))
             (setq ispell-alternate-dictionary file)))
         ;; Kludge to make the personal dictionary work on my Cygwin setup.
         (when (eq basis/system-type 'windows+cygwin)
           (advice-add 'ispell-init-process :around
                       #'basis/ispell-init-process))))

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
  :init (progn
          (basis/define-prefix-command 'basis/flycheck-map (kbd "<f8>"))
          (let ((map basis/flycheck-map))
            (define-key map (kbd "c")    #'flycheck-buffer)
            (define-key map (kbd "n")    #'flycheck-next-error)
            (define-key map (kbd "p")    #'flycheck-previous-error)
            (define-key map (kbd "l")    #'flycheck-list-errors)
            (define-key map (kbd "s")    #'flycheck-select-checker)
            (define-key map (kbd "C")    #'flycheck-clear)
            (define-key map (kbd "<f8>") #'basis/flycheck-check-and-list-errors)
            (define-key map (kbd "h")    #'helm-flycheck)))
  :config (progn
            (setq flycheck-check-syntax-automatically nil)
            (unless (basis/libxml-available-p)
              (setq flycheck-xml-parser #'flycheck-parse-xml-region))
            ;; Check buffers with errors more frequently than ones without
            (make-variable-buffer-local 'flycheck-idle-change-delay)
            (add-hook 'flycheck-after-syntax-check-hook
                      #'basis/adjust-flycheck-idle-change-delay)
            (let ((map flycheck-error-list-mode-map))
              (define-key map "n" #'flycheck-error-list-next-error)
              (define-key map "p" #'flycheck-error-list-previous-error))))


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
            (when (eq system-type 'windows-nt)
              (advice-add 'ediff-make-empty-tmp-file :filter-args
                          #'basis/ediff-expand-tmp-name))
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
          (global-set-key (kbd "C-x M-g") #'magit-dispatch-popup)
          (global-set-key (kbd "C-c M-g") #'magit-file-popup))
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
    (setq magit-repository-directories
          (thread-last projectile-known-projects
            (seq-filter (lambda (file)
                          (and (not (file-remote-p file))
                               (file-exists-p (expand-file-name ".git" file)))))
            (cons "~/code/")
            (mapcar #'directory-file-name)))
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
                            'replace)
    ;; Cygwin compatibility stuff
    (when (eq basis/system-type 'windows+cygwin)
      (setq magit-need-cygwin-noglob t)
      (when (file-executable-p "/bin/git.exe")
        (setq magit-git-executable "/bin/git.exe")
        (setq magit-git-environment nil))
      (advice-add 'magit-toplevel :filter-return
                  #'basis/magit-expand-toplevel)
      (advice-add 'magit-list-repos :filter-return
                  #'basis/magit-list-repos-uniquely)
      (advice-add 'magit-save-repository-buffers :override
                  #'basis/magit-cygwin-save-repository-buffers)
      ;; I haven't figured out yet why the Magit commands for saving and popping
      ;; stashes fail on my Cygwin setup at work, but this gives me quick access
      ;; to the simplest usage in the meantime.
      (pcase-dolist (`(,key ,cmd ,before) '((?z save ?Z)
                                            (?Z snapshot ?p)
                                            (?p pop ?i)))
        (magit-define-popup-action 'magit-stash-popup
          key
          (capitalize (symbol-name cmd))
          (intern (format "basis/magit-stash-%s" cmd))
          before
          'prepend)))))

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
  (define-key map (kbd "b")   #'projectile-switch-to-buffer)
  (define-key map (kbd "d")   #'projectile-find-dir)
  (define-key map (kbd "C-f") #'projectile-find-file)
  (define-key map (kbd "ff")  #'projectile-find-file-dwim)
  (define-key map (kbd "fd")  #'projectile-find-file-in-directory)
  (define-key map (kbd "g")   #'projectile-grep)
  (define-key map (kbd "i")   #'projectile-ibuffer)
  (define-key map (kbd "K")   #'projectile-kill-buffers)
  (define-key map (kbd "o")   #'projectile-multi-occur)
  (define-key map (kbd "p")   #'projectile-switch-project)
  (define-key map (kbd "r")   #'projectile-recentf)
  (define-key map (kbd "x")   #'projectile-remove-known-project)
  (define-key map (kbd "X")   #'projectile-cleanup-known-projects)
  (define-key map (kbd "z")   #'projectile-cache-current-file))

(use-package projectile
  :ensure t
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'helm)
    (setq projectile-known-projects-file
          (basis/emacs-file "var/projectile-bookmarks.eld"))
    (setq projectile-cache-file (basis/emacs-file "var/projectile.cache"))
    (setq projectile-use-git-grep t)
    (unless (eq basis/system-type 'windows-nt)
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching nil))
    (projectile-global-mode)
    (global-set-key projectile-keymap-prefix
                    'basis/projectile-map)
    (advice-add 'projectile-regenerate-tags :around
                #'basis/projectile-regenerate-tags))
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

(use-package ls-lisp
  :defer t
  :config (when (eq basis/system-type 'windows+cygwin)
            (setq ls-lisp-use-insert-directory-program t)))

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
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                     "-alhGt"
                                   "-alht"))
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
  :config (setq find-ls-option (if (eq system-type 'windows-nt)
                                   '("-exec ls -ldhG {} +" . "-ldhG")
                                 '("-exec ls -ldh {} +" . "-ldh"))))

(use-package image-dired
  :defer t
  ;; Set `image-dired-dir' before loading the package because several other
  ;; options' default values are defined relative to it.
  :init (setq image-dired-dir (basis/emacs-dir "var/image-dired/")))

(use-package dired+
  :ensure t
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
      (define-key map (kbd "C-c C-l") #'helm-comint-input-ring)
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
  (dirtrack-mode))

(use-package shell
  :defer t
  :config (add-hook 'shell-mode-hook #'basis/init-shell-mode))

(use-package dirtrack
  :defer t
  ;; Not sure why comint/dirtrack see junk in front of my prompt with Cygwin's
  ;; zsh, so just work around it
  :config (setq-default dirtrack-list
                        (if (eq basis/system-type 'windows+cygwin)
                            '("^%[ \r]*\\(.+\\)>" 1)
                          '("^[^:\n]+@[^:\n]+:\\(.+\\)>" 1))))

(use-package bash-completion
  :ensure t
  :defer t
  ;; This doesn't work with Cygwin's bash, because bash detects that it's not
  ;; connected to a tty and behaves differently (e.g. doesn't echo its prompt)
  :unless (eq system-type 'windows-nt)
  :config (bash-completion-setup))

(use-package esh-mode
  :defer t
  :config
  (progn (setq eshell-directory-name (basis/emacs-dir "var/eshell/"))
         (when (eq basis/system-type 'windows+cygwin)
           (add-hook 'eshell-mode-hook #'basis/eshell-cygwin-path-env))))

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
                                    (append '("~/downloads/" "~/Downloads/")
                                            (and (eq system-type 'windows-nt)
                                                 '("e:/Downloads")))))))
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
