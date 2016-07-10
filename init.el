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

(defun basis/emacs-dir (name &optional create)
  "Return directory NAME expanded in `basis/emacs-dir'.
Create the directory if it does not exist and CREATE is non-nil."
  (if (string-suffix-p "/" name)
      (let ((dir (expand-file-name name basis/emacs-dir)))
        (when (and create (not (file-directory-p dir)))
          (make-directory dir t))
        dir)
    ;; This isn't actually necessary but will catch places I meant to use
    ;; `basis/emacs-file' instead
    (error "Directory name should end with a slash")))

(defun basis/emacs-file (name)
  "Return file NAME expanded in `basis/emacs-dir'."
  (if (not (string-suffix-p "/" name))
      (expand-file-name name basis/emacs-dir)
    (error "File name should not end with a slash")))

;; Make sure some directories exist
(dolist (dir '("var/" "var/autosaves/" "tmp/"))
  (basis/emacs-dir dir 'create))

;; Set up the load path
(add-to-list 'load-path (basis/emacs-dir "site-lisp/" 'create))

;; So e.g. `find-function' works on C functions in Emacsen I didn't build myself
(unless (file-directory-p source-directory)
  (let ((dir (format "~/src/emacs/emacs-%s/" emacs-version)))
    (when (file-directory-p dir)
      (setq source-directory dir))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set up package.el

(setq load-prefer-newer t)

(setq package-user-dir (basis/emacs-dir "elpa/"))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Opt out of automatically saving a list of installed packages
(when (fboundp 'package--save-selected-packages)
  ;; TODO: Can I hook into `use-package' to build `package-selected-packages'?
  (advice-add 'package--save-selected-packages :override #'ignore))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load some code

(require 'use-package)

(use-package auto-compile
  :ensure t
  :config (progn (auto-compile-on-load-mode)
                 (auto-compile-on-save-mode)))

(use-package bind-key
  :ensure t)

(use-package diminish
  :ensure t)

(use-package pcase)

(use-package subr-x)

(use-package seq)

(use-package stream
  :ensure t
  :defer t)

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

(autoload 'tramp-tramp-file-p "tramp"
  "Return t if NAME is a string with Tramp file name syntax.")

(load (basis/emacs-file "defuns") nil nil nil t)

(defun basis/maybe-load-local-init ()
  "Load \"local.el\" if it exists."
  (let ((local (basis/emacs-file "local.el")))
    (when (file-exists-p local)
      (load (string-remove-suffix ".el" local) nil nil nil t))))

;; Do this after init so that it can override anything set here
(add-hook 'after-init-hook #'basis/maybe-load-local-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operating system-specific configuration

;; A graphical Emacs on OS X doesn't automatically inherit $PATH
(use-package exec-path-from-shell
  :ensure t
  :if (eq window-system 'ns)
  :config (exec-path-from-shell-initialize))

(defvar basis/system-type
  (if (and (eq system-type 'windows-nt)
           (file-executable-p "c:/bin/bash.exe"))
      'windows+cygwin
    system-type)
  "Like `system-type' but with the additional option `windows+cygwin'.")

(defvar basis/cygwin-path-directories
  '("/bin" "/usr/bin" "/usr/local/bin"
    "/Windows" "/ProgramData/Oracle/Java/javapath")
  "Directories to add to PATH on Cygwin.")

(defun basis/init-for-cygwin ()
  (let* ((dirs (seq-filter #'file-directory-p basis/cygwin-path-directories))
         (home (getenv "HOME"))
         (home/bin (and home (concat (basis/windows->unix home)
                                     "/bin"))))
    (when (and home (file-directory-p home) (not after-init-time))
      (cd home))
    (when (and home/bin (file-directory-p home/bin))
      (push home/bin dirs))
    ;; Set paths
    (setenv "PATH" (mapconcat #'identity dirs ":"))
    (setq exec-path (mapcar (lambda (dir) (concat "c:" dir)) dirs))
    ;; Use zsh or bash as shell
    (let ((shell (or (executable-find "zsh")
                     (executable-find "bash"))))
      (setq shell-file-name shell)
      (setq explicit-shell-file-name shell)
      (setq ediff-shell shell)
      (setq null-device "/dev/null")
      (setenv "SHELL" shell))
    (setq locale-coding-system 'utf-8)
    ;; Since Emacs wasn't launched from a Cygwin shell, $LANG will be wonky
    ;; unless we fix it.
    (setenv "LANG" "en_US.UTF-8")
    (advice-add 'shell-quote-argument :filter-args
                #'basis/cygwin-shell-quote-argument)))

(when (eq basis/system-type 'windows+cygwin)
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
(setq indicate-empty-lines t)
(setq recenter-positions '(top middle bottom))
(setq scroll-preserve-screen-position t)
(setq delete-by-moving-to-trash t)
(setq gc-cons-threshold (* 20 1024 1024))
(setq temporary-file-directory (basis/emacs-dir "tmp/"))
(setq switch-to-buffer-preserve-window-point t)
(setq enable-recursive-minibuffers t)
(setq user-mail-address "jbm@jbm.io")
(setq user-full-name "John Mastro")
(setq mail-host-address "jbm.io")

(setq-default major-mode 'text-mode)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq-default truncate-lines t)

;; Don't prompt when killing buffers with live processes attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

(fset 'display-startup-echo-area-message #'ignore)

(fset 'yes-or-no-p #'y-or-n-p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic packages

(use-package custom
  :config
  (progn (setq custom-file (basis/emacs-file "custom.el"))
         (when (file-exists-p custom-file)
           (load custom-file))))

(defun basis/init-eval-expression-minibuffer ()
  (setq-local indent-line-function #'lisp-indent-line))

(use-package simple
  :config
  (progn
    (setq shift-select-mode nil)
    (setq line-number-mode t)
    (setq column-number-mode t)
    (setq next-line-add-newlines t)
    (size-indication-mode)
    (advice-add 'next-line :around #'basis/next-line-no-deactivate-mark)
    ;; Keep popping the mark until point actually moves
    (advice-add 'pop-to-mark-command
                :around
                #'basis/pop-to-mark-ensure-new-pos)
    (add-hook 'eval-expression-minibuffer-setup-hook
              #'basis/init-eval-expression-minibuffer)))

(use-package mule
  :config (prefer-coding-system 'utf-8))

(defun basis/maybe-set-coding ()
  (when (and (eq system-type 'windows-nt)
             (tramp-tramp-file-p buffer-file-name))
    (set-buffer-file-coding-system 'utf-8-unix)))

(use-package files
  :config
  (progn
    (setq-default require-final-newline t)
    (setq confirm-nonexistent-file-or-buffer nil)
    (setq backup-by-copying t)
    (setq backup-directory-alist
          `((".*" . ,(basis/emacs-dir "var/backups/"))))
    (setq auto-save-file-name-transforms
          `((".*" ,(basis/emacs-dir "var/autosaves/") t)))
    (setq auto-save-list-file-prefix
          (concat (basis/emacs-dir "var/auto-save-list/") ".saves-"))
    (pcase system-type
      (`darwin
       (when-let ((gls (executable-find "gls")))
         (setq insert-directory-program gls)))
      (`windows-nt
       (add-hook 'before-save-hook #'basis/maybe-set-coding)))))

(use-package windmove
  :defer t
  :config (windmove-default-keybindings))

(use-package cursor-sensor
  :config
  (let ((mpps minibuffer-prompt-properties)
        (prop '(cursor-intangible t)))
    (unless (memq 'cursor-intangible mpps)
      (setq minibuffer-prompt-properties (append mpps prop)))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

(use-package mouse
  :config (setq mouse-yank-at-point t))

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
  :config (setq ffap-machine-p-known 'reject))

(use-package advice
  :defer t
  :config (setq ad-redefinition-action 'accept))

(defun basis/set-default-input-method (&optional method)
  (setq default-input-method (or method "TeX")))

;; Haven't yet bothered looking into why this needs to be done after init
(add-hook 'after-init-hook #'basis/set-default-input-method)

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
  :config (progn (global-auto-revert-mode)
                 (setq global-auto-revert-non-file-buffers t)
                 (setq auto-revert-verbose nil)))

(use-package frame
  :config (blink-cursor-mode -1))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (basis/emacs-file "var/places")))

(use-package savehist
  :config
  (progn (setq savehist-additional-variables '(search-ring regexp-search-ring))
         (setq savehist-file (basis/emacs-file "var/history"))
         (savehist-mode)))

(use-package uniquify
  :config (progn (setq uniquify-buffer-name-style 'forward)
                 (setq uniquify-ignore-buffers-re "^\\*")))

(use-package whitespace
  :config (progn (setq whitespace-style '(face trailing lines-tail tabs))
                 (setq whitespace-line-column 80)
                 (put 'whitespace-line-column 'safe-local-variable #'integerp))
  :diminish whitespace-mode)

(use-package recentf
  :config
  (progn (setq recentf-max-saved-items 50)
         (setq recentf-save-file (basis/emacs-file "var/recentf"))
         (setq recentf-exclude (list #'tramp-tramp-file-p #'file-remote-p))
         (recentf-mode)))

(use-package tramp
  :defer t
  :config
  (progn
    (setq tramp-default-method
          (if (eq basis/system-type 'windows-nt)
              "plinkx"
            "sshx"))
    (setq tramp-persistency-file-name (basis/emacs-file "var/tramp"))
    ;; Have TRAMP use Cygwin's sh rather than Windows's cmd.exe
    (when (eq basis/system-type 'windows+cygwin)
      (setq tramp-encoding-shell (executable-find "sh"))
      (setq tramp-encoding-command-switch "-c")
      (setq tramp-encoding-command-interactive "-i"))))

(use-package time
  :defer t
  ;; Show all continental US time zones
  :config (setq display-time-world-list
                '(("PST8PDT"  "Los Angeles")
                  ("MST7MDT"  "Denver")
                  ("CST6CDT"  "Chicago")
                  ("EST5EDT"  "New York")
                  ("GMT0BST"  "London")
                  ("CET-1CDT" "Paris")
                  ("IST-5:30" "Bangalore")
                  ("JST-9"    "Tokyo"))))

(use-package minibuffer
  :config
  (let ((map minibuffer-inactive-mode-map))
    (define-key map [mouse-1] #'basis/toggle-echo-area-messages)))

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface

;; See the commentary in `unicode-fonts' for the "minimum useful fonts" to
;; install
(use-package unicode-fonts
  :ensure t
  :config (unicode-fonts-setup))

(use-package faces
  :config
  (when-let ((font (seq-find
                    (lambda (name) (find-font (font-spec :name name)))
                    (pcase system-type
                      (`darwin     '("Source Code Pro-11" "Andale Mono-12"))
                      (`windows-nt '("Consolas-10"))
                      (_           '("Inconsolata-11"))))))
    (set-face-attribute 'default nil :font font)))

(use-package solarized-theme
  :ensure color-theme-solarized
  :init (progn
          (set-frame-parameter nil 'background-mode 'dark)
          (set-terminal-parameter nil 'background-mode 'dark)
          (setq solarized-termcolors 256)
          (setq solarized-italic nil)
          ;; Some additional faces I've assembled
          (add-to-list 'custom-theme-load-path
                       (basis/emacs-dir "themes/solarized-moar/")))
  :config (progn (load-theme 'solarized t)
                 (load-theme 'solarized-moar t)))

(defun basis/get-frame-title ()
  "Return a frame title including the current project directory."
  (if-let ((file buffer-file-name))
      (concat (abbreviate-file-name file)
              (when (and (bound-and-true-p projectile-mode)
                         (projectile-project-p))
                (format " [%s]" (projectile-project-name))))
    "%b"))

(when (display-graphic-p)
  (setq frame-title-format '((:eval (basis/get-frame-title)))))

(use-package leuven-theme
  :ensure t
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation modes

(use-package help-mode
  :defer t
  :config (basis/define-keys help-mode-map
            ("n" #'next-line)
            ("p" #'previous-line)))

(use-package info
  :defer t
  :config
  (let ((info-path (basis/emacs-dir "doc/info/")))
    (when (file-directory-p info-path)
      (add-to-list 'Info-additional-directory-list info-path))))

(use-package apropos
  :defer t
  :config (setq apropos-do-all t))

(use-package man
  :defer t
  :config (setq Man-notify-method 'aggressive))

(defvar basis/system-man-p (executable-find "man")
  "Non-nil if a \"man\" executable is available on this system.")

(unless basis/system-man-p
  (require 'man)
  (fset 'original-man #'man)
  (fset 'man #'woman))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings

(pcase system-type
  (`darwin
   (setq mac-command-modifier 'meta)
   (setq mac-option-modifier 'super))
  (`windows-nt
   (setq w32-pass-apps-to-system nil)
   (setq w32-pass-lwindow-to-system nil)
   (setq w32-pass-rwindow-to-system nil)
   (setq w32-lwindow-modifier 'super)
   (setq w32-rwindow-modifier 'super)))

(use-package which-key
  :ensure t
  :config (progn
            (setq which-key-idle-delay 0.5)
            (which-key-mode))
  :diminish which-key-mode)

;; Don't prompt when using `C-x k'
(global-set-key (kbd "C-x k") #'basis/kill-buffer)

;; C-c <left>/<right> to move backward/forward through window configurations
(use-package winner
  :config (winner-mode))

;; Make `next-line-add-newlines' safe for keyboard macros
(global-set-key (kbd "C-n") #'basis/next-line)
(global-set-key (kbd "<down>") #'basis/next-line)

;; Newlines
(basis/define-keys global-map
  ("S-RET"        #'basis/open-line-below)
  ("<S-return>"   #'basis/open-line-below)
  ("C-S-RET"      #'basis/open-line-above)
  ("<C-S-return>" #'basis/open-line-above))

;; Whitespace
(global-set-key (kbd "M-\\") #'basis/cycle-spacing-fast)

;; Clever C-a
(global-set-key (kbd "C-a") #'basis/beginning-of-line)

;; Kill stuff
(basis/define-keys global-map
  ("M-k"                   #'kill-sexp)
  ("C-w"                   #'basis/kill-something)
  ("M-DEL"                 #'basis/kill-something)
  ([remap kill-whole-line] #'basis/kill-line-backward)
  ("<C-delete>"            #'basis/smart-kill-whole-line)
  ("<M-delete>"            #'basis/smart-kill-almost-whole-line)
  ("ESC <deletechar>"      #'basis/smart-kill-almost-whole-line))

;; Copy stuff
(basis/define-keys global-map
  ("M-w"    #'basis/kill-ring-save-something)
  ("<f2>"   #'basis/clipboard-save-something)
  ("s-w"    #'basis/kill-ring-save-indented))

;; Join lines
(basis/define-keys global-map
  ("M-^" #'basis/delete-indentation)
  ("M-q" #'basis/fill-or-unfill-paragraph))

;; Transpose stuff with M-t
(basis/define-map basis/transposition-map ("M-t")
  ("l"   #'transpose-lines)
  ("w"   #'transpose-words)
  ("s"   #'transpose-sexps)
  ("c"   #'transpose-chars)
  ("M-w" #'basis/transpose-windows)
  ("M-s" #'basis/toggle-window-split))

;; More comfortable {next,previous}-error
(global-set-key (kbd "M-n") #'next-error)
(global-set-key (kbd "M-p") #'previous-error)

;; Comment/uncomment stuff
(global-set-key (kbd "C-c ;") #'basis/comment-or-uncomment)
(global-set-key (kbd "C-x ;") #'basis/comment-region-lines)
;; Unfortunately, this one doesn't work at the terminal
(global-set-key (kbd "C-M-;") #'basis/comment-or-uncomment-sexp)

;; Eval and replace
(global-set-key (kbd "C-c M-e") #'basis/eval-and-replace)

;; I use M-SPC for `avy-goto-word-1'
(global-set-key (kbd "C-c SPC") #'just-one-space)

;; goto-line, with line numbers
(global-set-key (kbd "M-g M-g") #'basis/goto-line-with-numbers)

;; Movement by sexp
(global-set-key (kbd "M-e") #'forward-sexp)
(global-set-key (kbd "M-a") #'backward-sexp)

;; Treat `C-u' specially on `M-('
(global-set-key (kbd "M-(") #'basis/insert-parentheses)

;; M-x shell
(global-set-key (kbd "C-c <C-return>") #'shell)
(global-set-key (kbd "C-c C-^") #'shell)

;; Change word case
(basis/define-keys global-map
  ("M-u" #'basis/upcase-something)
  ("M-l" #'basis/downcase-something)
  ("M-c" #'basis/capitalize-something))

;; DWIM C-x C-c
(global-set-key (kbd "C-x C-c") #'basis/kill-frame-or-terminal)
;; Kill Emacs with M-x sayonara
(defalias 'sayonara #'save-buffers-kill-terminal)

;; Google stuff
(global-set-key (kbd "<f9>") #'basis/google)

;; Re-open recent files
(global-set-key (kbd "C-x C-r") #'basis/find-file-recentf)

;; Previous/next buffer
(global-set-key (kbd "<C-prior>") #'previous-buffer)
(global-set-key (kbd "<C-next>") #'next-buffer)

(basis/define-map basis/file-map ("C-c f")
  ("c" #'helm-locate)
  ("d" #'basis/diff-buffer-with-file)
  ("r" #'basis/rename-current-buffer-file)
  ("D" #'basis/delete-current-buffer-file)
  ("f" #'find-name-dired)
  ("F" #'find-dired)
  ("j" #'dired-jump)
  ("J" #'dired-jump-other-window)
  ("m" #'make-directory)
  ("n" #'basis/kill-ring-save-buffer-file-name)
  ("v" #'revert-buffer))

;; Open one or more files externally, using the `helm-external' machinery
(global-set-key (kbd "C-c C-x") #'basis/open-file-externally)

;; Emacs Lisp-style quotes
(global-set-key (kbd "C-c q") #'basis/quote-thing)

;; Random operations on regions
(basis/define-map basis/region-map ("C-c r")
  ("a" #'align)
  ("c" #'basis/count-words)
  ("l" #'basis/count-sloc-region)
  ("s" #'sort-lines))

;; Narrowing can be quite handy
(put 'narrow-to-region 'disabled nil)

(basis/define-map basis/find-lisp-map ("<f1> e")
  ("c" #'finder-commentary)
  ("e" #'view-echo-area-messages)
  ("f" #'find-function)
  ("F" #'find-face-definition)
  ("i" #'info-apropos)
  ("k" #'find-function-on-key)
  ("l" #'find-library)
  ("m" #'info-display-manual)
  ("v" #'find-variable)
  ("V" #'apropos-value)
  ("a" #'helm-apropos))

;; Make it harder to accidentally `suspend-frame'
(basis/define-map basis/ctl-z-map ("C-z")
  ("C-z" #'suspend-frame))

(global-set-key (kbd "C-x C-z") #'repeat)

(basis/define-map basis/ctl-h-map ("C-h")
  ("C-h" #'mark-paragraph))

(defvar basis/eval-keys
  '((last-sexp  . "C-x C-e")
    (definition . "C-M-x")
    (region     . "C-c C-r")
    (buffer     . "C-c C-b")
    (something  . "C-c C-c")
    (file       . "C-c C-f")
    (expand     . "C-c C-e"))
  "Key bindings used to evaluate various units of code.
See `basis/define-eval-keys'.")

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
  :init (progn (basis/define-keys global-map
                 ("M-]" #'mc/mark-next-like-this)
                 ("C->" #'mc/mark-next-like-this)
                 ("C-<" #'mc/mark-previous-like-this))
               (basis/define-map basis/mc-map ("C-c m")
                 ("e"   #'mc/edit-lines)
                 ("C-a" #'mc/edit-beginnings-of-lines)
                 ("C-e" #'mc/edit-ends-of-lines)
                 ("d"   #'mc/mark-all-like-this-dwim)
                 ("D"   #'mc/mark-all-dwim)
                 ("m"   #'mc/mark-more-like-this-extended)
                 ("s"   #'mc/mark-all-symbols-like-this-in-defun)
                 ("w"   #'mc/mark-all-words-like-this-in-defun)
                 ("n"   #'mc/insert-numbers)
                 ("l"   #'mc/insert-letters))))

(use-package multiple-cursors-core
  :defer t
  :config
  (progn (setq mc/list-file (basis/emacs-file "var/mc-lists.el"))
         ;; Make RET exit multiple-cursors-mode in the terminal too
         (define-key mc/keymap (kbd "RET") #'multiple-cursors-mode)))

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
  :init (defalias 'vqr #'vr/query-replace)
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement

(use-package imenu
  :defer t
  :config (setq imenu-auto-rescan t))

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

(defun basis/ace-window-kludge (original arg)
  "Advice for `ace-window'.
Ensure it always works with two windows, even when one (or both)
is read-only and empty."
  (if (and (eq aw-scope 'frame)
           (= (length (window-list)) 2))
      (pcase arg
        (4  (basis/transpose-windows 1))
        (16 (delete-other-windows))
        (_  (other-window 1)))
    (funcall original arg)))

(use-package ace-window
  :ensure t
  :defer t
  :init (global-set-key (kbd "M-o") #'ace-window)
  :config (progn (setq aw-keys '(?h ?j ?k ?l ?n ?m))
                 (setq aw-scope 'frame)
                 (advice-add 'ace-window :around #'basis/ace-window-kludge)))

(use-package jump-char
  :ensure t
  :defer t
  :init (progn (global-set-key (kbd "M-m") #'jump-char-forward)
               (global-set-key (kbd "M-M") #'jump-char-backward)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search

(setq search-default-regexp-mode #'character-fold-to-regexp)
(setq isearch-regexp-lax-whitespace t)
(setq isearch-allow-scroll t)

(basis/define-keys global-map
  ("C-s"     #'isearch-forward-regexp)
  ("C-r"     #'isearch-backward-regexp)
  ("C-M-s"   #'isearch-forward)
  ("C-M-r"   #'isearch-backward)
  ("ESC M-s" search-map))

(basis/define-keys isearch-mode-map
  ("DEL"         #'basis/isearch-backspace)
  ("<backspace>" #'basis/isearch-backspace)
  ("C-t"         #'basis/isearch-yank-something)
  ("C-g"         #'basis/isearch-cancel)
  ("<up>"        #'isearch-ring-retreat)
  ("<down>"      #'isearch-ring-advance)
  ("<left>"      #'isearch-repeat-backward)
  ("<right>"     #'isearch-repeat-forward))

(use-package swiper
  :ensure t
  :defer t
  :init (define-key isearch-mode-map (kbd "M-s") #'swiper-from-isearch)
  :config (progn (setq swiper-min-highlight 1)
                 (basis/define-keys swiper-map
                   ("M-%"   #'swiper-query-replace)
                   ("M-SPC" #'swiper-avy)
                   ("C-t"   #'basis/swiper-maybe-yank-something))))

(use-package swiper-helm
  :ensure t
  :defer t)

;; replace.el doesn't (provide 'replace)
(defalias 'qrr #'query-replace-regexp)
(global-set-key (kbd "ESC M-%") #'query-replace-regexp)
(define-key occur-mode-map (kbd "n") #'occur-next)
(define-key occur-mode-map (kbd "p") #'occur-prev)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External search

(basis/define-map basis/grep-map ("C-c g")
  ("a"  #'ag-regexp)
  ("g"  #'grep)
  ("s"  #'lgrep)
  ("r"  #'rgrep)
  ("z"  #'zrgrep)
  ("v"  #'projectile-grep)
  ("V"  #'vc-git-grep)
  ("f"  #'find-grep)
  ("d"  #'find-grep-dired)
  ("o"  #'basis/occur-dwim)
  ("mo" #'multi-occur)
  ("mm" #'multi-occur-in-matching-buffers))

(use-package grep
  :defer t
  :config
  (progn
    ;; Work around bug #23590
    (when (string-match-p "zsh" shell-file-name)
      (setq grep-files-aliases
            (cons '("all" . "* .[^.]* ..?*")
                  (seq-remove (lambda (elt) (equal (car elt) "all"))
                              grep-files-aliases))))
    ;; Add some more file aliases
    (pcase-dolist (`(,alias . ,files)
                   '(("clj" . "*.clj *.cljs *.cljc")
                     ("cl"  . "*.lisp *.cl")
                     ("txt" . "*.txt *.org *.rst *.md *.mkd *.markdown")))
      (unless (assoc alias grep-files-aliases)
        (add-to-list 'grep-files-aliases (cons alias files))))))

(use-package ag
  :ensure t
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion

(defun basis/init-ido-keys ()
  (basis/define-keys ido-file-completion-map
    ("M-w"     #'ido-copy-current-file-name)
    ("C-x g"   #'ido-enter-magit-status)
    ("C-c C-x" #'basis/ido-open-file-externally)))

(use-package ido
  :defer t
  :config
  (progn (setq ido-enable-prefix nil)
         (setq ido-enable-flex-matching t)
         (setq ido-auto-merge-work-directories-length -1)
         (setq ido-create-new-buffer 'always)
         (setq ido-use-filename-at-point nil)
         (setq ido-use-virtual-buffers t)
         (setq ido-use-faces nil)
         (setq ido-max-prospects 10)
         (setq ido-ignore-extensions t)
         (setq ido-save-directory-list-file (basis/emacs-file "var/ido.last"))
         (add-hook 'ido-setup-hook #'basis/init-ido-keys)))

(use-package ido-ubiquitous
  :ensure t
  :defer t
  :after ido
  :config (when (bound-and-true-p ido-mode)
            (ido-ubiquitous-mode)))

(use-package flx-ido
  :ensure t
  :defer t
  :after ido
  :config (progn (setq flx-ido-threshhold 10000)
                 (flx-ido-mode)))

(use-package ido-vertical-mode
  :ensure t
  :defer t
  :after ido
  :config (ido-vertical-mode))

(use-package idomenu
  :ensure t
  :defer t)

(use-package smex
  :ensure t
  :defer t
  :init (global-set-key (kbd "M-X") #'smex-major-mode-commands)
  :config (setq smex-save-file (basis/emacs-file "var/smex-items")))

(use-package helm-flx
  :ensure t
  :config (helm-flx-mode))

(use-package helm-config
  :config (global-unset-key (kbd helm-command-prefix-key)))

(use-package helm
  :ensure t
  :init (basis/define-map basis/helm-map ("C-c h")
          ("a" #'helm-apropos)
          ("b" #'helm-buffers-list)
          ("c" #'helm-colors)
          ("e" #'helm-register)
          ("f" #'helm-find-files)
          ("g" #'helm-do-grep)
          ("i" #'helm-info-at-point)
          ("k" #'helm-man-woman)
          ("l" #'helm-bookmarks)
          ("m" #'helm-all-mark-rings)
          ("o" #'helm-occur)
          ("O" #'helm-multi-occur)
          ("p" #'helm-list-emacs-process)
          ("r" #'helm-regexp)
          ("R" #'helm-resume)
          ("s" #'helm-swoop)
          ("t" #'helm-top)
          ("x" #'helm-M-x)
          ("y" #'helm-show-kill-ring)
          ("/" #'helm-find)
          (":" #'helm-eval-expression-with-eldoc))
  :config (progn
            (setq helm-split-window-default-side 'other)
            (setq helm-split-window-in-side-p t)
            (setq helm-quick-update t)
            (setq helm-truncate-lines t)
            (setq helm-display-header-line nil)
            (basis/define-keys helm-map
              ("TAB" #'helm-execute-persistent-action)
              ("M-s" #'helm-select-action)
              ("DEL" #'basis/helm-backspace)
              ("M-y" #'helm-yank-text-at-point)
              ("C-w" nil))
            (add-to-list 'display-buffer-alist
                         '("\\`\\*helm.*\\*\\'"
                           (display-buffer-in-side-window)
                           (inhibit-same-window . t)
                           (window-height . 0.4)))
            (set-face-attribute 'helm-source-header nil :height 1.0)))

(use-package helm-adaptive
  :config
  (progn (setq helm-adaptive-history-file
               (basis/emacs-file "var/helm-adaptive-history"))
         (helm-adaptive-mode)))

(use-package helm-mode
  :diminish helm-mode
  :config (progn
            (setq helm-mode-handle-completion-in-region nil)
            (helm-mode)
            (dolist (cons '((multi-occur . ido-completing-read)
                            (Info-goto-node)))
              (add-to-list 'helm-completing-read-handlers-alist cons))))

(use-package helm-files
  :defer t
  :init (global-set-key (kbd "C-x C-f") #'basis/helm-find-files)
  :config
  (progn
    (setq helm-ff-newfile-prompt-p nil)
    (setq helm-ff-file-name-history-use-recentf t)
    (setq helm-ff-search-library-in-sexp t)
    (setq helm-ff-skip-boring-files t)
    (setq helm-recentf-fuzzy-match t)
    (setq helm-boring-file-regexp-list '("\\.o$" "\\.elc$" "\\.pyc$" "\\.pyo$"))
    (basis/define-keys helm-find-files-map
      ("TAB"     #'helm-execute-persistent-action)
      ("M-s"     #'helm-select-action)
      ("DEL"     #'basis/helm-backspace)
      ("C-c C-b" #'basis/helm-run-bookmarks)
      ("C-x g"   #'basis/helm-ff-run-magit-status))
    ;; Disable `ffap' behavior
    (advice-add 'helm-find-files-input :override #'ignore)))

(use-package helm-locate
  :config (setq helm-locate-fuzzy-match nil))

(use-package helm-buffers
  :defer t
  :init (global-set-key (kbd "C-x b") #'helm-mini)
  :config
  (progn
    (setq helm-buffers-fuzzy-matching t)
    (define-key helm-buffer-map (kbd "C-c C-b") #'basis/helm-run-bookmarks)))

(use-package helm-command
  :defer t
  :init (global-set-key (kbd "M-x") #'helm-M-x)
  :config (setq helm-M-x-fuzzy-match t))

(use-package helm-imenu
  :defer t
  :init (global-set-key (kbd "M-i") #'helm-imenu)
  :config (progn (setq helm-imenu-execute-action-at-once-if-one nil)
                 (setq helm-imenu-fuzzy-match t)))

(use-package helm-ring
  :defer t
  :init (progn (global-set-key (kbd "M-y") #'helm-show-kill-ring)
               (global-set-key (kbd "M-`") #'helm-all-mark-rings)))

(use-package helm-elisp
  :defer t
  :init (global-set-key (kbd "<f1> SPC") #'helm-apropos)
  :config (progn (setq helm-apropos-fuzzy-match t)
                 (setq helm-lisp-fuzzy-completion t)))

(use-package helm-man
  :defer t
  :config  (unless basis/system-man-p
             (setq helm-man-or-woman-function #'woman)))

(use-package helm-descbinds
  :ensure t
  :defer t
  :init (global-set-key (kbd "<f1> b") #'helm-descbinds))

(use-package helm-projectile
  :ensure t
  :config (setq helm-projectile-fuzzy-match t))

(use-package helm-ag
  :ensure t
  :defer t)

(use-package helm-swoop
  :ensure t
  :defer t
  :init (define-key isearch-mode-map (kbd "M-s") #'helm-swoop-from-isearch)
  :config (progn
            (setq helm-swoop-use-line-number-face t)
            (define-key helm-swoop-map (kbd "C-s") #'helm-next-line)
            (define-key helm-swoop-map (kbd "C-r") #'helm-previous-line)
            ;; I prefer M-s for this
            (define-key isearch-mode-map (kbd "M-i") nil)))

(use-package helm-external
  :defer t
  :config (when (eq system-type 'windows-nt)
            (advice-add 'helm-open-file-externally
                        :override
                        #'basis/helm-open-file-w32)))

;; To prevent a delay the first time I use M-x
(when (eq (key-binding (kbd "M-x")) #'helm-M-x)
  (require 'helm-command))

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
  :config (fset 'helm-pages-get-next-header
                #'basis/helm-pages-get-next-header))

(use-package ivy
  :ensure swiper
  :config
  (progn (setq ivy-format-function #'ivy-format-function-arrow)
         (define-key ivy-minibuffer-map (kbd "C-r") #'ivy-previous-line)))

(use-package counsel
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (progn
    (add-hook 'after-init-hook #'global-company-mode)
    (basis/define-keys company-active-map
      ("TAB"    #'company-complete)
      ([tab]    #'company-complete)
      ("<C-f1>" #'company-show-location)
      ("<M-f1>" #'company-show-location)
      ("C-w"    nil)
      ("C-h"    nil)
      ("RET"    nil)
      ([return] nil))
    (set-default
     (make-variable-buffer-local 'company-backends)
     '(company-capf
       (company-dabbrev-code company-gtags company-etags company-keywords)
       company-files
       company-dabbrev))
    (setq company-minimum-prefix-length 2)
    (setq company-tooltip-flip-when-above t)
    (advice-add 'company-auto-begin
                :before-until
                #'basis/company-maybe-block-completion)
    (when (string-match-p "\\`sierra\\(\\.\\|$\\)" system-name)
      (advice-add 'company-auto-begin
                  :before-until
                  #'basis/company-no-srv-completion))
    (with-eval-after-load 'cc-mode
      (when-let ((prog (basis/find-clang-program))
                 (args (basis/build-clang-args 'c)))
        (require 'company-clang)
        (setq company-clang-executable prog)
        (setq company-clang-arguments args)
        (add-hook 'c-mode-hook #'basis/maybe-enable-company-clang)))))

(use-package company-statistics
  :ensure t
  :config (progn (setq company-statistics-file
                       (basis/emacs-file "var/company-statistics-cache.el"))
                 (add-hook 'after-init-hook #'company-statistics-mode t)))

(use-package company-emoji
  :ensure t
  :defer t
  :init (when (eq system-type 'darwin)
          (add-hook 'after-make-frame-functions #'basis/maybe-set-emoji-font))
  :config (add-to-list 'company-backends #'company-emoji))

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
    (basis/define-keys yas-minor-mode-map
      ("C-t"   #'basis/yas-expand-or-insert)
      ("TAB"   nil)
      ([(tab)] nil))
    (define-key yas-keymap (kbd "RET") #'yas-exit-all-snippets)
    (setq yas-snippet-dirs (list (basis/emacs-dir "snippets/")))
    (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
    (setq yas-wrap-around-region t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming modes

(defun basis/init-prog-mode ()
  (basis/maybe-enable-whitespace-mode)
  (basis/maybe-enable-flyspell-prog-mode)
  (unless (eq major-mode 'sql-mode)
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode)))

(use-package prog-mode
  :config (progn (define-key prog-mode-map (kbd "RET") #'basis/electric-return)
                 (add-hook 'prog-mode-hook #'basis/init-prog-mode)))

(defvar basis/lisp-modes
  '(lisp-mode
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
  "List of Lisp modes.
Use `paredit' in these modes rather than `smartparens'.")

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

(defun basis/init-hippie-expand-for-elisp ()
  "Enable Lisp symbol completion in `hippie-exp'."
  (let ((functions (make-local-variable 'hippie-expand-try-functions-list)))
    (add-to-list functions #'try-complete-lisp-symbol t)
    (add-to-list functions #'try-complete-lisp-symbol-partially t)))

(defun basis/init-emacs-lisp-modes ()
  "Enable features useful when working with Emacs Lisp."
  ;; Paredit is enabled by `basis/init-lisp-generic'
  (basis/init-hippie-expand-for-elisp)
  ;; Normally `lexical-binding' should be set within a file, but that doesn't
  ;; work for *scratch* and *ielm*
  (when (member (buffer-name) '("*scratch*" "*ielm*"))
    (setq lexical-binding t))
  ;; Use `common-lisp-indent-function', if it knows about Emacs Lisp
  ;; XXX: Disabled
  (when (and nil (get 'if 'common-lisp-indent-function-for-elisp))
    (setq-local lisp-indent-function #'common-lisp-indent-function)))

(defun basis/init-emacs-lisp-mode ()
  (unless no-byte-compile
    (basis/maybe-enable-flycheck)))

(use-package elisp-mode
  :mode ("\\.eld\\'" . emacs-lisp-mode)
  :config
  (progn
    (basis/define-eval-keys emacs-lisp-mode-map
      (last-sexp  #'eval-last-sexp)
      (definition #'eval-defun)
      (region     #'eval-region)
      (buffer     #'eval-buffer)
      (something  #'basis/eval-something)
      (file       #'load-file)
      (expand     #'macrostep-expand))
    (basis/define-eval-keys lisp-interaction-mode-map
      (last-sexp  #'basis/eval-last-sexp)
      (definition #'eval-defun)
      (region     #'eval-region)
      (buffer     #'eval-buffer)
      (something  #'basis/eval-something)
      (file       #'load-file)
      (expand     #'macrostep-expand))
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
  :config (global-eldoc-mode)
  :diminish eldoc-mode)

(use-package macrostep
  :ensure t
  :defer t)

(use-package pcre2el
  :ensure t
  :defer t)

(use-package redshank
  :ensure t
  :config (redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t)
  :diminish redshank-mode)

(defun basis/start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

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
    (basis/define-eval-keys slime-mode-map
      (last-sexp  #'slime-eval-last-expression)
      (definition #'slime-eval-defun)
      (region     #'slime-eval-region)
      (buffer     #'slime-eval-buffer)
      (something  #'basis/slime-eval-something)
      (file       #'slime-compile-and-load-file)
      (expand     #'slime-expand-1))))

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
  (when-let ((lein (executable-find "lein")))
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
    ;; Indentation tweaks
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
            (basis/define-eval-keys cider-mode-map
              (last-sexp  #'cider-eval-last-sexp)
              (definition #'cider-eval-defun-at-point)
              (region     #'cider-eval-region)
              (buffer     #'cider-eval-buffer)
              (something  #'basis/cider-eval-something)
              (file       #'cider-load-current-buffer)
              (expand     #'cider-macroexpand-1))))

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
            (basis/define-eval-keys scheme-mode-map
              (last-sexp  #'scheme-send-last-sexp)
              (definition #'scheme-send-definition)
              (region     #'scheme-send-region)
              (something  #'basis/scheme-send-something)
              (file       #'scheme-load-file)
              (expand     #'scheme-expand-current-form))
            (add-hook 'scheme-mode-hook #'basis/init-lisp-generic)))

(use-package quack
  :ensure t
  :defer t
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
  :config (basis/define-eval-keys geiser-mode-map
            (last-sexp  #'geiser-eval-last-sexp)
            (definition #'geiser-eval-definition)
            (region     #'geiser-eval-region)
            (buffer     #'geiser-eval-buffer)
            (file       #'geiser-load-file)
            (something  #'basis/geiser-eval-something)
            (expand     #'basis/geiser-expand-something)))

(use-package geiser-repl
  :defer t
  :config (add-hook 'geiser-repl-mode-hook #'basis/init-lisp-generic))

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
    (basis/define-eval-keys python-mode-map
      (definition #'python-shell-send-defun)
      (buffer     #'python-shell-send-buffer)
      (region     #'python-shell-send-region)
      (something  #'basis/python-send-something)
      (file       #'python-shell-send-file))
    (basis/define-keys python-mode-map
      ("DEL"     #'basis/sp-python-backspace)
      ("C-c C-D" #'python-eldoc-at-point)
      ("C-c C-p" #'basis/run-python)
      ("C-h C-p" #'basis/insert-python-docstring-quotes))
    (add-hook 'python-mode-hook #'basis/init-python-mode)
    (add-hook 'inferior-python-mode-hook #'basis/init-inferior-python-mode)
    ;; Jedi has 2 Python dependencies: jedi and epc
    (when (basis/jedi-installed-p)
      (setq jedi:setup-keys t)
      (setq jedi:tooltip-method nil)
      (add-hook 'python-mode-hook #'jedi:setup))
    (when (eq system-type 'windows-nt)
      ;; Use the launcher when available
      (when (executable-find "py")
        (setq python-shell-interpreter "py"))
      ;; Hopefully-temporary workaround
      (when (boundp 'python-shell-completion-native-enable)
        (setq python-shell-completion-native-enable nil)))))

(use-package pyvenv
  :ensure t
  :defer t)

(use-package jedi
  :ensure t
  :defer t)

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
  :config (basis/define-keys interactive-haskell-mode-map
            ("C-c C-z" #'haskell-interactive-bring)
            ("C-c C-l" #'haskell-process-load-or-reload)
            ("C-c C-k" #'haskell-interactive-mode-clear)
            ("C-c C-c" #'haskell-process-cabal-build)
            ("C-c c"   #'haskell-process-cabal)
            ("M-."     #'haskell-mode-goto-loc)
            ("M-?"     #'haskell-mode-find-uses)
            ("C-c C-t" #'haskell-mode-show-type-at)))

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
  :defer t
  :after (js2-mode sgml-mode css-mode)
  :config (progn
            (skewer-setup) ; hook into {js2,html,css}-mode
            (basis/define-eval-keys skewer-mode-map
              (last-sexp  #'skewer-eval-last-sexp)
              (definition #'skewer-eval-defun)
              (buffer     #'skewer-load-buffer))))

(use-package skewer-repl
  :defer t
  :config (define-key skewer-repl-mode-map (kbd "TAB") #'hippie-expand))

(use-package skewer-css
  :defer t
  :config (basis/define-eval-keys skewer-css-mode-map
            (last-sexp  #'skewer-css-eval-current-declaration)
            (definition #'skewer-css-eval-current-rule)
            (buffer     #'skewer-css-eval-buffer)))

(defun basis/init-sql-mode ()
  (basis/sql-set-product)
  (setq tab-width 4))

(use-package sql
  :defer t
  ;; When using Emacs as $PSQL_EDITOR, open the files in `sql-mode'
  :mode ("/psql.edit.[0-9]+\\'" . sql-mode)
  :config (progn
           ;; But I also work with other products and it's often easier not to
           ;; switch `sql-product' around.
           (let ((more-keywords '("unload" "elsif" "endif" "while")))
             (add-to-list
              'sql-mode-postgres-font-lock-keywords
              (apply #'sql-font-lock-keywords-builder
                     'font-lock-keyword-face nil more-keywords)))
           (basis/define-keys sql-mode-map
             ("TAB"   #'basis/sql-indent)
             ("DEL"   #'basis/sql-backspace-dedent)
             ("M-n"   #'basis/sql-forward-clause)
             ("M-p"   #'basis/sql-backward-clause)
             ("C-M-a" #'basis/sql-beginning-of-defun)
             ("C-M-e" #'basis/sql-end-of-defun))
           (add-hook 'sql-mode-hook #'basis/init-sql-mode)
           ;; Put the advice on `sql-highlight-product' rather than
           ;; `sql-set-product' because the former is potentially re-run later,
           ;; as part of `hack-local-variables-hook', and would undo our
           ;; changes.
           (advice-add 'sql-highlight-product :after
                       #'basis/sql-modify-syntax-table)))

(defun basis/init-c-base ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq-local comment-style 'extra-line)
  ;; (c-toggle-auto-newline 1)
  (dolist (cleanup '(brace-else-brace
                     brace-elseif-brace
                     defun-close-semi
                     empty-defun-braces))
    (add-to-list 'c-cleanup-list cleanup))
  (subword-mode))

(defun basis/init-c ()
  (c-set-style "python")
  (basis/init-c-base))

(defun basis/init-c++ ()
  (basis/init-c)
  (c-set-offset 'innamespace 0)
  (dolist (cleanup '(brace-catch-brace scope-operator))
    (add-to-list 'c-cleanup-list cleanup)))

(defun basis/init-java ()
  (c-set-style "java")
  (basis/init-c-base))

(use-package cc-mode
  :defer t
  :config
  (progn
    (define-key c-mode-base-map (kbd "C-j") #'c-context-line-break)
    (define-key c-mode-map (kbd "C-c C-v") #'ff-find-other-file)
    (add-hook 'c-mode-hook    #'basis/init-c)
    (add-hook 'c++-mode-hook  #'basis/init-c++)
    (add-hook 'java-mode-hook #'basis/init-java)))

(defun basis/init-go-mode ()
  (subword-mode))

(use-package go-mode
  :ensure t
  :defer t
  :config (add-hook 'go-mode-hook #'basis/init-go-mode))

(use-package swift-mode
  :ensure t
  :defer t)

(use-package nasm-mode
  :ensure t
  :defer t)

(defun basis/init-sh-mode ()
  (setq tab-width 4)
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) ".zshrc"))
    (sh-set-shell "zsh")))

(use-package sh-script
  :defer t
  :config (add-hook 'sh-mode-hook #'basis/init-sh-mode))

(use-package gforth
  :defer t
  :mode ("\\.f\\'" "\\.fs\\'" "\\.fth\\'")
  :config (with-eval-after-load 'forth-mode
            (define-key forth-mode-map (kbd "M-o")   nil)
            (define-key forth-mode-map (kbd "M-SPC") nil)))

(use-package batch-mode
  :ensure t
  :defer t
  :mode "\\.bat\\'")

(use-package ahk-mode
  :ensure t
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text, markup, and configuration modes

(defun basis/init-text-mode ()
  (setq indicate-buffer-boundaries 'right)
  (auto-fill-mode)
  (basis/maybe-enable-flyspell)
  (when (and ispell-alternate-dictionary
             (not (derived-mode-p 'sgml-mode 'nxml-mode)))
    (add-to-list 'company-backends 'company-ispell)))

(add-hook 'text-mode-hook #'basis/init-text-mode)

(use-package org
  :ensure t
  :defer t
  :init
  (basis/define-keys global-map
    ("C-c a" #'org-agenda)
    ("C-c c" #'org-capture)
    ("C-c l" #'org-store-link))
  :config
  (progn
    ;; Paths
    (setq org-directory "~/Dropbox/org/")
    (setq org-default-notes-file (expand-file-name "refile.org" org-directory))
    (setq org-archive-location "%s.archive::")
    (setq org-agenda-files (mapcar (lambda (name)
                                     (expand-file-name name org-directory))
                                   '("todo.org" "work.org")))
    ;; Misc. options
    (setq org-completion-use-ido t)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-reverse-note-order t)
    (setq org-log-done t)
    (setq org-special-ctrl-a/e t)
    (setq org-ellipsis "…")
    ;; Agenda
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    ;; Code blocks & org-babel
    (setq org-src-fontify-natively t)
    (setq org-confirm-babel-evaluate nil)
    ;; Capture
    (setq org-capture-templates
          `(("t" "Todo" entry (file+headline ,org-default-notes-file "Tasks")
             "* TODO %?\n %i\n")
            ("w" "Work todo" entry (file+headline "~/Dropbox/org/work.org"
                                                  "Tasks")
             "* TODO %?\n %i\n")
            ("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
             "* %u %?")))
    ;; Refiling
    (setq org-refile-use-outline-path 'file)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-targets '((nil :maxlevel . 2)
                               (org-agenda-files :maxlevel . 2)))
    ;; Todo keywords
    (setq org-todo-keywords
          '((sequence
             "TODO(t)" "STARTED(s@)" "WAITING(w@/!)" "DELEGATED(l@)" "|"
             "DONE(d!)" "DEFERRED(f@)" "CANCELLED(c@)")))
    (define-key org-mode-map (kbd "RET") #'org-return-indent)
    (setq org-structure-template-alist
          (mapcar (pcase-lambda (`(,key ,val)) (list key (downcase val)))
                  org-structure-template-alist))
    (require 'ob)
    (require 'ob-tangle)
    (require 'ob-clojure)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C          . t)
       (clojure    . t)
       (emacs-lisp . t)
       (haskell    . t)
       (python     . t)
       (scheme     . t)
       (shell      . t)))))

(use-package ob-tangle
  :defer t
  :config (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj")))

(use-package ob-clojure
  :defer t
  :config (fset 'org-babel-execute:clojure #'basis/org-babel-execute:clojure))

(defun basis/init-simplezen ()
  (setq-local yas-fallback-behavior
              '(apply simplezen-expand-or-indent-for-tab)))

(defun basis/init-html-mode ()
  (setq tab-width 4)
  (tagedit-mode))

(defun basis/sgml-delete-tag-reindent (&rest _ignore)
  "Advice for `sgml-delete-region' to reindent the buffer."
  (indent-region (point-min) (point-max)))

(use-package sgml-mode
  :defer t
  :config
  (progn
    (basis/define-keys html-mode-map
      ([remap forward-paragraph]  #'basis/move-to-next-blank-line)
      ([remap backward-paragraph] #'basis/move-to-previous-blank-line)
      ("RET"                      #'basis/html-newline-and-indent)
      ("M-RET"                    #'basis/html-multiline-expand)
      ("C-c C-w"                  #'basis/html-wrap-in-tag)
      ("C-c w"                    #'basis/html-wrap-in-tag))
    (add-hook 'sgml-mode-hook #'basis/init-simplezen)
    (add-hook 'html-mode-hook #'basis/init-html-mode)
    (advice-add 'sgml-delete-tag :after #'basis/sgml-delete-tag-reindent)))

(defun basis/tagedit-toggle-multiline-maybe-forward (original &rest args)
  "Advice for `tagedit-toggle-multiline-tag'.
Move forward by a line and indent if invoked directly between."
  (let ((move-forward-p (and (eq (char-before) ?>) (eq (char-after) ?<))))
    (apply original args)
    (when move-forward-p
      (forward-line 1)
      (indent-according-to-mode))))

(use-package tagedit
  :ensure t
  :defer t
  :after sgml-mode
  :config (progn (tagedit-add-paredit-like-keybindings)
                 (tagedit-add-experimental-features)
                 (advice-add 'tagedit-toggle-multiline-tag
                             :around
                             #'basis/tagedit-toggle-multiline-maybe-forward)))

(use-package simplezen
  :ensure t
  :defer t
  :after sgml-mode)

(defun basis/init-markdown-mode ()
  (setq tab-width 4)
  (when (eq major-mode 'gfm-mode)
    (auto-fill-mode -1)))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.markdown\\'" "\\.mkd\\'" "\\.md\\'")
  :config (progn
            (basis/define-keys markdown-mode-map
              ("DEL"     #'basis/sp-markdown-backspace)
              ("M-n"     #'forward-paragraph)
              ("M-p"     #'backward-paragraph)
              ("C-c r"   #'markdown-insert-reference-link-dwim)
              ("C-c C-r" #'markdown-insert-reference-link-dwim))
            (add-hook 'markdown-mode-hook #'basis/init-markdown-mode)))

(use-package writegood-mode
  :ensure t
  :defer t)

(defun basis/init-yaml-mode ()
  (basis/maybe-enable-flyspell-prog-mode)
  (when (basis/yaml-multiple-docs-p)
    (basis/yaml-multi-doc-mode)))

(use-package yaml-mode
  :ensure t
  :defer t
  :config (add-hook 'yaml-mode-hook #'basis/init-yaml-mode))

(use-package deft
  :ensure t
  :defer t
  :config (progn (setq deft-extension "md")
                 (setq deft-directory "~/Dropbox/deft")
                 (setq deft-text-mode 'gfm-mode)))

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

(use-package ssh-config-mode
  :ensure t
  :defer t
  :mode (".ssh/config\\'"  "sshd?_config\\'"))

(use-package css-mode
  :defer t
  :init (put 'css-indent-offset 'safe-local-variable #'integerp))


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
    (basis/define-keys paredit-mode-map
      ("M-?"             nil) ; Make room for `xref-find-references'
      ("M-)"             #'basis/paredit-wrap-round-from-behind)
      ("M-e"             #'paredit-forward)
      ("M-a"             #'paredit-backward)
      ("M-k"             #'kill-sexp)
      ("C-w"             #'basis/paredit-kill-something)
      ("M-DEL"           #'basis/paredit-kill-something))
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'basis/paredit-doublequote-space-p)
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

(use-package smartparens
  :ensure t
  :config
  (progn
    (smartparens-global-strict-mode)
    ;; I still prefer Paredit with Lisps, and having Smartparens enabled messes
    ;; with argument handling in `magit-key-mode'.
    (dolist (mode (cons 'magit-key-mode basis/lisp-modes))
      (add-to-list 'sp-ignore-modes-list mode))
    (sp-use-paredit-bindings)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-autoescape-string-quote nil)
    (setq sp-use-subword t)
    (setq-default sp-autoskip-closing-pair 'always)
    (sp-pair "'" nil
             :unless '(basis/sp-point-after-word-p)
             :actions '(insert wrap autoskip))
    (pcase-dolist (`(,mode ,open ,close ,actions)
                   '((org-mode  "=" "=" (wrap))
                     (rust-mode "'" nil (:rem insert autoskip))
                     (c-mode    "{" "}" (:rem insert autoskip))
                     (c++-mode  "{" "}" (:rem insert autoskip))
                     (java-mode "{" "}" (:rem insert autoskip))))
      (sp-local-pair mode open close :actions actions))
    (basis/define-keys sp-keymap
      ("M-DEL"           #'basis/sp-kill-something)
      ("C-DEL"           #'basis/sp-kill-something)
      ("<C-backspace>"   #'basis/sp-kill-something)
      ("C-w"             #'basis/sp-kill-something)
      ("M-k"             #'sp-kill-sexp)
      ("M-e"             #'sp-forward-sexp)
      ("M-a"             #'sp-backward-sexp)
      ("C-M-u"           #'basis/sp-backward-up))
    (advice-add 'sp--cleanup-after-kill :around #'basis/sp-cleanup-maybe-not)
    (advice-add 'sp--unwrap-sexp :filter-args #'basis/sp-unwrap-no-cleanup)
    ;; Treat raw prefix arguments like numeric arguments
    (advice-add 'sp-backward-delete-char
                :filter-args
                #'basis/sp-backward-delete-no-prefix)))


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
         ;; Ugly kludge to make the personal dictionary work on my
         ;; Windows+Cygwin setup.
         (when (eq basis/system-type 'windows+cygwin)
           (advice-add 'ispell-init-process :around
                       #'basis/ispell-init-process))))

(basis/define-map basis/flycheck-map (basis/ctl-h-map "l")
  ("c"   #'flycheck-buffer)
  ("n"   #'flycheck-next-error)
  ("p"   #'flycheck-previous-error)
  ("l"   #'flycheck-list-errors)
  ("s"   #'flycheck-select-checker)
  ("C"   #'flycheck-clear)
  ("SPC" #'basis/flycheck-check-and-list-errors)
  ("h"   #'helm-flycheck))

(use-package flycheck
  :ensure t
  :defer t
  :init (global-set-key (kbd "<f8>")  #'basis/flycheck-check-and-list-errors)
  :config (progn
            (setq flycheck-check-syntax-automatically nil)
            (unless (basis/libxml-available-p)
              (setq flycheck-xml-parser #'flycheck-parse-xml-region))
            ;; Check buffers with errors more frequently than ones without
            (make-variable-buffer-local 'flycheck-idle-change-delay)
            (add-hook 'flycheck-after-syntax-check-hook
                      #'basis/adjust-flycheck-idle-change-delay)
            ;; Keys for the errors buffer
            (basis/define-keys flycheck-error-list-mode-map
              ("n" #'flycheck-error-list-next-error)
              ("p" #'flycheck-error-list-previous-error))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diffing

(use-package diff-mode
  :defer t
  :config
  (progn
    (setq diff-default-read-only t)
    ;; `diff-goto-source' is still available on C-c C-c.
    (define-key diff-mode-map (kbd "M-o") nil)))

(defun basis/init-ediff ()
  (ediff-setup-keymap))

(use-package ediff
  :defer t
  :config (progn
            (setq ediff-window-setup-function #'ediff-setup-windows-plain)
            (setq ediff-split-window-function #'split-window-horizontally)
            (when (eq system-type 'windows-nt)
              (advice-add 'ediff-make-empty-tmp-file
                          :filter-args
                          #'basis/ediff-expand-tmp-name))
            (advice-add 'ediff-setup :before #'basis/ediff-save-window-config)
            (advice-add 'ediff-quit :after #'basis/ediff-quit-restore)
            (add-hook 'ediff-mode-hook #'basis/init-ediff)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit & other git things

(use-package magit
  :ensure t
  :defer t
  :init (basis/define-keys global-map
          ("C-x g"   #'magit-status)
          ("C-x M-g" #'magit-dispatch-popup)
          ("C-c M-g" #'magit-file-popup))
  :config
  (progn
    (setq magit-revert-buffers 'silent)
    (setq magit-save-repository-buffers 'dontask)
    (setq magit-popup-use-prefix-argument 'default)
    (setq magit-completing-read-function #'magit-ido-completing-read)
    (setq magit-revision-show-gravatars nil)
    (setq magit-diff-expansion-threshold 999.0) ; Work around Magit issue #2388
    (setq magit-repository-directories
          (thread-last projectile-known-projects
            (seq-remove #'tramp-tramp-file-p)
            (seq-filter (lambda (dir)
                          (file-directory-p (expand-file-name ".git" dir))))
            (cons "~/code/")
            (mapcar #'directory-file-name)))
    (add-hook 'magit-post-display-buffer-hook
              #'basis/magit-maybe-delete-other-windows)
    (define-key basis/file-map "g" #'magit-find-file)
    (define-key ctl-x-4-map "g" #'magit-find-file-other-window)
    (define-key magit-status-mode-map
      (kbd "C-c C-v") #'basis/magit-browse-pull-request-url)
    (when (eq basis/system-type 'windows+cygwin)
      (setq magit-need-cygwin-noglob t)
      (setq magit-cygwin-mount-points nil)
      (when (file-executable-p "/bin/git.exe")
        (setq magit-git-executable "/bin/git.exe")
        (setq magit-git-environment nil))
      (advice-add 'magit-toplevel
                  :filter-return
                  #'basis/magit-expand-toplevel)
      (advice-add 'magit-list-repos
                  :filter-return
                  #'basis/magit-list-repos-uniquely)
      (fset 'magit-save-repository-buffers
            #'basis/magit-cygwin-save-repository-buffers)
      ;; I haven't figured out yet why the Magit commands for saving and popping
      ;; stashes fail on my Windows+Cygwin setup at work, but this gives me
      ;; quick access to the simplest usage in the meantime.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project management

;; While "project management" doesn't quite fit `ibuffer' this is where it seems
;; to make the most sense.
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
    (basis/define-keys ibuffer-mode-map
      ("M-o"   nil) ;; don't shadow ace-window
      ("C-M-o" #'ibuffer-visit-buffer-1-window))
    (define-ibuffer-column size-h
      ;; a more readable size column
      (:name "Size" :inline t)
      (cond ((> (buffer-size) 1000000)
             (format "%7.1fM" (/ (buffer-size) 1000000.0)))
            ((> (buffer-size) 1000)
             (format "%7.1fk" (/ (buffer-size) 1000.0)))
            (t
             (format "%8d" (buffer-size)))))))

(use-package ibuffer-vc
  :ensure t
  :defer t
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

(basis/define-map basis/projectile-map ()
  ("b"   #'projectile-switch-to-buffer)
  ("d"   #'projectile-find-dir)
  ("C-f" #'projectile-find-file)
  ("ff"  #'projectile-find-file-dwim)
  ("fd"  #'projectile-find-file-in-directory)
  ("g"   #'projectile-grep)
  ("i"   #'projectile-ibuffer)
  ("K"   #'projectile-kill-buffers)
  ("o"   #'projectile-multi-occur)
  ("p"   #'projectile-switch-project)
  ("r"   #'projectile-recentf)
  ("x"   #'projectile-remove-known-project)
  ("X"   #'projectile-cleanup-known-projects)
  ("z"   #'projectile-cache-current-file))

(use-package projectile
  :ensure t
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-h p"))
    (setq projectile-completion-system 'helm)
    (setq projectile-known-projects-file
          (basis/emacs-file "var/projectile-bookmarks.eld"))
    (setq projectile-cache-file (basis/emacs-file "var/projectile.cache"))
    (setq projectile-use-git-grep t)
    ;; Projectile defaults to native indexing on Windows, but if we have
    ;; Cygwin set up we can use "alien".
    (if (eq basis/system-type 'windows-nt)
        (progn (setq projectile-indexing-method 'native)
               (setq projectile-enable-caching t))
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

(use-package dired
  :defer t
  :config
  (progn
    (basis/define-keys dired-mode-map
      ("RET"                       #'dired-find-alternate-file)
      ("M-RET"                     #'dired-find-file)
      ("e"                         #'basis/dired-open-files)
      ("-"                         #'diredp-up-directory-reuse-dir-buffer)
      ("^"                         #'diredp-up-directory-reuse-dir-buffer)
      ("Y"                         #'basis/dired-rsync)
      ("M-^"                       #'diredp-up-directory)
      ("M-m"                       #'dired-omit-mode)
      ("M-n"                       #'diredp-next-subdir)
      ("M-p"                       #'diredp-prev-subdir)
      ("M-e"                       #'dired-next-dirline)
      ("M-a"                       #'dired-prev-dirline)
      ("M-b"                       nil)
      ("M-o"                       nil)
      ([remap beginning-of-buffer] #'basis/dired-jump-to-top)
      ([remap end-of-buffer]       #'basis/dired-jump-to-bottom))
    (setq dired-recursive-deletes 'top)
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                     "-alhGt"
                                   "-alht"))
    (put 'dired-find-alternate-file 'disabled nil)
    (add-hook 'dired-mode-hook #'dired-omit-mode)))

(use-package dired-x
  :defer t
  :after dired
  :config
  (progn (setq dired-omit-verbose nil)
         (setq dired-omit-extensions (remove ".bak" dired-omit-extensions))))

(use-package find-dired
  :defer t
  :config (setq find-ls-option (if (eq system-type 'windows-nt)
                                   '("-exec ls -ldhG {} +" . "-ldhG")
                                 '("-exec ls -ldh {} +" . "-ldh")))
  :after dired)

(use-package dired+
  :ensure t
  :defer t
  :after dired)

(defun basis/init-comint-mode ()
  (setq comint-scroll-to-bottom-on-input 'this))

(use-package comint
  :defer t
  :init (setenv "PAGER" "cat")
  :config
  (progn
    (basis/define-keys comint-mode-map
      ("M-p"     #'comint-previous-matching-input-from-input)
      ("M-n"     #'comint-next-matching-input-from-input)
      ("C-c C-l" #'helm-comint-input-ring)
      ;; Because Paredit and Smartparens both use M-r
      ("C-M-r"   #'comint-history-isearch-backward-regexp))
    (dolist (cmd '(comint-previous-input
                   comint-next-input
                   comint-previous-matching-input-from-input
                   comint-next-matching-input-from-input))
      (advice-add cmd :before #'basis/comint-input-goto-bottom-if-necessary))
    (add-hook 'comint-mode-hook #'basis/init-comint-mode)))

(defun basis/init-shell-mode ()
  (setq comint-process-echoes t)
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

(defun basis/init-eshell ()
  (basis/define-keys eshell-mode-map
    ("S-DEL"   #'basis/eshell-kill-line-backward)
    ("C-S-DEL" #'basis/eshell-kill-whole-line)))

(use-package esh-mode
  :defer t
  :config (progn (setq eshell-directory-name (basis/emacs-dir "var/eshell/"))
                 (add-hook 'eshell-mode-hook #'basis/init-eshell)))

;; Proced
(use-package proced
  :defer t
  :init (global-set-key (kbd "C-x p") #'proced))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Applications

(use-package elfeed
  :ensure t
  :defer t
  :config (progn (setq elfeed-db-directory (basis/emacs-dir "var/elfeed/"))
                 (when-let ((curl (executable-find "curl")))
                   (setq elfeed-curl-program-name curl)
                   (setq elfeed-use-curl t))
                 (let ((feeds (basis/emacs-file "feeds.eld")))
                   (when (file-exists-p feeds)
                     (setq elfeed-feeds (basis/elfeed-load-feeds feeds))))))

(use-package shr
  :defer t
  :config (advice-add 'shr-colorize-region :override #'ignore))

(use-package eww
  :defer t
  :config (define-key eww-mode-map (kbd "<backtab>") #'shr-previous-link))

(use-package browse-url
  :defer t
  :init (when (and (not (display-graphic-p))
                   (executable-find "w3m"))
          (setq browse-url-browser-function #'w3m-browse-url)))

(use-package w3m
  :ensure t
  :defer t
  :config (progn
            (define-key w3m-mode-map "n" #'w3m-next-anchor)
            (define-key w3m-mode-map "p" #'w3m-previous-anchor)))

(defun basis/init-sx-question-mode ()
  (toggle-truncate-lines -1))

(use-package sx
  :ensure t
  :defer t
  :config (setq sx-cache-directory (basis/emacs-dir "var/sx/")))

(use-package sx-question-list
  :defer t
  :config
  (let ((map sx-question-list-mode-map))
    (define-key map (kbd "M-RET") #'basis/sx-display-full-screen)))

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

(use-package emms
  :ensure t
  :defer t
  :config
  (progn
    (setq emms-directory (basis/emacs-dir "var/emms/"))
    (setq emms-source-file-default-directory
          (thread-last '("~/Music" "~/Media/Music" "~/Dropbox/Music")
            (seq-mapcat (lambda (dir) (list dir (downcase dir))))
            (seq-some (lambda (dir) (and (file-directory-p dir) dir)))))))

(use-package emms-setup
  :defer t
  :after emms
  :config (progn (emms-standard)
                 (emms-default-players)))

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
    (advice-add 'message-insert-signature
                :after
                #'basis/message-maybe-delete-sig-dashes)))

(use-package sendmail
  :defer t
  :config (setq send-mail-function #'smtpmail-send-it))

(use-package smtpmail
  :defer t
  :config (progn (setq smtpmail-smtp-server "mail.messagingengine.com")
                 (setq smtpmail-smtp-user "jbm@fastmail.fm")
                 (setq smtpmail-smtp-service 465)
                 (setq smtpmail-stream-type 'ssl)))

(use-package mu4e
  :defer t
  :init
  (progn
    (global-set-key (kbd "C-x m") #'mu4e)
    (let ((dir "/usr/local/share/emacs/site-lisp/mu4e/"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)
        (autoload 'mu4e "mu4e" "Launch mu4e." t))))
  :config
  (progn
    ;; Mail
    (setq mu4e-get-mail-command "offlineimap")
    (setq mu4e-maildir (expand-file-name ".maildir/fastmail" (getenv "HOME")))
    (setq mu4e-sent-folder "/sent")
    (setq mu4e-drafts-folder "/drafts")
    (setq mu4e-trash-folder "/trash")
    (setq mu4e-compose-signature "jbm")
    ;; Shortcuts. Available as jX
    (setq mu4e-maildir-shortcuts '(("/archive" . ?a)
                                   ("/inbox"   . ?i)
                                   ("/sent"    . ?s)))
    ;; Addresses to consider "me" when searching
    (setq mu4e-user-mail-address-list '("jbm@jbm.io"
                                        "jbm@deft.li"
                                        "jbm@fastmail.com"
                                        "jbm@fastmail.fm"
                                        "jbm@mailforce.net"))
    ;; Convert HTML->text if no text version is available
    (setq mu4e-html2text-command (if (executable-find "html2text")
                                     "html2text -utf8 -width 72"
                                   #'basis/shr-html2text))
    ;; Where to save attachments
    (let ((dir (seq-some (lambda (dir) (and (file-directory-p dir) dir))
                         '("~/downloads" "~/Downloads" "~/"))))
      (setq mu4e-attachment-dir dir))
    ;; Composing messages
    (setq mu4e-reply-to-address "jbm@jbm.io")
    (setq mu4e-sent-messages-behavior 'delete)
    (add-to-list 'mu4e-view-actions
                 (cons "View in browser" #'basis/mu4e-action-view-in-browser)
                 t)))

;; Retrieving credentials
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
  :config (progn (setq gnus-use-dribble-file nil)
                 (setq gnus-always-read-dribble-file nil)
                 (setq gnus-read-newsrc-file nil)
                 (setq gnus-save-newsrc-file nil)))

(use-package mm-decode
  :defer t
  :config (add-to-list 'mm-discouraged-alternatives "text/html"))

;;; init.el ends here
