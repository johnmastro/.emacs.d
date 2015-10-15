;;; init.el      -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Early configuration

(eval-when-compile (require 'cl-lib))

;; Disable superfluous UI immediately to prevent momentary display
(let* ((modes '(tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
       (modes (if (eq system-type 'darwin)
                  modes
                (cons 'menu-bar-mode modes))))
  (dolist (mode modes)
    (when (fboundp mode)
      (funcall mode -1))))

(defconst basis/emacs-dir
  (file-name-directory (file-chase-links (or load-file-name buffer-file-name)))
  "This Emacs's configuration directory.")

(defun basis/emacs-dir (name &optional if-not-exists)
  "Return directory NAME expanded in `basis/emacs-dir'.
If CREATE is non-nil, create the directory (including parents) if
it doesn't exist."
  (if (string-suffix-p "/" name)
      (let ((dir (expand-file-name name basis/emacs-dir)))
        (unless (file-directory-p dir)
          (cond ((eq if-not-exists 'error)
                 (error "Directory does not exist: '%s'" dir))
                ((eq if-not-exists 'create)
                 (make-directory dir t))))
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
(when (boundp 'package-selected-packages)
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

(use-package seq
  :ensure t
  ;; Compatibility shim for older versions of `seq' that had `seq-some-p'
  ;; instead of `seq-some'
  :config (unless (fboundp 'seq-some)
            (defun seq-some (pred seq)
              (catch 'seq--break
                (seq-doseq (elt seq)
                  (let ((result (funcall pred elt)))
                    (when result
                      (throw 'seq--break result))))
                nil))))

(use-package map
  :if (>= emacs-major-version 25))

(use-package dash
  :ensure t)

(use-package dash-functional
  :ensure t)

(use-package s
  :ensure t)

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

;; Compatibility shims for some of the new macros in Emacs 25's `subr-x', using
;; the `dash' implementations.
(when (< emacs-major-version 25)
  (let* ((macros (seq-remove (lambda (list) (fboundp (car list)))
                             '((if-let       -if-let*   2)
                               (when-let     -when-let* 1)
                               (thread-first ->         1)
                               (thread-last  ->>        1)))))
    (pcase-dolist (`(,sym ,impl ,indent) macros)
      (defalias sym impl)
      (put sym 'lisp-indent-function indent))
    (basis/add-elisp-font-lock-keywords
     (cons "pcase-dolist" (mapcar (lambda (list) (symbol-name (car list)))
                                  macros)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operating system-specific configuration

;; A graphical Emacs on OS X doesn't automatically inherit $PATH
(use-package exec-path-from-shell
  :ensure t
  :if (eq window-system 'ns)
  :config (exec-path-from-shell-initialize))

;; Because there's a bug with `visible-bell' on OS X 10.11 (El Capitan)
(use-package echo-bell
  :ensure t
  :if (eq window-system 'ns)
  :init (echo-bell-mode)
  :config (progn (setq echo-bell-string (substring echo-bell-string 0 3)
                       echo-bell-background "#073642")
                 (echo-bell-update)))

(defvar basis/system-type
  (if (and (eq system-type 'windows-nt)
           (directory-files "c:/" nil "Cygwin")
           (file-directory-p "c:/bin"))
      'windows+cygwin
    system-type)
  "Like `system-type' but with the additional option `windows+cygwin'.")

(defvar basis/cygwin-path-directories
  '("/bin" "/usr/bin" "/usr/local/bin"
    "/Python27" "/Python27/Scripts"
    "/ProgramData/Oracle/Java/javapath")
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
      (setq shell-file-name shell
            explicit-shell-file-name shell
            ediff-shell shell
            null-device "/dev/null")
      (setenv "SHELL" shell))
    (setq locale-coding-system 'utf-8)
    ;; Since Emacs wasn't launched from a Cygwin shell, $LANG will be wonky
    ;; unless we fix it.
    (setenv "LANG" "en_US.UTF-8")))

(when (eq basis/system-type 'windows+cygwin)
  (basis/init-for-cygwin))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various settings

(setq visible-bell t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      sentence-end-double-space nil
      indicate-empty-lines t
      custom-file (basis/emacs-file "custom.el")
      recenter-positions '(top middle bottom)
      scroll-preserve-screen-position t
      delete-by-moving-to-trash t
      gc-cons-threshold (* 20 1024 1024)
      temporary-file-directory (basis/emacs-dir "tmp/")
      enable-recursive-minibuffers t)

;; Prevent point from entering the minibuffer prompt
(setq minibuffer-prompt-properties
      (append minibuffer-prompt-properties
              '(point-entered minibuffer-avoid-prompt)))

(setq-default major-mode 'text-mode
              indent-tabs-mode nil
              fill-column 80
              truncate-lines t)

(when (file-exists-p custom-file)
  (load custom-file))

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

(use-package simple
  :init (progn (setq shift-select-mode nil
                     line-number-mode t
                     column-number-mode t)
               ;; Let C-n add newlines, and don't deactivate the mark
               (setq next-line-add-newlines t))
  :config (progn
            (size-indication-mode)
            (advice-add 'next-line :around #'basis/next-line-no-deactivate-mark)
            ;; Keep popping the mark until point actually moves
            (advice-add 'pop-to-mark-command
                        :around
                        #'basis/pop-to-mark-ensure-new-pos)))

(use-package mule
  :config
  (let ((fns (list #'prefer-coding-system
                   #'set-default-coding-systems
                   #'set-terminal-coding-system
                   #'set-keyboard-coding-system
                   #'set-language-environment
                   #'set-selection-coding-system)))
    (dolist (fn (if (eq system-type 'windows-nt) (butlast fns) fns))
      (funcall fn 'utf-8))))

(defun basis/maybe-set-coding ()
  (when (and (eq system-type 'windows-nt)
             (tramp-tramp-file-p buffer-file-name))
    (set-buffer-file-coding-system 'utf-8-unix)))

(use-package files
  :init
  (progn (setq-default require-final-newline t)
         (setq confirm-nonexistent-file-or-buffer nil)
         (setq backup-by-copying t)
         (setq backup-directory-alist
               `((".*" . ,(basis/emacs-dir "var/backups/"))))
         (setq auto-save-file-name-transforms
               `((".*" ,(basis/emacs-dir "var/autosaves/") t)))
         (setq auto-save-list-file-prefix
               (concat (basis/emacs-dir "var/auto-save-list/") ".saves-")))
  :config
  (progn
    (pcase system-type
      (`darwin
       (when-let ((gls (executable-find "gls")))
         (setq insert-directory-program gls)))
      (`windows-nt
       (add-hook 'before-save-hook #'basis/maybe-set-coding)))))

(use-package mouse
  :init (setq mouse-yank-at-point t))

(use-package url
  :defer t
  :init (setq url-configuration-directory (basis/emacs-dir "var/url/")))

(use-package bookmark
  :defer t
  :init (setq bookmark-default-file (basis/emacs-file "var/bookmarks")
              bookmark-save-flag 1))

(use-package ffap
  :defer t
  :init (setq ffap-machine-p-known 'reject))

(use-package advice
  :defer t
  :init (setq ad-redefinition-action 'accept))

(defun basis/set-default-input-method (&optional method)
  (setq default-input-method (or method "TeX")))

;; Haven't yet bothered looking into why this needs to be done after init
(add-hook 'after-init-hook #'basis/set-default-input-method)

(use-package server
  :init (setq server-auth-dir (basis/emacs-dir "var/server/"))
  :config (unless (and server-name (server-running-p server-name))
            (server-start)))

(use-package hl-line
  :config (global-hl-line-mode))

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
                 (setq global-auto-revert-non-file-buffers t
                       auto-revert-verbose nil)))

(use-package frame
  :config (blink-cursor-mode -1))

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (basis/emacs-file "var/places")))

(use-package savehist
  :init (setq savehist-additional-variables '(search-ring regexp-search-ring)
              savehist-file (basis/emacs-file "var/history"))
  :config (savehist-mode t))

(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward
              uniquify-ignore-buffers-re "^\\*"))

(use-package whitespace
  :init
  (setq whitespace-style '(face trailing lines-tail tabs)
        whitespace-line-column 80)
  (put 'whitespace-line-column 'safe-local-variable #'integerp)
  :diminish whitespace-mode)

(use-package recentf
  :init (setq recentf-max-saved-items 50
              recentf-save-file (basis/emacs-file "var/recentf")
              recentf-exclude (list #'tramp-tramp-file-p #'file-remote-p))
  :config (recentf-mode))

(use-package tramp
  :defer t
  :init
  (setq tramp-default-method
        (if (eq basis/system-type 'windows-nt)
            "plinkx"
          "sshx"))
  (setq tramp-persistency-file-name (basis/emacs-file "var/tramp"))
  ;; Have TRAMP use Cygwin's sh rather than Windows's cmd.exe
  (when (eq basis/system-type 'windows+cygwin)
    (setq tramp-encoding-shell (executable-find "sh")
          tramp-encoding-command-switch "-c"
          tramp-encoding-command-interactive "-i")))

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
  (let ((fonts (pcase system-type
                 (`gnu/linux  '("Inconsolata-11"))
                 (`darwin     '("Source Code Pro-11" "Andale Mono-12"))
                 (`windows-nt '("Consolas-10")))))
    (catch 'done
      (dolist (font fonts)
        (when (ignore-errors (set-face-attribute 'default nil :font font)
                             t)
          (throw 'done t))))))

(use-package solarized-theme
  :ensure color-theme-solarized
  :init (progn
          (set-frame-parameter nil 'background-mode 'dark)
          (set-terminal-parameter nil 'background-mode 'dark)
          (setq solarized-termcolors 256
                solarized-italic nil)
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

(use-package page-break-lines
  :ensure t
  :defer t
  ;; :config (global-page-break-lines-mode)
  :diminish page-break-lines-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation modes

(use-package help-mode
  :defer t
  :config (basis/define-keys help-mode-map
            ("n" #'next-line)
            ("p" #'previous-line)
            ("b" #'help-go-back)
            ("f" #'help-go-forward)))

(use-package info
  :defer t
  :config
  (let ((info-path (basis/emacs-dir "doc/info/")))
    (when (file-directory-p info-path)
      (add-to-list 'Info-additional-directory-list info-path))))

(use-package apropos
  :defer t
  :init (setq apropos-do-all t))

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
   (setq mac-command-modifier 'meta
         mac-option-modifier 'super))
  (`windows-nt
   (setq w32-pass-apps-to-system nil
         w32-pass-lwindow-to-system nil
         w32-pass-rwindow-to-system nil
         w32-lwindow-modifier 'super
         w32-rwindow-modifier 'super)))

(use-package guide-key
  :ensure t
  :config (progn
            (guide-key-mode)
            (setq guide-key/popup-window-position 'bottom)
            (setq guide-key/idle-delay 0.0)
            (setq guide-key/guide-key-sequence
                  '("C-x r" "C-x 4" "C-x v" "C-x 8"
                    (dired-mode "*" "C-t")
                    (ibuffer-mode "/" "*" "%" "M-s" "M-s a")
                    (calc-mode "V"))))
  :diminish guide-key-mode)

;; When splitting windows, select the newly created window
(global-set-key (kbd "C-x 2") #'basis/split-window-below)
(global-set-key (kbd "C-x 3") #'basis/split-window-right)

;; Treat `C-u C-l' the same as `C-u 4 C-l'
(global-set-key (kbd "C-l") #'basis/recenter-top-bottom)

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
  ("ESC <deletechar>"      #'basis/smart-kill-almost-whole-line)
  ("<C-M-backspace>"       #'backward-kill-sexp)
  ("C-M-_"                 #'backward-kill-sexp)
  ("ESC M-DEL"             #'backward-kill-sexp))

;; Copy stuff
(basis/define-keys global-map
  ("M-w"    #'basis/kill-ring-save-something)
  ("<f2>"   #'basis/clipboard-save-something)
  ("s-w"    #'basis/kill-ring-save-indented))

;; Join lines
(basis/define-keys global-map
  ("C-c C-j" #'basis/join-next-line)
  ("C-c j"   #'basis/join-next-line)
  ("ESC M-q" #'basis/unfill-paragraph))

;; Transpose stuff with M-t
(basis/define-map basis/transposition-map (:key "M-t")
  ("l"   #'transpose-lines)
  ("w"   #'transpose-words)
  ("s"   #'transpose-sexps)
  ("c"   #'transpose-chars)
  ("M-w" #'basis/transpose-windows)
  ("M-s" #'basis/toggle-window-split))

;; Slightly more SLIME-like interface for tags. But don't change anything if
;; `xref' is available, since it has an improved interface.
(unless (require 'xref nil t)
  (define-key esc-map "." #'basis/find-tag)
  (define-key esc-map "," #'pop-tag-mark)
  (define-key esc-map "*" #'tags-loop-continue))

;; More comfortable {next,previous}-error
(global-set-key (kbd "M-g M-f") #'next-error)
(global-set-key (kbd "M-g M-b") #'previous-error)

;; Comment/uncomment stuff
(global-set-key (kbd "C-c ;") #'basis/comment-or-uncomment)
(global-set-key (kbd "C-x ;") #'basis/comment-region-lines)
;; Unfortunately, this one doesn't work at the terminal
(global-set-key (kbd "C-M-;") #'basis/comment-or-uncomment-sexp)

;; Eval
(global-set-key (kbd "C-x C-e") #'basis/eval-last-sexp)
(global-set-key (kbd "C-c M-e") #'basis/eval-and-replace)

;; I use M-SPC for `avy-goto-word-1'
(global-set-key (kbd "C-c SPC") #'just-one-space)

;; goto-line, with line numbers
(global-set-key (kbd "M-g M-g") #'basis/goto-line-with-numbers)

;; Movement by sexp
(global-set-key (kbd "M-e") #'forward-sexp)
(global-set-key (kbd "M-a") #'backward-sexp)

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
(global-set-key (kbd "C-c g") #'basis/google)

;; Re-open recent files
(global-set-key (kbd "C-x C-r") #'basis/find-file-recentf)

;; Previous/next buffer
(global-set-key (kbd "<C-prior>") #'previous-buffer)
(global-set-key (kbd "<C-next>") #'next-buffer)

(basis/define-map basis/file-map (:key "C-c f")
  ("c" #'helm-locate)
  ("d" #'basis/diff-buffer-with-file)
  ("r" #'basis/rename-current-buffer-file)
  ("D" #'basis/delete-current-buffer-file)
  ("f" #'find-name-dired)
  ("F" #'find-dired)
  ("m" #'make-directory)
  ("v" #'revert-buffer))

;; Open one or more files externally, using the `helm-external' machinery
(global-set-key (kbd "C-c C-x") #'basis/open-file-externally)

;; Emacs Lisp-style quotes
(global-set-key (kbd "C-c q") #'basis/elisp-quote)

;; Random operations on regions
(basis/define-map basis/region-map (:key "C-c r")
  ("a" #'align)
  ("c" #'basis/count-words)
  ("l" #'basis/count-sloc-region)
  ("s" #'sort-lines))

;; Narrowing can be quite handy
(put 'narrow-to-region 'disabled nil)

(basis/define-map basis/find-lisp-map (:key "<f1> e")
  ("c" #'finder-commentary)
  ("e" #'view-echo-area-messages)
  ("f" #'find-function)
  ("F" #'find-face-definition)
  ("i" #'info-apropos)
  ("k" #'find-function-on-key)
  ("l" #'find-library)
  ("m" #'info-display-manual)
  ("s" #'basis/scratch)
  ("v" #'find-variable)
  ("V" #'apropos-value)
  ("a" #'helm-apropos))

;; Make it harder to accidentally `suspend-frame'
(basis/define-map basis/ctl-z-map (:key "C-z")
  ("C-z" #'suspend-frame))

(global-set-key (kbd "C-x C-z") #'repeat)

(basis/define-map basis/h-map ()
  ("C-k" #'basis/kill-this-buffer)
  ("C-h" #'mark-paragraph))

;; Note sure which will be better
(global-set-key (kbd "C-h") 'basis/h-map)
(global-set-key (kbd "M-h") 'basis/h-map)

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

;; A number of non-alphanumeric keys don't work by default when Emacs is
;; running in tmux. This attempts to fix that by adding entries to the
;; `key-translation-map'. It's based on code from ArchWiki's Emacs page.

;; `setw -g xterm-keys on` must be set in ~/.tmux.conf for this to work.

;; TODO: <home> and <end> still don't work
(when (getenv "TMUX")
  (pcase-dolist (`(,n . ,k) '((2 . "S-")
                              (3 . "M-")
                              (4 . "M-S-")
                              (5 . "C-")
                              (6 . "C-S-")
                              (7 . "C-M-")
                              (8 . "C-M-S-")))
    (basis/define-key-translations
      ((format "M-[ 1 ; %d A" n)  (format "%s<up>" k))
      ((format "M-[ 1 ; %d B" n)  (format "%s<down>" k))
      ((format "M-[ 1 ; %d C" n)  (format "%s<right>" k))
      ((format "M-[ 1 ; %d D" n)  (format "%s<left>" k))
      ((format "M-[ 1 ; %d H" n)  (format "%s<home>" k))
      ((format "M-[ 1 ; %d F" n)  (format "%s<end>" k))
      ((format "M-[ 5 ; %d ~" n)  (format "%s<prior>" k))
      ((format "M-[ 6 ; %d ~" n)  (format "%s<next>" k))
      ((format "M-[ 2 ; %d ~" n)  (format "%s<delete>" k))
      ((format "M-[ 3 ; %d ~" n)  (format "%s<delete>" k))
      ((format "M-[ 1 ; %d P" n)  (format "%s<f1>" k))
      ((format "M-[ 1 ; %d Q" n)  (format "%s<f2>" k))
      ((format "M-[ 1 ; %d R" n)  (format "%s<f3>" k))
      ((format "M-[ 1 ; %d S" n)  (format "%s<f4>" k))
      ((format "M-[ 15 ; %d ~" n) (format "%s<f5>" k))
      ((format "M-[ 17 ; %d ~" n) (format "%s<f6>" k))
      ((format "M-[ 18 ; %d ~" n) (format "%s<f7>" k))
      ((format "M-[ 19 ; %d ~" n) (format "%s<f8>" k))
      ((format "M-[ 20 ; %d ~" n) (format "%s<f9>" k))
      ((format "M-[ 21 ; %d ~" n) (format "%s<f10>" k))
      ((format "M-[ 23 ; %d ~" n) (format "%s<f11>" k))
      ((format "M-[ 24 ; %d ~" n) (format "%s<f12>" k))
      ((format "M-[ 25 ; %d ~" n) (format "%s<f13>" k))
      ((format "M-[ 26 ; %d ~" n) (format "%s<f14>" k))
      ((format "M-[ 28 ; %d ~" n) (format "%s<f15>" k))
      ((format "M-[ 29 ; %d ~" n) (format "%s<f16>" k))
      ((format "M-[ 31 ; %d ~" n) (format "%s<f17>" k))
      ((format "M-[ 32 ; %d ~" n) (format "%s<f18>" k))
      ((format "M-[ 33 ; %d ~" n) (format "%s<f19>" k))
      ((format "M-[ 34 ; %d ~" n) (format "%s<f20>" k))))
  (basis/define-key-translations
    ("M-[ 1 ; 5 k" "C-=")
    ("M-[ 1 ; 5 l" "C-,")
    ("M-[ 1 ; 5 n" "C-.")
    ("M-[ 1 ; 6 k" "C-+")
    ("M-[ 1 ; 6 l" "C-<")
    ("M-[ 1 ; 6 n" "C->")
    ("M-[ 1 ; 6 y" "C-(")
    ("M-[ 1 ; 7 k" "C-M-=")
    ("M-[ 1 ; 7 n" "C-M-.")
    ("M-[ 1 ; 7 l" "C-M-,")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
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
               (setq mc/list-file (basis/emacs-file "var/mc-lists.el"))))

(use-package multiple-cursors-core
  :ensure multiple-cursors
  :defer t
  ;; Make RET exit multiple-cursors-mode in the terminal too
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
  :init (defalias 'vqr #'vr/query-replace)
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement

(use-package imenu
  :defer t
  :init (setq imenu-auto-rescan t))

(use-package avy
  :ensure t
  :defer t
  :init (progn (setq avy-keys '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?u ?i)
                     avy-style 'pre)
               (global-set-key (kbd "M-SPC") #'avy-goto-word-1)
               (global-set-key (kbd "M-s-SPC") #'avy-goto-line))
  :config (advice-add 'avy-push-mark :after #'basis/avy-push-mark))

(defun basis/ace-window-kludge (function arg)
  "Advice for `ace-window'.
Ensure it always works with two windows, even when one (or both)
is read-only and empty."
  (if (and (eq aw-scope 'frame)
           (= (length (window-list)) 2))
      (pcase arg
        (4  (basis/transpose-windows 1))
        (16 (delete-other-windows))
        (_  (other-window 1)))
    (funcall function arg)))

(use-package ace-window
  :ensure t
  :defer t
  :init (progn (setq aw-keys '(?h ?j ?k ?l ?n ?m)
                     aw-scope 'frame)
               (global-set-key (kbd "M-o") #'ace-window))
  :config (advice-add 'ace-window :around #'basis/ace-window-kludge))

(use-package jump-char
  :ensure t
  :defer t
  :init (progn (global-set-key (kbd "M-m") #'jump-char-forward)
               (global-set-key (kbd "M-M") #'jump-char-backward)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search

(setq character-fold-search t)

(basis/define-keys global-map
  ("C-s"   #'isearch-forward-regexp)
  ("C-r"   #'isearch-backward-regexp)
  ("C-M-s" #'isearch-forward)
  ("C-M-r" #'isearch-backward))

(basis/define-keys isearch-mode-map
  ("DEL"         #'basis/isearch-backspace)
  ("<backspace>" #'basis/isearch-backspace)
  ("C-t"         #'basis/isearch-yank-something)
  ("C-g"         #'basis/isearch-cancel))

(use-package swiper
  :ensure t
  :defer t
  :init (progn (setq swiper-min-highlight 1)
               (define-key isearch-mode-map (kbd "M-s") #'swiper-from-isearch))
  :config (basis/define-keys swiper-map
            ("M-%"   #'swiper-query-replace)
            ("M-SPC" #'swiper-avy)
            ("C-t"   #'basis/swiper-maybe-yank-something)))

(use-package swiper-helm
  :ensure t
  :defer t)

;; replace.el doesn't ‘(provide 'replace)’
(defalias 'qrr #'query-replace-regexp)
(global-set-key (kbd "ESC M-%") #'query-replace-regexp)
(define-key occur-mode-map (kbd "n") #'occur-next)
(define-key occur-mode-map (kbd "p") #'occur-prev)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External search

(basis/define-map basis/grep-map (:key "<f9>")
  ("a"  #'ack-and-a-half)
  ("g"  #'grep)
  ("s"  #'lgrep)
  ("r"  #'rgrep)
  ("z"  #'zrgrep)
  ("v"  #'projectile-grep)
  ("V"  #'vc-git-grep)
  ("f"  #'find-grep)
  ("d"  #'find-grep-dired)
  ("o"  #'occur)
  ("mo" #'multi-occur)
  ("mm" #'multi-occur-in-matching-buffers))

(use-package grep
  :defer t
  :config
  ;; On OS X, prefer GNU Grep if it's available
  (when (and (eq system-type 'darwin)
             (executable-find "ggrep"))
    (setq grep-program "ggrep"))
  ;; Add some more file aliases
  (pcase-dolist (`(,alias . ,files)
                 '(("clj" . "*.clj *.cljs *.cljc")
                   ("cl"  . "*.lisp *.cl")
                   ("txt" . "*.txt *.org *.rst *.md *.mkd *.markdown")))
    (unless (assoc alias grep-files-aliases)
      (add-to-list 'grep-files-aliases (cons alias files)))))

(use-package ack-and-a-half
  :defer t
  :commands (ack-and-a-half)
  :init (progn
          (defalias 'ack #'ack-and-a-half)
          (defalias 'ack-same #'ack-and-a-half-same)
          (defalias 'ack-find-file #'ack-and-a-half-find-file)
          (defalias 'ack-find-file-same #'ack-and-a-half-find-file-same)
          (when (file-exists-p "~/bin/ack")
            (setq ack-and-a-half-executable "~/bin/ack")))
  :config
  (progn
    (basis/define-keys ack-and-a-half-mode-map
      ("n" #'compilation-next-error)
      ("p" #'compilation-previous-error)
      ("]" #'compilation-next-file)
      ("[" #'compilation-previous-file))
    (setq ack-and-a-half-use-ido t)
    ;; Make Cygwin happy
    (when (and (eq basis/system-type 'windows+cygwin)
               (stringp ack-and-a-half-executable)
               (string-prefix-p "c:" ack-and-a-half-executable))
      (setq ack-and-a-half-executable
            (substring ack-and-a-half-executable 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion

(defun basis/init-ido-keys ()
  (basis/define-keys ido-file-completion-map
    ("M-w"     #'ido-copy-current-file-name)
    ("C-x g"   #'ido-enter-magit-status)
    ("C-c C-x" #'basis/ido-open-file-externally)))

(use-package ido
  :init (setq ido-enable-prefix nil
              ido-enable-flex-matching t
              ido-auto-merge-work-directories-length -1
              ido-create-new-buffer 'always
              ido-use-filename-at-point nil
              ido-use-virtual-buffers t
              ido-use-faces nil
              ido-max-prospects 10
              ido-ignore-extensions t
              ido-save-directory-list-file (basis/emacs-file "var/ido.last"))
  :config (add-hook 'ido-setup-hook #'basis/init-ido-keys))

(use-package ido-ubiquitous
  :ensure t)

(use-package flx-ido
  :ensure t
  :init (setq flx-ido-threshhold 10000)
  :config (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode))

(use-package idomenu
  :ensure t
  :defer t)

(use-package smex
  :ensure t
  :defer t
  :init (progn (global-set-key (kbd "M-X") #'smex-major-mode-commands)
               (setq smex-save-file (basis/emacs-file "var/smex-items"))))

(use-package helm-config
  :ensure helm
  :config (global-unset-key (kbd helm-command-prefix-key)))

(use-package helm
  :ensure t
  :defer t
  :init (progn
          (setq helm-split-window-default-side 'other
                helm-split-window-in-side-p t
                helm-quick-update t
                helm-truncate-lines t
                helm-display-header-line nil)
          (basis/define-map basis/helm-map (:key "C-c h")
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
            (":" #'helm-eval-expression-with-eldoc)))
  :config (progn
            (require 'helm-utils) ; For the `helm-selection-line' face
            (basis/define-keys helm-map
              ("TAB" #'helm-execute-persistent-action)
              ("M-s" #'helm-select-action)
              ("DEL" #'basis/helm-backspace))
            (set-face-attribute 'helm-source-header nil :height 1.0)
            (require 'helm-adaptive)
            ;; Always display Helm buffers at the bottom, using 40% of the
            ;; frame's height
            (add-to-list 'display-buffer-alist
                         '("\\`\\*helm.*\\*\\'"
                           (display-buffer-in-side-window)
                           (inhibit-same-window . t)
                           (window-height . 0.4)))))

(use-package helm-adaptive
  :ensure helm
  :init (setq helm-adaptive-history-file
              (basis/emacs-file "var/helm-adaptive-history"))
  :config (helm-adaptive-mode))

(use-package helm-mode
  :ensure helm
  :init (setq helm-mode-handle-completion-in-region nil)
  :config
  (progn (helm-mode)
         ;; None of helm, ido, or ivy seem to handle `Info-goto-node'
         (add-to-list 'helm-completing-read-handlers-alist '(Info-goto-node))))

(use-package helm-files
  :ensure helm
  :defer t
  :init (progn (global-set-key (kbd "C-x C-f") #'helm-find-files)
               (setq helm-ff-newfile-prompt-p nil
                     helm-ff-file-name-history-use-recentf t
                     helm-ff-search-library-in-sexp t
                     helm-recentf-fuzzy-match t))
  :config (progn (basis/define-keys helm-find-files-map
                   ("TAB"   #'helm-execute-persistent-action)
                   ("M-s"   #'helm-select-action)
                   ("DEL"   #'basis/helm-backspace)
                   ("C-x g" #'basis/helm-ff-run-magit-status))
                 ;; Disable `ffap' behavior
                 (advice-add 'helm-find-files-input :override #'ignore)))

(use-package helm-locate
  :ensure helm
  :init (setq helm-locate-fuzzy-match nil))

(use-package helm-buffers
  :ensure helm
  :defer t
  :init (progn (setq helm-buffers-fuzzy-matching t)
               (global-set-key (kbd "C-x b") #'helm-mini)))

(use-package helm-command
  :ensure helm
  :defer t
  :init (progn (setq helm-M-x-fuzzy-match t)
               (global-set-key (kbd "M-x") #'helm-M-x)))

(use-package helm-imenu
  :ensure helm
  :defer t
  :init (progn (setq helm-imenu-execute-action-at-once-if-one nil
                     helm-imenu-fuzzy-match t)
               (global-set-key (kbd "M-i") #'helm-imenu)))

(use-package helm-ring
  :ensure helm
  :defer t
  :init (progn (global-set-key (kbd "M-y") #'helm-show-kill-ring)
               (global-set-key (kbd "M-`") #'helm-all-mark-rings)))

(use-package helm-elisp
  :ensure helm
  :defer t
  :init (progn (setq helm-apropos-fuzzy-match t
                     helm-lisp-fuzzy-completion t)
               (global-set-key (kbd "<f1> SPC") #'helm-apropos)))

(use-package helm-man
  :ensure helm
  :init (unless basis/system-man-p
          (setq helm-man-or-woman-function #'woman)))

(use-package helm-descbinds
  :ensure t
  :defer t
  :init (global-set-key (kbd "<f1> b") #'helm-descbinds))

(use-package helm-projectile
  :ensure t
  :init (setq helm-projectile-fuzzy-match t))

(use-package helm-swoop
  :ensure t
  :defer t
  :init (progn
          (setq helm-swoop-use-line-number-face t)
          (define-key isearch-mode-map (kbd "M-s") #'helm-swoop-from-isearch))
  :config (progn
            (define-key helm-swoop-map (kbd "C-s") #'helm-next-line)
            (define-key helm-swoop-map (kbd "C-r") #'helm-previous-line)
            ;; I prefer M-s for this
            (define-key isearch-mode-map (kbd "M-i") nil)))

(use-package helm-external
  :ensure helm
  :defer t
  :config
  (when (eq system-type 'windows-nt)
    (advice-add 'helm-open-file-externally
                :override
                #'basis/helm-open-file-w32)))

;; To prevent a delay the first time I use M-x
(when (eq (key-binding (kbd "M-x")) #'helm-M-x)
  (require 'helm-command))

(use-package helm-grep
  :ensure helm
  :defer t
  ;; On OS X, prefer GNU grep if it's available
  :config
  (when (and (eq system-type 'darwin)
             (executable-find "ggrep"))
    (dolist (sym '(helm-grep-default-command helm-grep-default-recurse-command))
      (let ((cmd (symbol-value sym)))
        (set sym (replace-regexp-in-string "\\`grep" "ggrep" cmd))))))

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
  :init (setq ivy-format-function #'basis/ivy-format-function)
  :config (define-key ivy-minibuffer-map (kbd "C-r") #'ivy-previous-line))

(use-package counsel
  :ensure t)

(use-package company
  :ensure t
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
    (setq company-minimum-prefix-length 2
          company-tooltip-flip-when-above t)
    (advice-add 'company-auto-begin
                :around
                #'basis/company-no-completion-in-docstring)
    (advice-add 'company-auto-begin
                :around
                #'basis/company-sh-no-complete-fi)
    (when (eq basis/current-hostname 'sierra)
      (advice-add 'company-auto-begin
                  :around
                  #'basis/company-no-srv-completion))
    (when (eq system-type 'windows-nt)
      (advice-add 'company-auto-begin
                  :around
                  #'basis/company-no-tramp-completion))
    (with-eval-after-load 'cc-mode
      (when-let ((args (basis/build-clang-args 'c)))
        (require 'company-clang)
        (setq company-clang-arguments args)
        (add-hook 'c-mode-hook #'basis/enable-company-clang)))))

(use-package company-statistics
  :ensure t
  :init (setq company-statistics-file
              (basis/emacs-file "var/company-statistics-cache.el"))
  :config (add-hook 'after-init-hook #'company-statistics-mode t))

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
    (setq yas-snippet-dirs (list (basis/emacs-dir "snippets/"))
          yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
          yas-wrap-around-region t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming modes

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook #'basis/maybe-enable-whitespace-mode)
  (add-hook 'prog-mode-hook #'basis/maybe-enable-flyspell-prog-mode))

(defvar basis/lisp-modes
  '(lisp-mode
    emacs-lisp-mode
    inferior-emacs-lisp-mode
    lisp-interaction-mode
    cider-repl-mode
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
  :init (setq inferior-lisp-program (or (executable-find "sbcl")
                                        (executable-find "ccl")
                                        "lisp"))
  :config (add-hook 'inferior-lisp-mode-hook #'basis/init-lisp-generic))

(use-package cl-indent
  :init (setq lisp-lambda-list-keyword-alignment t
              lisp-lambda-list-keyword-parameter-alignment t
              lisp-loop-forms-indentation 6))

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
  (elisp-slime-nav-mode t)
  (basis/init-hippie-expand-for-elisp)
  (turn-on-eldoc-mode)
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
      (last-sexp  #'basis/eval-last-sexp)
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

(use-package elisp-slime-nav
  :ensure t
  :defer t
  :diminish elisp-slime-nav-mode)

(use-package eldoc
  :defer t
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :diminish eldoc-mode)

(use-package macrostep
  :ensure t
  :defer t)

(use-package pcre2el
  :ensure t
  :defer t)

(use-package redshank
  :ensure t
  :init (redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t)
  :diminish redshank-mode)

(defun basis/start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(use-package slime
  :ensure t
  :defer t
  :init (progn (setq slime-lisp-implementations
                     '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
                       (ccl ("ccl"))))
               (setq slime-default-lisp 'sbcl
                     slime-contribs '(slime-fancy)))
  :config
  (progn
    (basis/define-eval-keys slime-mode-map
      (last-sexp  #'slime-eval-last-expression)
      (definition #'slime-eval-defun)
      (region     #'slime-eval-region)
      (buffer     #'slime-eval-buffer)
      (something  #'basis/slime-eval-something)
      (file       #'slime-compile-and-load-file)
      (expand     #'slime-expand-1))
    (setq slime-autodoc-use-multiline-p t)))

(use-package slime-repl
  :ensure slime
  :defer t
  :config (add-hook 'slime-repl-mode-hook #'basis/init-lisp-generic))

(use-package slime-company
  :ensure t
  :defer t)

(defun basis/init-clojure-mode ()
  (subword-mode)
  (clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-h m"))

(defun basis/init-cider-repl-mode ()
  (subword-mode)
  (cider-turn-on-eldoc-mode))

(defun basis/init-cider-mode ()
  (cider-turn-on-eldoc-mode))

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
  :config
  (progn
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
  :ensure cider
  :defer t
  :init (setq nrepl-log-messages t))

(use-package cider-repl
  :ensure cider
  :defer t
  :init (setq cider-repl-use-pretty-printing t)
  :config (progn
            (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-return)
            (add-hook 'cider-repl-mode-hook #'basis/init-lisp-generic)
            (add-hook 'cider-repl-mode-hook #'basis/init-cider-repl-mode)))

(use-package clj-refactor
  :ensure t
  :defer t
  :diminish clj-refactor-mode)

(use-package clojure-cheatsheet
  :ensure t
  :defer t)

(use-package quack
  :ensure t
  :defer t
  :init (progn (setq quack-default-program
                     (if (eq system-type 'windows-nt)
                         "racket"
                       "guile"))
               (setq quack-fontify-style 'emacs)))

(use-package scheme
  :defer t
  :config (progn
            (require 'quack)
            (basis/define-eval-keys scheme-mode-map
              (last-sexp  #'scheme-send-last-sexp)
              (definition #'scheme-send-definition)
              (region     #'scheme-send-region)
              (something  #'basis/scheme-send-something)
              (file       #'scheme-load-file)
              (expand     #'scheme-expand-current-form))
            (add-hook 'scheme-mode-hook #'basis/init-lisp-generic)))

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
  :ensure geiser
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
    (basis/define-eval-keys python-mode-map
      (definition #'python-shell-send-defun)
      (buffer     #'python-shell-send-buffer)
      (region     #'python-shell-send-region)
      (something  #'basis/python-send-something)
      (file       #'python-shell-send-file))
    (basis/define-keys python-mode-map
      ("RET"     #'basis/electric-return)
      ("DEL"     #'basis/sp-python-backspace)
      ("C-c C-D" #'python-eldoc-at-point)
      ("C-c C-p" #'basis/run-python)
      ("C-h C-p" #'basis/insert-python-docstring-quotes)
      ("C-c C-j" nil))
    (setq python-fill-docstring-style 'pep-257-nn)
    (add-hook 'python-mode-hook #'basis/init-python-mode)
    (add-hook 'inferior-python-mode-hook #'basis/init-inferior-python-mode)
    (when (eq basis/system-type 'windows+cygwin)
      (advice-add (if (fboundp 'python-shell-calculate-command)
                      'python-shell-calculate-command
                    'python-shell-parse-command)
                  :filter-return
                  #'basis/cygwin-fix-file-name))
    ;; Jedi has 2 Python dependencies: jedi and epc
    (when (basis/jedi-installed-p)
      (setq jedi:setup-keys t
            jedi:tooltip-method nil)
      (add-hook 'python-mode-hook #'jedi:setup))
    ;; Hopefully-temporary workaround
    (when (and (eq system-type 'windows-nt)
               (boundp 'python-shell-completion-native-enable))
      (setq python-shell-completion-native-enable nil))))

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
  (turn-on-haskell-indentation)
  (interactive-haskell-mode))

(use-package haskell-mode
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook #'basis/init-haskell-mode))

(use-package interactive-haskell-mode
  :ensure haskell-mode
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
  :ensure haskell-mode
  :defer t
  :mode "\\.ghci\\'")

(defun basis/rust-set-compile-command ()
  (unless (or (file-exists-p "Makefile")
              (file-exists-p "makefile"))
    (setq-local compile-command
                (if (file-exists-p "Cargo.toml")
                    "cargo build"
                  (format "rustc %s"
                          (if buffer-file-name
                              (shell-quote-argument buffer-file-name)
                            ""))))))

(defun basis/init-rust-mode ()
  (subword-mode)
  (basis/rust-set-compile-command))

(use-package rust-mode
  :ensure t
  :defer t
  :config (progn
            (define-key rust-mode-map (kbd "RET") #'basis/electric-return)
            (add-hook 'rust-mode-hook #'basis/init-rust-mode)))

(defun basis/init-js2-mode ()
  (setq tab-width 4)
  (subword-mode)
  (js2-imenu-extras-setup))

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'"
  :init (setq-default js2-basic-offset 2
                      js2-show-parse-errors nil
                      js2-allow-rhino-new-expr-initializer nil
                      js2-strict-inconsistent-return-warning nil
                      js2-strict-missing-semi-warning nil
                      js2-strict-trailing-comma-warning t)
  :config (progn
            (js2r-add-keybindings-with-prefix "C-h m")
            (define-key js2-mode-map (kbd "C-;") #'basis/eol-maybe-semicolon)
            (add-hook 'js2-mode-hook #'basis/init-js2-mode)))

(use-package js-comint
  :ensure t
  :defer t)

(use-package js2-refactor
  :ensure t
  :defer t)

(use-package skewer-mode
  :ensure t
  :init (skewer-setup) ; hook into js2, html, and css modes
  :config (basis/define-eval-keys skewer-mode-map
            (last-sexp  #'skewer-eval-last-sexp)
            (definition #'skewer-eval-defun)
            (buffer     #'skewer-load-buffer)))

(use-package skewer-repl
  :ensure skewer-mode
  :defer t
  :config (define-key skewer-repl-mode-map (kbd "TAB") #'hippie-expand))

(use-package skewer-css
  :ensure skewer-mode
  :config (basis/define-eval-keys skewer-css-mode-map
            (last-sexp  #'skewer-css-eval-current-declaration)
            (definition #'skewer-css-eval-current-rule)
            (buffer     #'skewer-css-eval-buffer)))

(defun basis/init-sql-mode ()
  (basis/sql-set-product)
  (setq tab-width 4)
  (basis/sql-modify-syntax-table))

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
             ("RET"   #'basis/electric-return)
             ("TAB"   #'basis/sql-indent)
             ("DEL"   #'basis/sql-backspace-dedent)
             ("M-n"   #'basis/sql-forward-clause)
             ("M-p"   #'basis/sql-backward-clause)
             ("C-M-a" #'basis/sql-beginning-of-defun)
             ("C-M-e" #'basis/sql-end-of-defun))
           (add-hook 'sql-mode-hook #'basis/init-sql-mode)))

(defun basis/init-c-base ()
  (setq indent-tabs-mode nil
        c-basic-offset 4)
  (setq-local comment-style 'extra-line)
  (c-toggle-auto-newline 1)
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
    (add-hook 'c-mode-hook    #'basis/init-c)
    (add-hook 'c++-mode-hook  #'basis/init-c++)
    (add-hook 'java-mode-hook #'basis/init-java)))

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
  (auto-fill-mode)
  (basis/maybe-enable-flyspell)
  (when ispell-alternate-dictionary
    (add-to-list 'company-backends 'company-ispell)))

(add-hook 'text-mode-hook #'basis/init-text-mode)

(use-package org
  :defer t
  :init
  (progn
    (basis/define-keys global-map
      ("C-c a" #'org-agenda)
      ("C-c c" #'org-capture)
      ("C-c l" #'org-store-link))
    ;; Paths
    (setq org-directory "~/Dropbox/org/"
          org-default-notes-file (expand-file-name "refile.org" org-directory)
          org-archive-location "%s.archive::"
          org-agenda-files (mapcar (lambda (name)
                                     (expand-file-name name org-directory))
                                   '("todo.org" "work.org")))
    ;; Misc. options
    (setq org-completion-use-ido t
          org-outline-path-complete-in-steps nil
          org-reverse-note-order t
          org-log-done t
          org-special-ctrl-a/e t)
    ;; Agenda
    (setq org-agenda-start-on-weekday nil
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t)
    ;; Code blocks & org-babel
    (setq org-src-fontify-natively t
          org-confirm-babel-evaluate nil)
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
    (setq org-refile-use-outline-path 'file
          org-refile-allow-creating-parent-nodes 'confirm
          org-refile-targets '((nil :maxlevel . 2)
                               (org-agenda-files :maxlevel . 2)))
    ;; Todo keywords
    (setq org-todo-keywords
          '((sequence
             "TODO(t)" "STARTED(s@)" "WAITING(w@/!)" "DELEGATED(l@)" "|"
             "DONE(d!)" "DEFERRED(f@)" "CANCELLED(c@)"))))
  :config
  (progn
    (define-key org-mode-map (kbd "RET") #'org-return-indent)
    (setq org-structure-template-alist
          (mapcar (lambda (elt)
                    (pcase-let ((`(,key ,val) elt))
                      (list key (downcase val))))
                  org-structure-template-alist))
    (require 'ob)
    (require 'ob-tangle)
    (require 'ob-clojure)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (clojure    . t)
       (python     . t)
       (sh         . t)))))

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
    (require 'tagedit)
    (require 'simplezen)
    (basis/define-keys html-mode-map
      ([remap forward-paragraph]  #'basis/move-to-next-blank-line)
      ([remap backward-paragraph] #'basis/move-to-previous-blank-line)
      ("RET"                      #'basis/html-newline-and-indent)
      ("M-RET"                    #'basis/html-multiline-expand)
      ("C-c C-w"                  #'basis/html-wrap-in-tag)
      ("C-c w"                    #'basis/html-wrap-in-tag))
    (add-hook 'sgml-mode-hook #'basis/init-simplezen)
    (add-hook 'html-mode-hook #'basis/init-html-mode)
    (tagedit-add-paredit-like-keybindings)
    (tagedit-add-experimental-features)
    (advice-add 'sgml-delete-tag :after #'basis/sgml-delete-tag-reindent)))

(defun basis/tagedit-toggle-multiline-maybe-forward (function &rest args)
  "Advice for `tagedit-toggle-multiline-tag'.
Move forward by a line and indent if invoked directly between."
  (let ((move-forward-p (and (eq (char-before) ?>) (eq (char-after) ?<))))
    (apply function args)
    (when move-forward-p
      (forward-line 1)
      (indent-according-to-mode))))

(use-package tagedit
  :ensure t
  :defer t
  :config (advice-add 'tagedit-toggle-multiline-tag
                      :around
                      #'basis/tagedit-toggle-multiline-maybe-forward))

(use-package simplezen
  :ensure t
  :defer t)

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
  :init (setq deft-extension "md"
              deft-directory "~/Dropbox/deft"
              deft-text-mode  'gfm-mode))

(use-package csv-mode
  :ensure t
  :defer t
  :config
  ;; Prevent `csv-mode' from being enabled automatically
  (dolist (elt auto-mode-alist)
    (pcase elt
      ((and (or `(,rgx . ,sym) `(,rgx ,sym . ,_))
            (guard (eq sym 'csv-mode)))
       (add-to-list 'auto-mode-alist (cons rgx 'text-mode))))))

(use-package ssh-config-mode
  :ensure t
  :defer t
  :mode (".ssh/config\\'"  "sshd?_config\\'"))

(use-package css-mode
  :defer t
  :init (put 'css-indent-offset 'safe-local-variable #'integerp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Brackets

(defun basis/maybe-map-paredit-newline ()
  "Map `paredit-newline' except in some interactive modes."
  (unless (or (minibufferp) (memq major-mode '(inferior-emacs-lisp-mode
                                               inferior-lisp-mode
                                               inferior-scheme-mode
                                               geiser-repl-mode
                                               cider-repl-mode)))
    (local-set-key (kbd "RET") #'paredit-newline)))

(defun basis/maybe-enable-paredit-mode ()
  "Enable Paredit during Lisp-related minibuffer commands."
  (let ((paredit-minibuffer-commands '(eval-expression
                                       pp-eval-expression
                                       eval-expression-with-eldoc
                                       slime-interactive-eval
                                       helm-eval-expression-with-eldoc)))
    (when (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode))))

(use-package paredit
  :ensure t
  :init (add-hook 'minibuffer-setup-hook #'basis/maybe-enable-paredit-mode)
  :config
  (progn
    (basis/define-keys paredit-mode-map
      ("M-)"             #'basis/paredit-wrap-round-from-behind)
      ("M-e"             #'paredit-forward)
      ("M-a"             #'paredit-backward)
      ("M-k"             #'kill-sexp)
      ("C-w"             #'basis/paredit-kill-something)
      ("M-DEL"           #'basis/paredit-kill-something)
      ("<C-M-backspace>" #'backward-kill-sexp)
      ("C-M-_"           #'backward-kill-sexp))
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
    (add-hook 'paredit-mode-hook #'basis/maybe-map-paredit-newline)
    (pcase-dolist (`(,sym . ,act) '((paredit-kill            . supersede)
                                    (paredit-forward-delete  . supersede)
                                    (paredit-backward-delete . supersede)
                                    (paredit-newline         . t)))
      (put sym 'delete-selection act))))

(use-package smartparens
  :ensure t
  :init (smartparens-global-strict-mode)
  :config
  (progn
    ;; I still prefer Paredit with Lisps, and having Smartparens enabled messes
    ;; with argument handling in `magit-key-mode'.
    (dolist (mode (cons 'magit-key-mode basis/lisp-modes))
      (add-to-list 'sp-ignore-modes-list mode))
    (sp-use-paredit-bindings)
    (setq sp-cancel-autoskip-on-backward-movement nil
          sp-autoescape-string-quote nil
          sp-use-subword t)
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
      ("<C-M-backspace>" #'sp-backward-kill-sexp)
      ("C-M-_"           #'sp-backward-kill-sexp))
    ;; These commands invoke `indent-according-to-mode' but, when
    ;; `indent-line-function' is `indent-relative', that often doesn't work out
    ;; too well.
    (basis/disable-relative-reindent-for
     '(sp-kill-word
       sp-backward-kill-word
       sp-kill-sexp
       sp-kill-hybrid-sexp
       basis/sp-kill-something))
    ;; Treat raw prefix arguments like numeric arguments
    (advice-add 'sp-backward-delete-char
                :filter-args
                #'basis/sp-backward-delete-no-prefix)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error checking

(use-package ispell
  :defer t
  :init (setq ispell-program-name (executable-find "aspell")
              ispell-personal-dictionary "~/.aspell.en.pws"
              ispell-extra-args '("--sug-mode=ultra"))
  :config (condition-case nil
              (lookup-words "whatever")
            (error
             (when (file-readable-p "~/Dropbox/dict/words")
               (setq ispell-alternate-dictionary "~/Dropbox/dict/words")))))

(basis/define-map basis/flycheck-map (:key "C-h l")
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

(defun basis/init-diff-mode ()
  (setq buffer-read-only t))

(use-package diff-mode
  :defer t
  :config
  ;; `diff-goto-source' is still available on C-c C-c.
  (define-key diff-mode-map (kbd "M-o") nil)
  (add-hook 'diff-mode-hook #'basis/init-diff-mode))

(defun basis/init-ediff ()
  (ediff-setup-keymap))

(use-package ediff
  :defer t
  :init (setq ediff-window-setup-function #'ediff-setup-windows-plain
              ediff-split-window-function #'split-window-horizontally)
  :config (progn
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
          ("<f10>"   #'magit-status)
          ("C-x M-g" #'magit-dispatch-popup)
          ("C-c M-g" #'magit-file-popup))
  :config
  (progn
    (setq magit-revert-buffers 'silent)
    (setq magit-save-repository-buffers 'dontask)
    (setq magit-push-always-verify nil)
    (setq magit-popup-use-prefix-argument 'default)
    (setq magit-completing-read-function #'magit-ido-completing-read)
    (setq magit-revision-show-gravatars nil)
    (setq magit-display-buffer-function #'basis/magit-display-buffer)
    (setq magit-repository-directories
          (thread-last projectile-known-projects
            (seq-remove #'tramp-tramp-file-p)
            (seq-filter (lambda (dir)
                          (file-directory-p (expand-file-name ".git" dir))))
            (cons "~/code/")
            ;; Remove the trailing slashes
            (mapcar #'directory-file-name)))
    (setq magit-branch-arguments (remove "--track" magit-branch-arguments))
    (when (eq basis/system-type 'windows+cygwin)
      (setq magit-need-cygwin-noglob t)
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
          'prepend)))
    ;; Add a command on `C-c C-v' to view the pull request URL. It would be even
    ;; better to add this to Magit's menus but nowhere sticks out as obviously
    ;; appropriate.
    (define-key magit-status-mode-map
      (kbd "C-c C-v") #'basis/magit-browse-pull-request-url)))

(use-package with-editor
  :ensure t
  :defer t
  ;; On Cygwin, fix `with-editor-emacsclient-executable' and advice
  ;; `with-editor-locate-emacsclient' so that its result is accurate for any
  ;; future uses.
  :config (when (eq basis/system-type 'windows+cygwin)
            (when-let ((client with-editor-emacsclient-executable)
                       (client (basis/cygwin-fix-file-name client)))
              (setq with-editor-emacsclient-executable client))
            (advice-add 'with-editor-locate-emacsclient
                        :filter-return
                        #'basis/with-editor-cygwin-fix-file-name)))

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
  :init (progn
          (defalias 'ls #'ibuffer)
          (global-set-key [remap list-buffers] #'ibuffer)
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
          (setq ibuffer-show-empty-filter-groups nil))
  :config
  (progn
    (require 'ibuffer-vc)
    (basis/define-keys ibuffer-mode-map
      ("M-o"   nil) ;; don't shadow ace-window
      ("C-M-o" #'ibuffer-visit-buffer-1-window)
      ("\\"    #'basis/ibuffer-toggle-vc-grouping))
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
  :config
  (advice-add 'ibuffer-vc-root :around #'basis/ibuffer-vc-root-files-only))

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
  :init (progn
          (setq projectile-keymap-prefix (kbd "C-h p")
                projectile-completion-system 'helm)
          (setq projectile-known-projects-file
                (basis/emacs-file "var/projectile-bookmarks.eld"))
          (setq projectile-cache-file (basis/emacs-file "var/projectile.cache"))
          (setq projectile-use-git-grep t)
          ;; Projectile defaults to native indexing on Windows, but if we have
          ;; Cygwin set up we can use "alien".
          (if (eq basis/system-type 'windows-nt)
              (setq projectile-indexing-method 'native
                    projectile-enable-caching t)
            (setq projectile-indexing-method 'alien
                  projectile-enable-caching nil))
          (projectile-global-mode)
          (global-set-key projectile-keymap-prefix
                          'basis/projectile-map))
  :config
  (when (eq basis/system-type 'windows+cygwin)
    (define-key projectile-mode-map
      [remap projectile-regenerate-tags] #'basis/projectile-regenerate-tags))
  :diminish projectile-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Processes, shells, and the filesystem

(use-package compile
  :defer t
  :init (progn (global-set-key (kbd "C-c b c") #'compile)
               (global-set-key (kbd "C-c b b") #'recompile))
  :config (setq compilation-ask-about-save nil
                compilation-always-kill t
                compilation-scroll-output 'first-error
                compilation-context-lines 2))

(use-package ls-lisp
  :defer t
  :init (when (eq basis/system-type 'windows+cygwin)
          (setq ls-lisp-use-insert-directory-program t)))

(use-package dired
  :defer t
  :config
  (progn
    (require 'dired+)
    (require 'find-dired)
    (basis/define-keys dired-mode-map
      ("RET"                       #'dired-find-alternate-file)
      ("M-RET"                     #'dired-find-file)
      ("e"                         #'basis/dired-open-files)
      ("-"                         #'diredp-up-directory-reuse-dir-buffer)
      ("^"                         #'diredp-up-directory-reuse-dir-buffer)
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
    (setq dired-omit-extensions (remove ".bak" dired-omit-extensions)
          dired-omit-verbose nil
          dired-recursive-deletes 'top)
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                     "-alhGt"
                                   "-alht"))
    (setq find-ls-option (if (eq system-type 'windows-nt)
                             '("-exec ls -ldhG {} +" . "-ldhG")
                           '("-exec ls -ldh {} +" . "-ldh")))
    (put 'dired-find-alternate-file 'disabled nil)
    (add-hook 'dired-mode-hook #'dired-omit-mode)))

(use-package dired-x
  :defer t
  :init (global-set-key (kbd "C-h C-j") #'dired-jump))

(use-package dired+
  :ensure t
  :defer t)

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
  ;; Not sure why comint/dirtrack see junk in front of my prompt with Cygwin's
  ;; zsh, so just work around it
  :init (setq-default dirtrack-list
                      (if (eq basis/system-type 'windows+cygwin)
                          '("^%[ \r]*\\(.+\\)>" 1)
                        '("^[^:\n]+@[^:\n]+:\\(.+\\)>" 1)))
  :config (add-hook 'shell-mode-hook #'basis/init-shell-mode))

(defun basis/init-eshell ()
  (basis/define-keys eshell-mode-map
    ("S-DEL"   #'basis/eshell-kill-line-backward)
    ("C-S-DEL" #'basis/eshell-kill-whole-line)))

(use-package esh-mode
  :defer t
  :init (setq eshell-directory-name (basis/emacs-dir "var/eshell/"))
  :config (add-hook 'eshell-mode-hook #'basis/init-eshell))

;; Proced
(use-package proced
  :defer t
  :init (global-set-key (kbd "C-x p") #'proced))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Applications

(use-package elfeed
  :ensure t
  :defer t
  :init (progn (setq elfeed-db-directory (basis/emacs-dir "var/elfeed/"))
               (let ((feeds (basis/emacs-file "feeds.eld")))
                 (when (file-exists-p feeds)
                   (setq elfeed-feeds (basis/elfeed-load-feeds feeds))))))

(use-package shr
  :defer t
  :config (advice-add 'shr-colorize-region :override #'ignore))

(use-package eww
  :defer t
  :config (define-key eww-mode-map (kbd "<backtab>") #'shr-previous-link))

(use-package w3m
  :ensure t
  :defer t
  :init (when (and (not (display-graphic-p))
                   (executable-find "w3m"))
          (setq browse-url-browser-function #'w3m-browse-url))
  :config (progn
            (define-key w3m-mode-map "n" #'w3m-next-anchor)
            (define-key w3m-mode-map "p" #'w3m-previous-anchor)))

(defun basis/init-sx-question-mode ()
  (toggle-truncate-lines -1))

(use-package sx
  :ensure t
  :defer t
  :init (setq sx-cache-directory (basis/emacs-dir "var/sx/"))
  :config (add-hook 'sx-question-mode-hook #'basis/init-sx-question-mode))

(use-package debbugs
  :ensure t
  :defer t
  :init (setq debbugs-gnu-persistency-file (basis/emacs-file "var/debbugs")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Email & news

(defun basis/init-message-mode ()
  (setq fill-column 72)
  (setq-local org-footnote-tag-for-non-org-mode-files nil))

(use-package message
  :defer t
  :init (setq user-mail-address "jbm@jbm.io"
              user-full-name "John Mastro"
              message-auto-save-directory (basis/emacs-dir "tmp/")
              message-subject-trailing-was-query nil
              message-signature "jbm"
              message-kill-buffer-on-exit t
              message-dont-reply-to-names nil
              message-send-mail-function #'smtpmail-send-it)
  :config (progn
            (define-key message-mode-map (kbd "C-c n") #'org-footnote-action)
            (add-hook 'message-mode-hook #'basis/init-message-mode)))

(use-package sendmail
  :defer t
  :init (setq send-mail-function #'smtpmail-send-it
              smtp-default-smtp-server "mail.messagingengine.com"))

(use-package smtpmail
  :defer t
  :init (setq smtpmail-smtp-server "mail.messagingengine.com"
              smtpmail-smtp-user "jbm@fastmail.fm"
              smtpmail-smtp-service 465
              smtpmail-stream-type 'ssl))

(use-package mu4e
  :defer t
  :init
  (progn
    (global-set-key (kbd "C-x m") #'mu4e)
    (let ((dir "/usr/local/share/emacs/site-lisp/mu4e/"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))
    ;; Mail
    (setq mu4e-get-mail-command "offlineimap"
          mu4e-maildir (expand-file-name ".maildir/fastmail" (getenv "HOME"))
          mu4e-sent-folder "/sent"
          mu4e-drafts-folder "/drafts"
          mu4e-trash-folder "/trash"
          mu4e-compose-signature "jbm")
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
    (setq mu4e-reply-to-address "jbm@jbm.io"
          mu4e-sent-messages-behavior 'delete)) ; They're saved on the server
  :config
  (add-to-list 'mu4e-view-actions
               (cons "View in browser" #'basis/mu4e-action-view-in-browser)
               t))

;; Retrieving passwords
(use-package auth-source
  :defer t
  :init (setq auth-sources (if (eq system-type 'darwin)
                               '(macos-keychain-internet)
                             '("~/.authinfo.gpg"))))

(use-package gnus
  :defer t
  :init (setq gnus-use-dribble-file nil
              gnus-always-read-dribble-file nil
              gnus-read-newsrc-file nil
              gnus-save-newsrc-file nil))


;;; init.el ends here
