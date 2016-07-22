;;; minimal.el     -*- coding: utf-8; no-byte-compile: t -*-

;; Something not quite `emacs -Q'

(let* ((modes '(tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
       (modes (if (eq system-type 'darwin)
                  modes
                (cons 'menu-bar-mode modes))))
  (dolist (mode modes)
    (when (fboundp mode)
      (funcall mode -1))))

(setq load-prefer-newer t)
(setq visible-bell t)
(setq inhibit-default-init t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq indicate-empty-lines t)
(setq delete-by-moving-to-trash t)
(setq enable-recursive-minibuffers t)

(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(setq-default truncate-lines t)

(fset 'display-startup-echo-area-message #'ignore)

(fset 'yes-or-no-p #'y-or-n-p)

(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)

(global-set-key (kbd "M-o") #'other-window)

(defun load-some-code ()
  "Load some essential libraries."
  (require 'subr-x)
  (require 'cl-lib)
  (require 'seq)
  (require 'map)
  (require 'pcase))

(defun load-solarized ()
  "Load the \"solarized\" theme."
  (package-initialize)
  (set-frame-parameter nil 'background-mode 'dark)
  (set-terminal-parameter nil 'background-mode 'dark)
  (setq solarized-termcolors 256)
  (setq solarized-italic nil)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized-moar/")
  (load-theme 'solarized t)
  (load-theme 'solarized-moar t))

