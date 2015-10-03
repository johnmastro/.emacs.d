;;; minimal.el     -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-

;; Something not quite `emacs -Q'

(let* ((modes '(tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
       (modes (if (eq system-type 'darwin)
                  modes
                (cons 'menu-bar-mode modes))))
  (dolist (mode modes)
    (when (fboundp mode)
      (funcall mode -1))))

(setq load-prefer-newer t
      visible-bell t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      indicate-empty-lines t
      delete-by-moving-to-trash t
      enable-recursive-minibuffers t)

(setq-default indent-tabs-mode nil
              fill-column 80
              truncate-lines t)

(fset 'display-startup-echo-area-message #'ignore)

(fset 'yes-or-no-p #'y-or-n-p)

(global-set-key (kbd "C-s") #'isearch-forward-regexp)
(global-set-key (kbd "C-r") #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)

(global-set-key (kbd "M-o") #'other-window)

(defun load-some-essentials ()
  "Load some essential libraries."
  (require 'subr-x)
  (require 'cl-lib)
  (require 'seq)
  (require 'map)
  (require 'pcase))
