;;; early-init.el -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-

(let ((modes '(menu-bar-mode
               tool-bar-mode
               scroll-bar-mode
               horizontal-scroll-bar-mode)))
  (dolist (mode (if (eq window-system 'ns) (cdr modes) modes))
    (when (fboundp mode)
      (funcall mode -1))))

(setq load-prefer-newer t)

(let ((dir (format "elpa/%d/" emacs-major-version)))
  (setq package-user-dir (expand-file-name dir "~/.emacs.d/")))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
