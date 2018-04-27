;;; early-init.el -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-

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

(setq load-prefer-newer t)

(let ((dir (format "elpa/%d/" emacs-major-version)))
  (setq package-user-dir (expand-file-name dir basis/emacs-dir)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
