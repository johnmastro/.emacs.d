;; -----------------------------------------------------------------------------
;; init-settings.el
;; -----------------------------------------------------------------------------

(setq visible-bell t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      confirm-nonexistent-file-or-buffer nil
      sentence-end-double-space nil
      indicate-empty-lines t
      shift-select-mode nil
      mouse-yank-at-point t
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      truncate-lines t
      ediff-window-setup-function 'ediff-setup-windows-plain
      line-number-mode t
      column-number-mode t
      global-font-lock-mode 1
      imenu-auto-rescan t
      custom-file basis/custom-file)

(when (file-exists-p custom-file)
  (load custom-file))

;; Tabs
(setq-default indent-tabs-mode nil
              fill-column 79)

(setq tab-stop-list
      (loop for n from 4 to 120 by 4
            collect n))

;; Whitespace mode in programming modes except REPL/shell-style modes
(defun maybe-turn-on-whitespace-mode ()
  (interactive)
  (unless (or (derived-mode-p 'comint-mode)
              (eq major-mode 'eshell-mode))
    (whitespace-mode 1)))

(add-hook 'prog-mode-hook 'maybe-turn-on-whitespace-mode)

;; Enable some miscellaneous helpful modes
(size-indication-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(cua-selection-mode 1)
(auto-compression-mode 1)
(global-undo-tree-mode 1)

;; TRAMP
(setq tramp-default-method
      (if (eq system-type 'windows-nt)
          "plinkx"
        "sshx"))

;; ispell
(setq ispell-program-name
      (if (eq system-type 'windows-nt)
          "c:\\Program Files (x86)\\Aspell\\bin\\aspell.exe"
        "aspell"))
(setq ispell-personal-dictionary "~/.aspell.en.pws")

;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(setq recentf-save-file (concat basis/emacs-dir ".recentf"))

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; UTF-8 everywhere
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)

;; Backups, autosaves, and temporary files
(setq backup-by-copying t
      backup-directory-alist `((".*" . ,basis/backups-dir))
      auto-save-file-name-transforms `((".*" ,basis/autosaves-dir t))
      save-place-file (concat basis/emacs-dir "places")
      temporary-file-directory basis/tmp-dir)

;; Automatically refresh buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Don't prompt when killing buffers with live processes attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; No blinking please
(blink-cursor-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'list-buffers 'ibuffer)


(provide 'init-settings)
