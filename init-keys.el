;; -----------------------------------------------------------------------------
;; init-keys.el
;; -----------------------------------------------------------------------------

(defmacro basis/define-hyper (keymap key def)
  "Define a Hyper- modified key binding.
On OS X, instead define a binding with <kp-enter> as prefix."
  (declare (indent defun))
  `(define-key ,keymap
     (kbd ,(if (eq system-type 'darwin)
               (concat "<kp-enter> " key)
             (concat "H-" key)))
     ,def))

(defun init-modifiers/linux ()
  (define-key key-translation-map
    (kbd "<menu>")
    'event-apply-hyper-modifier))

(defun init-modifiers/os-x ()
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)
  ;; Use <kp-enter>, conveniently located to the right of the space
  ;; bar on my MBP, as a stand-in for mapping the <menu>/<apps> key on
  ;; PC keyboards to hyper.
  (define-prefix-command 'fake-hyper)
  (global-set-key (kbd "<kp-enter>") 'fake-hyper))

(defun init-modifiers/windows ()
  (setq w32-pass-apps-to-system nil
        w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)
  (define-key key-translation-map
    (kbd "<apps>")
    'event-apply-hyper-modifier))

(case system-type
  (gnu/linux  (init-modifiers/linux))
  (darwin     (init-modifiers/os-x))
  (windows-nt (init-modifiers/windows)))

;; Hyper- mappings
(basis/define-hyper global-map "f" 'ido-find-file)
(basis/define-hyper global-map "b" 'ido-switch-buffer)
(basis/define-hyper global-map "d" 'basis/ido-dir-selector)
(basis/define-hyper global-map "D" 'basis/dired-dir-selector)
(basis/define-hyper global-map "s" 'save-buffer)
(basis/define-hyper global-map "i" 'imenu)
(basis/define-hyper global-map "0" 'delete-window)
(basis/define-hyper global-map "1" 'delete-other-windows)
(basis/define-hyper global-map "2" 'split-window-below)
(basis/define-hyper global-map "3" 'split-window-right)

;; Easier window management
(winner-mode 1)
(windmove-default-keybindings)
(global-set-key (kbd "M-o") 'other-window)

;; Movement by sexp
(global-set-key (kbd "<M-right>") 'forward-sexp)
(global-set-key (kbd "<M-left>") 'backward-sexp)

;; Easier mapping for backward-kill-word
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; Start eshell or switch to it if it's active
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; M-x without using meta
(global-set-key (kbd "C-c m") 'smex)
(global-set-key (kbd "C-c x") 'execute-extended-command)

;; Expand-region
(global-set-key (kbd "M-+") 'er/expand-region)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including, the ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Kill frames with C-x C-c
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Aliases ---------------------------------------------------------------------

(defalias 'ls 'ibuffer)
(defalias 'qrr 'query-replace-regexp)

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(defalias 'sayonara 'save-buffers-kill-terminal)


(provide 'init-keys)
