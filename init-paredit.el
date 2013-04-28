;; -----------------------------------------------------------------------------
;; init-paredit.el
;; -----------------------------------------------------------------------------

(autoload 'enable-paredit-mode "paredit")

(defun basis/maybe-map-paredit-newline ()
  "Map `paredit-newline` except in some interactive modes."
  (unless (or (minibufferp) (memq major-mode '(inferior-emacs-lisp-mode
                                               inferior-lisp-mode
                                               inferior-scheme-mode)))
    (local-set-key (kbd "<return>") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'basis/maybe-map-paredit-newline)

(defun paredit-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "[") 'paredit-open-round)
     (define-key paredit-mode-map (kbd "M-[") 'paredit-open-square)
     (define-key paredit-mode-map (kbd "M-e") 'paredit-forward)
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward)
     (define-key paredit-mode-map (kbd "M-a") 'paredit-backward)
     (define-key paredit-mode-map (kbd "<M-left>") 'paredit-backward)
     (define-key paredit-mode-map [remap kill-sentence] 'paredit-kill)
     (define-key paredit-mode-map
       (kbd "C-w")
       'paredit-kill-region-or-backward-word)
     (define-key paredit-mode-map
       (kbd "M-<backspace>")
       'paredit-kill-region-or-backward-word)
     (define-key paredit-mode-map
       [remap backward-kill-sentence]
       'backward-kill-sexp)))

(defun basis/maybe-enable-paredit-mode ()
  "Enable Paredit during Lisp-related minibuffer commands."
  (let ((paredit-minibuffer-commands '(eval-expression
                                       pp-eval-expression
                                       eval-expression-with-eldoc
                                       slime-interactive-eval)))
    (when (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode))))

(add-hook 'minibuffer-setup-hook 'basis/maybe-enable-paredit-mode)

(provide 'init-paredit)
