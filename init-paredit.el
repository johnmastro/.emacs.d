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

(defun basis/paredit-doublequote-space-p (endp delimiter)
  "Don't insert an extraneous space when entering a CL pathname."
  ;; If any of `paredit-space-for-delimiter-predicates` returns nil
  ;; a space isn't inserted.
  (let ((pathname-opening-p
         (and (not endp)
              (eql delimiter ?\")
              (memq major-mode '(lisp-mode common-lisp-mode))
              (save-excursion
                (backward-char 2)
                (looking-at "#p")))))
    (not pathname-opening-p)))

(defun paredit-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

(eval-after-load 'paredit
  '(progn
     (basis/define-keys paredit-mode-map
       ((kbd "[")                      'paredit-open-round)
       ((kbd "M-[")                    'paredit-open-square)
       ((kbd "M-e")                    'paredit-forward)
       ((kbd "<M-right>")              'paredit-forward)
       ((kbd "M-a")                    'paredit-backward)
       ((kbd "<M-left>")               'paredit-backward)
       ([remap kill-sentence]          'paredit-kill)
       ((kbd "C-w")                    'paredit-kill-region-or-backward-word)
       ((kbd "M-<backspace>")          'paredit-kill-region-or-backward-word)
       ([remap backward-kill-sentence] 'backward-kill-sexp))
     (add-to-list 'paredit-space-for-delimiter-predicates
             'basis/paredit-doublequote-space-p)))

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
