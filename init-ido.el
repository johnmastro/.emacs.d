;; -----------------------------------------------------------------------------
;; init-ido.el
;; -----------------------------------------------------------------------------

(ido-mode 1)
(ido-everywhere t)
(ido-ubiquitous-mode t)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length 0
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-max-prospects 10
      ido-ignore-extensions t
      ido-save-directory-list-file "~/.emacs.d/.ido.last")

(defun basis/setup-ido ()
  (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-completion-map (kbd "M-w") 'ido-copy-current-file-name))

(add-hook 'ido-setup-hook 'basis/setup-ido)

;; smex ------------------------------------------------------------------------

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(provide 'init-ido)
