;; -----------------------------------------------------------------------------
;; init-dired.el
;; -----------------------------------------------------------------------------

(eval-after-load 'dired
  '(progn
     (require 'dired+)
     (define-key dired-mode-map (kbd "M-o") 'other-window)
     (define-key dired-mode-map (kbd "C-c o") 'dired-omit-mode)
     (setq dired-recursive-deletes 'top)
     (put 'dired-find-alternate-file 'disabled nil)))

(provide 'init-dired)
