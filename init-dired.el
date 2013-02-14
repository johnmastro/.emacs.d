;; -----------------------------------------------------------------------------
;; init-dired.el
;; -----------------------------------------------------------------------------

(eval-after-load 'dired
  '(progn
     (require 'dired+)
     (setq dired-recursive-deletes 'top)
     (put 'dired-find-alternate-file 'disabled nil)))

(provide 'init-dired)
