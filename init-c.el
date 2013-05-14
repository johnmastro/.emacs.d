;; -----------------------------------------------------------------------------
;; init-c.el
;; -----------------------------------------------------------------------------

(defun basis/init-c ()
  (c-set-style "python")
  (setq indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-newline 1))

(add-hook 'c-mode-hook 'basis/init-c)

(provide 'init-c)
