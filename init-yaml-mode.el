;; -----------------------------------------------------------------------------
;; init-yaml-mode.el
;; -----------------------------------------------------------------------------

(eval-after-load 'yaml-mode
  '(progn
     (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent)))

(provide 'init-yaml-mode)
