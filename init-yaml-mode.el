;; -----------------------------------------------------------------------------
;; init-yaml-mode.el
;; -----------------------------------------------------------------------------

(defun basis/yaml-map-newline-and-indent ()
  (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent))

(add-hook 'yaml-mode-hook 'basis/yaml-map-newline-and-indent)

(provide 'init-yaml-mode)
