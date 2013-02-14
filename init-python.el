;; -----------------------------------------------------------------------------
;; init-python.el
;; -----------------------------------------------------------------------------

(defun basis/python-setup ()
  (define-key python-mode-map "\C-m" 'newline-and-indent))

(add-hook 'python-mode-hook 'basis/python-setup)

(provide 'init-python)
