;; -----------------------------------------------------------------------------
;; init-python.el
;; -----------------------------------------------------------------------------

(defun basis/python-send-something ()
  (interactive)
  (if (region-active-p)
      (python-shell-send-region (region-beginning)
                                (region-end))
    (python-shell-send-defun nil)))

(eval-after-load 'python-mode
  '(progn
     (basis/define-keys
       ((kbd "<f6>") 'python-shell-send-something)
       ((kbd "<f8>") 'python-shell-send-buffer)
       ((kbd "<M-f8>") 'python-shell-send-file))))

(add-hook 'python-mode-hook #'(lambda () (smartparens-mode -1)))

(provide 'init-python)
