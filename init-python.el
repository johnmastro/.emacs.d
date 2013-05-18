;; -----------------------------------------------------------------------------
;; init-python.el
;; -----------------------------------------------------------------------------

;; Interactive development mappings --------------------------------------------

(defun basis/python-send-something ()
  (interactive)
  (if (region-active-p)
      (python-shell-send-region (region-beginning)
                                (region-end))
    (python-shell-send-defun nil)))

(eval-after-load 'python
  '(progn
     (basis/define-keys python-mode-map
       ((kbd "<f6>")   'python-shell-send-something)
       ((kbd "<f8>")   'python-shell-send-buffer)
       ((kbd "<M-f8>") 'python-shell-send-file))))

;; Jedi ------------------------------------------------------------------------

(setq jedi:setup-keys t
      jedi:tooltip-method nil) ; show function signatures in the minibuffer

(add-hook 'python-mode-hook 'jedi:setup)

;; Autopair --------------------------------------------------------------------

(defun basis/setup-autopair-for-python ()
  (setq autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action)))

(add-hook 'python-mode-hook 'basis/setup-autopair-for-python)


(provide 'init-python)
