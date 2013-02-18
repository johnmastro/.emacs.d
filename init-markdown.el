;; -----------------------------------------------------------------------------
;; init-markdown.el
;; -----------------------------------------------------------------------------

(mapc #'(lambda (ext)
          (add-to-list 'auto-mode-alist (cons ext 'markdown-mode)))
      (list "\\.markdown" "\\.mkd" "\\.md"))

(defun basis/init-markdown-mode ()
  (unless (eq major-mode 'gfm-mode)
    (turn-on-auto-fill))
  (define-key markdown-mode-map
      (kbd "C-c r") 'markdown-insert-reference-link-dwim))

(defun basis/init-gfm-mode ()
  (define-key gfm-mode-map
      (kbd "C-c r")'markdown-insert-reference-link-dwim))

(add-hook 'markdown-mode-hook 'basis/init-markdown-mode)
(add-hook 'gfm-mode-hook 'basis/init-gfm-mode)

(provide 'init-markdown)
