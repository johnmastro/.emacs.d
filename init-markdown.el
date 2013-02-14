;; -----------------------------------------------------------------------------
;; init-markdown.el
;; -----------------------------------------------------------------------------

(mapc #'(lambda (ext)
          (add-to-list 'auto-mode-alist (cons ext 'markdown-mode)))
      (list "\\.markdown" "\\.mkd" "\\.md"))

(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

(provide 'init-markdown)
