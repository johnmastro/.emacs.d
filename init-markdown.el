;; -----------------------------------------------------------------------------
;; init-markdown.el
;; -----------------------------------------------------------------------------

(mapc #'(lambda (ext)
          (add-to-list 'auto-mode-alist (cons ext 'markdown-mode)))
      (list "\\.markdown" "\\.mkd" "\\.md"))

(defun maybe-turn-on-auto-fill ()
  (unless (eq major-mode 'gfm-mode)
    (turn-on-auto-fill)))

(add-hook 'markdown-mode-hook 'maybe-turn-on-auto-fill)

(eval-after-load 'markdown
  '(basis/define-hyper markdown-mode-map "r" markdown-insert-reference-link-dwim))

(provide 'init-markdown)
