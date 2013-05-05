;; -----------------------------------------------------------------------------
;; init-markdown.el
;; -----------------------------------------------------------------------------

(mapc #'(lambda (ext)
          (add-to-list 'auto-mode-alist (cons ext 'markdown-mode)))
      (list "\\.markdown" "\\.mkd" "\\.md"))

(defun basis/init-markdown-mode ()
  (unless (eq major-mode 'gfm-mode)
    (turn-on-auto-fill)))

(add-hook 'markdown-mode-hook 'basis/init-markdown-mode)

(eval-after-load 'markdown-mode
  '(progn
     (define-key markdown-mode-map
       (kbd "C-c r") 'markdown-insert-reference-link-dwim)
     (define-key gfm-mode-map
       (kbd "C-c r") 'markdown-insert-reference-link-dwim)))

(provide 'init-markdown)
