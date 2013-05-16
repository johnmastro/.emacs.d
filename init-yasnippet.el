;; -----------------------------------------------------------------------------
;; init-yasnippet.el
;; -----------------------------------------------------------------------------

(defun basis/yas-expand-or-insert ()
  "Call `yas-expand` or `yas-insert-snippet` depending on context.
If point is after what might be a snippet key, call `yas-expand`,
otherwise call `yas-insert-snippet`."
  (interactive)
  (call-interactively
   (if (looking-at "\\>") #'yas-expand #'yas-insert-snippet)))

(eval-after-load 'yasnippet
  '(progn
     (basis/define-hyper global-map "<tab>" 'basis/yas-expand-or-insert)
     (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)))

(setq yas-snippet-dirs '("~/.emacs.d/snippets/")
      yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
      yas-wrap-around-region t
      ac-source-yasnippet nil)

(yas-global-mode 1)

(provide 'init-yasnippet)
