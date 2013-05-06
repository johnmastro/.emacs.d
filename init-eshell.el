;; -----------------------------------------------------------------------------
;; init-eshell.el
;; -----------------------------------------------------------------------------

(defun basis/eshell-kill-line-backward ()
  "Kill the current line backward, respecting Eshell's prompt."
  (interactive)
  (kill-region (save-excursion (eshell-bol) (point))
               (point)))

(defun basis/eshell-kill-whole-line ()
  "Kill the current line, respecting Eshell's prompt."
  (interactive)
  (kill-region (save-excursion (eshell-bol) (point))
               (save-excursion (move-end-of-line 1) (point))))

(defun basis/init-eshell ()
  (basis/define-keys eshell-mode-map
    ((kbd "<C-backspace>") 'basis/eshell-kill-line-backward)
    ((kbd "<C-S-backspace>") 'basis/eshell-kill-whole-line)))

(add-hook 'eshell-mode-hook 'basis/init-eshell)

(provide 'init-eshell)












