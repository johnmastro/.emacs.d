;; -----------------------------------------------------------------------------
;; init-ace-jump.el
;; -----------------------------------------------------------------------------

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(global-set-key (kbd "M-SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(provide 'init-ace-jump)
