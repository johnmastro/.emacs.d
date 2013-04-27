;; -----------------------------------------------------------------------------
;; init-hippie-expand.el
;; -----------------------------------------------------------------------------

(global-set-key (kbd "M-/") 'hippie-expand)

(dolist (f '(try-expand-line try-expand-list try-expand-all-abbrevs))
  (setq hippie-expand-try-functions-list
        (delete f hippie-expand-try-functions-list)))

(provide 'init-hippie-expand)
