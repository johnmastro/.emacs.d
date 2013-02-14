;; -----------------------------------------------------------------------------
;; init-completion.el
;; -----------------------------------------------------------------------------

;; hippie-expand ---------------------------------------------------------------

(global-set-key (kbd "M-/") 'hippie-expand)

(dolist (f '(try-expand-line try-expand-list try-expand-all-abbrevs))
  (delete f hippie-expand-try-functions-list))

;; smart tab -------------------------------------------------------------------

(require 'smart-tab)

(global-smart-tab-mode 1)

(setq smart-tab-using-hippie-expand t)

(setq smart-tab-disabled-major-modes
      '(lisp-mode slime-repl-mode scheme-mode org-mode term-mode))


(provide 'init-completion)
