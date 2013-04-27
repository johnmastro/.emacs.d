;; -----------------------------------------------------------------------------
;; init-smartparens.el
;; -----------------------------------------------------------------------------

(require 'smartparens-config)

;; Stick to Paredit for Lisps
(mapc #'(lambda (mode)
          (add-to-list 'sp-ignore-modes-list mode))
      sp--lisp-modes)

(smartparens-global-mode t)
(sp-use-paredit-bindings)

(provide 'init-smartparens)
