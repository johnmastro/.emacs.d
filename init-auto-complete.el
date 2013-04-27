;; -----------------------------------------------------------------------------
;; init-auto-complete.el
;; -----------------------------------------------------------------------------

(require 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode t)  ;; SLIME integration is in init-slime.el
(ac-config-default)

(setq ac-comphist-file "~/.emacs.d/.ac-comphist.dat")

(setq-default ac-sources
              '(ac-source-imenu
                ac-source-dictionary
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers))

(provide 'init-auto-complete)
