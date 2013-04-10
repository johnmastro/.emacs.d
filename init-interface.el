;; -----------------------------------------------------------------------------
;; init-interface.el
;; -----------------------------------------------------------------------------

(add-to-list 'custom-theme-load-path
             (concat basis/themes-dir "solarized/"))

(setq solarized-termcolors 256)
(load-theme 'solarized-dark t)

(defvar basis/default-font
  (case system-type
    (gnu/linux  "Inconsolata-11")
    (darwin     "Andale Mono-12")
    (windows-nt "Consolas-10")))

(when (and (display-graphic-p) basis/default-font)
  (set-face-attribute 'default nil :font basis/default-font))

(eval-after-load 'elisp-slime-nav
  '(diminish 'elisp-slime-nav-mode))
(eval-after-load 'eldoc
  '(diminish 'eldoc-mode))
(eval-after-load 'smart-tab
  '(diminish 'smart-tab-mode))
(eval-after-load 'undo-tree
  '(diminish 'undo-tree-mode))

(provide 'init-interface)
