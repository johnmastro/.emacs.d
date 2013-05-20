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

(eval-after-load 'paredit
  '(diminish 'paredit-mode " π"))            ; pi
(eval-after-load 'auto-complete
  '(diminish 'auto-complete-mode " α"))      ; alpha
(eval-after-load 'yasnippet
  '(diminish 'yas-minor-mode " υ"))          ; upsilon
(eval-after-load 'redshank
  '(diminish 'redshank-mode " ρ"))           ; rho
(eval-after-load 'autopair
  '(diminish 'autopair-mode " φ"))           ; psi
(eval-after-load 'whitespace
  '(diminish 'whitespace-mode " ϝ"))         ; digamma
(eval-after-load 'elisp-slime-nav
  '(diminish 'elisp-slime-nav-mode " ε"))    ; epsilon
(eval-after-load 'eldoc
  '(diminish 'eldoc-mode " δ"))              ; delta
(eval-after-load 'undo-tree
  '(diminish 'undo-tree-mode " τ"))          ; tau

(provide 'init-interface)
