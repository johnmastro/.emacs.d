;; -----------------------------------------------------------------------------
;; init-slime.el
;; -----------------------------------------------------------------------------

(let ((quicklisp-slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (if (file-exists-p quicklisp-slime-helper)
      (load quicklisp-slime-helper)
    (message "%s" "SLIME is not installed. Use Quicklisp to install it.")))

(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

(defun basis/start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'basis/start-slime)

(defun basis/slime-repl-paredit-backspace ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(add-hook 'slime-repl-mode-hook 'basis/slime-repl-paredit-backspace)

(defun basis/slime-eval-something ()
  "Eval the active region, if any; otherwise eval the toplevel form."
  (interactive)
  (if (region-active-p)
      (slime-eval-region (region-beginning)
                         (region-end))
    (slime-eval-defun)))

(defun basis/slime-expand-defun (&optional repeatedly)
  "Display the macro expansion of the form surrounding point.
Use `slime-expand-1` to produce the expansion."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (slime-expand-1 repeatedly)))

(eval-after-load 'slime
  '(progn
     (setq slime-autodoc-use-multiline-p t)
     (global-set-key (kbd "<f9>") 'slime-selector)
     (define-key slime-mode-map
       (kbd "<tab>") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map
       (kbd "<f5>") 'slime-eval-last-expression)
     (define-key slime-mode-map
       (kbd "<M-f5>") 'slime-eval-last-expression-in-repl)
     (define-key slime-mode-map
       (kbd "<C-f5>") 'slime-pprint-eval-last-expression)
     (define-key slime-mode-map
       (kbd "<f6>") 'basis/slime-eval-something)
     (define-key slime-mode-map
       (kbd "<M-f6>") 'slime-compile-defun)
     (define-key slime-mode-map
       (kbd "<C-f6>") 'slime-pprint-region)
     (define-key slime-mode-map
       (kbd "<f7>") 'slime-expand-1)
     (define-key slime-mode-map
       (kbd "<f8>") 'slime-compile-and-load-file)))


(provide 'init-slime)
