;; -----------------------------------------------------------------------------
;; init-slime.el
;; -----------------------------------------------------------------------------

(let ((quicklisp-slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (if (file-exists-p quicklisp-slime-helper)
      (load quicklisp-slime-helper)
    (message "%s" "SLIME is not installed. Use Quicklisp to install it.")))

(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
        (ccl ("ccl"))))

(setq slime-default-lisp 'sbcl)

(defun basis/start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'basis/start-slime)

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
     (basis/define-keys slime-mode-map
       ((kbd "<f5>")   'slime-eval-last-expression)
       ((kbd "<M-f5>") 'slime-eval-last-expression-in-repl)
       ((kbd "<C-f5>") 'slime-pprint-eval-last-expression)
       ((kbd "<f6>")   'basis/slime-eval-something)
       ((kbd "<M-f6>") 'slime-compile-defun)
       ((kbd "<C-f6>") 'slime-pprint-region)
       ((kbd "<f7>")   'slime-expand-1)
       ((kbd "<f8>")   'slime-compile-and-load-file))
     (global-set-key (kbd "<f9>") 'slime-selector)
     (setq slime-autodoc-use-multiline-p t)))

(eval-after-load 'slime-repl-mode
  '(progn
     (define-key slime-repl-mode-map
       (read-kbd-macro paredit-backward-delete-key) nil)))

;; auto-complete setup ---------------------------------------------------------

(defun basis/set-up-slime-fuzzy-ac ()
  "Integrate SLIME's fuzzy completion with auto-complete."
  (interactive)
  (set-up-slime-ac t))

(add-hook 'slime-mode-hook 'basis/set-up-slime-fuzzy-ac)
(add-hook 'slime-repl-mode-hook 'basis/set-up-slime-fuzzy-ac)

(eval-after-load "auto-complete"
  `(add-to-list 'ac-modes 'slime-repl-mode))

;; redshank --------------------------------------------------------------------

(require 'redshank-loader)

(eval-after-load "redshank-loader"
  '(redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t))


(provide 'init-slime)
