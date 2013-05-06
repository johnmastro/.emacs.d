;; -----------------------------------------------------------------------------
;; init-lisp.el
;; -----------------------------------------------------------------------------

(setq inferior-lisp-program "/usr/bin/sbcl")

(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

(defun basis/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (paredit-mode +1))

(defun basis/lisp-repl-setup ()
  "Enable features useful in interactive Lisp modes."
  (whitespace-mode -1))

(defun set-up-hippie-expand-for-elisp ()
  "Enable Lisp symbol completion in Hippie Expand."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially t))

(defun basis/emacs-lisp-setup ()
  "Enable features useful when working with Emacs Lisp."
  (paredit-mode +1)
  (elisp-slime-nav-mode t)
  (set-up-hippie-expand-for-elisp)
  (turn-on-eldoc-mode))

(let* ((elispy-hooks '(emacs-lisp-mode-hook
                       lisp-interaction-mode-hook
                       ielm-mode-hook))
       (lispy-hooks (append elispy-hooks
                            '(lisp-mode-hook
                              slime-repl-mode-hook
                              inferior-lisp-mode-hook
                              scheme-mode-hook
                              inferior-scheme-mode-hook)))
       (lispy-repl-hooks '(ielm-mode-hook
                           inferior-lisp-mode-hook
                           inferior-scheme-mode-hook)))
  (dolist (hook lispy-hooks)
    (add-hook hook 'basis/lisp-setup))
  (dolist (hook elispy-hooks)
    (add-hook hook 'basis/emacs-lisp-setup))
  (dolist (hook lispy-repl-hooks)
    (add-hook hook 'basis/lisp-repl-setup)))

(setq lisp-lambda-list-keyword-alignment t
      lisp-lambda-list-keyword-parameter-alignment t
      lisp-loop-forms-indentation 6)

;; emacs lisp ------------------------------------------------------------------

(defun basis/eval-something ()
  "Eval the active region, if any; otherwise eval the toplevel form."
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning)
                   (region-end))
    (eval-defun nil)))

(dolist (mode (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (basis/define-keys mode
    ((kbd "<f5>") 'pp-eval-last-sexp)
    ((kbd "<f6>") 'basis/eval-something)
    ((kbd "<f7>") 'macroexpand-point)
    ((kbd "<f8>") 'eval-buffer)))


(provide 'init-lisp)
