;; -----------------------------------------------------------------------------
;; init-lisp.el
;; -----------------------------------------------------------------------------

;; paredit ---------------------------------------------------------------------

(autoload 'enable-paredit-mode "paredit")

(defun basis/maybe-map-paredit-newline ()
  "Map `paredit-newline` except in some interactive modes."
  (unless (or (minibufferp) (memq major-mode '(inferior-emacs-lisp-mode
                                               inferior-lisp-mode
                                               inferior-scheme-mode)))
    (local-set-key (kbd "<return>") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'basis/maybe-map-paredit-newline)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-w") 'paredit-backward-kill-word)
     (define-key paredit-mode-map (kbd "M-e") 'paredit-forward)
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward)
     (define-key paredit-mode-map (kbd "M-a") 'paredit-backward)
     (define-key paredit-mode-map (kbd "<M-left>") 'paredit-backward)
     (define-key paredit-mode-map [remap kill-sentence] 'paredit-kill)
     (define-key paredit-mode-map
       [remap backward-kill-sentence]
       'backward-kill-sexp)))

(defvar basis/paredit-minibuffer-commands '(eval-expression
					    pp-eval-expression
					    eval-expression-with-eldoc
                                            slime-interactive-eval)
  "Interactive commands for which Paredit should be enabled in the minibuffer.")

(defun basis/maybe-enable-paredit-mode ()
  "Enable Paredit during lisp-related minibuffer commands."
  (if (memq this-command basis/paredit-minibuffer-commands)
      (enable-paredit-mode)))

(add-hook 'minibuffer-setup-hook 'basis/maybe-enable-paredit-mode)

;; hippie-expand ---------------------------------------------------------------

(defun set-up-hippie-expand-for-elisp ()
  "Enable Lisp symbol completion in Hippie Expand."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially t))

;; set up the lisp modes -------------------------------------------------------

(setq inferior-lisp-program "/usr/bin/sbcl")

(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

(defun basis/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (paredit-mode +1))

(defun basis/lisp-repl-setup ()
  "Enable features useful in interactive Lisp modes."
  (whitespace-mode -1))

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
      lisp-loop-forms-indentation 6
      lisp-indent-function #'common-lisp-indent-function)

;; emacs lisp ------------------------------------------------------------------

(defun basis/eval-something ()
  "Eval the active region, if any; otherwise eval the toplevel form."
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning)
		   (region-end))
    (eval-defun nil)))

(dolist (mode (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (define-key mode (kbd "<f5>") 'eval-last-sexp)
  (define-key mode (kbd "<f6>") 'basis/eval-something)
  (define-key mode (kbd "<f7>") 'macroexpand-point)
  (define-key mode (kbd "<f8>") 'eval-buffer))

;; more scheme setup -----------------------------------------------------------

(setq quack-default-program
      (if (eq system-type 'windows-nt)
          "larceny"
        "scheme"))

(require 'quack)

(autoload 'scheme-smart-complete "scheme-complete" nil t)
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)

(defun basis/enable-scheme-eldoc ()
  "Enable ElDoc in Scheme mode, via scheme-complete."
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
  (eldoc-mode))

(add-hook 'scheme-mode-hook 'basis/enable-scheme-eldoc)

(defun basis/scheme-send-something ()
  (interactive)
  (if (region-active-p)
      (scheme-send-region (region-beginning)
                          (region-end))
    (scheme-send-definition)))

(eval-after-load 'scheme
  '(progn
     (define-key scheme-mode-map (kbd "<tab>")
       'scheme-complete-or-indent)
     (define-key scheme-mode-map (kbd "<f5>")
       'scheme-send-last-sexp)
     (define-key scheme-mode-map (kbd "<f6>")
       'basis/scheme-send-something)
     (define-key scheme-mode-map (kbd "<M-f6>")
       'scheme-compile-definition-and-go)
     (define-key scheme-mode-map (kbd "<f8>")
       'scheme-compile-file)
     (define-key scheme-mode-map (kbd "<M-f8>")
       'scheme-load-file)))


(provide 'init-lisp)
