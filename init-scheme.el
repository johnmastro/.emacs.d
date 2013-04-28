;; -----------------------------------------------------------------------------
;; init-scheme.el
;; -----------------------------------------------------------------------------

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

(provide 'init-scheme)
