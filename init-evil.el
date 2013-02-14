(require 'evil)
(evil-mode 1)

;; (setcdr evil-insert-state-map nil)
;; (define-key evil-insert-state-map
;;   (read-kbd-macro evil-toggle-key) 'evil-normal-state)

(define-key evil-normal-state-map ";" 'evil-ex)

(define-key evil-normal-state-map "L" 'evil-end-of-line)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)

(define-key evil-normal-state-map ",f" 'ido-find-file)
(define-key evil-normal-state-map ",b" 'ido-switch-buffer)
(define-key evil-normal-state-map ",x" 'smex)
(define-key evil-normal-state-map ",X" 'smex-major-mode-commands)

(dolist (mode-map (list lisp-interaction-mode-map emacs-lisp-mode-map))
  (evil-define-key 'normal mode-map
    (read-kbd-macro "\\b") 'eval-buffer
    (read-kbd-macro "\\d") 'eval-defun
    (read-kbd-macro "\\e") 'eval-last-sexp
    (read-kbd-macro "\\r") 'eval-region))

(evil-define-key 'normal lisp-mode-map
  (read-kbd-macro "\\c") 'run-lisp
  (read-kbd-macro "\\d") 'lisp-eval-defun
  (read-kbd-macro "\\e") 'lisp-eval-last-sexp
  (read-kbd-macro "\\r") 'lisp-eval-region)

(evil-define-key 'normal slime-mode-map
  ;; eval
  (read-kbd-macro "\\d") 'slime-eval-defun
  (read-kbd-macro "\\e") 'slime-eval-last-expression
  (read-kbd-macro "\\r") 'slime-eval-region
  ;; compile
  (read-kbd-macro "\\D") 'slime-compile-defun
  (read-kbd-macro "\\L") 'slime-compile-and-load-file
  (read-kbd-macro "\\F") 'slime-compile-file
  ;; un-define
  (read-kbd-macro "\\u") 'slime-undefine-function
  ;; macro expansion
  (read-kbd-macro "\\1") 'slime-expand-1
  (read-kbd-macro "\\m") 'slime-macroexpand-all
  ;; documentation
  (read-kbd-macro "\\s") 'slime-describe-symbol
  (read-kbd-macro "\\A") 'slime-apropos
  (read-kbd-macro "\\h") 'slime
  ;; other
  (read-kbd-macro "\\C") 'slime-call-defun)

(evil-define-key 'normal slime-popup-buffer-mode-map
  (kbd "q") 'slime-popup-buffer-quit-function)

(evil-define-key 'insert slime-mode-map
  (kbd "TAB") 'slime-indent-and-complete-symbol)

(evil-define-key 'normal scheme-mode-map
  (read-kbd-macro "\\c") 'run-scheme
  (read-kbd-macro "\\d") 'scheme-send-definition
  (read-kbd-macro "\\e") 'scheme-send-last-sexp
  (read-kbd-macro "\\r") 'scheme-send-region)

(dolist (mode '(slime-repl-mode
                inferior-lisp-mode
                inferior-scheme-mode
                inferior-emacs-lisp-mode
                comint-mode
                shell-mode
                eshell-mode
                term-mode))
  (evil-set-initial-state mode 'emacs))


(provide 'init-evil)
