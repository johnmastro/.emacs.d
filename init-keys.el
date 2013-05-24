;; -----------------------------------------------------------------------------
;; init-keys.el
;; -----------------------------------------------------------------------------

(defmacro basis/define-hyper (keymap key def)
  "Define a Hyper- modified key binding.
On OS X, instead define a binding with <kp-enter> as prefix."
  `(define-key ,keymap
     (kbd ,(if (eq system-type 'darwin)
               (concat "<kp-enter> " key)
             (concat "H-" key)))
     ,def))

(put 'basis/define-hyper 'lisp-indent-function 'defun)

(defmacro basis/define-keys (keymap &rest keydefs)
  "Define multiple key bindings for KEYMAP."
  `(progn
     ,@(mapcar #'(lambda (keydef)
                   (let ((key (car keydef))
                         (def (cadr keydef)))
                     `(define-key ,keymap ,key ,def)))
               keydefs)))

(put 'basis/define-keys 'lisp-indent-function 'defun)

(defmacro create-simple-keybinding-command (name key)
  ;; Based on code from
  ;; github.com/magnars/.emacs.d/blob/master/defuns/misc-defuns.el
  `(defun ,name (def &optional keymap)
     (define-key (or keymap global-map) (read-kbd-macro ,key) def)))

(create-simple-keybinding-command f2 "<f2>")
(create-simple-keybinding-command f3 "<f3>")
(create-simple-keybinding-command f4 "<f4>")
(create-simple-keybinding-command f5 "<f5>")
(create-simple-keybinding-command f6 "<f6>")
(create-simple-keybinding-command f7 "<f7>")
(create-simple-keybinding-command f8 "<f8>")
(create-simple-keybinding-command f9 "<f9>")
(create-simple-keybinding-command f10 "<f10>")
(create-simple-keybinding-command f11 "<f11>")
(create-simple-keybinding-command f12 "<f12>")

(defun init-modifiers/linux ()
  (define-key key-translation-map
    (kbd "<menu>")
    'event-apply-hyper-modifier))

(defun init-modifiers/os-x ()
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)
  ;; Use <kp-enter>, conveniently located to the right of the space
  ;; bar on my MBP, as a stand-in for mapping the <menu>/<apps> key on
  ;; PC keyboards to hyper.
  (define-prefix-command 'quasi-hyper)
  (global-set-key (kbd "<kp-enter>") 'quasi-hyper))

(defun init-modifiers/windows ()
  (setq w32-pass-apps-to-system nil
        w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)
  (define-key key-translation-map (kbd "<apps>") 'event-apply-hyper-modifier))

(case system-type
  (gnu/linux  (init-modifiers/linux))
  (darwin     (init-modifiers/os-x))
  (windows-nt (init-modifiers/windows)))

;; Hyper- mappings
(basis/define-hyper global-map "f" 'ido-find-file)
(basis/define-hyper global-map "b" 'ido-switch-buffer)
(basis/define-hyper global-map "d" 'basis/ido-dir-selector)
(basis/define-hyper global-map "D" 'basis/dired-dir-selector)
(basis/define-hyper global-map "s" 'save-buffer)
(basis/define-hyper global-map "i" 'imenu)
(basis/define-hyper global-map "0" 'delete-window)
(basis/define-hyper global-map "1" 'delete-other-windows)
(basis/define-hyper global-map "2" 'split-window-below)
(basis/define-hyper global-map "3" 'split-window-right)
(basis/define-hyper global-map "g" 'basis/ido-tramp-selector)
(basis/define-hyper global-map "r" ctl-x-r-map)

;; Easier window management
(winner-mode 1)
(windmove-default-keybindings)
(global-set-key (kbd "M-o") 'other-window)

;; Less tab
(global-set-key (kbd "C-m") 'newline-and-indent)

;; Clever C-a
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

;; Movement by paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Kill stuff
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "<M-backspace>") 'kill-region-or-backward-word)
(global-set-key (kbd "<C-backspace>") 'kill-line-backward)
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line) ; C-S-backspace
(global-set-key (kbd "<s-backspace>") 'smart-kill-whole-line)
(global-set-key (kbd "<S-backspace>") 'smart-kill-almost-whole-line)
(global-set-key (kbd "ESC <M-backspace>") 'backward-kill-sexp)

;; ... and then browse it with M-y
(browse-kill-ring-default-keybindings)

;; Join lines
(global-set-key (kbd "C-c j") 'join-line)

;; Moves lines or regions
(global-set-key (kbd "<M-s-up>") 'move-text-up)
(global-set-key (kbd "<M-s-down>") 'move-text-down)

;; Transpose stuff with M-t
;; from github.com/magnars/.emacs.d/
(global-unset-key (kbd "M-t"))
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t M-w") 'transpose-windows)

;; Move between errors
(global-set-key (kbd "s-.") 'next-error)
(global-set-key (kbd "s-,") 'previous-error)

(global-set-key (kbd "s-i") 'imenu)

;; Occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)

;; Expand-region
(global-set-key (kbd "C-:") 'er/expand-region)
(global-set-key (kbd "s-'") 'er/expand-region)

;; Multiple cursors
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "s-]") 'mc/mark-next-like-this)
(global-set-key (kbd "s-[") 'mc/mark-previous-like-this)
(global-set-key (kbd "s-\\") 'mc/mark-more-like-this-extended)

;; Comment/uncomment stuff
(global-set-key (kbd "s-;") 'basis/comment-or-uncomment)
(global-set-key (kbd "C-c ;") 'basis/comment-or-uncomment)

;; Eval
(global-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)
(global-set-key (kbd "C-c C-e") 'basis/eval-and-replace)
(global-set-key (kbd "<s-return>") 'eval-expression)

;; I use Meta-space for ace-jump-mode
(global-set-key (kbd "C-c SPC") 'just-one-space)

;; jump-char
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;; goto-line, with line numbers
(global-set-key (kbd "M-g g") 'basis/goto-line-with-numbers)
(global-set-key (kbd "M-g M-g") 'basis/goto-line-with-numbers)

;; Movement by sexp
(global-set-key (kbd "<M-right>") 'forward-sexp)
(global-set-key (kbd "<M-left>") 'backward-sexp)

;; Start eshell or switch to it if it's active
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; M-x without using meta
(global-set-key (kbd "C-c m") 'smex)
(global-set-key (kbd "C-c x") 'execute-extended-command)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including, the ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Change word case
(global-set-key (kbd "M-u") 'basis/upcase-something)
(global-set-key (kbd "M-l") 'basis/downcase-something)
(global-set-key (kbd "M-c") 'basis/capitalize-something)

;; Kill frames with C-x C-c
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Google stuff
(global-set-key (kbd "C-c g") 'basis/google)

;; Proced
(global-set-key (kbd "C-x p") 'proced)

;; Help map --------------------------------------------------------------------

;; from github.com/jwiegley/dot-emacs

(define-prefix-command 'lisp-find-map)

(global-set-key (kbd "C-h e") 'lisp-find-map)

(defun scratch! ()
  "Switch to the scratch buffer, creating it if necessary."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*")))

(global-set-key (kbd "C-h e c") 'finder-commentary)
(global-set-key (kbd "C-h e e") 'view-echo-area-messages)
(global-set-key (kbd "C-h e f") 'find-function)
(global-set-key (kbd "C-h e F") 'find-face-definition)
(global-set-key (kbd "C-h e i") 'info-apropos)
(global-set-key (kbd "C-h e k") 'find-function-on-key)
(global-set-key (kbd "C-h e l") 'find-library)
(global-set-key (kbd "C-h e s") 'scratch!)
(global-set-key (kbd "C-h e v") 'find-variable)
(global-set-key (kbd "C-h e V") 'apropos-value)
(global-set-key (kbd "C-h e a") 'helm-apropos)

;; Aliases ---------------------------------------------------------------------

(defalias 'ls 'ibuffer)
(defalias 'qrr 'query-replace-regexp)

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(defalias 'sayonara 'save-buffers-kill-terminal)


(provide 'init-keys)
