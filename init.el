;;; init.el        -*- coding: utf-8; -*-

(require 'cl)

;; Disable superfluous UI immediately to prevent momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Define directories for various purposes and ensure they exist
(defvar basis/emacs-dir (file-name-directory load-file-name))
(defvar basis/site-lisp-dir (concat basis/emacs-dir "site-lisp/"))
(defvar basis/defuns-file (concat basis/emacs-dir "defuns.el"))
(defvar basis/backups-dir (concat basis/emacs-dir "backups/"))
(defvar basis/autosaves-dir (concat basis/emacs-dir "autosaves/"))
(defvar basis/tmp-dir (concat basis/emacs-dir "tmp/"))
(defvar basis/custom-file (concat basis/emacs-dir "custom.el"))
(defvar basis/themes-dir (concat basis/emacs-dir "themes/"))

(dolist (dir (list basis/backups-dir basis/autosaves-dir basis/tmp-dir))
  (unless (file-exists-p dir)
    (make-directory dir)))

(when (eq system-type 'windows-nt)
  (ignore-errors
    (cd "c:\\Local")))

;; Emacs doesn't seem to respect $PATH on OS X
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin/")
  (add-to-list 'exec-path "~/bin/"))

;; Set up the load path
(add-to-list 'load-path basis/emacs-dir)
(add-to-list 'load-path basis/site-lisp-dir)
(add-to-list 'load-path (concat basis/site-lisp-dir "tramp-2.2.7/"))

;; Info is awesome
(when (file-exists-p "~/.emacs.d/doc/info")
  (add-to-list 'Info-default-directory-list "~/.emacs.d/doc/info"))

;; package ---------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; Make sure every archive is present in the elpa/archives/ folder
(let* ((archive-folder "~/.emacs.d/elpa/archives/")
       (archive-folders (mapcar #'(lambda (archive)
                                    (concat archive-folder (car archive)))
                                package-archives)))
  (unless (every #'file-exists-p archive-folders)
    (package-refresh-contents)))

;; Ensure that everything specified here is installed
(let* ((basis/required-packages
        '(paredit
          elisp-slime-nav
          expand-region
          smex
          dired+
          diminish
          ido-ubiquitous
          undo-tree
          ack-and-a-half
          markdown-mode
          deft
          ace-jump-mode
          jump-char
          magit
          multiple-cursors
          helm
          auto-complete
          ac-slime
          redshank
          yaml-mode
          dash
          s
          move-text
          browse-kill-ring
          jedi
          autopair
          yasnippet
          tagedit
          simplezen
          js2-mode
          ac-js2
          js2-refactor
          js-comint
          skewer-mode
          flycheck
          ))
       (basis/uninstalled-packages
        (remove-if #'package-installed-p basis/required-packages)))
  (when basis/uninstalled-packages
    (package-refresh-contents)
    (mapc #'package-install basis/uninstalled-packages)))

;; load defuns -----------------------------------------------------------------

;; Make dash and s available for use in defuns
(require 'dash)
(require 's)

;; Load custom functions
(when (file-exists-p basis/defuns-file)
  (load basis/defuns-file))

;; various settings ------------------------------------------------------------

(setq visible-bell t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      confirm-nonexistent-file-or-buffer nil
      sentence-end-double-space nil
      indicate-empty-lines t
      shift-select-mode nil
      mouse-yank-at-point t
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      truncate-lines t
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      line-number-mode t
      column-number-mode t
      global-font-lock-mode 1
      imenu-auto-rescan t
      custom-file basis/custom-file)

(when (file-exists-p custom-file)
  (load custom-file))

;; Tabs
(setq-default indent-tabs-mode nil
              fill-column 79)

(setq tab-stop-list
      (loop for n from 4 to 120 by 4
            collect n))

;; Whitespace mode in programming modes except REPL/shell-style modes
(defun maybe-turn-on-whitespace-mode ()
  (interactive)
  (unless (or (derived-mode-p 'comint-mode)
              (eq major-mode 'eshell-mode))
    (whitespace-mode 1)))

(add-hook 'prog-mode-hook 'maybe-turn-on-whitespace-mode)

;; Enable some miscellaneous helpful modes
(size-indication-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(cua-selection-mode 1)
(auto-compression-mode 1)
(global-undo-tree-mode 1)
(delete-selection-mode 1)

;; TRAMP
(setq tramp-default-method
      (if (eq system-type 'windows-nt)
          "plinkx"
        "sshx"))

;; ispell
(setq ispell-program-name
      (if (eq system-type 'windows-nt)
          "c:\\Program Files (x86)\\Aspell\\bin\\aspell.exe"
        "aspell"))
(setq ispell-personal-dictionary "~/.aspell.en.pws")

;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(setq recentf-save-file (concat basis/emacs-dir ".recentf"))

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Backups, autosaves, and temporary files
(setq backup-by-copying t
      backup-directory-alist `((".*" . ,basis/backups-dir))
      auto-save-file-name-transforms `((".*" ,basis/autosaves-dir t))
      save-place-file (concat basis/emacs-dir "places")
      temporary-file-directory basis/tmp-dir)

;; Automatically refresh buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Don't prompt when killing buffers with live processes attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; No blinking please
(blink-cursor-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; interface -------------------------------------------------------------------

(add-to-list 'custom-theme-load-path
             (concat basis/themes-dir "solarized/"))

(setq solarized-termcolors 256)
(load-theme 'solarized-dark t)

(when (display-graphic-p)
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
  (-when-let (default-font
               (case system-type
                 (gnu/linux  "Inconsolata-11")
                 (darwin     "Andale Mono-12")
                 (windows-nt "Consolas-10")))
    (set-face-attribute 'default nil :font default-font)))

(after-load 'paredit
  (diminish 'paredit-mode " π"))            ; pi
(after-load 'auto-complete
  (diminish 'auto-complete-mode " α"))      ; alpha
(after-load 'yasnippet
  (diminish 'yas-minor-mode " υ"))          ; upsilon
(after-load 'autopair
  (diminish 'autopair-mode " φ"))           ; psi
(after-load 'tagedit
  (diminish 'tagedit-mode " τ"))            ; tau
(after-load 'skewer-mode
  (diminish 'skewer-mode " σ"))             ; sigma
(after-load 'skewer-html
  (diminish 'skewer-html-mode " σ/html"))
(after-load 'skewer-css
  (diminish 'skewer-css-mode " σ/css"))

(after-load 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))
(after-load 'eldoc
  (diminish 'eldoc-mode))
(after-load 'undo-tree
  (diminish 'undo-tree-mode))
(after-load 'redshank
  (diminish 'redshank-mode))
(after-load 'whitespace
  (diminish 'whitespace-mode))

;; uniquify --------------------------------------------------------------------

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; key bindings ----------------------------------------------------------------

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
(basis/define-hyper global-map "a" 'beginning-of-defun)
(basis/define-hyper global-map "e" 'end-of-defun)
(basis/define-hyper global-map "f" 'ido-find-file)
(basis/define-hyper global-map "b" 'ido-switch-buffer)
(basis/define-hyper global-map "d" 'basis/ido-dir-selector)
(basis/define-hyper global-map "D" 'basis/dired-dir-selector)
(basis/define-hyper global-map "t" 'basis/ido-tramp-selector)
(basis/define-hyper global-map "s" 'save-buffer)
(basis/define-hyper global-map "g" 'magit-status)
(basis/define-hyper global-map "i" 'imenu)
(basis/define-hyper global-map "0" 'delete-window)
(basis/define-hyper global-map "1" 'delete-other-windows)
(basis/define-hyper global-map "2" 'split-window-below)
(basis/define-hyper global-map "3" 'split-window-right)
(basis/define-hyper global-map "r" ctl-x-r-map)
(basis/define-hyper global-map "c" 'basis/toggle-case)

;; Easier window management
(winner-mode 1)
(windmove-default-keybindings)
(global-set-key (kbd "M-o") 'other-window)

;; Better buffer management
(global-set-key [remap list-buffers] 'ibuffer)

;; Newlines
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "<S-return>") 'basis/open-line-below)
(global-set-key (kbd "<C-S-return>") 'basis/open-line-above)

;; Clever C-a
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

;; Movement by paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Move to matching delimiters
(global-set-key (kbd "s-o") 'basis/other-sexp-delimiter)

;; Kill stuff
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "<M-backspace>") 'kill-region-or-backward-word)
(global-set-key (kbd "<C-backspace>") 'kill-line-backward)
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line) ; C-S-backspace
(global-set-key (kbd "<s-backspace>") 'smart-kill-whole-line)
(global-set-key (kbd "<S-backspace>") 'smart-kill-almost-whole-line)
(global-set-key (kbd "ESC <M-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "M-w") 'basis/kill-ring-save-something)
(global-set-key (kbd "<f2>") 'basis/clipboard-save-buffer)
(global-set-key (kbd "<M-f2>") 'basis/kill-ring-save-buffer)

;; ... and then browse it with M-y
(browse-kill-ring-default-keybindings)

;; Join lines
(global-set-key (kbd "s-j") 'basis/join-next-line)
(global-set-key (kbd "s-J") 'join-line)

;; Moves lines or regions
(global-set-key (kbd "<M-s-up>") 'move-text-up)
(global-set-key (kbd "<M-s-down>") 'move-text-down)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t"))
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t c") 'transpose-chars)
(global-set-key (kbd "M-t M-w") 'transpose-windows)

;; Move between errors
(global-set-key (kbd "s-.") 'next-error)
(global-set-key (kbd "s-,") 'previous-error)

;; Imenu is great
(global-set-key (kbd "s-i") 'imenu)

;; Occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)

;; Mark commands
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)
(define-key global-map
  [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

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

;; Eval expression on {meta,super}-hyper
(-when-let* ((template (case system-type
                         (gnu/linux  "<%s-menu>")
                         (darwin     "<%s-kp-enter>")
                         (windows-nt "<%s-apps>")))
             (meta-hyper (format template "M"))
             (super-hyper (format template "s")))
  (global-set-key (read-kbd-macro meta-hyper) 'eval-expression)
  (global-set-key (read-kbd-macro super-hyper) 'eval-expression))

;; I use Meta-space for ace-jump-mode
(global-set-key (kbd "C-c SPC") 'just-one-space)

;; jump-char
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;; goto-line, with line numbers
(global-set-key (kbd "M-g g") 'basis/goto-line-with-numbers)
(global-set-key (kbd "M-g M-g") 'basis/goto-line-with-numbers)

;; Movement by sexp
(global-set-key (kbd "M-e") 'forward-sexp)
(global-set-key (kbd "M-a") 'backward-sexp)
(global-set-key (kbd "<M-right>") 'forward-sexp)
(global-set-key (kbd "<M-left>") 'backward-sexp)

;; Start eshell or switch to it if it's active
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; M-x without using meta
(global-set-key (kbd "C-c m") 'smex)
(global-set-key (kbd "C-c x") 'execute-extended-command)

;; recetf+ido
(global-set-key (kbd "C-c r") 'basis/recentf-ido-find-file)

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

;; C-v et al. are uncomfortable on my MBP
(global-set-key (kbd "<s-up>") 'cua-scroll-down)
(global-set-key (kbd "<s-down>") 'cua-scroll-up)
(global-set-key (kbd "<M-s-up>") 'scroll-other-window-down)
(global-set-key (kbd "<M-s-down>") 'scroll-other-window)

;; I want to like smartparens but haven't gotten there yet
(global-set-key (kbd "C-c p") 'basis/toggle-between-autopair-and-smartparens)

;; Map for finding elisp stuff
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

;; aliases
(defalias 'ls 'ibuffer)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(defalias 'sayonara 'save-buffers-kill-terminal)

;; ace-jump-mode ---------------------------------------------------------------

(after-load "ace-jump-mode"
  (ace-jump-mode-enable-mark-sync))

(global-set-key (kbd "M-SPC") 'ace-jump-mode)
(global-set-key (kbd "s-SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; magit -----------------------------------------------------------------------

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun basis/init-magit-log-edit ()
  (turn-on-auto-fill)
  (setq fill-column 72))

(add-hook 'magit-log-edit-mode-hook 'basis/init-magit-log-edit)

(after-load 'magit
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; ibuffer ---------------------------------------------------------------------

(after-load 'ibuffer
  (define-ibuffer-column size-h
    ;; a more readable size column
    (:name "Size" :inline t)
    (cond ((> (buffer-size) 1000000)
           (format "%7.1fM" (/ (buffer-size) 1000000.0)))
          ((> (buffer-size) 1000)
           (format "%7.1fk" (/ (buffer-size) 1000.0)))
          (t
           (format "%8d" (buffer-size))))))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

;; dired -----------------------------------------------------------------------

(after-load 'dired
  (require 'dired+)
  (basis/define-keys dired-mode-map
    ((kbd "M-o")                 'other-window)
    ((kbd "C-c o")               'dired-omit-mode)
    ((kbd "M-n")                 'dired-next-subdir)
    ((kbd "M-p")                 'dired-prev-subdir)
    ((kbd "M-e")                 'dired-next-dirline)
    ((kbd "M-a")                 'dired-prev-dirline)
    ([remap beginning-of-buffer] 'basis/dired-jump-to-top)
    ([remap end-of-buffer]       'basis/dired-jump-to-bottom))
  (basis/define-hyper dired-mode-map "a" 'basis/dired-jump-to-top)
  (basis/define-hyper dired-mode-map "e" 'basis/dired-jump-to-bottom)
  (setq dired-recursive-deletes 'top)
  (put 'dired-find-alternate-file 'disabled nil))

;; eshell ----------------------------------------------------------------------

(defun basis/init-eshell ()
  (basis/define-keys eshell-mode-map
    ((kbd "<C-backspace>") 'basis/eshell-kill-line-backward)
    ((kbd "<C-S-backspace>") 'basis/eshell-kill-whole-line)))

(add-hook 'eshell-mode-hook 'basis/init-eshell)

;; ido -------------------------------------------------------------------------

(ido-mode 1)
(ido-everywhere t)
(ido-ubiquitous-mode t)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length 0
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-max-prospects 10
      ido-ignore-extensions t
      ido-save-directory-list-file "~/.emacs.d/.ido.last")

(defun basis/setup-ido ()
  (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-completion-map (kbd "M-w") 'ido-copy-current-file-name))

(add-hook 'ido-setup-hook 'basis/setup-ido)

;; smex ------------------------------------------------------------------------

(setq smex-save-file (concat basis/emacs-dir ".smex-items"))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; hippie expand ---------------------------------------------------------------

(global-set-key (kbd "M-/") 'hippie-expand)

(dolist (f '(try-expand-line try-expand-list try-expand-all-abbrevs))
  (setq hippie-expand-try-functions-list
        (delete f hippie-expand-try-functions-list)))

;; auto-complete ---------------------------------------------------------------

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

(define-key ac-completing-map (kbd "ESC") 'ac-stop)

;; yasnippet -------------------------------------------------------------------

(defun basis/yas-expand-or-insert ()
  "Call `yas-expand` or `yas-insert-snippet` depending on context.
If point is after what might be a snippet key, call `yas-expand`,
otherwise call `yas-insert-snippet`."
  (interactive)
  (call-interactively
   (if (looking-at "\\>") #'yas-expand #'yas-insert-snippet)))

(after-load 'yasnippet
  (basis/define-hyper global-map "<tab>" 'basis/yas-expand-or-insert)
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets))

(setq yas-snippet-dirs '("~/.emacs.d/snippets/")
      yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
      yas-wrap-around-region t
      ac-source-yasnippet nil)

(yas-global-mode 1)

;; lisp ------------------------------------------------------------------------

(setq inferior-lisp-program "/usr/bin/sbcl")

(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

(defun basis/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (paredit-mode +1)
  (autopair-mode -1))

(defun set-up-hippie-expand-for-elisp ()
  "Enable Lisp symbol completion in Hippie Expand."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially t))

(defun basis/emacs-lisp-setup ()
  "Enable features useful when working with Emacs Lisp."
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
                              inferior-scheme-mode-hook))))
  (dolist (hook lispy-hooks)
    (add-hook hook 'basis/lisp-setup))
  (dolist (hook elispy-hooks)
    (add-hook hook 'basis/emacs-lisp-setup)))

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

;; paredit ---------------------------------------------------------------------

(defun basis/maybe-map-paredit-newline ()
  "Map `paredit-newline` except in some interactive modes."
  (unless (or (minibufferp) (memq major-mode '(inferior-emacs-lisp-mode
                                               inferior-lisp-mode
                                               inferior-scheme-mode)))
    (local-set-key (kbd "<return>") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'basis/maybe-map-paredit-newline)

(defun basis/maybe-enable-paredit-mode ()
  "Enable Paredit during Lisp-related minibuffer commands."
  (let ((paredit-minibuffer-commands '(eval-expression
                                       pp-eval-expression
                                       eval-expression-with-eldoc
                                       slime-interactive-eval)))
    (when (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode))))

(add-hook 'minibuffer-setup-hook 'basis/maybe-enable-paredit-mode)

(after-load 'paredit
  (basis/define-keys paredit-mode-map
    ((kbd "[")                      'paredit-open-round)
    ((kbd "M-[")                    'paredit-open-square)
    ((kbd "M-)")                    'basis/paredit-wrap-round-from-behind)
    ((kbd "M-e")                    'paredit-forward)
    ((kbd "<M-right>")              'paredit-forward)
    ((kbd "M-a")                    'paredit-backward)
    ((kbd "<M-left>")               'paredit-backward)
    ((kbd "s-u")                    'paredit-backward-up)
    ((kbd "M-k")                    'kill-sexp)
    ((kbd "C-w")                    'basis/paredit-kill-something)
    ((kbd "M-<backspace>")          'basis/paredit-kill-something)
    ([remap backward-kill-sentence] 'backward-kill-sexp))
  (add-to-list 'paredit-space-for-delimiter-predicates
               'basis/paredit-doublequote-space-p))

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

;; slime -----------------------------------------------------------------------

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

(after-load 'slime
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
  (setq slime-autodoc-use-multiline-p t))

(after-load 'slime-repl-mode
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

;; slime auto-complete support
(defun basis/set-up-slime-fuzzy-ac ()
  "Integrate SLIME's fuzzy completion with auto-complete."
  (interactive)
  (set-up-slime-ac t))

(add-hook 'slime-mode-hook 'basis/set-up-slime-fuzzy-ac)
(add-hook 'slime-repl-mode-hook 'basis/set-up-slime-fuzzy-ac)

(after-load "auto-complete"
  (add-to-list 'ac-modes 'slime-repl-mode))

;; redshank --------------------------------------------------------------------

(require 'redshank-loader)

(after-load "redshank-loader"
  (redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t))

;; scheme ----------------------------------------------------------------------

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

(after-load 'scheme
  (basis/define-keys scheme-mode-map
    ((kbd "<f5>")   'scheme-send-last-sexp)
    ((kbd "<f6>")   'basis/scheme-send-something)
    ((kbd "<M-f6>") 'scheme-compile-definition-and-go)
    ((kbd "<f8>")   'scheme-compile-file)
    ((kbd "<M-f8>") 'scheme-load-file)))

;; autopair --------------------------------------------------------------------

(setq autopair-blink nil)

(autopair-global-mode) ; disabled for lisps in init-lisp.el

;; smartparens -----------------------------------------------------------------

(after-load 'smartparens
  (sp-use-paredit-bindings)
  (basis/define-keys smartparens-mode-map
    ((kbd "RET")                  'basis/electric-return)
    ([remap delete-char]          'sp-delete-char)
    ([remap backward-delete-char] 'sp-backward-delete-char)
    ([remap kill-word]            'sp-kill-word)
    ((kbd "<M-backspace>")        'basis/sp-backward-kill-something)))

;; flycheck --------------------------------------------------------------------

(defun basis/adjust-flycheck-idle-change-delay ()
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 5.0)))

(defvar basis/flycheck-keymap nil
  "Custom keybindings for Flycheck.")

(after-load 'flycheck
  (when (eq system-type 'windows-nt)
    (setq flycheck-xml-parser 'flycheck-parse-xml-region))

  ;; Check buffers with errors more frequently than ones without
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'basis/adjust-flycheck-idle-change-delay)

  ;; Don't trigger a check on newline
  (setq flycheck-check-syntax-automatically
        (remq 'new-line flycheck-check-syntax-automatically))

  ;; Initialize the keymap
  (setq basis/flycheck-keymap
        (let ((map (make-sparse-keymap)))
          (basis/define-keys map
            ((kbd "c") 'flycheck-buffer)
            ((kbd "i") 'flycheck-info)
            ((kbd "n") 'flycheck-next-error)
            ((kbd "p") 'flycheck-previous-error)
            ((kbd "l") 'flycheck-list-errors)
            ((kbd "s") 'flycheck-select-checker)
            ((kbd "C") 'flycheck-clear))
          map))
  (global-set-key (kbd "s-k") basis/flycheck-keymap))

;; python ----------------------------------------------------------------------

(after-load 'python
  (basis/define-keys python-mode-map
    ((kbd "M-e")    'python-nav-forward-sexp)
    ((kbd "M-a")    'basis/python-nav-backward-sexp)
    ((kbd "<f6>")   'python-shell-send-something)
    ((kbd "<f8>")   'python-shell-send-buffer)
    ((kbd "<M-f8>") 'python-shell-send-file)))

;; Jedi
(setq jedi:setup-keys t
      jedi:tooltip-method nil) ; show function signatures in the minibuffer

(add-hook 'python-mode-hook 'jedi:setup)

(defun basis/setup-autopair-for-python ()
  (setq autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action)))

(defun basis/init-python-mode ()
  (basis/setup-autopair-for-python)
  (unless (and buffer-file-name (tramp-tramp-file-p buffer-file-name))
    (flycheck-mode 1)))

(add-hook 'python-mode-hook 'basis/init-python-mode)

;; javascript ------------------------------------------------------------------

(add-to-list 'auto-mode-alist (cons "\\.js\\'" 'js2-mode))

(setq-default
 js2-show-parse-errors nil
 js2-allow-rhino-new-expr-initializer nil
 js2-strict-inconsistent-return-warning nil
 js2-strict-missing-semi-warning nil
 js2-strict-trailing-comma-warning t)

(setq js2-basic-offset 2)

(after-load 'js2-mode
  (require 'js2-refactor)
  (js2r-add-keybindings-with-prefix "M-r")
  (js2-imenu-extras-setup))

(defun basis/init-js2-mode ()
  (subword-mode 1)
  (unless (and buffer-file-name (tramp-tramp-file-p buffer-file-name))
    (flycheck-mode 1)))

(add-hook 'js2-mode-hook 'basis/init-js2-mode)

;; skewer ----------------------------------------------------------------------

(skewer-setup) ; hook into js2, html, and css modes

(after-load 'skewer-mode
  (basis/define-keys skewer-mode-map
    ((kbd "<f5>") 'skewer-eval-last-expression)
    ((kbd "<f6>") 'skewer-eval-defun)
    ((kbd "<f8>") 'skewer-load-buffer)))

(after-load 'skewer-repl
  (define-key skewer-repl-mode-map (kbd "TAB") 'hippie-expand))

(after-load 'skewer-html
  (define-key skewer-html-mode-map (kbd "<f6>") 'skewer-html-eval-tag))

(after-load 'skewer-css
  (basis/define-keys skewer-css-mode-map
    ((kbd "<f5>") 'skewer-css-eval-current-declaration)
    ((kbd "<f6>") 'skewer-css-eval-current-rule)
    ((kbd "<f8>") 'skewer-css-eval-buffer)))

;; sql -------------------------------------------------------------------------

(defun basis/init-sql-mode ()
  (auto-complete-mode 1)
  (sql-set-product "postgres"))

(add-hook 'sql-mode-hook 'basis/init-sql-mode)

;; c ---------------------------------------------------------------------------

(defun basis/init-c ()
  (c-set-style "python")
  (setq indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-newline 1))

(add-hook 'c-mode-hook 'basis/init-c)

;; ack and a half --------------------------------------------------------------

(after-load 'ack-and-a-half
  (basis/define-keys ack-and-a-half-mode-map
    ((kbd "n") 'compilation-next-error)
    ((kbd "p") 'compilation-previous-error)
    ((kbd "]") 'compilation-next-file)
    ((kbd "[") 'compilation-previous-file)))

;; org -------------------------------------------------------------------------

;; Paths
(setq org-directory (expand-file-name "~/Dropbox/org/"))
(setq org-default-notes-file (concat org-directory "refile.org"))
(setq org-agenda-files
      (loop for name in '("todo.org" "work.org")
            collect (concat org-directory name)))
(setq org-archive-location "%s.archive::")

;; Misc. options
(setq org-completion-use-ido t
      org-outline-path-complete-in-steps nil
      org-reverse-note-order t
      org-log-done t)

;; Agenda
(setq org-agenda-start-on-weekday nil
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t)

;; Capture
(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,org-default-notes-file "Tasks")
             "* TODO %?\n %i\n")
        ("w" "Work todo" entry (file+headline "~/Dropbox/org/work.org" "Tasks")
             "* TODO %?\n %i\n")
        ("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
             "* %u %?")))

;; Refiling
(setq org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 2)))

;; Todo keyworks
(setq org-todo-keywords
      '((sequence
         "TODO(t)" "STARTED(s@)" "WAITING(w@/!)" "DELEGATED(l@)" "|"
         "DONE(d!)" "DEFERRED(f@)" "CANCELLED(c@)")))

;; Keys
(basis/define-keys global-map
  ((kbd "C-c a") 'org-agenda)
  ((kbd "C-c b") 'org-iswitchb)
  ((kbd "C-c c") 'org-capture)
  ((kbd "C-c l") 'org-store-link))

;; html ------------------------------------------------------------------------

(defun basis/init-simplezen ()
  (set (make-local-variable 'yas-fallback-behavior)
       '(apply simplezen-expand-or-indent-for-tab)))

(defun basis/init-html-mode ()
  (tagedit-mode 1))

(add-hook 'sgml-mode-hook 'basis/init-simplezen)
(add-hook 'html-mode-hook 'basis/init-html-mode)

(after-load "sgml-mode"
  (require 'tagedit)
  (require 'simplezen)
  (basis/define-keys html-mode-map
    ([remap forward-paragraph]  'basis/move-to-next-blank-line)
    ([remap backward-paragraph] 'basis/move-to-previous-blank-line)
    ((kbd "RET")                'basis/html-newline-and-indent)
    ((kbd "<M-return>")         'basis/html-multiline-expand)
    ((kbd "C-c C-w")            'basis/html-wrap-in-tag)
    ((kbd "C-c w")              'basis/html-wrap-in-tag)
    ((kbd "<f8>")               'browse-url-of-buffer))
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features))

(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(defadvice tagedit-toggle-multiline-tag (around maybe-forward activate)
  "Move forward by a line and indent if invoked directly between
two tags."
  (let ((move-forward-p (and (looking-at "<") (looking-back ">"))))
    ad-do-it
    (when move-forward-p
      (forward-line 1)
      (indent-according-to-mode))))

;; markdown --------------------------------------------------------------------

(dolist (ext (list "\\.markdown" "\\.mkd" "\\.md"))
  (add-to-list 'auto-mode-alist (cons ext 'markdown-mode)))

(defun basis/init-markdown-mode ()
  (unless (eq major-mode 'gfm-mode)
    (turn-on-auto-fill)))

(add-hook 'markdown-mode-hook 'basis/init-markdown-mode)

(after-load 'markdown-mode
  (define-key markdown-mode-map
    (kbd "C-c r") 'markdown-insert-reference-link-dwim)
  (define-key gfm-mode-map
    (kbd "C-c r") 'markdown-insert-reference-link-dwim))

;; deft ------------------------------------------------------------------------

(setq deft-extension "md"
      deft-directory "~/Dropbox/deft"
      deft-text-mode  'gfm-mode)


;;; init.el ends here
