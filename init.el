;;; init.el        -*- coding: utf-8; -*-

(require 'cl)

;; Disable superfluous UI immediately to prevent momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Define directories for various purposes and ensure they exist
(defvar basis/emacs-dir (file-name-directory load-file-name))
(defvar basis/site-lisp-dir (expand-file-name "site-lisp/" basis/emacs-dir))
(defvar basis/defuns-file (expand-file-name "defuns.el" basis/emacs-dir))
(defvar basis/backups-dir (expand-file-name "backups/" basis/emacs-dir))
(defvar basis/autosaves-dir (expand-file-name "autosaves/" basis/emacs-dir))
(defvar basis/tmp-dir (expand-file-name "tmp/" basis/emacs-dir))
(defvar basis/custom-file (expand-file-name "custom.el" basis/emacs-dir))
(defvar basis/themes-dir (expand-file-name "themes/" basis/emacs-dir))

(dolist (dir (list basis/backups-dir basis/autosaves-dir basis/tmp-dir))
  (unless (file-exists-p dir)
    (make-directory dir)))

;; Emacs doesn't seem to respect $PATH on OS X
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin/")
  (add-to-list 'exec-path "~/bin/"))

;; Set up the load path
(add-to-list 'load-path basis/emacs-dir)
(add-to-list 'load-path basis/site-lisp-dir)
(add-to-list 'load-path (expand-file-name "tramp-2.2.7/" basis/site-lisp-dir))

;; package ---------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; Make sure every archive is present in the elpa/archives/ folder
(let* ((archive-folder "~/.emacs.d/elpa/archives/")
       (archive-folders (mapcar #'(lambda (archive)
                                    (let ((name (car archive)))
                                      (expand-file-name name archive-folder)))
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
          dash-functional
          s
          move-text
          browse-kill-ring
          jedi
          yasnippet
          tagedit
          simplezen
          js2-mode
          ac-js2
          js2-refactor
          js-comint
          skewer-mode
          flycheck
          clojure-mode
          cider
          ac-nrepl
          clojure-cheatsheet
          key-chord
          writegood-mode
          projectile
          flx-ido
          ido-vertical-mode
          guide-key
          geiser
          ac-geiser
          smartparens
          color-theme-solarized
          gist
          clj-refactor
          discover
          ))
       (basis/uninstalled-packages
        (remove-if #'package-installed-p basis/required-packages)))
  (when basis/uninstalled-packages
    (package-refresh-contents)
    (mapc #'package-install basis/uninstalled-packages)))

;; load defuns -----------------------------------------------------------------

;; Make dash and s available for use in defuns
(require 'dash)
(require 'dash-functional)
(require 's)

(dash-enable-font-lock)

;; Load custom functions
(when (file-exists-p basis/defuns-file)
  (load basis/defuns-file))

;; cygwin ----------------------------------------------------------------------

(defvar basis/cygwin-p (and (eq system-type 'windows-nt)
                            (directory-files "c:/" nil "Cygwin")
                            (file-directory-p "c:/bin"))
  "True if this is a Windows system with Cygwin installed.")

(when basis/cygwin-p
  (require 'cygwin-mount)
  (-when-let (home (getenv "HOME"))
    (cd home))
  (let* ((path (getenv "PATH"))
         (entries (s-split ";" path)))
    (unless (member "c:\\bin" entries)
      (setenv "PATH" (concat "c:\\bin;" path))))
  (unless (member "c:/bin" exec-path)
    (add-to-list 'exec-path "c:/bin"))
  (-when-let (shell (or (executable-find "zsh")
                        (executable-find "bash")))
    (setq shell-file-name shell
          explicit-shell-file-name shell
          ediff-shell shell
          null-device "/dev/null")
    (setenv "SHELL" shell)))

;; various settings ------------------------------------------------------------

(setq visible-bell t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
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
      apropos-do-all t
      custom-file basis/custom-file
      dirtrack-list '("^(~?[a-zA-Z0-9/ _-]+)>" 1)
      ;; This causes errors on OS X
      save-interprogram-paste-before-kill (not (eq system-type 'darwin)))

(when (file-exists-p custom-file)
  (load custom-file))

;; Tabs
(setq-default indent-tabs-mode nil
              fill-column 79)

(setq tab-stop-list (number-sequence 4 120 4))

;; Enable some miscellaneous helpful modes
(size-indication-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(cua-selection-mode 1)
(auto-compression-mode 1)
(delete-selection-mode 1)

;; TRAMP
(setq tramp-default-method
      (if (and (eq system-type 'windows-nt)
               (not basis/cygwin-p))
          "plinkx"
        "sshx"))

;; ispell
(let ((aspell (if (and (eq system-type 'windows-nt)
                       (not basis/cygwin-p))
                  "c:\\Program Files (x86)\\Aspell\\bin\\aspell.exe"
                "aspell")))
  (setq ispell-program-name aspell
        ispell-personal-dictionary "~/.aspell.en.pws"
        aspell-installed-p (executable-find aspell)))

;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 50
      recentf-save-file (expand-file-name ".recentf" basis/emacs-dir)
      recentf-exclude '(file-remote-p))

;; safe local variables
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; Don't use CRLF on remote Unix machines
(defun basis/maybe-set-coding ()
  (when (and buffer-file-name
             (s-starts-with? "/plinkx:" buffer-file-name))
    (set-buffer-file-coding-system 'utf-8-unix)))

(when (eq system-type 'windows-nt)
  (add-hook 'before-save-hook 'basis/maybe-set-coding))

;; Backups, autosaves, and temporary files
(setq backup-by-copying t
      backup-directory-alist `((".*" . ,basis/backups-dir))
      auto-save-file-name-transforms `((".*" ,basis/autosaves-dir t))
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
(after-load 'cider
  (diminish 'cider-mode " ν"))              ; nu
(after-load 'auto-complete
  (diminish 'auto-complete-mode " α"))      ; alpha
(after-load 'yasnippet
  (diminish 'yas-minor-mode " υ"))          ; upsilon
(after-load 'smartparens
  (diminish 'smartparens-mode " φ"))        ; phi
(after-load 'tagedit
  (diminish 'tagedit-mode " τ"))            ; tau
(after-load 'flyspell
  (diminish 'flyspell-mode " ι"))           ; iota
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
(after-load 'guide-key
  (diminish 'guide-key-mode))
(after-load 'clj-refactor
  (diminish 'clj-refactor-mode))

;; info ------------------------------------------------------------------------

(after-load 'info
  (let ((info-path (expand-file-name "~/.emacs.d/doc/info")))
    (when (file-exists-p info-path)
      (add-to-list 'Info-additional-directory-list info-path))))

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

;; Easier window management
(winner-mode 1)
(windmove-default-keybindings)
(global-set-key (kbd "M-o") 'other-window)

;; Newlines
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "S-RET") 'basis/open-line-below)
(global-set-key (kbd "C-S-RET") 'basis/open-line-above)

;; Clever C-a
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

;; Movement by paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Kill/save stuff
(global-set-key (kbd "M-k") 'kill-sexp)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)
(global-set-key (kbd "M-DEL") 'kill-region-or-backward-word)
(global-set-key (kbd "C-DEL") 'kill-line-backward)
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line) ; C-S-backspace
(global-set-key (kbd "s-DEL") 'smart-kill-whole-line)
(global-set-key (kbd "S-DEL") 'smart-kill-almost-whole-line)
(global-set-key (kbd "ESC M-DEL") 'backward-kill-sexp)
(global-set-key (kbd "M-w") 'basis/kill-ring-save-something)
(global-set-key (kbd "<f2>") 'basis/clipboard-save-something)
(global-set-key (kbd "<M-f2>") 'basis/kill-ring-save-buffer)
(global-set-key (kbd "s-w") 'basis/kill-ring-save-indented)

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
(global-set-key (kbd "M-t M-s") 'toggle-window-split) ;; hmmm

;; Occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)

;; Mark commands
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)
(global-set-key
 [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; Expand-region
(global-set-key (kbd "M-=") 'er/expand-region)

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

;; Regexp search by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

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

;; h-map -----------------------------------------------------------------------

(define-prefix-command 'basis/h-map)

;; Note sure which will be better
(global-set-key (kbd "C-h") 'basis/h-map)
(global-set-key (kbd "M-h") 'basis/h-map)

(basis/define-keys global-map
  ((kbd "C-h a")   'beginning-of-defun)
  ((kbd "C-h e")   'end-of-defun)
  ((kbd "C-h f")   'ido-find-file)
  ((kbd "C-h b")   'ido-switch-buffer)
  ((kbd "C-h g")   'magit-status)
  ((kbd "C-h i")   'imenu)
  ((kbd "C-h 0")   'delete-window)
  ((kbd "C-h 1")   'delete-other-windows)
  ((kbd "C-h 2")   'split-window-below)
  ((kbd "C-h 3")   'split-window-right)
  ((kbd "C-h r")   ctl-x-r-map)
  ((kbd "C-h C-h") 'mark-paragraph))

;; find elisp map --------------------------------------------------------------

(define-prefix-command 'lisp-find-map)

(global-set-key (kbd "<f1> e") 'lisp-find-map)

(defun scratch! ()
  "Switch to the scratch buffer, creating it if necessary."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*")))

(global-set-key (kbd "<f1> e c") 'finder-commentary)
(global-set-key (kbd "<f1> e e") 'view-echo-area-messages)
(global-set-key (kbd "<f1> e f") 'find-function)
(global-set-key (kbd "<f1> e F") 'find-face-definition)
(global-set-key (kbd "<f1> e i") 'info-apropos)
(global-set-key (kbd "<f1> e k") 'find-function-on-key)
(global-set-key (kbd "<f1> e l") 'find-library)
(global-set-key (kbd "<f1> e s") 'scratch!)
(global-set-key (kbd "<f1> e v") 'find-variable)
(global-set-key (kbd "<f1> e V") 'apropos-value)
(global-set-key (kbd "<f1> e a") 'helm-apropos)

;; aliases ---------------------------------------------------------------------

(defalias 'qrr 'query-replace-regexp)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(defalias 'sayonara 'save-buffers-kill-terminal)

;; tmux ------------------------------------------------------------------------

;; A number of non-alphanumeric keys don't work by default when Emacs is
;; running in tmux. This attempts to fix that by adding entries to the
;; `key-translation-map'.

;; `setw -g xterm-keys on` must be set in ~/.tmux.conf for this to work.

;; The code below is based on that in the ArchWiki page on Emacs but refactored
;; a bit.

;; TODO: <home> and <end> still don't work
(when (getenv "TMUX")
  (let ((specs '((2 . "S-")
                 (3 . "M-")
                 (4 . "M-S-")
                 (5 . "C-")
                 (6 . "C-S-")
                 (7 . "C-M-")
                 (8 . "C-M-S-"))))
    (dolist (spec specs)
      (let ((n (car spec))
            (k (cdr spec)))
        (basis/define-key-translations
          ((format "M-[ 1 ; %d A" n)  (format "%s<up>" k))       ;; left arrow
          ((format "M-[ 1 ; %d B" n)  (format "%s<down>" k))     ;; down arrow
          ((format "M-[ 1 ; %d C" n)  (format "%s<right>" k))    ;; right arrow
          ((format "M-[ 1 ; %d D" n)  (format "%s<left>" k))     ;; left arrow
          ((format "M-[ 1 ; %d H" n)  (format "%s<home>" k))     ;; home
          ((format "M-[ 1 ; %d F" n)  (format "%s<end>" k))      ;; end
          ((format "M-[ 5 ; %d ~" n)  (format "%s<prior>" k))    ;; page up
          ((format "M-[ 6 ; %d ~" n)  (format "%s<next>" k))     ;; page down
          ((format "M-[ 2 ; %d ~" n)  (format "%s<delete>" k))   ;; insert
          ((format "M-[ 3 ; %d ~" n)  (format "%s<delete>" k))   ;; delete
          ((format "M-[ 1 ; %d P" n)  (format "%s<f1>" k))       ;; f1
          ((format "M-[ 1 ; %d Q" n)  (format "%s<f2>" k))       ;; f2
          ((format "M-[ 1 ; %d R" n)  (format "%s<f3>" k))       ;; f3
          ((format "M-[ 1 ; %d S" n)  (format "%s<f4>" k))       ;; f4
          ((format "M-[ 15 ; %d ~" n) (format "%s<f5>" k))       ;; f5
          ((format "M-[ 17 ; %d ~" n) (format "%s<f6>" k))       ;; f6
          ((format "M-[ 18 ; %d ~" n) (format "%s<f7>" k))       ;; f7
          ((format "M-[ 19 ; %d ~" n) (format "%s<f8>" k))       ;; f8
          ((format "M-[ 20 ; %d ~" n) (format "%s<f9>" k))       ;; f9
          ((format "M-[ 21 ; %d ~" n) (format "%s<f10>" k))      ;; f10
          ((format "M-[ 23 ; %d ~" n) (format "%s<f11>" k))      ;; f11
          ((format "M-[ 24 ; %d ~" n) (format "%s<f12>" k))      ;; f12
          ((format "M-[ 25 ; %d ~" n) (format "%s<f13>" k))      ;; f13
          ((format "M-[ 26 ; %d ~" n) (format "%s<f14>" k))      ;; f14
          ((format "M-[ 28 ; %d ~" n) (format "%s<f15>" k))      ;; f15
          ((format "M-[ 29 ; %d ~" n) (format "%s<f16>" k))      ;; f16
          ((format "M-[ 31 ; %d ~" n) (format "%s<f17>" k))      ;; f17
          ((format "M-[ 32 ; %d ~" n) (format "%s<f18>" k))      ;; f18
          ((format "M-[ 33 ; %d ~" n) (format "%s<f19>" k))      ;; f19
          ((format "M-[ 34 ; %d ~" n) (format "%s<f20>" k))))))) ;; f20

;; mintty ----------------------------------------------------------------------

(defun basis/define-mintty-translations ()
  "Define key translations to better support mintty."
  ;; TODO: is there a way to automatically detect whether Emacs is running in
  ;; mintty?
  (interactive)
  (basis/define-key-translations
    ("M-[ 1 ; 6 l" "C-<")
    ("M-[ 1 ; 6 n" "C->")
    ("M-[ 1 ; 6 y" "C-(")
    ("M-[ 1 ; 6 k" "C-+")))

;; help-mode -------------------------------------------------------------------

(after-load 'help-mode
  (basis/define-keys help-mode-map
    ((kbd "n") 'next-line)
    ((kbd "p") 'previous-line)
    ((kbd "b") 'help-go-back)
    ((kbd "f") 'help-go-forward)))

;; prog-mode -------------------------------------------------------------------

(add-hook 'prog-mode-hook 'basis/maybe-enable-whitespace-mode)
(add-hook 'prog-mode-hook 'basis/maybe-enable-flyspell-prog-mode)

;; diff-mode -------------------------------------------------------------------

(after-load 'diff-mode
  ;; Don't shadow my beloved `other-window' binding. `diff-goto-source' is
  ;; still available on C-c C-c.
  (define-key diff-mode-map (kbd "M-o") nil))

;; discover-mode ---------------------------------------------------------------

(require 'discover)
(global-discover-mode 1)

(discover-add-context-menu
 :context-menu (assq 'isearch discover-context-menus)
 :mode nil
 :mode-hook nil
 :bind "C-c s")

;; guide-key -------------------------------------------------------------------

(guide-key-mode 1)
(setq guide-key/guide-key-sequence '("C-x 4" "C-x v" "C-x 8"))

;; key-chord -------------------------------------------------------------------

(require 'key-chord)

(key-chord-mode 1)

(dolist (entry '(("jk" . ace-jump-word-mode)
                 (",." . ido-switch-buffer)
                 (",," . ibuffer)
                 ("`1" . delete-other-windows)
                 ("0-" . delete-window)
                 ("j2" . split-window-below)
                 ("j3" . split-window-right)
                 (" 9" . "(")))
  (key-chord-define-global (car entry) (cdr entry)))

;; undo-tree -------------------------------------------------------------------

(global-undo-tree-mode 1)

;; The couple bindings undo-tree puts behind C-x r prevent discover.el's C-x r
;; context menu from working
(define-key undo-tree-map (kbd "C-x r") nil)

;; multiple-cursors ------------------------------------------------------------

(require 'multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "<f9>") 'mc/mark-more-like-this-extended)

;; Make RET exit multiple-cursors-mode in the terminal too
(define-key mc/keymap (kbd "RET") 'multiple-cursors-mode)

;; `multiple-cursors' automatically stores lists of for-all and just-once
;; commands in ~/.emacs.d/.mc-lists.el. However, bouncing between machines
;; means being prompted about some commands repeatedly; so, I pre-populate it
;; with some of the more common ones that aren't covered by mc's defaults.
(dolist (cmd '(backward-kill-sexp
               backward-sexp
               basis/paredit-kill-something
               basis/paredit-wrap-round-from-behind
               beginning-of-line-or-indentation
               delete-horizontal-space
               forward-sexp
               kill-line-backward
               kill-sexp
               markdown-exdent-or-delete
               org-delete-char
               org-self-insert-command
               paredit-backward
               paredit-backward-kill-word
               paredit-forward
               paredit-forward-delete
               paredit-forward-kill-word
               paredit-splice-sexp
               paredit-split-sexp
               paredit-wrap-round
               sp--self-insert-command
               sp-backward-delete-char
               sp-backward-delete-char
               sp-backward-kill-sexp
               sp-backward-kill-word
               sp-backward-sexp
               sp-delete-char
               sp-forward-sexp
               sp-kill-sexp
               sp-kill-word))
  (add-to-list 'mc/cmds-to-run-for-all cmd))

(dolist (cmd '(eval-buffer
               isearch-backward-regexp
               isearch-forward-regexp
               smex))
  (add-to-list 'mc/cmds-to-run-once cmd))

;; saveplace -------------------------------------------------------------------

(require 'saveplace)

(setq-default save-place t)

(setq save-place-file (expand-file-name "places" basis/emacs-dir))

;; ace-jump-mode ---------------------------------------------------------------

(after-load 'ace-jump-mode
  (ace-jump-mode-enable-mark-sync))

(global-set-key (kbd "M-SPC") 'ace-jump-mode)
(global-set-key (kbd "s-SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; mu4e ------------------------------------------------------------------------

(let ((mu4e-path "/usr/local/share/emacs/site-lisp/mu4e/"))
  (when (and (not (eq system-type 'windows-nt))
             (file-directory-p  mu4e-path)
             (executable-find "mu"))
    (add-to-list 'load-path mu4e-path)
    (require 'mu4e)))

;; Mail
(setq mu4e-get-mail-command "offlineimap"
      mu4e-maildir "/home/jbm/.maildir/fastmail"
      mu4e-sent-folder "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder "/trash")

;; Shortcuts. Available as jX
(setq mu4e-maildir-shortcuts '(("/archive" . ?a)
                               ("/inbox"   . ?i)
                               ("/sent"    . ?s)))

;; Addresses to consider "me" when searching
(setq mu4e-user-mail-address-list '("jbm@deft.li"
                                    "jbm@fastmail.fm"
                                    "jbm@mailforce.net"))

;; Convert HTML->text if no text version is available
(setq mu4e-html2text-command (if (executable-find "html2text")
                                 "html2text -utf8 -width 72"
                               #'html2text))

;; Where to save attachments
(setq mu4e-attachment-dir (-first #'file-directory-p
                                  (mapcar #'expand-file-name
                                          '("~/downloads"
                                            "~/Downloads"
                                            "~/"))))

;; Composing messages
(setq mu4e-reply-to-address "jbm@deft.li"
      user-mail-address "jbm@deft.li"
      user-full-name "John Mastro"
      message-signature "- John"
      mu4e-sent-messages-behavior 'delete ;; they're saved on the server
      message-kill-buffer-on-exit t)

;; SMTP
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtp-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-user "jbm@fastmail.fm"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

(after-load 'mu4e
  (add-hook 'mu4e-compose-mode-hook 'basis/maybe-enable-flyspell))

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
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  (-when-let (home (getenv "HOME"))
    (setq magit-repo-dirs (list (expand-file-name "code" home)))))

;; ibuffer ---------------------------------------------------------------------

(defalias 'ls 'ibuffer)

(global-set-key [remap list-buffers] 'ibuffer)

(after-load 'ibuffer
  (basis/define-keys ibuffer-mode-map
    ((kbd "M-o") 'other-window)
    ((kbd "C-M-o") 'ibuffer-visit-buffer-1-window))

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
  (setq dired-omit-files "^\\.?#" ; don't omit . and ..
        dired-recursive-deletes 'top)
  (put 'dired-find-alternate-file 'disabled nil))

;; eshell ----------------------------------------------------------------------

(defun basis/init-eshell ()
  (basis/define-keys eshell-mode-map
    ((kbd "C-DEL")   'basis/eshell-kill-line-backward)
    ((kbd "S-DEL")   'basis/eshell-kill-line-backward)
    ((kbd "C-S-DEL") 'basis/eshell-kill-whole-line)))

(add-hook 'eshell-mode-hook 'basis/init-eshell)

;; sh-mode ---------------------------------------------------------------------

(defun basis/maybe-set-shell-to-zsh ()
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) ".zshrc"))
    (sh-set-shell "zsh")))

(add-hook 'sh-mode-hook 'basis/maybe-set-shell-to-zsh)

;; ido -------------------------------------------------------------------------

(require 'flx-ido)

(ido-mode 1)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length 0
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-use-faces nil
      ido-max-prospects 10
      ido-ignore-extensions t
      ido-save-directory-list-file "~/.emacs.d/.ido.last")

(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)

(defun basis/setup-ido ()
  (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-completion-map (kbd "M-w") 'ido-copy-current-file-name))

(add-hook 'ido-setup-hook 'basis/setup-ido)

;; smex ------------------------------------------------------------------------

(setq smex-save-file (expand-file-name ".smex-items" basis/emacs-dir))

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

(global-auto-complete-mode t)
(ac-config-default)

(setq ac-comphist-file "~/.emacs.d/.ac-comphist.dat")

(setq-default ac-sources
              '(ac-source-imenu
                ac-source-dictionary
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers))

(define-key ac-completing-map (kbd "ESC") 'ac-stop)
(define-key ac-completing-map (kbd "RET") nil)
(define-key ac-completing-map (kbd "<return>") nil) ;; necessary even w/ above

(dolist (mode '(slime-repl cider-repl geiser-repl inferior-python sql))
  (let ((mode (intern (concat (symbol-name mode) "-mode"))))
    (add-to-list 'ac-modes mode)))

;; yasnippet -------------------------------------------------------------------

(require 'yasnippet)

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(defun basis/yas-expand-or-insert ()
  "Call `yas-expand' or `yas-insert-snippet' depending on context.
If point is after what might be a snippet key, call `yas-expand',
otherwise call `yas-insert-snippet'."
  (interactive)
  (call-interactively
   (if (looking-at "\\>") #'yas-expand #'yas-insert-snippet)))

;; Steal C-t for expanding snippets. `transpose-chars' is still available on
;; M-t c
(define-key yas-minor-mode-map (kbd "C-t") 'basis/yas-expand-or-insert)
(define-key yas-keymap (kbd "RET") 'yas-exit-all-snippets)

(setq yas-snippet-dirs '("~/.emacs.d/snippets/")
      yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
      yas-wrap-around-region t
      ac-source-yasnippet nil)

(yas-global-mode 1)

;; projectile ------------------------------------------------------------------

(setq projectile-keymap-prefix (kbd "C-h p")
      projectile-completion-system 'ido
      projectile-mode-line " ρ")  ; rho

(setq projectile-known-projects-file
      (expand-file-name "~/.emacs.d/.projectile-bookmarks.eld"))

(setq projectile-cache-file
      (expand-file-name "~/.emacs.d/.projectile.cache"))

;; Projectile defaults to native indexing on Windows, but if we have Cygwin
;; set up we can use "alien".
(if (and (eq system-type 'windows-nt)
         (not basis/cygwin-p))
    (setq projectile-indexing-method 'native
          projectile-enable-caching t)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching nil))

(projectile-global-mode)

(setq projectile-mode-line-lighter " ρ")

;; lisp ------------------------------------------------------------------------

(defvar basis/lisp-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    inferior-emacs-lisp-mode
    clojure-mode
    cider-repl-mode
    lisp-mode
    slime-repl-mode
    inferior-lisp-mode
    scheme-mode
    inferior-scheme-mode
    geiser-repl-mode)
  "List of all Lisp modes used. Useful for e.g. setting Paredit
  as opposed to Smartparens.")

(-when-let (sbcl (executable-find "sbcl"))
  (setq inferior-lisp-program sbcl))

(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

(defun basis/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (paredit-mode +1))

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
                              clojure-mode-hook
                              cider-repl-mode-hook
                              inferior-lisp-mode-hook
                              scheme-mode-hook
                              inferior-scheme-mode-hook
                              geiser-repl-mode-hook))))
  (dolist (hook lispy-hooks)
    (add-hook hook 'basis/lisp-setup))
  (dolist (hook elispy-hooks)
    (add-hook hook 'basis/emacs-lisp-setup)))

(setq lisp-lambda-list-keyword-alignment t
      lisp-lambda-list-keyword-parameter-alignment t
      lisp-loop-forms-indentation 6)

;; emacs lisp ------------------------------------------------------------------

(dolist (mode (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (basis/define-keys mode
    ((kbd "<f5>") 'basis/eval-last-sexp)
    ((kbd "<f6>") 'basis/eval-something)
    ((kbd "<f7>") 'basis/expand-sexp-at-point)
    ((kbd "<f8>") 'eval-buffer)))

;; paredit ---------------------------------------------------------------------

(defun basis/maybe-map-paredit-newline ()
  "Map `paredit-newline' except in some interactive modes."
  (unless (or (minibufferp) (memq major-mode '(inferior-emacs-lisp-mode
                                               inferior-lisp-mode
                                               inferior-scheme-mode
                                               geiser-repl-mode
                                               cider-repl-mode)))
    (local-set-key (kbd "RET") 'paredit-newline)))

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
    ((kbd "[")                      'basis/paredit-open-something)
    ((kbd "C-c [")                  'paredit-open-square)
    ((kbd "M-)")                    'basis/paredit-wrap-round-from-behind)
    ((kbd "M-e")                    'paredit-forward)
    ((kbd "<M-right>")              'paredit-forward)
    ((kbd "M-a")                    'paredit-backward)
    ((kbd "<M-left>")               'paredit-backward)
    ((kbd "M-k")                    'kill-sexp)
    ((kbd "C-w")                    'basis/paredit-kill-something)
    ((kbd "M-DEL")                  'basis/paredit-kill-something)
    ([remap backward-kill-sentence] 'backward-kill-sexp))
  (add-to-list 'paredit-space-for-delimiter-predicates
               'basis/paredit-doublequote-space-p))

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

;; slime -----------------------------------------------------------------------

(-if-let (quicklisp-slime-helper
          (-first #'file-exists-p
                  (mapcar #'expand-file-name
                          '("~/code/lisp/quicklisp/slime-helper.el"
                            "~/quicklisp/slime-helper.el"))))
    (load quicklisp-slime-helper)
  (message "%s" "SLIME is not installed. Use Quicklisp to install it."))

(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
        (ccl ("ccl"))))

(setq slime-default-lisp 'sbcl)

(defun basis/start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(add-hook 'slime-mode-hook 'basis/start-slime)

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

;; redshank --------------------------------------------------------------------

(require 'redshank-loader)

(after-load 'redshank-loader
  (redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t))

;; clojure ---------------------------------------------------------------------

(defun basis/init-clojure-mode ()
  (subword-mode)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-h m"))

(defun basis/init-cider-repl-mode ()
  (subword-mode)
  (ac-nrepl-setup)
  (cider-turn-on-eldoc-mode))

(defun basis/init-cider-mode ()
  (ac-nrepl-setup)
  (cider-turn-on-eldoc-mode))

(defun basis/setup-lein-path-for-mac ()
  (-when-let (lein (executable-find "lein"))
    (setq cider-lein-command lein)
    (when (s-starts-with? "lein" cider-server-command)
      (setq cider-server-command
            (s-replace "lein" cider-lein-command cider-server-command)))))

(defun basis/setup-lein-path-for-cygwin ()
  (let ((lein "/home/jbm/bin/lein"))
    (when (file-exists-p lein)
      (setq cider-lein-command lein)
      (when (s-starts-with? "lein" cider-server-command)
        (setq cider-server-command
              (s-replace "lein" cider-lein-command cider-server-command))))))

(defvar basis/clojure-indent-specs
  '((-> 1)
    (->> 1)
    (cond-> 1)
    (cond->> 1)
    (some-> 1)
    (some->> 1)
    (add-watch 2)
    (match 1)
    (query 1)
    (symbol-macrolet 1)
    (defsymbolmacro defun)
    (with-symbol-macros defun)
    (run 2)
    (run* 1)
    (fresh 1))
  "Additional form indentation settings for `clojure-mode'.")

(after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'basis/init-clojure-mode)

  ;; Indentation tweaks
  (dolist (spec basis/clojure-indent-specs)
    (put-clojure-indent (car spec) (cadr spec)))
  (put 'macrolet 'clojure-backtracking-indent '((2) 2))

  ;; Command completion for lein in eshell
  (require 'pcmpl-lein))

(after-load 'cider
  (cond ((eq system-type 'darwin)
         (basis/setup-lein-path-for-mac))
        (basis/cygwin-p
         (basis/setup-lein-path-for-cygwin)))

  (add-hook 'cider-repl-mode-hook 'basis/init-cider-repl-mode)
  (add-hook 'cider-mode-hook 'basis/init-cider-mode)

  (setq cider-repl-use-pretty-printing t)

  (global-set-key (kbd "<f10>") 'cider-selector)

  (define-key cider-repl-mode-map (kbd "RET") 'cider-repl-return)

  (basis/define-keys cider-mode-map
    ((kbd "<f5>")    'cider-eval-last-expression)
    ((kbd "<f6>")    'basis/cider-eval-something)
    ((kbd "<f7>")    'cider-macroexpand-1)
    ((kbd "<M-f7>")  'cider-macroexpand-all)
    ((kbd "<f8>")    'cider-eval-buffer)
    ((kbd "<M-f8>")  'cider-load-current-buffer)
    ((kbd "C-c C-d") 'ac-nrepl-popup-doc)))

;; scheme ----------------------------------------------------------------------

(setq quack-default-program
      (if (eq system-type 'windows-nt)
          "racket"
        "guile"))

(require 'quack)

(after-load 'scheme
  (basis/define-keys scheme-mode-map
    ((kbd "<f5>")   'scheme-send-last-sexp)
    ((kbd "<f6>")   'basis/scheme-send-something)
    ((kbd "<C-f6>") 'scheme-compile-definition-and-go)
    ((kbd "<f8>")   'scheme-compile-file)
    ((kbd "<C-f8>") 'scheme-load-file)))

(defun basis/geiser-map-keys ()
  ;; Can't do this until the REPL is started because otherwise
  ;; `geiser-mode-map' is null.
  (basis/define-keys geiser-mode-map
    ((kbd "<f5>")   'geiser-eval-last-sexp)
    ((kbd "<f6>")   'basis/geiser-eval-something)
    ((kbd "<C-f6>") 'basis/geiser-eval-something-and-go)
    ((kbd "<f7>")   'basis/geiser-expand-something)
    ((kbd "<C-f7>") 'geiser-expand-definition)
    ((kbd "<f8>")   'geiser-eval-buffer)
    ((kbd "<C-f8>") 'geiser-eval-buffer-and-go)))

(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'basis/geiser-map-keys)

;; smartparens -----------------------------------------------------------------

(defadvice sp--cleanup-after-kill (around restrict-python-cleanup activate)
  "Smartparens sometimes kills too much whitespace in but I
haven't looked into the root cause yet."
  (unless (and (memq major-mode '(python-mode sql-mode))
               (looking-back "^[[:space:]]*"))
    ad-do-it))

(smartparens-global-strict-mode)

(after-load 'smartparens
  ;; I still prefer Paredit with lisps
  (dolist (mode basis/lisp-modes)
    (add-to-list 'sp-ignore-modes-list mode))

  (sp-use-paredit-bindings)

  ;; Don't insert single quotes as pairs in natural language-oriented modes.
  ;; They're apostrophes too often for it to be useful.
  (let ((natural-language-modes '(org-mode
                                  markdown-mode
                                  gfm-mode
                                  rst-mode
                                  html-mode
                                  text-mode
                                  mu4e-compose-mode
                                  git-commit-mode)))
    (sp-local-pair natural-language-modes
                   "'" nil :actions '(:rem insert)))

  ;; Also ignore single quotes in strings and comments since these are, again,
  ;; very likely apostrophes.
  (sp-pair "'"
           nil
           :unless '(sp-in-comment-p sp-in-string-p)
           :actions '(insert wrap autoskip))

  (basis/define-keys sp-keymap
    ((kbd "RET")    'basis/maybe-electric-return)
    ((kbd "M-DEL")  'basis/sp-backward-kill-something)
    ((kbd "M-k")    'sp-kill-sexp)
    ((kbd "M-e")    'basis/maybe-sp-forward-sexp)
    ((kbd "M-a")    'basis/maybe-sp-backward-sexp)
    ((kbd "]")      'sp-up-sexp)
    ((kbd "M-]")    'basis/insert-right-bracket)))

;; flycheck --------------------------------------------------------------------

(defvar basis/flycheck-keymap nil
  "Custom keybindings for Flycheck.")

(after-load 'flycheck
  (setq flycheck-check-syntax-automatically nil)

  (when (eq system-type 'windows-nt)
    (setq flycheck-xml-parser 'flycheck-parse-xml-region))

  ;; Check buffers with errors more frequently than ones without
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'basis/adjust-flycheck-idle-change-delay)

  ;; Initialize the keymap
  (setq basis/flycheck-keymap
        (let ((map (make-sparse-keymap)))
          (basis/define-keys map
            ((kbd "c") 'flycheck-buffer)
            ((kbd "n") 'flycheck-next-error)
            ((kbd "p") 'flycheck-previous-error)
            ((kbd "l") 'flycheck-list-errors)
            ((kbd "s") 'flycheck-select-checker)
            ((kbd "C") 'flycheck-clear))
          map))
  (global-set-key (kbd "C-h l") basis/flycheck-keymap)

  ;; Keys for the errors buffer
  (basis/define-keys flycheck-error-list-mode-map
    ((kbd "n")   'compilation-next-error)
    ((kbd "p")   'compilation-previous-error)
    ((kbd "M-n") 'next-error)
    ((kbd "M-p") 'previous-error)))

;; python ----------------------------------------------------------------------

(after-load 'python
  (basis/define-keys python-mode-map
    ((kbd "<f6>")        'basis/python-send-something)
    ((kbd "<f8>")        'python-shell-send-buffer)
    ((kbd "<M-f8>")      'python-shell-send-file)
    ((kbd "DEL")         'basis/sp-python-backspace)))

(when (basis/jedi-installed-p)
  ;; Jedi has 2 Python dependencies: jedi and epc
  (setq jedi:setup-keys t
        jedi:tooltip-method nil)
  (add-hook 'python-mode-hook 'jedi:setup))

(defun basis/init-python-mode ()
  (set (make-variable-buffer-local 'whitespace-line-column) 79)
  (unless (and buffer-file-name
               (file-remote-p buffer-file-name))
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
  (js2r-add-keybindings-with-prefix "C-h C-j")
  (define-key js2-mode-map (kbd "C-;") 'basis/eol-maybe-semicolon))

(defun basis/init-js2-mode ()
  (subword-mode 1)
  (ac-js2-mode 1)
  (js2-imenu-extras-setup))

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

(defun basis/sql-set-product ()
  (sql-set-product "postgres"))

(add-hook 'sql-mode-hook 'basis/sql-set-product)

(after-load 'sql
  ;; But I also work with other products and it's often easier not to switch
  ;; `sql-product' around.
  (let ((more-builtins '("elsif" "endif")))
    (add-to-list
     'sql-mode-postgres-font-lock-keywords
     (apply #'sql-font-lock-keywords-builder
            'font-lock-builtin-face nil more-builtins))))

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
(setq org-directory (expand-file-name "~/Dropbox/org/")
      org-default-notes-file (expand-file-name "refile.org" org-directory)
      org-archive-location "%s.archive::"
      org-agenda-files (--map (expand-file-name it org-directory)
                              '("todo.org" "work.org")))

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

(after-load 'sgml-mode
  (require 'tagedit)
  (require 'simplezen)
  (basis/define-keys html-mode-map
    ([remap forward-paragraph]  'basis/move-to-next-blank-line)
    ([remap backward-paragraph] 'basis/move-to-previous-blank-line)
    ((kbd "RET")                'basis/html-newline-and-indent)
    ((kbd "M-RET")              'basis/html-multiline-expand)
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
    (turn-on-auto-fill))
  (basis/maybe-enable-flyspell))

(add-hook 'markdown-mode-hook 'basis/init-markdown-mode)

(after-load 'markdown-mode
  (basis/define-keys markdown-mode-map
    ((kbd "DEL")         'basis/sp-markdown-backspace)
    ((kbd "M-n")         'forward-paragraph)
    ((kbd "M-p")         'backward-paragraph)
    ((kbd "C-c r")       'markdown-insert-reference-link-dwim)
    ((kbd "C-c C-r")     'markdown-insert-reference-link-dwim)))

;; deft ------------------------------------------------------------------------

(setq deft-extension "md"
      deft-directory "~/Dropbox/deft"
      deft-text-mode  'gfm-mode)


;;; init.el ends here
