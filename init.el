;;; init.el        -*- coding: utf-8; lexical-binding: t -*-

(eval-when-compile (require 'cl-lib))

;; Disable superfluous UI immediately to prevent momentary display
(mapc (lambda (mode)
        (when (fboundp mode)
          (funcall mode -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))

;; Make sure some directories exist
(dolist (dir '("var/" "tmp/"))
  (let ((path (expand-file-name dir "~/.emacs.d/")))
    (unless (file-exists-p path)
      (make-directory path))))

;; Set up the load path
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; So e.g. `find-function' works on C functions
(let ((source (format "~/src/emacs/emacs-%s/" emacs-version)))
  (when (file-directory-p source)
    (setq source-directory source)))

;; package ---------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Make sure every archive is present in the elpa/archives/ directory
(let* ((archive-root "~/.emacs.d/elpa/archives/")
       (archive-dirs (mapcar (lambda (archive)
                               (let ((name (car archive)))
                                 (expand-file-name name archive-root)))
                             package-archives)))
  (unless (cl-loop for dir in archive-dirs
                   always (file-exists-p dir))
    (package-refresh-contents)))

;; Ensure that everything specified here is installed
(let* ((required-packages
        '(ace-jump-mode
          ace-window
          ack-and-a-half
          batch-mode
          browse-kill-ring
          cider
          cider-browse-ns
          clj-refactor
          clojure-cheatsheet
          clojure-mode
          company
          dash
          dash-functional
          deft
          diminish
          dired+
          direx
          discover
          elfeed
          elisp-slime-nav
          exec-path-from-shell
          expand-region
          flx-ido
          flycheck
          geiser
          gist
          git-timemachine
          guide-key
          haskell-mode
          helm
          helm-projectile
          ibuffer-vc
          idle-highlight-mode
          ido-at-point
          ido-ubiquitous
          ido-vertical-mode
          idomenu
          jedi
          js-comint
          js2-mode
          js2-refactor
          jump-char
          key-chord
          leuven-theme
          magit
          markdown-mode
          move-text
          multiple-cursors
          page-break-lines
          paredit
          projectile
          pyvenv
          redshank
          rust-mode
          s
          simplezen
          skewer-mode
          slime
          slime-company
          smartparens
          smex
          ssh-config-mode
          tagedit
          undo-tree
          writegood-mode
          yaml-mode
          yasnippet
          ))
       (install-packages
        (cl-loop for pkg in required-packages
                 unless (package-installed-p pkg) collect pkg)))
  (when install-packages
    (package-refresh-contents)
    (mapc #'package-install install-packages)))

;; load some code --------------------------------------------------------------

(require 'pcase)
(require 'subr-x)
(require 'dash)
(require 'dash-functional)
(require 's)

(load "~/.emacs.d/defuns.el")

;; Why not? Idea taken from `dash-enable-font-lock', although I think that
;; overdoes it a bit
(basis/add-elisp-font-lock-keywords '("pcase-dolist"
                                      "->"
                                      "->>"
                                      "-->"
                                      "-when-let"
                                      "-when-let*"
                                      "--when-let"
                                      "-if-let"
                                      "-if-let*"
                                      "--if-let"
                                      "-let*"
                                      "-let"
                                      "-lambda"))

;; os x ------------------------------------------------------------------------

;; A graphical Emacs on OS X doesn't automatically inherit $PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; cygwin ----------------------------------------------------------------------

(defvar basis/cygwin-p (and (eq system-type 'windows-nt)
                            (directory-files "c:/" nil "Cygwin")
                            (file-directory-p "c:/bin"))
  "True if this is a Windows system with Cygwin installed.")

(defun basis/init-for-cygwin ()
  (require 'cygwin-mount)
  (let* ((dirs (->> '("/bin" "/usr/bin" "/Python27" "/Python27/Scripts")
                 (-filter #'file-directory-p)))
         (home (getenv "HOME"))
         (home/bin (when home
                     (concat (basis/windows->unix home)
                             "/bin")))
         (jdk-path (let* ((regexp (regexp-quote "Java\\jdk"))
                          (dir (-first (lambda (dir)
                                         (string-match-p regexp dir))
                                       (split-string (getenv "PATH") ";"))))
                     (when dir (basis/windows->unix dir)))))
    (when (and home (file-directory-p home))
      (cd home))
    (dolist (path (list jdk-path home/bin))
      (when (and path (file-directory-p path))
        (push path dirs)))
    ;; Set paths
    (setenv "PATH" (mapconcat #'identity dirs ":"))
    (setq exec-path (mapcar (lambda (dir) (concat "c:" dir)) dirs))
    ;; Use zsh or bash as shell
    (let ((shell (or (executable-find "zsh")
                     (executable-find "bash"))))
      (setq shell-file-name shell
            explicit-shell-file-name shell
            ediff-shell shell
            null-device "/dev/null")
      (setenv "SHELL" shell))
    ;; Since Emacs wasn't launched from a Cygwin shell, $LANG will be wonky
    ;; unless we fix it.
    (setenv "LANG" "en_US.UTF-8")))

(when basis/cygwin-p
  (basis/init-for-cygwin))

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
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      line-number-mode t
      column-number-mode t
      imenu-auto-rescan t
      next-line-add-newlines t
      apropos-do-all t
      custom-file "~/.emacs.d/custom.el"
      scroll-preserve-screen-position t
      delete-by-moving-to-trash t
      server-auth-dir "~/.emacs.d/var/server/"
      bookmark-default-file "~/.emacs.d/var/bookmarks"
      url-configuration-directory "~/.emacs.d/var/url/"
      load-prefer-newer t)

(setq-default indent-tabs-mode nil
              fill-column 80
              truncate-lines t
              require-final-newline t)

(when (file-exists-p custom-file)
  (load custom-file))

;; Enable some miscellaneous helpful modes
(size-indication-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(auto-compression-mode 1)
(delete-selection-mode 1)
(global-page-break-lines-mode 1)

;; Safe local variables
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
  (when (and (eq system-type 'windows-nt)
             buffer-file-name
             (or (string-match-p "\\`/sshx?:" buffer-file-name)
                 (string-match-p "\\`/plinkx?:" buffer-file-name)))
    (set-buffer-file-coding-system 'utf-8-unix)))

(when (eq system-type 'windows-nt)
  (add-hook 'before-save-hook 'basis/maybe-set-coding))

;; Backups, autosaves, and temporary files
(setq backup-by-copying t
      backup-directory-alist `((".*" . "~/.emacs.d/var/backups/"))
      auto-save-file-name-transforms `((".*" "~/.emacs.d/var/autosaves/" t))
      auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/.saves-"
      temporary-file-directory "~/.emacs.d/tmp/")

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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized/")
(load-theme 'solarized-dark t)

;; Don't italicize comments and docstrings
(dolist (face '(font-lock-comment-face
                font-lock-comment-delimiter-face
                font-lock-doc-face))
  (set-face-attribute face nil :slant 'normal))

(defun basis/get-frame-title ()
  "Return a frame title including the current project directory."
  (-if-let (file buffer-file-name)
      (concat (abbreviate-file-name file)
              (when (and (bound-and-true-p projectile-mode)
                         (projectile-project-p))
                (format " [%s]" (projectile-project-name))))
    "%b"))

(when (display-graphic-p)
  (setq frame-title-format
        '((:eval (basis/get-frame-title))))
  (-when-let (default-font
               (pcase system-type
                 (`gnu/linux  "Inconsolata-11")
                 (`darwin     "Andale Mono-12")
                 (`windows-nt "Consolas-10")))
    (set-face-attribute 'default nil :font default-font))
  (when (member "Symbola" (font-family-list))
    ;; (set-fontset-font t 'unicode "Symbola" nil 'prepend)
    (let ((symbola (font-spec :size 20 :name "Symbola")))
      (set-fontset-font "fontset-default" nil symbola))))

(pcase-dolist (`(,feature ,mode) '((elisp-slime-nav   elisp-slime-nav-mode)
                                   (eldoc             eldoc-mode)
                                   (undo-tree         undo-tree-mode)
                                   (redshank          redshank-mode)
                                   (whitespace        whitespace-mode)
                                   (guide-key         guide-key-mode)
                                   (clj-refactor      clj-refactor-mode)
                                   (projectile        projectile-mode)
                                   (page-break-lines  page-break-lines-mode)
                                   (magit             magit-auto-revert-mode)
                                   (subword           subword-mode)
                                   (superword         superword-mode)))
  (eval-after-load feature
    `(diminish ',mode)))

;; info ------------------------------------------------------------------------

(with-eval-after-load 'info
  (let ((info-path "~/.emacs.d/doc/info"))
    (when (file-exists-p info-path)
      (add-to-list 'Info-additional-directory-list info-path))))

;; uniquify --------------------------------------------------------------------

(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")

;; key bindings ----------------------------------------------------------------

(defun basis/init-modifiers-for-linux ()
  (define-key key-translation-map (kbd "<menu>") 'event-apply-hyper-modifier))

(defun basis/init-modifiers-for-os-x ()
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)
  ;; Use <kp-enter>, conveniently located to the right of the space
  ;; bar on my MBP, as a stand-in for mapping the <menu>/<apps> key on
  ;; PC keyboards to hyper.
  (define-prefix-command 'quasi-hyper)
  (global-set-key (kbd "<kp-enter>") 'quasi-hyper))

(defun basis/init-modifiers-for-windows ()
  (setq w32-pass-apps-to-system nil
        w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)
  (define-key key-translation-map (kbd "<apps>") 'event-apply-hyper-modifier))

(pcase system-type
  (`gnu/linux  (basis/init-modifiers-for-linux))
  (`darwin     (basis/init-modifiers-for-os-x))
  (`windows-nt (basis/init-modifiers-for-windows)))

;; C-c <left>/<right> to move backward/forward through window configurations
(winner-mode 1)

;; Newlines
(basis/define-keys global-map
  ("RET"          'newline-and-indent)
  ("S-RET"        'basis/open-line-below)
  ("<S-return>"   'basis/open-line-below)
  ("C-S-RET"      'basis/open-line-above)
  ("<C-S-return>" 'basis/open-line-above))

;; Whitespace
(global-set-key (kbd "M-\\") 'cycle-spacing)

;; Clever C-a
(global-set-key (kbd "C-a") 'basis/beginning-of-line-or-indentation)

;; Movement by sentence (I use the forward- and backward-sexp commands from
;; Paredit and Smartparens on M-a and M-e).
(global-set-key (kbd "C-M-f") 'forward-sentence)
(global-set-key (kbd "C-M-b") 'backward-sentence)

;; Movement by paragraph
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Kill stuff
(basis/define-keys global-map
  ("M-k"                   'kill-sexp)
  ("C-w"                   'basis/kill-region-or-backward-word)
  ("M-DEL"                 'basis/kill-region-or-backward-word)
  ("C-DEL"                 'basis/kill-region-or-backward-word)
  ("<C-backspace>"         'basis/kill-region-or-backward-word)
  ([remap kill-whole-line] 'basis/kill-line-backward)
  ("<C-delete>"            'basis/smart-kill-whole-line)
  ("<M-delete>"            'basis/smart-kill-almost-whole-line)
  ("ESC C-DEL"             'backward-kill-sexp)
  ("ESC M-DEL"             'backward-kill-sexp))

;; Copy stuff
(basis/define-keys global-map
  ("M-w"    'basis/kill-ring-save-something)
  ("<f2>"   'basis/clipboard-save-something)
  ("<M-f2>" 'basis/kill-ring-save-buffer)
  ("s-w"    'basis/kill-ring-save-indented))

;; Browse the kill ring with M-y
(browse-kill-ring-default-keybindings)

;; Join lines
(global-set-key (kbd "C-c C-j") 'basis/join-next-line)
(global-set-key (kbd "C-c j") 'basis/join-next-line)

;; Moves lines or regions
(global-set-key (kbd "<M-s-up>") 'move-text-up)
(global-set-key (kbd "<M-s-down>") 'move-text-down)

;; Transpose stuff with M-t
(define-prefix-command 'basis/transposition-map)
(global-set-key (kbd "M-t") 'basis/transposition-map)

(basis/define-keys basis/transposition-map
  ("l"   'transpose-lines)
  ("w"   'transpose-words)
  ("s"   'transpose-sexps)
  ("c"   'transpose-chars)
  ("M-w" 'basis/transpose-windows)
  ("M-s" 'basis/toggle-window-split))

;; More comfortable {next,previous}-error
(global-set-key (kbd "M-g M-f") 'next-error)
(global-set-key (kbd "M-g M-b") 'previous-error)

;; Grep
(global-set-key (kbd "<f9>") 'rgrep)
(global-set-key (kbd "<C-f9>") 'lgrep)

;; Occur
(define-key occur-mode-map (kbd "n") 'occur-next)
(define-key occur-mode-map (kbd "p") 'occur-prev)
(global-set-key (kbd "<C-f2>") 'basis/multi-occur-this-mode)

;; Mark commands
(global-set-key (kbd "C-`") 'basis/push-mark-no-activate)
(global-set-key (kbd "M-`") 'basis/jump-to-mark)

;; Expand-region
(global-set-key (kbd "M-=") 'er/expand-region)

;; Comment/uncomment stuff
(global-set-key (kbd "s-;") 'basis/comment-or-uncomment)
(global-set-key (kbd "C-c ;") 'basis/comment-or-uncomment)

;; Eval
(global-set-key (kbd "C-x C-e") 'basis/eval-last-sexp)
(global-set-key (kbd "C-c C-e") 'basis/eval-and-replace)

;; Eval expression on {meta,super}-hyper
(-when-let* ((template (pcase system-type
                         (`gnu/linux  "<%s-menu>")
                         (`darwin     "<%s-kp-enter>")
                         (`windows-nt "<%s-apps>")))
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
(basis/define-keys global-map
  ("M-e"       'forward-sexp)
  ("M-a"       'backward-sexp)
  ("<M-right>" 'forward-sexp)
  ("<M-left>"  'backward-sexp))

;; M-x shell
(global-set-key (kbd "C-c RET") 'shell)

;; Start eshell or switch to it if it's active
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; M-x without meta, when necessary
(global-set-key (kbd "C-c x") 'smex)
(global-set-key (kbd "C-c m") 'execute-extended-command)

;; recetf+ido
(global-set-key (kbd "C-x C-r") 'basis/ido-recentf)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including, the ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Change word case
(basis/define-keys global-map
  ("M-u" 'basis/upcase-something)
  ("M-l" 'basis/downcase-something)
  ("M-c" 'basis/capitalize-something))

;; Kill frames with C-x C-c
(global-set-key (kbd "C-x C-c") 'basis/kill-frame-or-terminal)
;; Kill Emacs with M-x sayonara
(defalias 'sayonara 'save-buffers-kill-terminal)

;; Google stuff
(global-set-key (kbd "C-c g") 'basis/google)

;; Proced
(global-set-key (kbd "C-x p") 'proced)

;; C-v et al. are uncomfortable on my MBP
(basis/define-keys global-map
  ("<s-up>"     'scroll-up-command)
  ("<s-down>"   'scroll-down-command)
  ("<M-s-up>"   'scroll-other-window)
  ("<M-s-down>" 'scroll-other-window-down))

;; Previous/next buffer
(global-set-key (kbd "<C-prior>") 'previous-buffer)
(global-set-key (kbd "<C-next>") 'next-buffer)

;; h-map -----------------------------------------------------------------------

(define-prefix-command 'basis/h-map)

;; Note sure which will be better
(global-set-key (kbd "C-h") 'basis/h-map)
(global-set-key (kbd "M-h") 'basis/h-map)

(basis/define-keys basis/h-map
  ("C-k" 'basis/kill-this-buffer)
  ("i"   'idomenu)
  ("C-h" 'mark-paragraph))

;; find elisp map --------------------------------------------------------------

(define-prefix-command 'lisp-find-map)

(global-set-key (kbd "<f1> e") 'lisp-find-map)

(defun basis/scratch! ()
  "Switch to the scratch buffer, creating it if necessary."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*")))

(basis/define-keys lisp-find-map
  ("c" 'finder-commentary)
  ("e" 'view-echo-area-messages)
  ("f" 'find-function)
  ("F" 'find-face-definition)
  ("i" 'info-apropos)
  ("k" 'find-function-on-key)
  ("l" 'find-library)
  ("s" 'basis/scratch!)
  ("v" 'find-variable)
  ("V" 'apropos-value)
  ("a" 'helm-apropos))

;; eval map --------------------------------------------------------------------

(defvar-local basis/eval-defun-function nil
  "Function to eval the defun at point.")

(defvar-local basis/eval-last-sexp-function nil
  "Function to eval the sexp before point.")

(defvar-local basis/eval-region-function nil
  "Function to eval the active region.")

(defvar-local basis/eval-buffer-function nil
  "Function to eval the current buffer.")

(defvar-local basis/eval-file-function nil
  "Function to eval and load the current buffer's file.")

(defun basis/do-eval (thing)
  "Call an appropriate function to evaluate THING."
  (let* ((sym (intern (format "basis/eval-%s-function" thing)))
         (val (symbol-value sym)))
    (if val
        (call-interactively val)
      (error "No function to eval '%s' for %s" thing major-mode))))

(defmacro basis/def-eval-map (&rest defs)
  (cl-flet ((make-eval-name (thing)
              (intern (format "basis/do-eval-%s" thing)))
            (keyword->symbol (keyword)
              (intern (substring (symbol-name keyword) 1))))
    (let ((map (make-symbol "map")))
      `(defvar basis/eval-map
         (let ((,map (make-sparse-keymap)))
           ,@(mapcar (lambda (def)
                       (let ((thing (keyword->symbol (car def)))
                             (key (cadr def)))
                         `(progn
                            (defun ,(make-eval-name thing) ()
                              (interactive)
                              (basis/do-eval ',thing))
                            (define-key ,map ,key ',(make-eval-name thing)))))
                     (-partition 2 defs))
           ,map)))))

(basis/def-eval-map
 :last-sexp "l"
 :defun     "d"
 :buffer    "b"
 :region    "r"
 :file      "f")

;; tmux ------------------------------------------------------------------------

;; A number of non-alphanumeric keys don't work by default when Emacs is
;; running in tmux. This attempts to fix that by adding entries to the
;; `key-translation-map'.

;; `setw -g xterm-keys on` must be set in ~/.tmux.conf for this to work.

;; The code below is based on that in the ArchWiki page on Emacs but refactored
;; a bit.

;; TODO: <home> and <end> still don't work
(when (getenv "TMUX")
  (pcase-dolist (`(,n . ,k) '((2 . "S-")
                              (3 . "M-")
                              (4 . "M-S-")
                              (5 . "C-")
                              (6 . "C-S-")
                              (7 . "C-M-")
                              (8 . "C-M-S-")))
    (basis/define-key-translations
      ((format "M-[ 1 ; %d A" n)  (format "%s<up>" k))     ;; left arrow
      ((format "M-[ 1 ; %d B" n)  (format "%s<down>" k))   ;; down arrow
      ((format "M-[ 1 ; %d C" n)  (format "%s<right>" k))  ;; right arrow
      ((format "M-[ 1 ; %d D" n)  (format "%s<left>" k))   ;; left arrow
      ((format "M-[ 1 ; %d H" n)  (format "%s<home>" k))   ;; home
      ((format "M-[ 1 ; %d F" n)  (format "%s<end>" k))    ;; end
      ((format "M-[ 5 ; %d ~" n)  (format "%s<prior>" k))  ;; page up
      ((format "M-[ 6 ; %d ~" n)  (format "%s<next>" k))   ;; page down
      ((format "M-[ 2 ; %d ~" n)  (format "%s<delete>" k)) ;; insert
      ((format "M-[ 3 ; %d ~" n)  (format "%s<delete>" k)) ;; delete
      ((format "M-[ 1 ; %d P" n)  (format "%s<f1>" k))     ;; f1
      ((format "M-[ 1 ; %d Q" n)  (format "%s<f2>" k))     ;; f2
      ((format "M-[ 1 ; %d R" n)  (format "%s<f3>" k))     ;; f3
      ((format "M-[ 1 ; %d S" n)  (format "%s<f4>" k))     ;; f4
      ((format "M-[ 15 ; %d ~" n) (format "%s<f5>" k))     ;; f5
      ((format "M-[ 17 ; %d ~" n) (format "%s<f6>" k))     ;; f6
      ((format "M-[ 18 ; %d ~" n) (format "%s<f7>" k))     ;; f7
      ((format "M-[ 19 ; %d ~" n) (format "%s<f8>" k))     ;; f8
      ((format "M-[ 20 ; %d ~" n) (format "%s<f9>" k))     ;; f9
      ((format "M-[ 21 ; %d ~" n) (format "%s<f10>" k))    ;; f10
      ((format "M-[ 23 ; %d ~" n) (format "%s<f11>" k))    ;; f11
      ((format "M-[ 24 ; %d ~" n) (format "%s<f12>" k))    ;; f12
      ((format "M-[ 25 ; %d ~" n) (format "%s<f13>" k))    ;; f13
      ((format "M-[ 26 ; %d ~" n) (format "%s<f14>" k))    ;; f14
      ((format "M-[ 28 ; %d ~" n) (format "%s<f15>" k))    ;; f15
      ((format "M-[ 29 ; %d ~" n) (format "%s<f16>" k))    ;; f16
      ((format "M-[ 31 ; %d ~" n) (format "%s<f17>" k))    ;; f17
      ((format "M-[ 32 ; %d ~" n) (format "%s<f18>" k))    ;; f18
      ((format "M-[ 33 ; %d ~" n) (format "%s<f19>" k))    ;; f19
      ((format "M-[ 34 ; %d ~" n) (format "%s<f20>" k))))) ;; f20

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

;; recentf ---------------------------------------------------------------------

(recentf-mode 1)

(setq recentf-max-saved-items 50
      recentf-save-file "~/.emacs.d/var/recentf"
      recentf-exclude '(file-remote-p basis/git-commit-msg-p))

;; TRAMP -----------------------------------------------------------------------

(setq tramp-default-method
      (if (and (eq system-type 'windows-nt)
               (not basis/cygwin-p))
          "plinkx"
        "sshx"))

(setq tramp-persistency-file-name "~/.emacs.d/var/tramp/")

;; Have TRAMP use Cygwin's sh rather than Windows's cmd.exe
(when basis/cygwin-p
  (setq tramp-encoding-shell (executable-find "sh")
        tramp-encoding-command-switch "-c"
        tramp-encoding-command-interactive "-i"))

;; help-mode -------------------------------------------------------------------

(with-eval-after-load 'help-mode
  (basis/define-keys help-mode-map
    ("n" 'next-line)
    ("p" 'previous-line)
    ("b" 'help-go-back)
    ("f" 'help-go-forward)))

;; man and woman ---------------------------------------------------------------

;; The man.el library needs to be loaded before making man an alias for woman
;; (below). Otherwise, when woman loads man it will overwrite the alias.
(require 'man)

;; So I can still get to the original man command
(fset 'original-man (symbol-function 'man))

;; Easier to type
(defalias 'man 'woman)

;; isearch ---------------------------------------------------------------------

;; Regexp search by default
(basis/define-keys global-map
  ("C-s"   'isearch-forward-regexp)
  ("C-r"   'isearch-backward-regexp)
  ("C-M-s" 'isearch-forward)
  ("C-M-r" 'isearch-backward))

;; Like C-w but the current region or symbol
(define-key isearch-mode-map (kbd "C-t") 'basis/isearch-yank-something)

;; ispell ----------------------------------------------------------------------

(let ((aspell (or (executable-find "aspell")
                  (and (eq system-type 'windows-nt)
                       (executable-find
                        "c:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")))))
  (setq ispell-program-name aspell
        ispell-personal-dictionary "~/.aspell.en.pws"))

;; prog-mode -------------------------------------------------------------------

(add-hook 'prog-mode-hook 'basis/maybe-enable-whitespace-mode)
(add-hook 'prog-mode-hook 'basis/maybe-enable-flyspell-prog-mode)

;; diff-mode -------------------------------------------------------------------

(with-eval-after-load 'diff-mode
  ;; `diff-goto-source' is still available on C-c C-c.
  (define-key diff-mode-map (kbd "M-o") nil))

;; discover-mode ---------------------------------------------------------------

(global-discover-mode 1)

(discover-add-context-menu
 :context-menu (assq 'isearch discover-context-menus)
 :mode nil
 :mode-hook nil
 :bind "C-c s")

;; guide-key -------------------------------------------------------------------

(guide-key-mode 1)

(setq guide-key/popup-window-position 'bottom
      guide-key/idle-delay 0.0)

(setq guide-key/guide-key-sequence
      '("C-x 4" "C-x v" "C-x 8"
        (dired-mode "*" "C-t")
        (ibuffer-mode "/" "*" "%" "M-s" "M-s a")
        (calc-mode "V")))

;; key-chord -------------------------------------------------------------------

(key-chord-mode 1)

(pcase-dolist (`(,key . ,cmd) '(("jk" . ace-jump-word-mode)
                                (",." . ido-switch-buffer)
                                ("l;" . ibuffer)
                                ("`1" . delete-other-windows)
                                ("0-" . delete-window)
                                ("j2" . split-window-below)
                                ("j3" . split-window-right)
                                (" 9" . "(")))
  (key-chord-define-global key cmd))

(defun basis/define-more-bracket-chords (&optional keymap)
  (interactive)
  (let ((keymap (or keymap global-map)))
    (key-chord-define keymap " [" "{")
    (key-chord-define keymap " ," "<")))

;; undo-tree -------------------------------------------------------------------

(global-undo-tree-mode 1)

;; The couple bindings undo-tree puts behind C-x r prevent discover.el's C-x r
;; context menu from working
(define-key undo-tree-map (kbd "C-x r") nil)

;; multiple-cursors ------------------------------------------------------------

(basis/define-keys global-map
  ("M-]" 'mc/mark-next-like-this)
  ("C->" 'mc/mark-next-like-this)
  ("C-<" 'mc/mark-previous-like-this))

(setq mc/list-file "~/.emacs.d/var/mc-lists.el")

(with-eval-after-load 'multiple-cursors-core
  (add-to-list 'mc/temporarily-disabled-minor-modes 'idle-highlight-mode)
  ;; Make RET exit multiple-cursors-mode in the terminal too
  (define-key mc/keymap (kbd "RET") 'multiple-cursors-mode))

;; saveplace -------------------------------------------------------------------

(require 'saveplace)

(setq-default save-place t)

(setq save-place-file "~/.emacs.d/var/places")

;; savehist --------------------------------------------------------------------

(require 'savehist)

(setq savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/var/history")

(savehist-mode t)

;; ace-jump-mode ---------------------------------------------------------------

(with-eval-after-load 'ace-jump-mode
  (ace-jump-mode-enable-mark-sync))

(basis/define-keys global-map
  ("M-SPC"   'ace-jump-mode)
  ("s-SPC"   'ace-jump-mode)
  ("C-h SPC" 'ace-jump-mode-pop-mark))

;; ace-window ------------------------------------------------------------------

(global-set-key (kbd "M-o") 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; mu4e ------------------------------------------------------------------------

(autoload 'mu4e "mu4e" "Launch mu4e" t nil)

;; Mail
(setq mu4e-get-mail-command "offlineimap"
      mu4e-maildir "/home/jbm/.maildir/fastmail"
      mu4e-sent-folder "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder "/trash"
      mu4e-compose-signature "jbm")

;; Shortcuts. Available as jX
(setq mu4e-maildir-shortcuts '(("/archive" . ?a)
                               ("/inbox"   . ?i)
                               ("/sent"    . ?s)))

;; Addresses to consider "me" when searching
(setq mu4e-user-mail-address-list '("jbm@jbm.io"
                                    "jbm@deft.li"
                                    "jbm@fastmail.fm"
                                    "jbm@mailforce.net"))

;; Convert HTML->text if no text version is available
(setq mu4e-html2text-command (if (executable-find "html2text")
                                 "html2text -utf8 -width 72"
                               #'html2text))

;; Where to save attachments
(let ((dir (-first #'file-directory-p '("~/downloads" "~/Downloads" "~/"))))
  (setq mu4e-attachment-dir dir))

;; Composing messages
(setq mu4e-reply-to-address "jbm@jbm.io"
      user-mail-address "jbm@jbm.io"
      user-full-name "John Mastro"
      message-signature "jbm"
      mu4e-sent-messages-behavior 'delete    ;; they're saved on the server
      message-kill-buffer-on-exit t)

;; SMTP
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtp-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-user "jbm@fastmail.fm"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

(with-eval-after-load 'mu4e
  (add-hook 'mu4e-compose-mode-hook 'basis/maybe-enable-flyspell)
  (add-hook 'mu4e-compose-mode-hook 'turn-on-orgstruct))

;; elfeed ----------------------------------------------------------------------

(setq elfeed-db-directory "~/.emacs.d/var/elfeed/")

(when (file-exists-p "~/.emacs.d/feeds.el")
  (setq elfeed-feeds (basis/elfeed-load-feeds "~/.emacs.d/feeds.el")))


;; w3m -------------------------------------------------------------------------

(when (and (not (display-graphic-p))
           (executable-find "w3m"))
  (setq browse-url-browser-function 'w3m-browse-url))

(with-eval-after-load 'w3m
  (define-key w3m-mode-map "n" 'w3m-next-anchor)
  (define-key w3m-mode-map "p" 'w3m-previous-anchor))

;; magit -----------------------------------------------------------------------

(global-set-key (kbd "<f10>") 'magit-status)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun basis/magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun basis/init-magit-log-edit ()
  (turn-on-auto-fill)
  (setq fill-column 72))

(add-hook 'magit-log-edit-mode-hook 'basis/init-magit-log-edit)

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "q") 'basis/magit-quit-session)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-repo-dirs '("~/code/"))
  ;; Tell magit where to find emacsclientw.exe on Windows
  (when (eq system-type 'windows-nt)
    (let ((exe (format "~/emacs/emacs-%s/bin/emacsclientw.exe"
                       emacs-version)))
      (when (file-exists-p exe)
        (setq magit-emacsclient-executable exe)))))

;; ibuffer ---------------------------------------------------------------------

(defalias 'ls 'ibuffer)

(global-set-key [remap list-buffers] 'ibuffer)

(with-eval-after-load 'ibuffer
  (require 'ibuffer-vc)
  (basis/define-keys ibuffer-mode-map
    ("M-o"   nil) ;; don't shadow ace-window
    ("C-M-o" 'ibuffer-visit-buffer-1-window)
    ("\\"    'basis/ibuffer-toggle-vc-grouping))

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
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(setq ibuffer-show-empty-filter-groups nil)

;; company ---------------------------------------------------------------------

(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (defadvice company-auto-begin (around no-freeze activate)
    ;; Kludge to work around a problem I haven't figured out yet
    (unless (and (eq major-mode 'python-mode)
                 (basis/in-string-p))
      ad-do-it))
  (basis/define-keys company-active-map
    ("C-t"    'company-complete-selection)
    ("C-c"    'company-abort)
    ("C-l"    'company-show-location)
    ("C-w"    nil)
    ("RET"    nil)
    ([return] nil))
  (set-default
   (make-variable-buffer-local 'company-backends)
   '(company-capf
     (company-dabbrev-code company-gtags company-etags company-keywords)
     company-files
     company-dabbrev))
  (setq company-minimum-prefix-length 2
        company-tooltip-flip-when-above t)
  (with-eval-after-load 'cc-mode
    (-when-let (args (basis/build-clang-args 'c))
      (require 'company-clang)
      (setq company-clang-arguments args)
      (add-hook 'c-mode-hook 'basis/enable-company-clang))))

;; dired -----------------------------------------------------------------------

(with-eval-after-load 'dired
  (require 'dired+)
  (require 'find-dired)
  (basis/define-keys dired-mode-map
    ("RET"                       'dired-find-alternate-file)
    ("M-RET"                     'dired-find-file)
    ("e"                         'basis/dired-open-files)
    ("-"                         'diredp-up-directory-reuse-dir-buffer)
    ("^"                         'diredp-up-directory-reuse-dir-buffer)
    ("M-^"                       'diredp-up-directory)
    ("M-m"                       'dired-omit-mode)
    ("M-n"                       'diredp-next-subdir)
    ("M-p"                       'diredp-prev-subdir)
    ("M-e"                       'dired-next-dirline)
    ("M-a"                       'dired-prev-dirline)
    ("M-o"                       nil)
    ([remap beginning-of-buffer] 'basis/dired-jump-to-top)
    ([remap end-of-buffer]       'basis/dired-jump-to-bottom))
  (setq dired-omit-extensions (remove ".bak" dired-omit-extensions)
        dired-recursive-deletes 'top
        dired-listing-switches "-alh"
        find-ls-options '("-exec ls -ldh {} +" . "-ldh"))
  (put 'dired-find-alternate-file 'disabled nil))

(defadvice dired-omit-expunge (around maybe-omit-verbose activate)
  ;; I only want to see messages about files being omitted when I'm actually in
  ;; a Dired window. It's can be annoying to get one of those messages while
  ;; you're e.g. in the minibuffer because some buried Dired buffer has
  ;; auto-reverted.
  (cl-letf (((symbol-value 'dired-omit-verbose)
             (with-current-buffer (window-buffer (selected-window))
               (eq major-mode 'dired-mode))))
    ad-do-it))

(add-hook 'dired-mode-hook 'dired-omit-mode)

(autoload 'dired-jump "dired-x"
  "Jump to dired buffer corresponding to current buffer." t)

(global-set-key (kbd "C-h C-j") 'dired-jump)

;; direx -----------------------------------------------------------------------

(global-set-key (kbd "C-h j") 'direx:jump-to-directory)
(global-set-key (kbd "C-h p C-j") 'basis/direx-jump-to-project-root)

(with-eval-after-load 'direx
  (define-key direx:direx-mode-map (kbd "M-n") 'direx:next-sibling-item)
  (define-key direx:direx-mode-map (kbd "M-p") 'direx:previous-sibling-item))

;; shell-mode ------------------------------------------------------------------

;; Not sure why comint/dirtrack see junk in front of my prompt with Cygwin's
;; zsh, so just work around it
(setq-default dirtrack-list
              (if basis/cygwin-p
                  '("^%[ \r]*\\(.+\\)>" 1)
                '("^[^:\n]+@[^:\n]+:\\(.+\\)>" 1)))

(defun basis/init-shell-mode ()
  (setq comint-process-echoes t)
  (shell-dirtrack-mode -1)
  (dirtrack-mode +1))

(add-hook 'shell-mode-hook 'basis/init-shell-mode)

;; eshell ----------------------------------------------------------------------

(setq eshell-directory-name "~/.emacs.d/var/eshell/")

(defun basis/init-eshell ()
  (basis/define-keys eshell-mode-map
    ("S-DEL"   'basis/eshell-kill-line-backward)
    ("C-S-DEL" 'basis/eshell-kill-whole-line)))

(add-hook 'eshell-mode-hook 'basis/init-eshell)

;; sh-mode ---------------------------------------------------------------------

(defun basis/maybe-set-shell-to-zsh ()
  (setq tab-width 4)
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) ".zshrc"))
    (sh-set-shell "zsh")))

(add-hook 'sh-mode-hook 'basis/maybe-set-shell-to-zsh)

;; ido -------------------------------------------------------------------------

(require 'flx-ido)

(ido-mode 1)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-use-virtual-buffers t
      ido-use-faces nil
      ido-max-prospects 10
      ido-ignore-extensions t
      ido-save-directory-list-file "~/.emacs.d/var/ido.last")

(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(ido-at-point-mode)

;; For ido-powered completion at point. Need a better key binding for this.
(global-set-key (kbd "C-c TAB") 'completion-at-point)
(global-set-key (kbd "C-c <C-tab>") 'completion-at-point)

(unless (eq system-type 'darwin)
  ;; No offense to OS X; my Mac is just old
  (setq flx-ido-threshhold 10000))

(defun basis/init-ido-keys ()
  (basis/define-keys ido-file-completion-map
    ("C-w"   'ido-delete-backward-updir)
    ("M-w"   'ido-copy-current-file-name)
    ("C-M-e" 'basis/ido-sort-files-by-modtime)))

(add-hook 'ido-setup-hook 'basis/init-ido-keys)

;; smex ------------------------------------------------------------------------

(setq smex-save-file "~/.emacs.d/var/smex-items")

(basis/define-keys global-map
  ("M-x"     'smex)
  ("M-X"     'smex-major-mode-commands)
  ("C-h M-x" 'execute-extended-command))

;; helm ------------------------------------------------------------------------

(define-prefix-command 'basis/helm-map)

(global-set-key (kbd "C-c h") 'basis/helm-map)

(basis/define-keys basis/helm-map
  ("a" 'helm-apropos)
  ("b" 'helm-buffers-list)
  ("c" 'helm-colors)
  ("e" 'helm-register)
  ("f" 'helm-find-files)
  ("g" 'helm-do-grep)
  ("i" 'helm-semantic-or-imenu)
  ("k" 'helm-man-woman)
  ("m" 'helm-all-mark-rings)
  ("o" 'helm-occur)
  ("O" 'helm-multi-occur)
  ("p" 'helm-list-emacs-process)
  ("r" 'helm-regexp)
  ("R" 'helm-resume)
  ("t" 'helm-top)
  ("y" 'helm-show-kill-ring)
  ("/" 'helm-find))

(setq helm-split-window-default-side 'other
      helm-split-window-in-side-p t
      helm-ff-file-name-history-use-recentf t
      helm-ff-search-library-in-sexp t
      helm-buffers-fuzzy-matching t
      helm-man-or-woman-function 'woman
      helm-quick-update t)

(with-eval-after-load 'helm
  (require 'helm-utils) ; For the `helm-selection-line' face
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action))

;; hippie expand ---------------------------------------------------------------

(global-set-key (kbd "M-/") 'hippie-expand)

(dolist (f '(try-expand-line try-expand-list try-expand-all-abbrevs))
  (setq hippie-expand-try-functions-list
        (delete f hippie-expand-try-functions-list)))

;; yasnippet -------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(with-eval-after-load 'yasnippet
  ;; Steal C-t for expanding snippets. `transpose-chars' is still available on
  ;; M-t c
  (basis/define-keys yas-minor-mode-map
    ("C-t"   'basis/yas-expand-or-insert)
    ("TAB"   nil)
    ([(tab)] nil))
  (define-key yas-keymap (kbd "RET") 'yas-exit-all-snippets)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/")
        yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
        yas-wrap-around-region t))

;; projectile ------------------------------------------------------------------

(setq projectile-keymap-prefix (kbd "C-h p")
      projectile-completion-system 'ido
      projectile-known-projects-file "~/.emacs.d/var/projectile-bookmarks.eld"
      projectile-cache-file "~/.emacs.d/var/projectile.cache")

;; Projectile defaults to native indexing on Windows, but if we have Cygwin
;; set up we can use "alien".
(if (and (eq system-type 'windows-nt)
         (not basis/cygwin-p))
    (setq projectile-indexing-method 'native
          projectile-enable-caching t)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching nil))

(projectile-global-mode)

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

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

(defun basis/init-lisp-generic ()
  "Enable features useful in any Lisp mode."
  (paredit-mode +1))

(let ((lispy-hooks '(emacs-lisp-mode-hook
                     lisp-interaction-mode-hook
                     ielm-mode-hook
                     lisp-mode-hook
                     slime-repl-mode-hook
                     clojure-mode-hook
                     cider-repl-mode-hook
                     inferior-lisp-mode-hook
                     scheme-mode-hook
                     inferior-scheme-mode-hook
                     geiser-repl-mode-hook)))
  (dolist (hook lispy-hooks)
    (add-hook hook 'basis/init-lisp-generic)))

(setq lisp-lambda-list-keyword-alignment t
      lisp-lambda-list-keyword-parameter-alignment t
      lisp-loop-forms-indentation 6)

;; emacs lisp ------------------------------------------------------------------

(defun basis/set-up-hippie-expand-for-elisp ()
  "Enable Lisp symbol completion in Hippie Expand."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially t))

(defun basis/init-emacs-lisp-modes ()
  "Enable features useful when working with Emacs Lisp."
  ;; Paredit is enabled by `basis/init-lisp-generic'
  (elisp-slime-nav-mode t)
  (basis/set-up-hippie-expand-for-elisp)
  (turn-on-eldoc-mode)
  ;; Normally `lexical-binding' should be set within a file, but that doesn't
  ;; work for *scratch* and *ielm*
  (when (member (buffer-name) '("*scratch*" "*ielm*"))
    (setq lexical-binding t)))

(defun basis/init-emacs-lisp-eval-map ()
  (setq basis/eval-defun-function 'eval-defun
        basis/eval-last-sexp-function 'eval-last-sexp
        basis/eval-region-function 'eval-region
        basis/eval-buffer-function 'eval-buffer))

(dolist (hook '(emacs-lisp-mode-hook
                lisp-interaction-mode-hook
                ielm-mode-hook))
  (add-hook hook 'basis/init-emacs-lisp-modes))

(dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
  (add-hook hook 'basis/init-emacs-lisp-eval-map))

(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (basis/define-keys map
    ("<f5>" 'basis/eval-last-sexp)
    ("<f6>" 'basis/eval-something)
    ("<f7>" 'basis/expand-sexp-at-point)
    ("<f8>" 'eval-buffer)))

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

(with-eval-after-load 'paredit
  (basis/define-keys paredit-mode-map
    ("["                            'basis/paredit-open-something)
    ("C-c ["                        'paredit-open-square)
    ("M-)"                          'basis/paredit-wrap-round-from-behind)
    ("M-e"                          'paredit-forward)
    ("<M-right>"                    'paredit-forward)
    ("M-a"                          'paredit-backward)
    ("<M-left>"                     'paredit-backward)
    ("M-k"                          'kill-sexp)
    ("C-w"                          'basis/paredit-kill-region-or-backward-word)
    ("M-DEL"                        'basis/paredit-kill-region-or-backward-word)
    ("C-DEL"                        'basis/paredit-kill-region-or-backward-word)
    ("<C-backspace>"                'basis/paredit-kill-region-or-backward-word)
    ([remap backward-kill-sentence] 'backward-kill-sexp))
  (add-to-list 'paredit-space-for-delimiter-predicates
               'basis/paredit-doublequote-space-p))

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

;; slime -----------------------------------------------------------------------

(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
        (ccl ("ccl"))))

(setq slime-default-lisp 'sbcl
      slime-contribs '(slime-fancy))

(defun basis/start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

;; (add-hook 'slime-mode-hook 'basis/start-slime)

(with-eval-after-load 'slime
  (basis/define-keys slime-mode-map
    ("<f5>"   'slime-eval-last-expression)
    ("<M-f5>" 'slime-eval-last-expression-in-repl)
    ("<C-f5>" 'slime-pprint-eval-last-expression)
    ("<f6>"   'basis/slime-eval-something)
    ("<M-f6>" 'slime-compile-defun)
    ("<C-f6>" 'slime-pprint-region)
    ("<f7>"   'slime-expand-1)
    ("<f8>"   'slime-compile-and-load-file))
  (global-set-key (kbd "<f12>") 'slime-selector)
  (setq slime-autodoc-use-multiline-p t))

;; redshank --------------------------------------------------------------------

(redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t)

;; clojure ---------------------------------------------------------------------

(defun basis/init-clojure-mode ()
  (subword-mode)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-h m"))

(defun basis/init-cider-repl-mode ()
  (subword-mode)
  (cider-turn-on-eldoc-mode))

(defun basis/init-cider-mode ()
  (setq basis/eval-defun-function 'cider-eval-defun-at-point
        basis/eval-last-sexp-function 'cider-eval-last-sexp
        basis/eval-buffer-function 'cider-eval-buffer
        basis/eval-region-function 'cider-eval-region
        basis/eval-file-function 'cider-eval-load-file)
  (cider-turn-on-eldoc-mode))

(defun basis/set-lein-command-for-mac ()
  (-when-let (lein (executable-find "lein"))
    (setq cider-lein-command lein)))

(defun basis/set-lein-command-for-cygwin ()
  (let ((lein "~/bin/lein"))
    (when (file-exists-p lein)
      (setq cider-lein-command (expand-file-name lein)))))

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

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'basis/init-clojure-mode)

  ;; Indentation tweaks
  (pcase-dolist (`(,sym ,n) basis/clojure-indent-specs)
    (put-clojure-indent sym n))
  (put 'macrolet 'clojure-backtracking-indent '((2) 2))

  ;; Command completion for lein in eshell
  (require 'pcmpl-lein))

(with-eval-after-load 'cider
  (cond ((eq system-type 'darwin)
         (basis/set-lein-command-for-mac))
        (basis/cygwin-p
         (basis/set-lein-command-for-cygwin)))

  (add-hook 'cider-repl-mode-hook 'basis/init-cider-repl-mode)
  (add-hook 'cider-mode-hook 'basis/init-cider-mode)

  (setq cider-repl-use-pretty-printing t
        nrepl-log-messages t)

  (global-set-key (kbd "<f11>") 'cider-selector)

  (define-key cider-repl-mode-map (kbd "RET") 'cider-repl-return)

  (basis/define-keys cider-mode-map
    ("<f5>"    'cider-eval-last-expression)
    ("<f6>"    'basis/cider-eval-something)
    ("<f7>"    'cider-macroexpand-1)
    ("<M-f7>"  'cider-macroexpand-all)
    ("<f8>"    'cider-eval-buffer)
    ("<M-f8>"  'cider-load-current-buffer)))

;; scheme ----------------------------------------------------------------------

(setq quack-default-program
      (if (eq system-type 'windows-nt)
          "racket"
        "guile"))

(with-eval-after-load 'scheme
  (require 'quack)
  (basis/define-keys scheme-mode-map
    ("<f5>"   'scheme-send-last-sexp)
    ("<f6>"   'basis/scheme-send-something)
    ("<C-f6>" 'scheme-compile-definition-and-go)
    ("<f8>"   'scheme-compile-file)
    ("<C-f8>" 'scheme-load-file)))

(defun basis/geiser-map-keys ()
  ;; Can't do this until the REPL is started because otherwise
  ;; `geiser-mode-map' is null.
  (basis/define-keys geiser-mode-map
    ("<f5>"   'geiser-eval-last-sexp)
    ("<f6>"   'basis/geiser-eval-something)
    ("<C-f6>" 'basis/geiser-eval-something-and-go)
    ("<f7>"   'basis/geiser-expand-something)
    ("<C-f7>" 'geiser-expand-definition)
    ("<f8>"   'geiser-eval-buffer)
    ("<C-f8>" 'geiser-eval-buffer-and-go)))

(add-hook 'geiser-repl-mode-hook 'basis/geiser-map-keys)

;; smartparens -----------------------------------------------------------------

(smartparens-global-strict-mode)

(with-eval-after-load 'smartparens
  ;; I still prefer Paredit with lisps
  (dolist (mode basis/lisp-modes)
    (add-to-list 'sp-ignore-modes-list mode))

  (sp-use-paredit-bindings)

  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-autoescape-string-quote nil)

  (sp-pair "'"
           nil
           :unless '(basis/sp-point-after-word-p)
           :actions '(insert wrap autoskip))

  (sp-local-pair 'org-mode "=" "=" :actions '(wrap))

  (sp-with-modes '(c-mode c++-mode java-mode)
    (sp-local-pair "{" "}" :actions '(:rem insert autoskip)))

  (basis/define-keys sp-keymap
    ("M-DEL"         'basis/sp-kill-region-or-backward-word)
    ("C-DEL"         'basis/sp-kill-region-or-backward-word)
    ("<C-backspace>" 'basis/sp-kill-region-or-backward-word)
    ("C-w"           'basis/sp-kill-region-or-backward-word)
    ("M-k"           'sp-kill-sexp)
    ("M-e"           'basis/maybe-sp-forward-sexp)
    ("M-a"           'basis/maybe-sp-backward-sexp)
    ("]"             'sp-up-sexp)
    ("C-c ]"         'basis/insert-right-bracket)))

;; flycheck --------------------------------------------------------------------

(defvar basis/flycheck-map
  (let ((map (make-sparse-keymap)))
    (basis/define-keys map
      ("c"   'flycheck-buffer)
      ("n"   'flycheck-next-error)
      ("p"   'flycheck-previous-error)
      ("l"   'flycheck-list-errors)
      ("s"   'flycheck-select-checker)
      ("C"   'flycheck-clear)
      ("SPC" 'basis/flycheck-check-and-list-errors))
    map)
  "Custom keybindings for Flycheck.")

(with-eval-after-load 'flycheck
  (setq flycheck-check-syntax-automatically nil)

  (unless (basis/libxml-available-p)
    (setq flycheck-xml-parser 'flycheck-parse-xml-region))

  ;; Check buffers with errors more frequently than ones without
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'basis/adjust-flycheck-idle-change-delay)

  (global-set-key (kbd "C-h l") basis/flycheck-map)

  ;; Keys for the errors buffer
  (basis/define-keys flycheck-error-list-mode-map
    ("n" 'flycheck-error-list-next-error)
    ("p" 'flycheck-error-list-previous-error)))

;; python ----------------------------------------------------------------------

(with-eval-after-load 'python
  (basis/define-keys python-mode-map
    ("C-c C-c"  'basis/python-send-something)
    ("<f6>"     'basis/python-send-something)
    ("C-c C-k"  'python-shell-send-file)
    ("<f8>"     'python-shell-send-file)
    ("C-c M-k"  'python-shell-send-buffer)
    ("<M-f8>"   'python-shell-send-buffer)
    ("RET"      'basis/electric-return)
    ("DEL"      'basis/sp-python-backspace)
    ("C-h C-p"  'basis/insert-python-docstring-quotes))
  (setq python-fill-docstring-style 'pep-257-nn))

(when (basis/jedi-installed-p)
  ;; Jedi has 2 Python dependencies: jedi and epc
  (setq jedi:setup-keys t
        jedi:tooltip-method nil)
  (add-hook 'python-mode-hook 'jedi:setup))

(defun basis/init-python-mode ()
  (subword-mode 1)
  (unless (and buffer-file-name (file-remote-p buffer-file-name))
    (flycheck-mode 1))
  (setq fill-column 79)
  (setq tab-width 4)
  (set (make-local-variable 'whitespace-line-column) 79)
  (setq basis/eval-defun-function 'python-shell-send-defun
        basis/eval-region-function 'python-shell-send-region
        basis/eval-buffer-function 'python-shell-send-buffer
        basis/eval-file-function 'python-shell-send-file))

(defun basis/init-inferior-python-mode ()
  (subword-mode 1)
  (setq tab-width 4))

(add-hook 'python-mode-hook 'basis/init-python-mode)
(add-hook 'inferior-python-mode-hook 'basis/init-inferior-python-mode)

;; haskell ---------------------------------------------------------------------

(defun basis/init-haskell-mode ()
  (setq basis/eval-defun-function 'inferior-haskell-send-decl
        basis/eval-file-function 'inferior-haskell-load-file)
  (turn-on-haskell-indentation))

(add-hook 'haskell-mode-hook 'basis/init-haskell-mode)

;; rust ------------------------------------------------------------------------

(defun basis/rust-set-compile-command ()
  (unless (or (file-exists-p "Makefile")
              (file-exists-p "makefile"))
    (set (make-local-variable 'compile-command)
         (if (file-exists-p "Cargo.toml")
             "cargo build"
           (format "rustc %s"
                   (if buffer-file-name
                       (shell-quote-argument buffer-file-name)
                     ""))))))

(defun basis/init-rust-mode ()
  (subword-mode 1)
  (basis/rust-set-compile-command))

(add-hook 'rust-mode-hook 'basis/init-rust-mode)

(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "RET") 'basis/electric-return))

;; javascript ------------------------------------------------------------------

(add-to-list 'auto-mode-alist (cons "\\.js\\'" 'js2-mode))

(setq-default
 js2-show-parse-errors nil
 js2-allow-rhino-new-expr-initializer nil
 js2-strict-inconsistent-return-warning nil
 js2-strict-missing-semi-warning nil
 js2-strict-trailing-comma-warning t)

(setq js2-basic-offset 2)

(with-eval-after-load 'js2-mode
  (js2r-add-keybindings-with-prefix "C-h m")
  (define-key js2-mode-map (kbd "C-;") 'basis/eol-maybe-semicolon))

(defun basis/init-js2-mode ()
  (setq tab-width 4)
  (subword-mode 1)
  (js2-imenu-extras-setup))

(add-hook 'js2-mode-hook 'basis/init-js2-mode)

;; skewer ----------------------------------------------------------------------

(skewer-setup) ; hook into js2, html, and css modes

(with-eval-after-load 'skewer-mode
  (basis/define-keys skewer-mode-map
    ("<f5>" 'skewer-eval-last-expression)
    ("<f6>" 'skewer-eval-defun)
    ("<f8>" 'skewer-load-buffer)))

(with-eval-after-load 'skewer-repl
  (define-key skewer-repl-mode-map (kbd "TAB") 'hippie-expand))

(with-eval-after-load 'skewer-html
  (define-key skewer-html-mode-map (kbd "<f6>") 'skewer-html-eval-tag))

(with-eval-after-load 'skewer-css
  (basis/define-keys skewer-css-mode-map
    ("<f5>" 'skewer-css-eval-current-declaration)
    ("<f6>" 'skewer-css-eval-current-rule)
    ("<f8>" 'skewer-css-eval-buffer)))

;; sql -------------------------------------------------------------------------

(defun basis/init-sql-mode ()
  (sql-set-product "postgres")
  (setq tab-width 4)
  (basis/modify-sql-syntax-table))

(add-hook 'sql-mode-hook 'basis/init-sql-mode)

(with-eval-after-load 'sql
  ;; But I also work with other products and it's often easier not to switch
  ;; `sql-product' around.
  (let ((more-builtins '("elsif" "endif" "while")))
    (add-to-list
     'sql-mode-postgres-font-lock-keywords
     (apply #'sql-font-lock-keywords-builder
            'font-lock-builtin-face nil more-builtins)))
  (define-key sql-mode-map (kbd "RET") 'basis/electric-return))

;; cc-mode ---------------------------------------------------------------------

(defun basis/init-c-base ()
  (setq indent-tabs-mode nil
        c-basic-offset 4)
  (set (make-local-variable 'comment-style) 'extra-line)
  (c-toggle-auto-newline 1)
  (dolist (cleanup '(brace-else-brace
                     brace-elseif-brace
                     defun-close-semi
                     empty-defun-braces))
    (add-to-list 'c-cleanup-list cleanup))
  (subword-mode 1))

(defun basis/init-c ()
  (c-set-style "python")
  (basis/init-c-base))

(defun basis/init-c++ ()
  (basis/init-c)
  (c-set-offset 'innamespace 0)
  (dolist (cleanup '(brace-catch-brace scope-operator))
    (add-to-list 'c-cleanup-list cleanup)))

(defun basis/init-java ()
  (c-set-style "java")
  (basis/init-c-base))

(add-hook 'c-mode-hook    'basis/init-c)
(add-hook 'c++-mode-hook  'basis/init-c++)
(add-hook 'java-mode-hook 'basis/init-java)

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-j") 'c-context-line-break))

;; ack and a half --------------------------------------------------------------

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(global-set-key (kbd "<M-f9>") 'ack-and-a-half)

(when (file-exists-p "~/bin/ack")
  (setq ack-and-a-half-executable "~/bin/ack"))

(with-eval-after-load 'ack-and-a-half
  (basis/define-keys ack-and-a-half-mode-map
    ("n" 'compilation-next-error)
    ("p" 'compilation-previous-error)
    ("]" 'compilation-next-file)
    ("[" 'compilation-previous-file))
  (setq ack-and-a-half-use-ido t)
  ;; Make Cygwin happy
  (when (and basis/cygwin-p
             (stringp ack-and-a-half-executable)
             (string-prefix-p "c:" ack-and-a-half-executable))
    (setq ack-and-a-half-executable (substring ack-and-a-half-executable 2))))

;; org -------------------------------------------------------------------------

;; Paths
(setq org-directory "~/Dropbox/org/"
      org-default-notes-file (expand-file-name "refile.org" org-directory)
      org-archive-location "%s.archive::"
      org-agenda-files (mapcar (-rpartial #'expand-file-name org-directory)
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

;; Code blocks & org-babel
(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

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
  ("C-c a" 'org-agenda)
  ("C-c b" 'org-iswitchb)
  ("C-c c" 'org-capture)
  ("C-c l" 'org-store-link))

(add-hook 'org-mode-hook 'basis/maybe-enable-flyspell)

;; org-babel + clojure ---------------------------------------------------------

(with-eval-after-load 'org
  (require 'ob)
  (require 'ob-tangle)
  (require 'ob-clojure)

  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure    . t)
     (python     . t)
     (sh         . t)))

  ;; The `org-babel-execute:clojure' in `ob-clojure' is for SLIME so we replace
  ;; it with one for Cider.
  (fset 'org-babel-execute:clojure 'basis/org-babel-execute:clojure))

;; html ------------------------------------------------------------------------

(defun basis/init-simplezen ()
  (set (make-local-variable 'yas-fallback-behavior)
       '(apply simplezen-expand-or-indent-for-tab)))

(defun basis/init-html-mode ()
  (setq tab-width 4)
  (tagedit-mode 1))

(add-hook 'sgml-mode-hook 'basis/init-simplezen)
(add-hook 'html-mode-hook 'basis/init-html-mode)

(with-eval-after-load 'sgml-mode
  (require 'tagedit)
  (require 'simplezen)
  (basis/define-keys html-mode-map
    ([remap forward-paragraph]  'basis/move-to-next-blank-line)
    ([remap backward-paragraph] 'basis/move-to-previous-blank-line)
    ("RET"                      'basis/html-newline-and-indent)
    ("M-RET"                    'basis/html-multiline-expand)
    ("C-c C-w"                  'basis/html-wrap-in-tag)
    ("C-c w"                    'basis/html-wrap-in-tag)
    ("<f8>"                     'browse-url-of-buffer))
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features))

(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(defadvice tagedit-toggle-multiline-tag (around maybe-forward activate)
  "Move forward by a line and indent if invoked directly between
two tags."
  (let ((move-forward-p (and (looking-at-p "<") (basis/looking-back-p ">"))))
    ad-do-it
    (when move-forward-p
      (forward-line 1)
      (indent-according-to-mode))))

;; forth -----------------------------------------------------------------------

(autoload 'forth-mode "gforth" "Forth mode" t)

(with-eval-after-load 'forth-mode
  (define-key forth-mode-map (kbd "M-o")   nil)
  (define-key forth-mode-map (kbd "M-SPC") nil))

(dolist (ext '("f" "fs" "fth"))
  (add-to-list 'auto-mode-alist `(,(format "\\.%s\\'" ext) . forth-mode)))

;; markdown --------------------------------------------------------------------

(dolist (ext (list "markdown" "mkd" "md"))
  (add-to-list 'auto-mode-alist `(,(format "\\.%s\\'" ext) . markdown-mode)))

(defun basis/init-markdown-mode ()
  (setq tab-width 4)
  (unless (eq major-mode 'gfm-mode)
    (turn-on-auto-fill))
  (basis/maybe-enable-flyspell))

(add-hook 'markdown-mode-hook 'basis/init-markdown-mode)

(with-eval-after-load 'markdown-mode
  (basis/define-keys markdown-mode-map
    ("DEL"     'basis/sp-markdown-backspace)
    ("M-n"     'forward-paragraph)
    ("M-p"     'backward-paragraph)
    ("C-c r"   'markdown-insert-reference-link-dwim)
    ("C-c C-r" 'markdown-insert-reference-link-dwim)))

;; deft ------------------------------------------------------------------------

(setq deft-extension "md"
      deft-directory "~/Dropbox/deft"
      deft-text-mode  'gfm-mode)

;; gnus ------------------------------------------------------------------------

(setq gnus-use-dribble-file nil
      gnus-always-read-dribble-file nil
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil)

;; message-mode ----------------------------------------------------------------

(defun basis/init-message-mode ()
  (setq fill-column 72)
  (auto-fill-mode 1)
  (flyspell-mode 1))

(setq message-auto-save-directory "~/.emacs.d/tmp/"
      message-subject-trailing-was-query nil
      message-dont-reply-to-names (rx (or (seq "jbm@" (or "jbm.io" "deft.li"))
                                          "john.b.mastro@gmail.com")))

(add-hook 'message-mode-hook 'basis/init-message-mode)

;; ssh-config-mode -------------------------------------------------------------

(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))

;; batch-mode ------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))


;;; init.el ends here
