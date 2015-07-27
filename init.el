;;; init.el      -*- coding: utf-8; lexical-binding: t; no-byte-compile: t -*-

(eval-when-compile (require 'cl-lib))

;; Disable superfluous UI immediately to prevent momentary display
(let* ((modes '(tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
       (modes (if (eq system-type 'darwin)
                  modes
                (cons 'menu-bar-mode modes))))
  (dolist (mode modes)
    (when (fboundp mode)
      (funcall mode -1))))

(defconst basis/emacs-dir
  (file-name-directory (file-chase-links (or load-file-name buffer-file-name)))
  "This Emacs's configuration directory.")

(defun basis/emacs-dir (name &optional if-not-exists)
  "Return directory NAME expanded in `basis/emacs-dir'.
If CREATE is non-nil, create the directory (including parents) if
it doesn't exist."
  (if (string-suffix-p "/" name)
      (let ((dir (expand-file-name name basis/emacs-dir)))
        (unless (file-directory-p dir)
          (cond ((eq if-not-exists 'error)
                 (error "Directory does not exist: '%s'" dir))
                ((eq if-not-exists 'create)
                 (make-directory dir t))))
        dir)
    ;; This isn't actually necessary but will catch places I meant to use
    ;; `basis/emacs-file' instead
    (error "Directory name should end with a slash")))

(defun basis/emacs-file (name)
  "Return file NAME expanded in `basis/emacs-dir'."
  (if (not (string-suffix-p "/" name))
      (expand-file-name name basis/emacs-dir)
    (error "File name should not end with a slash")))

;; Make sure some directories exist
(dolist (dir '("var/" "var/autosaves/" "tmp/"))
  (basis/emacs-dir dir 'create))

;; Set up the load path
(add-to-list 'load-path (basis/emacs-dir "site-lisp/" 'create))

;; Enable auto-compile
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;; So e.g. `find-function' works on C functions
(let ((source (format "~/src/emacs/emacs-%s/" emacs-version)))
  (when (file-directory-p source)
    (setq source-directory source)))

;; package ---------------------------------------------------------------------

(setq package-user-dir (basis/emacs-dir "elpa/"))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(defvar basis/package-list
  '(ace-window
    batch-mode
    browse-kill-ring
    cider
    clj-refactor
    clojure-cheatsheet
    clojure-mode
    color-theme-solarized
    company
    company-statistics
    counsel
    csv-mode
    dash
    dash-functional
    debbugs
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
    gitattributes-mode
    gitconfig-mode
    gitignore-mode
    god-mode
    guide-key
    haskell-mode
    helm
    helm-descbinds
    helm-make
    helm-projectile
    helm-swoop
    hydra
    ibuffer-vc
    ido-at-point
    ido-ubiquitous
    ido-vertical-mode
    idomenu
    jedi
    js-comint
    js2-mode
    js2-refactor
    jump-char
    leuven-theme
    macrostep
    magit
    markdown-mode
    move-text
    multiple-cursors
    nasm-mode
    page-break-lines
    paredit
    pcre2el
    persistent-soft
    php-mode
    projectile
    pyvenv
    quack
    redshank
    request
    rust-mode
    s
    seq
    simplezen
    skewer-mode
    slime
    slime-company
    smartparens
    smex
    ssh-config-mode
    swiper
    swiper-helm
    sx
    tagedit
    undo-tree
    unicode-fonts
    writegood-mode
    yaml-mode
    yasnippet
    zop-to-char)
  "List of packages to automatically install.")

(defun basis/install-packages (packages)
  "Install any of PACKAGES that aren't already installed."
  (let ((packages (delq nil (mapcar (lambda (pkg)
                                      (unless (package-installed-p pkg)
                                        pkg))
                                    packages))))
    (when packages
      (package-refresh-contents)
      (mapc #'package-install packages))))

;; Opt out of automatically saving a list of installed packages
(when (boundp 'package-selected-packages)
  (advice-add 'package--save-selected-packages :override #'ignore)
  (setq package-selected-packages (copy-sequence basis/package-list)))

(basis/install-packages basis/package-list)

;; load some code --------------------------------------------------------------

(require 'pcase)
(require 'subr-x)
(require 'seq)
(require 'dash)
(require 'dash-functional)
(require 's)

(autoload 'tramp-tramp-file-p "tramp"
  "Return t if NAME is a string with Tramp file name syntax.")

(load (basis/emacs-file "defuns") nil nil nil t)

(defun basis/maybe-load-local-init ()
  "Load \"local.el\" if it exists."
  (let ((local (basis/emacs-file "local.el")))
    (when (file-exists-p local)
      (load (string-remove-suffix ".el" local) nil nil nil t))))

;; Do this after init so that it can override anything set here
(add-hook 'after-init-hook #'basis/maybe-load-local-init)

;; Compatibility shims for some of the new macros in Emacs 25's `subr-x', using
;; the `dash' implementations.
(when (version< emacs-version "25")
  (let* ((macros (seq-remove (lambda (list) (fboundp (car list)))
                             '((if-let       -if-let*   2)
                               (when-let     -when-let* 1)
                               (thread-first ->         1)
                               (thread-last  ->>        1)))))
    (pcase-dolist (`(,sym ,impl ,indent) macros)
      (defalias sym impl)
      (put sym 'lisp-indent-function indent))
    (basis/add-elisp-font-lock-keywords
     (cons "pcase-dolist" (mapcar (lambda (list) (symbol-name (car list)))
                                  macros)))))

;; os x ------------------------------------------------------------------------

;; A graphical Emacs on OS X doesn't automatically inherit $PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; cygwin ----------------------------------------------------------------------

(defvar basis/cygwin-p (and (eq system-type 'windows-nt)
                            (directory-files "c:/" nil "Cygwin")
                            (file-directory-p "c:/bin"))
  "Non-nil if this is a Windows system with Cygwin installed.")

(defvar basis/cygwin-path-directories
  (append '("/bin" "/usr/bin" "/usr/local/bin")
          '("/Python27" "/Python27/Scripts")
          '("/ProgramData/Oracle/Java/javapath"))
  "Directories to add to PATH on Cygwin.")

(defun basis/init-for-cygwin ()
  (let* ((dirs (seq-filter #'file-directory-p basis/cygwin-path-directories))
         (home (getenv "HOME"))
         (home/bin (when home
                     (concat (basis/windows->unix home)
                             "/bin"))))
    (when (and home (file-directory-p home) (not after-init-time))
      (cd home))
    (when (and home/bin (file-directory-p home/bin))
      (push home/bin dirs))
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
      line-number-mode t
      column-number-mode t
      imenu-auto-rescan t
      apropos-do-all t
      custom-file (basis/emacs-file "custom.el")
      recenter-positions '(top middle bottom)
      scroll-preserve-screen-position t
      delete-by-moving-to-trash t
      server-auth-dir (basis/emacs-dir "var/server/")
      bookmark-default-file (basis/emacs-file "var/bookmarks")
      url-configuration-directory (basis/emacs-dir "var/url/")
      gc-cons-threshold 20000000 ; 20MB
      bookmark-save-flag 1)

;; Prevent point from entering the minibuffer prompt
(setq minibuffer-prompt-properties
      (append minibuffer-prompt-properties
              '(point-entered minibuffer-avoid-prompt)))

(setq-default major-mode 'text-mode
              indent-tabs-mode nil
              fill-column 80
              truncate-lines t
              require-final-newline t)

(when (file-exists-p custom-file)
  (load custom-file))

(defun basis/set-default-input-method (&optional method)
  (setq default-input-method (or method "TeX")))

;; Haven't yet bothered looking into why this needs to be done after init
(add-hook 'after-init-hook #'basis/set-default-input-method)

;; Start the server, unless it's already running
(require 'server)
(unless (and server-name (server-running-p server-name))
  (server-start))

;; Enable some miscellaneous helpful modes
(size-indication-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(auto-compression-mode 1)
(delete-selection-mode 1)
(global-page-break-lines-mode 1)

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
             (tramp-tramp-file-p buffer-file-name))
    (set-buffer-file-coding-system 'utf-8-unix)))

(when (eq system-type 'windows-nt)
  (add-hook 'before-save-hook #'basis/maybe-set-coding))

;; Backups, autosaves, and temporary files
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,(basis/emacs-dir "var/backups/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(basis/emacs-dir "var/autosaves/") t)))
(setq auto-save-list-file-prefix
      (concat (basis/emacs-dir "var/auto-save-list/") ".saves-"))
(setq temporary-file-directory (basis/emacs-dir "tmp/"))

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

(defalias 'yes-or-no-p #'y-or-n-p)

;; Let C-n add newlines, and don't deactivate the mark
(setq next-line-add-newlines t)
(advice-add 'next-line :around #'basis/next-line-no-deactivate-mark)

;; Keep popping the mark until point actually moves
(advice-add 'pop-to-mark-command :around #'basis/pop-to-mark-ensure-new-pos)

;; fonts -----------------------------------------------------------------------

;; See the commentary in `unicode-fonts' for the "minimum useful fonts" to
;; install
(unicode-fonts-setup)

(let ((default-font
        (pcase system-type
          (`gnu/linux  "Inconsolata-11")
          (`darwin     "Andale Mono-12")
          (`windows-nt "Consolas-10"))))
  (when default-font
    (set-face-attribute 'default nil :font default-font)))

;; interface -------------------------------------------------------------------

(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)

(setq solarized-termcolors 256
      solarized-italic nil)

;; Some additional faces I've assembled
(add-to-list 'custom-theme-load-path (basis/emacs-dir "themes/solarized-moar/"))

(load-theme 'solarized t)
(load-theme 'solarized-moar t)

(defun basis/get-frame-title ()
  "Return a frame title including the current project directory."
  (if-let (file buffer-file-name)
      (concat (abbreviate-file-name file)
              (when (and (bound-and-true-p projectile-mode)
                         (projectile-project-p))
                (format " [%s]" (projectile-project-name))))
    "%b"))

(when (display-graphic-p)
  (setq frame-title-format '((:eval (basis/get-frame-title)))))

(pcase-dolist (`(,feature ,mode) '((elisp-slime-nav   elisp-slime-nav-mode)
                                   (eldoc             eldoc-mode)
                                   (undo-tree         undo-tree-mode)
                                   (redshank          redshank-mode)
                                   (whitespace        whitespace-mode)
                                   (guide-key         guide-key-mode)
                                   (clj-refactor      clj-refactor-mode)
                                   (projectile        projectile-mode)
                                   (page-break-lines  page-break-lines-mode)
                                   (subword           subword-mode)
                                   (superword         superword-mode)))
  (eval-after-load feature
    `(diminish ',mode)))

;; info ------------------------------------------------------------------------

(with-eval-after-load 'info
  (let ((info-path (basis/emacs-dir "doc/info/")))
    (when (file-directory-p info-path)
      (add-to-list 'Info-additional-directory-list info-path))))

;; uniquify --------------------------------------------------------------------

(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")

;; whitespace-mode -------------------------------------------------------------

(setq whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80)

(put 'whitespace-line-column 'safe-local-variable #'integerp)

;; key bindings ----------------------------------------------------------------

(defun basis/init-modifiers-for-linux ()
  'pass)

(defun basis/init-modifiers-for-os-x ()
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

(defun basis/init-modifiers-for-windows ()
  (setq w32-pass-apps-to-system nil
        w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super))

(pcase system-type
  (`gnu/linux  (basis/init-modifiers-for-linux))
  (`darwin     (basis/init-modifiers-for-os-x))
  (`windows-nt (basis/init-modifiers-for-windows)))

;; C-c <left>/<right> to move backward/forward through window configurations
(winner-mode 1)

;; Make `next-line-add-newlines' safe for keyboard macros
(global-set-key (kbd "C-n") #'basis/next-line)
(global-set-key (kbd "<down>") #'basis/next-line)

;; Newlines
(basis/define-keys global-map
  ("RET"          #'newline-and-indent)
  ("S-RET"        #'basis/open-line-below)
  ("<S-return>"   #'basis/open-line-below)
  ("C-S-RET"      #'basis/open-line-above)
  ("<C-S-return>" #'basis/open-line-above))

;; Whitespace
(global-set-key (kbd "M-\\") #'cycle-spacing)

;; Clever C-a
(global-set-key (kbd "C-a") #'basis/beginning-of-line-or-indentation)

;; Kill stuff
(basis/define-keys global-map
  ("M-k"                   #'kill-sexp)
  ("C-w"                   #'basis/kill-something)
  ("M-DEL"                 #'basis/kill-something)
  ([remap kill-whole-line] #'basis/kill-line-backward)
  ("<C-delete>"            #'basis/smart-kill-whole-line)
  ("<M-delete>"            #'basis/smart-kill-almost-whole-line)
  ("ESC <deletechar>"      #'basis/smart-kill-almost-whole-line)
  ("<C-M-backspace>"       #'backward-kill-sexp)
  ("C-M-_"                 #'backward-kill-sexp)
  ("ESC M-DEL"             #'backward-kill-sexp))

;; Copy stuff
(basis/define-keys global-map
  ("M-w"    #'basis/kill-ring-save-something)
  ("<f2>"   #'basis/clipboard-save-something)
  ("s-w"    #'basis/kill-ring-save-indented))

;; Join lines
(global-set-key (kbd "C-c C-j") #'basis/join-next-line)
(global-set-key (kbd "C-c j") #'basis/join-next-line)

;; Moves lines or regions
(global-set-key (kbd "<M-s-up>") #'move-text-up)
(global-set-key (kbd "<M-s-down>") #'move-text-down)

;; Transpose stuff with M-t
(define-prefix-command 'basis/transposition-map)
(global-set-key (kbd "M-t") 'basis/transposition-map)

(basis/define-keys basis/transposition-map
  ("l"   #'transpose-lines)
  ("w"   #'transpose-words)
  ("s"   #'transpose-sexps)
  ("c"   #'transpose-chars)
  ("M-w" #'basis/transpose-windows)
  ("M-s" #'basis/toggle-window-split))

;; Slightly more SLIME-like interface for tags. But don't change anything if
;; `xref' is available, since it has an improved interface.
(unless (require 'xref nil t)
  (define-key esc-map "." #'basis/find-tag)
  (define-key esc-map "," #'pop-tag-mark)
  (define-key esc-map "*" #'tags-loop-continue))

;; More comfortable {next,previous}-error
(global-set-key (kbd "M-g M-f") #'next-error)
(global-set-key (kbd "M-g M-b") #'previous-error)

;; Comment/uncomment stuff
(global-set-key (kbd "C-c ;") #'basis/comment-or-uncomment)
(global-set-key (kbd "C-x ;") #'basis/comment-region-lines)

;; Eval
(global-set-key (kbd "C-x C-e") #'basis/eval-last-sexp)
(global-set-key (kbd "C-c M-e") #'basis/eval-and-replace)

;; I use M-SPC for `avy-goto-word-1'
(global-set-key (kbd "C-c SPC") #'just-one-space)

;; jump-char
(global-set-key (kbd "M-m") #'jump-char-forward)
(global-set-key (kbd "M-M") #'jump-char-backward)

;; goto-line, with line numbers
(global-set-key (kbd "M-g M-g") #'basis/goto-line-with-numbers)

;; Movement by sexp
(basis/define-keys global-map
  ("M-e" #'forward-sexp)
  ("M-a" #'backward-sexp))

;; M-x shell
(global-set-key (kbd "C-c <C-return>") #'shell)
(global-set-key (kbd "C-c C-^") #'shell)

;; Change word case
(basis/define-keys global-map
  ("M-u" #'basis/upcase-something)
  ("M-l" #'basis/downcase-something)
  ("M-c" #'basis/capitalize-something))

;; DWIM C-x C-c
(global-set-key (kbd "C-x C-c") #'basis/kill-frame-or-terminal)
;; Kill Emacs with M-x sayonara
(defalias 'sayonara #'save-buffers-kill-terminal)

;; Google stuff
(global-set-key (kbd "C-c g") #'basis/google)

;; Proced
(global-set-key (kbd "C-x p") #'proced)

;; Re-open recent files
(global-set-key (kbd "C-x C-r") #'basis/find-file-recentf)

;; Previous/next buffer
(global-set-key (kbd "<C-prior>") #'previous-buffer)
(global-set-key (kbd "<C-next>") #'next-buffer)

;; File operations
(defhydra basis/hydra-file (:color blue :columns 2)
  "Files"
  ("c" helm-locate "locate")
  ("d" basis/diff-buffer-with-file "diff")
  ("r" basis/rename-current-buffer-file "rename")
  ("D" basis/delete-current-buffer-file "delete")
  ("f" find-name-dired "find name")
  ("F" find-dired "find")
  ("m" make-directory "make dir")
  ("q" nil "cancel"))

(global-set-key (kbd "C-c f") #'basis/hydra-file/body)

;; Open one or more files externally, using the `helm-external' machinery
(global-set-key (kbd "C-c C-x") #'basis/open-file-externally)

;; Emacs Lisp-style quotes
(global-set-key (kbd "C-c q") #'basis/elisp-quote)

;; Random operations on regions
(define-prefix-command 'basis/region-map)
(global-set-key (kbd "C-c r") 'basis/region-map)

(basis/define-keys basis/region-map
  ("a" #'align)
  ("c" #'basis/count-words)
  ("l" #'basis/count-sloc-region)
  ("s" #'sort-lines))

;; These are mostly useful with `god-mode'
(basis/define-keys global-map
  ("C-x C-0" #'delete-window)
  ("C-x C-1" #'delete-other-windows)
  ("C-x C-2" #'split-window-below)
  ("C-x C-3" #'split-window-right))

;; Narrowing can be quite handy
(put 'narrow-to-region 'disabled nil)

;; find elisp documentation ----------------------------------------------------

(defhydra basis/hydra-find-lisp (:color blue :columns 2)
  ("c" finder-commentary "finder commentary")
  ("e" view-echo-area-messages "view echo area messages")
  ("f" find-function "find function")
  ("F" find-face-definition "find face definition")
  ("i" info-apropos "info apropos")
  ("k" find-function-on-key "find function on key")
  ("l" find-library "find library")
  ("m" info-display-manual "info display manual")
  ("s" basis/scratch "scratch")
  ("v" find-variable "find variable")
  ("V" apropos-value "apropos value")
  ("a" helm-apropos "helm apropos"))

(global-set-key (kbd "<f1> e") #'basis/hydra-find-lisp/body)

;; h-map -----------------------------------------------------------------------

(define-prefix-command 'basis/h-map)

;; Note sure which will be better
(global-set-key (kbd "C-h") 'basis/h-map)
(global-set-key (kbd "M-h") 'basis/h-map)

(basis/define-keys basis/h-map
  ("C-k" #'basis/kill-this-buffer)
  ("C-h" #'mark-paragraph))

;; eval keys -------------------------------------------------------------------

(defvar basis/eval-keys
  '((last-sexp  . "C-x C-e")
    (definition . "C-M-x")
    (region     . "C-c C-r")
    (buffer     . "C-c C-b")
    (something  . "C-c C-c")
    (file       . "C-c C-f")
    (expand     . "C-c C-e"))
  "Key bindings used to evaluate various units of code.
See `basis/define-eval-keys'.")

;; god-mode --------------------------------------------------------------------

(defvar basis/god-exempt-major-modes
  '(Custom-mode
    Info-mode
    Man-mode
    ack-and-a-half-mode
    cider-repl-mode
    compilation-mode
    completion-list-mode
    debugger-mode
    deft-mode
    eshell-mode
    magit-popup-mode
    magit-popup-sequence-mode
    sldb-mode
    slime-repl-mode
    special-mode
    wdired-mode))

(with-eval-after-load 'god-mode
  (require 'god-mode-isearch)
  (define-key god-local-mode-map "." #'repeat)
  (define-key god-local-mode-map "i" #'god-local-mode)
  (define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)
  (add-hook 'god-mode-enabled-hook #'basis/god-update-cursor)
  (add-hook 'god-mode-disabled-hook #'basis/god-update-cursor)
  (dolist (mode basis/god-exempt-major-modes)
    (add-to-list 'god-exempt-major-modes mode)))

;; (basis/enable-god-mode)

;; tmux ------------------------------------------------------------------------

;; A number of non-alphanumeric keys don't work by default when Emacs is
;; running in tmux. This attempts to fix that by adding entries to the
;; `key-translation-map'. It's based on code from ArchWiki's Emacs page.

;; `setw -g xterm-keys on` must be set in ~/.tmux.conf for this to work.

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
      ((format "M-[ 1 ; %d A" n)  (format "%s<up>" k))
      ((format "M-[ 1 ; %d B" n)  (format "%s<down>" k))
      ((format "M-[ 1 ; %d C" n)  (format "%s<right>" k))
      ((format "M-[ 1 ; %d D" n)  (format "%s<left>" k))
      ((format "M-[ 1 ; %d H" n)  (format "%s<home>" k))
      ((format "M-[ 1 ; %d F" n)  (format "%s<end>" k))
      ((format "M-[ 5 ; %d ~" n)  (format "%s<prior>" k))
      ((format "M-[ 6 ; %d ~" n)  (format "%s<next>" k))
      ((format "M-[ 2 ; %d ~" n)  (format "%s<delete>" k))
      ((format "M-[ 3 ; %d ~" n)  (format "%s<delete>" k))
      ((format "M-[ 1 ; %d P" n)  (format "%s<f1>" k))
      ((format "M-[ 1 ; %d Q" n)  (format "%s<f2>" k))
      ((format "M-[ 1 ; %d R" n)  (format "%s<f3>" k))
      ((format "M-[ 1 ; %d S" n)  (format "%s<f4>" k))
      ((format "M-[ 15 ; %d ~" n) (format "%s<f5>" k))
      ((format "M-[ 17 ; %d ~" n) (format "%s<f6>" k))
      ((format "M-[ 18 ; %d ~" n) (format "%s<f7>" k))
      ((format "M-[ 19 ; %d ~" n) (format "%s<f8>" k))
      ((format "M-[ 20 ; %d ~" n) (format "%s<f9>" k))
      ((format "M-[ 21 ; %d ~" n) (format "%s<f10>" k))
      ((format "M-[ 23 ; %d ~" n) (format "%s<f11>" k))
      ((format "M-[ 24 ; %d ~" n) (format "%s<f12>" k))
      ((format "M-[ 25 ; %d ~" n) (format "%s<f13>" k))
      ((format "M-[ 26 ; %d ~" n) (format "%s<f14>" k))
      ((format "M-[ 28 ; %d ~" n) (format "%s<f15>" k))
      ((format "M-[ 29 ; %d ~" n) (format "%s<f16>" k))
      ((format "M-[ 31 ; %d ~" n) (format "%s<f17>" k))
      ((format "M-[ 32 ; %d ~" n) (format "%s<f18>" k))
      ((format "M-[ 33 ; %d ~" n) (format "%s<f19>" k))
      ((format "M-[ 34 ; %d ~" n) (format "%s<f20>" k))))
  (basis/define-key-translations
    ("M-[ 1 ; 5 k" "C-=")
    ("M-[ 1 ; 6 k" "C-+")
    ("M-[ 1 ; 7 k" "C-M-=")))

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
      recentf-save-file (basis/emacs-file "var/recentf")
      recentf-exclude (list #'tramp-tramp-file-p #'file-remote-p))

;; TRAMP -----------------------------------------------------------------------

(setq tramp-default-method
      (if (and (eq system-type 'windows-nt)
               (not basis/cygwin-p))
          "plinkx"
        "sshx"))

(setq tramp-persistency-file-name (basis/emacs-file "var/tramp"))

;; Have TRAMP use Cygwin's sh rather than Windows's cmd.exe
(when basis/cygwin-p
  (setq tramp-encoding-shell (executable-find "sh")
        tramp-encoding-command-switch "-c"
        tramp-encoding-command-interactive "-i"))

;; help-mode -------------------------------------------------------------------

(with-eval-after-load 'help-mode
  (basis/define-keys help-mode-map
    ("n" #'next-line)
    ("p" #'previous-line)
    ("b" #'help-go-back)
    ("f" #'help-go-forward)))

;; man and woman ---------------------------------------------------------------

(defvar basis/system-man-p (executable-find "man")
  "Non-nil if a \"man\" executable is available on this system.")

(unless basis/system-man-p
  (require 'man)
  (fset 'original-man #'man)
  (fset 'man #'woman))

;; isearch ---------------------------------------------------------------------

(basis/define-keys isearch-mode-map
  ("DEL"         #'basis/isearch-backspace)
  ("<backspace>" #'basis/isearch-backspace)
  ("C-t"         #'basis/isearch-yank-something)
  ("C-g"         #'basis/isearch-cancel))

;; grep ------------------------------------------------------------------------

(defhydra basis/hydra-grep (:color blue :columns 2)
  "Grep"
  ("a"  ack-and-a-half "ack")
  ("g"  grep "grep")
  ("s"  lgrep "lgrep")
  ("r"  rgrep "rgrep")
  ("z"  zrgrep "zrgrep")
  ("f"  find-grep "find-grep")
  ("d"  find-grep-dired "find-grep-dired")
  ("o"  occur "occur")
  ("mo" multi-occur "multi-occur")
  ("mm" multi-occur-in-matching-buffers "multi-occur matching")
  ("q"  nil "cancel"))

(global-set-key (kbd "<f9>") #'basis/hydra-grep/body)

(with-eval-after-load 'grep
  ;; On OS X, prefer GNU Grep if it's available
  (when (and (eq system-type 'darwin)
             (executable-find "ggrep"))
    (setq grep-program "ggrep"))
  ;; Use POSIX extended regexps
  (grep-compute-defaults)
  (dolist (sym '(grep-command
                 grep-template
                 grep-find-template
                 grep-find-command))
    (pcase (symbol-value sym)
      ;; The value of `grep-find-command' may be a cons
      ((and `(,cmd . ,n) (guard (string-match "\\_<-nH\\_>" cmd)))
       (grep-apply-setting sym (cons (replace-match "-nHE" t t cmd)
                                     (1+ n))))
      ((and cmd (guard (string-match "\\_<-nH\\_>" cmd)))
       (grep-apply-setting sym (replace-match "-nHE" t t cmd)))))
  ;; Add some more file aliases
  (pcase-dolist (`(,alias . ,files)
                 '(("clj" . "*.clj *.cljs *.cljc")
                   ("cl"  . "*.lisp *.cl")
                   ("txt" . "*.txt *.org *.rst *.md *.mkd *.markdown")))
    (unless (assoc alias grep-files-aliases)
      (add-to-list 'grep-files-aliases (cons alias files)))))

;; occur -----------------------------------------------------------------------

(define-key occur-mode-map (kbd "n") #'occur-next)
(define-key occur-mode-map (kbd "p") #'occur-prev)

;; ispell ----------------------------------------------------------------------

(setq ispell-program-name (executable-find "aspell")
      ispell-personal-dictionary "~/.aspell.en.pws"
      ispell-extra-args '("--sug-mode=ultra"))

(with-eval-after-load 'ispell
  (condition-case nil
      (lookup-words "whatever")
    (error
     (when (file-readable-p "~/Dropbox/dict/words")
       (setq ispell-alternate-dictionary "~/Dropbox/dict/words")))))

;; prog-mode -------------------------------------------------------------------

(add-hook 'prog-mode-hook #'basis/maybe-enable-whitespace-mode)
(add-hook 'prog-mode-hook #'basis/maybe-enable-flyspell-prog-mode)

;; diff-mode -------------------------------------------------------------------

(defun basis/init-diff-mode ()
  (setq buffer-read-only t))

(with-eval-after-load 'diff-mode
  ;; `diff-goto-source' is still available on C-c C-c.
  (define-key diff-mode-map (kbd "M-o") nil)
  (add-hook 'diff-mode-hook #'basis/init-diff-mode))

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

;; undo-tree -------------------------------------------------------------------

(global-undo-tree-mode 1)

;; The couple bindings undo-tree puts behind C-x r prevent discover.el's C-x r
;; context menu from working
(define-key undo-tree-map (kbd "C-x r") nil)

;; expand-region ---------------------------------------------------------------

(global-set-key (kbd "M-=") #'er/expand-region)

;; multiple-cursors ------------------------------------------------------------

(basis/define-keys global-map
  ("M-]" #'mc/mark-next-like-this)
  ("C->" #'mc/mark-next-like-this)
  ("C-<" #'mc/mark-previous-like-this))

(setq mc/list-file (basis/emacs-file "var/mc-lists.el"))

(with-eval-after-load 'multiple-cursors-core
  ;; Make RET exit multiple-cursors-mode in the terminal too
  (define-key mc/keymap (kbd "RET") #'multiple-cursors-mode))

;; saveplace -------------------------------------------------------------------

(require 'saveplace)

(setq-default save-place t)

(setq save-place-file (basis/emacs-file "var/places"))

;; savehist --------------------------------------------------------------------

(require 'savehist)

(setq savehist-additional-variables '(search-ring regexp-search-ring)
      savehist-file (basis/emacs-file "var/history"))

(savehist-mode t)

;; avy -------------------------------------------------------------------------

(setq avy-keys '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?u ?i))
(setq avy-style 'pre)

(global-set-key (kbd "M-SPC") #'avy-goto-word-1)

;; ivy/swiper/counsel ----------------------------------------------------------

(setq ivy-format-function #'basis/ivy-format-function)

(setq swiper-min-highlight 1)

(global-set-key (kbd "C-s") #'swiper)
(global-set-key (kbd "C-r") #'basis/swiper-helm)

(with-eval-after-load 'swiper
  (require 'avy)
  (basis/define-keys swiper-map
    ("M-%"   #'swiper-query-replace)
    ("M-SPC" #'swiper-avy)
    ("C-t"   #'basis/swiper-maybe-yank-something)))

(with-eval-after-load 'ivy
  (require 'counsel)
  (define-key ivy-minibuffer-map (kbd "C-r") #'ivy-previous-line))

;; ace-window ------------------------------------------------------------------

(global-set-key (kbd "M-o") #'ace-window)

(setq aw-keys '(?h ?j ?k ?l ?n ?m))

(setq aw-scope 'frame)

(defun basis/ace-window-kludge (function arg)
  "Advice for `ace-window'.
Ensure it always works with two windows, even when one (or both)
is read-only and empty."
  (if (and (eq aw-scope 'frame)
           (= (length (window-list)) 2))
      (pcase arg
        (4  (basis/transpose-windows 1))
        (16 (delete-other-windows))
        (_  (other-window 1)))
    (funcall function arg)))

(advice-add 'ace-window :around #'basis/ace-window-kludge)

;; zop-to-char -----------------------------------------------------------------

(defun basis/zop-no-help ()
  "Advice for `zop-to-char-help-string'.
Return the empty string (i.e. get rid of the help string)."
  "")

;; Because I have `zop-to-char' on M-z, it's convenient to continue using meta
;; for its actions rather than switching to control
(with-eval-after-load 'zop-to-char
  (add-to-list 'zop-to-char-kill-keys ?\M-k)
  (add-to-list 'zop-to-char-next-keys ?\M-f)
  (add-to-list 'zop-to-char-prec-keys ?\M-b)
  (add-to-list 'zop-to-char-erase-keys ?\M-d)
  (add-to-list 'zop-to-char-quit-at-point-keys ?\M-q)
  (advice-add 'zop-to-char-help-string :override #'basis/zop-no-help))

(global-set-key (kbd "M-z") #'zop-up-to-char)
(global-set-key (kbd "M-Z") #'zop-to-char)

;; message-mode ----------------------------------------------------------------

(defun basis/init-message-mode ()
  (setq fill-column 72)
  (setq-local org-footnote-tag-for-non-org-mode-files nil))

(setq message-auto-save-directory (basis/emacs-dir "tmp/")
      message-subject-trailing-was-query nil)

(with-eval-after-load 'message
  (require 'org)
  (define-key message-mode-map (kbd "C-c n") #'org-footnote-action)
  (add-hook 'message-mode-hook #'basis/init-message-mode))

;; mu4e ------------------------------------------------------------------------

(let ((dir "/usr/local/share/emacs/site-lisp/mu4e/"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(autoload 'mu4e "mu4e" "Launch mu4e" t nil)

(global-set-key (kbd "C-x m") #'mu4e)

;; Mail
(setq mu4e-get-mail-command "offlineimap"
      mu4e-maildir (expand-file-name ".maildir/fastmail" (getenv "HOME"))
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
                                    "jbm@fastmail.com"
                                    "jbm@fastmail.fm"
                                    "jbm@mailforce.net"))

;; Convert HTML->text if no text version is available
(setq mu4e-html2text-command (if (executable-find "html2text")
                                 "html2text -utf8 -width 72"
                               #'basis/shr-html2text))

;; Where to save attachments
(let ((dir (seq-some-p #'file-directory-p '("~/downloads" "~/Downloads" "~/"))))
  (setq mu4e-attachment-dir dir))

;; Composing messages
(setq mu4e-reply-to-address "jbm@jbm.io"
      user-mail-address "jbm@jbm.io"
      user-full-name "John Mastro"
      message-signature "jbm"
      mu4e-sent-messages-behavior 'delete ; They're saved on the server
      message-kill-buffer-on-exit t
      message-dont-reply-to-names (regexp-opt mu4e-user-mail-address-list))

;; SMTP
(setq send-mail-function #'smtpmail-send-it
      message-send-mail-function #'smtpmail-send-it
      smtp-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-user "jbm@fastmail.fm"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

;; Retrieving passwords
(setq auth-sources (if (eq system-type 'darwin)
                       '(macos-keychain-internet)
                     '("~/.authinfo.gpg")))

(with-eval-after-load 'mu4e
  (add-to-list 'mu4e-view-actions
               (cons "View in browser" #'basis/mu4e-action-view-in-browser)
               t))

;; elfeed ----------------------------------------------------------------------

(setq elfeed-db-directory (basis/emacs-dir "var/elfeed/"))

(when (file-exists-p (basis/emacs-file "feeds.el"))
  (setq elfeed-feeds (basis/elfeed-load-feeds (basis/emacs-file "feeds.el"))))

;; shr/eww ---------------------------------------------------------------------

(defun basis/shr-tag-body-no-color (function &rest args)
  "Advice for `shr-tag-body'; don't colorize the region."
  (cl-letf (((symbol-function 'shr-colorize-region)
             #'ignore))
    (apply function args)))

(with-eval-after-load 'shr
  (advice-add 'shr-tag-body :around #'basis/shr-tag-body-no-color))

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "<backtab>") #'shr-previous-link))

;; w3m -------------------------------------------------------------------------

(when (and (not (display-graphic-p))
           (executable-find "w3m"))
  (setq browse-url-browser-function #'w3m-browse-url))

(with-eval-after-load 'w3m
  (define-key w3m-mode-map "n" #'w3m-next-anchor)
  (define-key w3m-mode-map "p" #'w3m-previous-anchor))

;; sx --------------------------------------------------------------------------

(setq sx-cache-directory (basis/emacs-dir "var/sx/"))

(defun basis/init-sx-question-mode ()
  (toggle-truncate-lines -1))

(with-eval-after-load 'sx-question-mode
  (add-hook 'sx-question-mode-hook #'basis/init-sx-question-mode))

;; ediff -----------------------------------------------------------------------

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

(defun basis/init-ediff ()
  (ediff-setup-keymap))

(with-eval-after-load 'ediff
  (when (eq system-type 'windows-nt)
    (advice-add 'ediff-make-empty-tmp-file
                :filter-args
                #'basis/ediff-expand-tmp-name))
  (advice-add 'ediff-setup :before #'basis/ediff-save-window-config)
  (advice-add 'ediff-quit :after #'basis/ediff-quit-restore)
  (add-hook 'ediff-mode-hook #'basis/init-ediff))

;; magit -----------------------------------------------------------------------

(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "<f10>") #'magit-status)
(global-set-key (kbd "C-x M-g") #'magit-dispatch-popup)

(with-eval-after-load 'magit
  (unless (boundp 'magit-backup-mode)
    ;; Temporary kludge to prevent an unbound-symbol error
    (setq magit-backup-mode nil))
  (setq magit-revert-buffers 'silent)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-popup-use-prefix-argument 'default)
  (setq magit-completing-read-function #'magit-ido-completing-read)
  (setq magit-repository-directories
        (thread-last (projectile-relevant-known-projects)
          (seq-remove #'tramp-tramp-file-p)
          (seq-filter (lambda (dir)
                        (file-directory-p (expand-file-name ".git" dir))))
          (cons "~/code/")
          ;; Remove the trailing slashes
          (mapcar #'directory-file-name)))
  (setq magit-branch-arguments (remove "--track" magit-branch-arguments))
  (add-hook 'magit-status-mode-hook #'delete-other-windows)
  (when (and (eq system-type 'windows-nt)
             basis/cygwin-p)
    (advice-add 'magit-get-top-dir
                :filter-return
                #'basis/magit-expand-top-dir))
  ;; Add a command on `C-c C-v' to view the pull request URL. It would be even
  ;; better to add this to Magit's menus but nowhere sticks out as obviously
  ;; appropriate.
  (define-key magit-status-mode-map
    (kbd "C-c C-v") #'basis/magit-browse-pull-request-url))

(with-eval-after-load 'with-editor
  ;; On Cygwin, fix `with-editor-emacsclient-executable' and advice
  ;; `with-editor-locate-emacsclient' so that its result is accurate for any
  ;; future uses.
  (when (and (eq system-type 'windows-nt)
             basis/cygwin-p)
    (when-let ((client with-editor-emacsclient-executable)
               (client (basis/fix-bad-cygwin-file-name client)))
      (setq with-editor-emacsclient-executable client))
    (advice-add 'with-editor-locate-emacsclient
                :filter-return
                #'basis/fix-located-emacsclient-file-name)))

;; text-mode -------------------------------------------------------------------

(defun basis/init-text-mode ()
  (auto-fill-mode 1)
  (basis/maybe-enable-flyspell)
  (when ispell-alternate-dictionary
    (add-to-list 'company-backends 'company-ispell)))

(add-hook 'text-mode-hook #'basis/init-text-mode)

;; ibuffer ---------------------------------------------------------------------

(defalias 'ls #'ibuffer)

(global-set-key [remap list-buffers] #'ibuffer)

(with-eval-after-load 'ibuffer
  (require 'ibuffer-vc)
  (basis/define-keys ibuffer-mode-map
    ("M-o"   nil) ;; don't shadow ace-window
    ("C-M-o" #'ibuffer-visit-buffer-1-window)
    ("\\"    #'basis/ibuffer-toggle-vc-grouping))

  (define-ibuffer-column size-h
    ;; a more readable size column
    (:name "Size" :inline t)
    (cond ((> (buffer-size) 1000000)
           (format "%7.1fM" (/ (buffer-size) 1000000.0)))
          ((> (buffer-size) 1000)
           (format "%7.1fk" (/ (buffer-size) 1000.0)))
          (t
           (format "%8d" (buffer-size))))))

(with-eval-after-load 'ibuffer-vc
  (advice-add 'ibuffer-vc-root :around #'basis/ibuffer-vc-root-files-only))

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
              (name 18 18 :left :elide)
              " "
              filename)))

(setq ibuffer-show-empty-filter-groups nil)

;; company ---------------------------------------------------------------------

(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'company-statistics-mode t)

(setq company-statistics-file
      (basis/emacs-file "var/company-statistics-cache.el"))

(with-eval-after-load 'company
  (basis/define-keys company-active-map
    ("TAB"    #'company-complete)
    ([tab]    #'company-complete)
    ("<C-f1>" #'company-show-location)
    ("<M-f1>" #'company-show-location)
    ("C-w"    nil)
    ("C-h"    nil)
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
  (advice-add 'company-auto-begin
              :around
              #'basis/company-no-completion-in-docstring)
  (advice-add 'company-auto-begin
              :around
              #'basis/company-sh-no-complete-fi)
  (when (eq system-type 'windows-nt)
    (advice-add 'company-auto-begin
                :around
                #'basis/company-no-tramp-completion))
  (with-eval-after-load 'cc-mode
    (when-let (args (basis/build-clang-args 'c))
      (require 'company-clang)
      (setq company-clang-arguments args)
      (add-hook 'c-mode-hook #'basis/enable-company-clang))))

;; dired -----------------------------------------------------------------------

(defun basis/dired-omit-expunge-quietly (function &rest args)
  "Advice for `dired-omit-expunge'.
Only print messages if the selected window contains a `dired'
buffer."
  (cl-letf (((symbol-value 'dired-omit-verbose)
             (with-current-buffer (window-buffer (selected-window))
               (eq major-mode 'dired-mode))))
    (apply function args)))

(with-eval-after-load 'dired
  (require 'dired+)
  (require 'find-dired)
  (basis/define-keys dired-mode-map
    ("RET"                       #'dired-find-alternate-file)
    ("M-RET"                     #'dired-find-file)
    ("e"                         #'basis/dired-open-files)
    ("-"                         #'diredp-up-directory-reuse-dir-buffer)
    ("^"                         #'diredp-up-directory-reuse-dir-buffer)
    ("M-^"                       #'diredp-up-directory)
    ("M-m"                       #'dired-omit-mode)
    ("M-n"                       #'diredp-next-subdir)
    ("M-p"                       #'diredp-prev-subdir)
    ("M-e"                       #'dired-next-dirline)
    ("M-a"                       #'dired-prev-dirline)
    ("M-b"                       nil)
    ("M-o"                       nil)
    ([remap beginning-of-buffer] #'basis/dired-jump-to-top)
    ([remap end-of-buffer]       #'basis/dired-jump-to-bottom))
  (setq dired-omit-extensions (remove ".bak" dired-omit-extensions)
        dired-recursive-deletes 'top
        dired-listing-switches "-alh"
        find-ls-options '("-exec ls -ldh {} +" . "-ldh"))
  ;; Use `ls' from GNU coreutils on OS X, when available
  (when (and (eq system-type 'darwin)
             (executable-find "gls"))
    (setq insert-directory-program "gls"))
  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (advice-add 'dired-omit-expunge :around #'basis/dired-omit-expunge-quietly))

(autoload 'dired-jump "dired-x"
  "Jump to dired buffer corresponding to current buffer." t)

(global-set-key (kbd "C-h C-j") #'dired-jump)

;; direx -----------------------------------------------------------------------

(global-set-key (kbd "C-h j") #'direx:jump-to-directory)
(global-set-key (kbd "C-h p C-j") #'basis/direx-jump-to-project-root)

(with-eval-after-load 'direx
  (define-key direx:direx-mode-map (kbd "M-n") #'direx:next-sibling-item)
  (define-key direx:direx-mode-map (kbd "M-p") #'direx:previous-sibling-item))

;; comint ----------------------------------------------------------------------

;; Not a `comint' setting, but it does improve results in some comint-derived
;; modes (e.g. it makes it possible to use Python's help function in
;; `inferior-python-mode' buffers).
(setenv "PAGER" "cat")

(defun basis/init-comint-mode ()
  (setq comint-scroll-to-bottom-on-input 'this))

(with-eval-after-load 'comint
  (basis/define-keys comint-mode-map
    ("M-p"     #'comint-previous-matching-input-from-input)
    ("M-n"     #'comint-next-matching-input-from-input)
    ("C-c C-l" #'helm-comint-input-ring)
    ;; Because Paredit and Smartparens both use M-r
    ("C-M-r"   #'comint-history-isearch-backward-regexp))
  (dolist (cmd '(comint-previous-input
                 comint-next-input
                 comint-previous-matching-input-from-input
                 comint-next-matching-input-from-input))
    (advice-add cmd :before #'basis/comint-input-goto-bottom-if-necessary))
  (add-hook 'comint-mode-hook #'basis/init-comint-mode))

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

(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook #'basis/init-shell-mode))

;; eshell ----------------------------------------------------------------------

(setq eshell-directory-name (basis/emacs-dir "var/eshell/"))

(defun basis/init-eshell ()
  (basis/define-keys eshell-mode-map
    ("S-DEL"   #'basis/eshell-kill-line-backward)
    ("C-S-DEL" #'basis/eshell-kill-whole-line)))

(with-eval-after-load 'esh-mode
  (add-hook 'eshell-mode-hook #'basis/init-eshell))

;; sh-mode ---------------------------------------------------------------------

(defun basis/init-sh-mode ()
  (setq tab-width 4)
  (when (and buffer-file-name
             (string= (file-name-nondirectory buffer-file-name) ".zshrc"))
    (sh-set-shell "zsh")))

(with-eval-after-load 'sh-script
  (add-hook 'sh-mode-hook #'basis/init-sh-mode))

;; ido -------------------------------------------------------------------------

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-use-virtual-buffers t
      ido-use-faces nil
      ido-max-prospects 10
      ido-ignore-extensions t
      ido-save-directory-list-file (basis/emacs-file "var/ido.last")
      flx-ido-threshhold 10000)

(require 'ido)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(ido-at-point-mode 1)

;; For ido-powered completion at point. Need a better key binding for this.
(global-set-key (kbd "C-c TAB") #'completion-at-point)
(global-set-key (kbd "C-c <C-tab>") #'completion-at-point)

(defun basis/init-ido-keys ()
  (basis/define-keys ido-file-completion-map
    ("M-w"     #'ido-copy-current-file-name)
    ("M-g"     #'basis/ido-magit-status)
    ("C-c C-x" #'basis/ido-open-file-externally)))

(add-hook 'ido-setup-hook #'basis/init-ido-keys)

;; smex ------------------------------------------------------------------------

(setq smex-save-file (basis/emacs-file "var/smex-items"))

(basis/define-keys global-map
  ;; ("M-x"     #'smex)
  ("M-X"     #'smex-major-mode-commands)
  ("C-h M-x" #'execute-extended-command))

;; helm ------------------------------------------------------------------------

(require 'helm-config)

(global-unset-key (kbd helm-command-prefix-key))

(setq helm-mode-handle-completion-in-region nil)

(require 'helm)
(require 'helm-mode)

(helm-mode 1)

;; None of helm, ido, or ivy seem to handle `Info-goto-node'
(add-to-list 'helm-completing-read-handlers-alist '(Info-goto-node))

(basis/define-keys global-map
  ("C-x C-f"  #'helm-find-files)
  ("C-x b"    #'helm-mini)
  ("M-x"      #'helm-M-x)
  ("M-i"      #'helm-imenu)
  ("M-y"      #'helm-show-kill-ring)
  ("M-`"      #'helm-all-mark-rings)
  ("<f1> SPC" #'helm-apropos)
  ("<f1> b"   #'helm-descbinds))

(define-prefix-command 'basis/helm-map)

(global-set-key (kbd "C-c h") 'basis/helm-map)

(basis/define-keys basis/helm-map
  ("a" #'helm-apropos)
  ("b" #'helm-buffers-list)
  ("c" #'helm-colors)
  ("e" #'helm-register)
  ("f" #'helm-find-files)
  ("g" #'helm-do-grep)
  ("i" #'helm-info-at-point)
  ("k" #'helm-man-woman)
  ("l" #'helm-bookmarks)
  ("m" #'helm-all-mark-rings)
  ("o" #'helm-occur)
  ("O" #'helm-multi-occur)
  ("p" #'helm-list-emacs-process)
  ("r" #'helm-regexp)
  ("R" #'helm-resume)
  ("s" #'helm-swoop)
  ("t" #'helm-top)
  ("x" #'helm-M-x)
  ("y" #'helm-show-kill-ring)
  ("/" #'helm-find)
  (":" #'helm-eval-expression-with-eldoc))

(setq helm-split-window-default-side 'other
      helm-split-window-in-side-p t
      helm-ff-newfile-prompt-p nil
      helm-ff-file-name-history-use-recentf t
      helm-ff-search-library-in-sexp t
      helm-quick-update t
      helm-truncate-lines t
      helm-imenu-execute-action-at-once-if-one nil
      helm-display-header-line nil)

(setq helm-M-x-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-imenu-fuzzy-match t
      helm-lisp-fuzzy-completion t
      helm-projectile-fuzzy-match t
      helm-recentf-fuzzy-match t
      helm-locate-fuzzy-match nil)

(unless basis/system-man-p
  (setq helm-man-or-woman-function #'woman))

(with-eval-after-load 'helm
  (require 'helm-utils) ; For the `helm-selection-line' face
  (basis/define-keys helm-map
    ("TAB" #'helm-execute-persistent-action)
    ("M-s" #'helm-select-action)
    ("DEL" #'basis/helm-backspace))
  (set-face-attribute 'helm-source-header nil :height 1.0)
  (setq helm-adaptive-history-file
        (basis/emacs-file "var/helm-adaptive-history"))
  (require 'helm-adaptive)
  (helm-adaptive-mode)
  ;; Always display Helm buffers at the bottom, using 40% of the frame's height
  (add-to-list 'display-buffer-alist
               '("\\`\\*helm.*\\*\\'"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4))))

(with-eval-after-load 'helm-files
  (basis/define-keys helm-find-files-map
    ("TAB"   #'helm-execute-persistent-action)
    ("M-s"   #'helm-select-action)
    ("DEL"   #'basis/helm-backspace)
    ("C-x g" #'basis/helm-ff-run-magit-status)))

;; `helm-swoop' config
(setq helm-swoop-use-line-number-face t)

(define-key isearch-mode-map (kbd "M-s") #'helm-swoop-from-isearch)

(with-eval-after-load 'helm-swoop
  (define-key helm-swoop-map (kbd "C-s") #'helm-next-line)
  (define-key helm-swoop-map (kbd "C-r") #'helm-previous-line)
  ;; I prefer M-s for this
  (define-key isearch-mode-map (kbd "M-i") nil))

(when (eq system-type 'windows-nt)
  (advice-add 'helm-open-file-externally :override #'basis/helm-open-file-w32))

;; To prevent a delay the first time I use M-x
(when (eq (key-binding (kbd "M-x")) #'helm-M-x)
  (require 'helm-command))

(with-eval-after-load 'helm-grep
  ;; On OS X, prefer GNU grep if it's available
  (when (and (eq system-type 'darwin)
             (executable-find "ggrep"))
    (dolist (sym '(helm-grep-default-command helm-grep-default-recurse-command))
      (let ((cmd (symbol-value sym)))
        (set sym (replace-regexp-in-string "\\`grep" "ggrep" cmd))))))

(when nil
  (setq helm-grep-default-command
        (concat grep-program " --color=always -d skip %e -n%cH -e %p %f"))
  (setq helm-grep-default-recurse-command
        (concat grep-program " --color=always -d recurse %e -n%cH -e %p %f"))
  (setenv "GREP_COLORS" "mt=30;43:sl=00;37:cx=:fn=35:ln=32:bn=32:se=36"))

;; hippie expand ---------------------------------------------------------------

(global-set-key (kbd "M-/") #'hippie-expand)

(dolist (f '(try-expand-line try-expand-list try-expand-all-abbrevs))
  (setq hippie-expand-try-functions-list
        (remq f hippie-expand-try-functions-list)))

;; yasnippet -------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(with-eval-after-load 'yasnippet
  ;; Steal C-t for expanding snippets. `transpose-chars' is still available on
  ;; M-t c
  (basis/define-keys yas-minor-mode-map
    ("C-t"   #'basis/yas-expand-or-insert)
    ("TAB"   nil)
    ([(tab)] nil))
  (define-key yas-keymap (kbd "RET") #'yas-exit-all-snippets)
  (setq yas-snippet-dirs (list (basis/emacs-dir "snippets/"))
        yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
        yas-wrap-around-region t))

;; projectile ------------------------------------------------------------------

(setq projectile-keymap-prefix (kbd "C-h p")
      projectile-completion-system 'helm)

(defhydra basis/hydra-projectile (:color blue :columns 4)
  "Projectile"
  ("b"   projectile-switch-to-buffer "switch to buffer")
  ("d"   projectile-find-dir "dir")
  ("C-f" projectile-find-file "file")
  ("ff"  projectile-find-file-dwim "file dwim")
  ("fd"  projectile-find-file-in-directory "file in dir")
  ("g"   projectile-grep "grep")
  ("i"   projectile-ibuffer "ibuffer")
  ("K"   projectile-kill-buffers "kill buffers")
  ("o"   projectile-multi-occur "multi-occur")
  ("p"   projectile-switch-project "switch")
  ("r"   projectile-recentf "recentf")
  ("x"   projectile-remove-known-project "remove known")
  ("X"   projectile-cleanup-known-projects "cleanup non-existing")
  ("z"   projectile-cache-current-file "cache current")
  ("q"   nil "cancel"))

(setq projectile-known-projects-file
      (basis/emacs-file "var/projectile-bookmarks.eld"))
(setq projectile-cache-file (basis/emacs-file "var/projectile.cache"))

;; Projectile defaults to native indexing on Windows, but if we have Cygwin
;; set up we can use "alien".
(if (and (eq system-type 'windows-nt)
         (not basis/cygwin-p))
    (setq projectile-indexing-method 'native
          projectile-enable-caching t)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching nil))

(projectile-global-mode)

(when (and (eq system-type 'windows-nt) basis/cygwin-p)
  (define-key projectile-mode-map
    [remap projectile-regenerate-tags] #'basis/projectile-regenerate-tags))

;; lisp ------------------------------------------------------------------------

(defvar basis/lisp-modes
  '((lisp-mode    . emacs-lisp-mode)
    (lisp-mode    . lisp-interaction-mode)
    (ielm         . inferior-emacs-lisp-mode)
    (clojure-mode . clojure-mode)
    (cider-repl   . cider-repl-mode)
    (lisp-mode    . lisp-mode)
    (slime-repl   . slime-repl-mode)
    (inf-lisp     . inferior-lisp-mode)
    (scheme       . scheme-mode)
    (cmuscheme    . inferior-scheme-mode)
    (geiser-repl  . geiser-repl-mode))
  "List of Lisp modes to configure.
Each element is a cons, (FEATURE . MODE).")

(defvar basis/lisp-hooks
  (mapcar (lambda (cons)
            (pcase-let ((`(,feature . ,mode) cons))
              (cons feature (intern (format "%s-hook" mode)))))
          basis/lisp-modes)
  "List of Lisp mode hooks.
Each element is a cons, (FEATURE . HOOK).")

(setq inferior-lisp-program (or (executable-find "sbcl")
                                (executable-find "ccl")
                                "lisp"))

(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

(defun basis/init-lisp-generic ()
  "Enable features useful in all Lisp modes."
  (paredit-mode +1))

(pcase-dolist (`(,feature . ,hook) basis/lisp-hooks)
  (eval-after-load feature
    `(add-hook ',hook #'basis/init-lisp-generic)))

(setq lisp-lambda-list-keyword-alignment t
      lisp-lambda-list-keyword-parameter-alignment t
      lisp-loop-forms-indentation 6)

;; emacs lisp ------------------------------------------------------------------

(defvar basis/emacs-lisp-modes
  '((lisp-mode . emacs-lisp-mode)
    (lisp-mode . lisp-interaction-mode)
    (ielm      . inferior-emacs-lisp-mode))
  "List of Emacs Lisp modes to configure.
Each element is a cons, (FEATURE . MODE).")

(defvar basis/emacs-lisp-hooks
  (mapcar (lambda (cons)
            (pcase-let ((`(,feature . ,mode) cons))
              (cons feature (intern (format "%s-hook" mode)))))
          basis/emacs-lisp-modes)
  "List of Emacs Lisp mode hooks.
Each element is a cons, (FEATURE . MODE).")

(defun basis/init-hippie-expand-for-elisp ()
  "Enable Lisp symbol completion in `hippie-exp'."
  (let ((functions (make-local-variable 'hippie-expand-try-functions-list)))
    (add-to-list functions #'try-complete-lisp-symbol t)
    (add-to-list functions #'try-complete-lisp-symbol-partially t)))

(defun basis/init-emacs-lisp-modes ()
  "Enable features useful when working with Emacs Lisp."
  ;; Paredit is enabled by `basis/init-lisp-generic'
  (elisp-slime-nav-mode t)
  (basis/init-hippie-expand-for-elisp)
  (turn-on-eldoc-mode)
  ;; Normally `lexical-binding' should be set within a file, but that doesn't
  ;; work for *scratch* and *ielm*
  (when (member (buffer-name) '("*scratch*" "*ielm*"))
    (setq lexical-binding t))
  ;; Use `common-lisp-indent-function', if it knows about Emacs Lisp
  (when (get 'if 'common-lisp-indent-function-for-elisp)
    (setq-local lisp-indent-function #'common-lisp-indent-function)))

(defun basis/init-emacs-lisp-mode ()
  (unless no-byte-compile
    (basis/maybe-enable-flycheck)))

(pcase-dolist (`(,feature . ,hook) basis/emacs-lisp-hooks)
  (eval-after-load feature
    `(add-hook ',hook #'basis/init-emacs-lisp-modes)))

(add-hook 'emacs-lisp-mode-hook #'basis/init-emacs-lisp-mode)

(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(with-eval-after-load 'lisp-mode
  (basis/define-eval-keys emacs-lisp-mode-map
    (last-sexp  #'basis/eval-last-sexp)
    (definition #'eval-defun)
    (region     #'eval-region)
    (buffer     #'eval-buffer)
    (something  #'basis/eval-something)
    (file       #'load-file)
    (expand     #'macrostep-expand))
  (basis/define-eval-keys lisp-interaction-mode-map
    (last-sexp  #'basis/eval-last-sexp)
    (definition #'eval-defun)
    (region     #'eval-region)
    (buffer     #'eval-buffer)
    (something  #'basis/eval-something)
    (file       #'load-file)
    (expand     #'macrostep-expand)))

;; paredit ---------------------------------------------------------------------

(defun basis/maybe-map-paredit-newline ()
  "Map `paredit-newline' except in some interactive modes."
  (unless (or (minibufferp) (memq major-mode '(inferior-emacs-lisp-mode
                                               inferior-lisp-mode
                                               inferior-scheme-mode
                                               geiser-repl-mode
                                               cider-repl-mode)))
    (local-set-key (kbd "RET") #'paredit-newline)))

(add-hook 'paredit-mode-hook #'basis/maybe-map-paredit-newline)

(defun basis/maybe-enable-paredit-mode ()
  "Enable Paredit during Lisp-related minibuffer commands."
  (let ((paredit-minibuffer-commands '(eval-expression
                                       pp-eval-expression
                                       eval-expression-with-eldoc
                                       slime-interactive-eval
                                       helm-eval-expression-with-eldoc)))
    (when (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode))))

(add-hook 'minibuffer-setup-hook #'basis/maybe-enable-paredit-mode)

(with-eval-after-load 'paredit
  (basis/define-keys paredit-mode-map
    ("M-)"             #'basis/paredit-wrap-round-from-behind)
    ("M-e"             #'paredit-forward)
    ("M-a"             #'paredit-backward)
    ("M-k"             #'kill-sexp)
    ("C-w"             #'basis/paredit-kill-something)
    ("M-DEL"           #'basis/paredit-kill-something)
    ("<C-M-backspace>" #'backward-kill-sexp)
    ("C-M-_"           #'backward-kill-sexp))
  (add-to-list 'paredit-space-for-delimiter-predicates
               #'basis/paredit-doublequote-space-p)
  ;; Show `eldoc' messages after Paredit motion commands
  (with-eval-after-load 'eldoc
    (eldoc-add-command 'paredit-forward
                       'paredit-forward-up
                       'paredit-forward-down
                       'paredit-backward
                       'paredit-backward-up
                       'paredit-backward-down
                       'paredit-newline)))

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-newline 'delete-selection t)

;; redshank --------------------------------------------------------------------

(redshank-setup '(lisp-mode-hook slime-repl-mode-hook) t)

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
  (basis/define-eval-keys slime-mode-map
    (last-sexp  #'slime-eval-last-expression)
    (definition #'slime-eval-defun)
    (region     #'slime-eval-region)
    (buffer     #'slime-eval-buffer)
    (something  #'basis/slime-eval-something)
    (file       #'slime-compile-and-load-file)
    (expand     #'slime-expand-1))
  (setq slime-autodoc-use-multiline-p t))

;; clojure ---------------------------------------------------------------------

(defun basis/init-clojure-mode ()
  (subword-mode)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-h m"))

(defun basis/init-cider-repl-mode ()
  (subword-mode)
  (cider-turn-on-eldoc-mode))

(defun basis/init-cider-mode ()
  (cider-turn-on-eldoc-mode))

(defun basis/set-lein-command-for-mac ()
  (when-let (lein (executable-find "lein"))
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
  (add-hook 'clojure-mode-hook #'basis/init-clojure-mode)

  ;; Indentation tweaks
  (pcase-dolist (`(,sym ,n) basis/clojure-indent-specs)
    (put-clojure-indent sym n))
  (put 'macrolet 'clojure-backtracking-indent '((2) 2)))

(with-eval-after-load 'cider
  (cond ((eq system-type 'darwin)
         (basis/set-lein-command-for-mac))
        (basis/cygwin-p
         (basis/set-lein-command-for-cygwin)))

  (add-hook 'cider-repl-mode-hook #'basis/init-cider-repl-mode)
  (add-hook 'cider-mode-hook #'basis/init-cider-mode)

  (setq cider-repl-use-pretty-printing t
        nrepl-log-messages t)

  (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-return)

  (basis/define-eval-keys cider-mode-map
    (last-sexp  #'cider-eval-last-sexp)
    (definition #'cider-eval-defun-at-point)
    (region     #'cider-eval-region)
    (buffer     #'cider-eval-buffer)
    (something  #'basis/cider-eval-something)
    (file       #'cider-load-current-buffer)
    (expand     #'cider-macroexpand-1)))

;; scheme ----------------------------------------------------------------------

(setq quack-default-program
      (if (eq system-type 'windows-nt)
          "racket"
        "guile"))

(setq quack-fontify-style 'emacs)

(with-eval-after-load 'scheme
  (require 'quack)
  (basis/define-eval-keys scheme-mode-map
    (last-sexp  #'scheme-send-last-sexp)
    (definition #'scheme-send-definition)
    (region     #'scheme-send-region)
    (something  #'basis/scheme-send-something)
    (file       #'scheme-load-file)
    (expand     #'scheme-expand-current-form)))

(with-eval-after-load 'geiser-mode
  (basis/define-eval-keys geiser-mode-map
    (last-sexp  #'geiser-eval-last-sexp)
    (definition #'geiser-eval-definition)
    (region     #'geiser-eval-region)
    (buffer     #'geiser-eval-buffer)
    (file       #'geiser-load-file)
    (something  #'basis/geiser-eval-something)
    (expand     #'basis/geiser-expand-something)))

;; smartparens -----------------------------------------------------------------

(smartparens-global-strict-mode)

(with-eval-after-load 'smartparens
  ;; I still prefer Paredit with Lisps, and having Smartparens enabled messes
  ;; with argument handling in `magit-key-mode'.
  (dolist (mode (cons 'magit-key-mode (mapcar #'cdr basis/lisp-modes)))
    (add-to-list 'sp-ignore-modes-list mode))

  (sp-use-paredit-bindings)

  (setq sp-cancel-autoskip-on-backward-movement nil
        sp-autoescape-string-quote nil
        sp-use-subword t)

  (setq-default sp-autoskip-closing-pair 'always)

  (sp-pair "'"
           nil
           :unless '(basis/sp-point-after-word-p)
           :actions '(insert wrap autoskip))

  (sp-local-pair 'org-mode "=" "=" :actions '(wrap))

  (sp-with-modes '(c-mode c++-mode java-mode)
    (sp-local-pair "{" "}" :actions '(:rem insert autoskip)))

  (basis/define-keys sp-keymap
    ("M-DEL"           #'basis/sp-kill-something)
    ("C-DEL"           #'basis/sp-kill-something)
    ("<C-backspace>"   #'basis/sp-kill-something)
    ("C-w"             #'basis/sp-kill-something)
    ("M-k"             #'sp-kill-sexp)
    ("M-e"             #'basis/maybe-sp-forward-sexp)
    ("M-a"             #'basis/maybe-sp-backward-sexp)
    ("<C-M-backspace>" #'sp-backward-kill-sexp)
    ("C-M-_"           #'sp-backward-kill-sexp))

  ;; These commands invoke `indent-according-to-mode' but, when
  ;; `indent-line-function' is `indent-relative', that often doesn't work out
  ;; too well.
  (basis/disable-relative-reindent-for
   '(sp-kill-word
     sp-backward-kill-word
     sp-kill-sexp
     sp-kill-hybrid-sexp
     basis/sp-kill-something))

  ;; Treat raw prefix arguments like numeric arguments
  (advice-add 'sp-backward-delete-char
              :filter-args
              #'basis/sp-backward-delete-no-prefix))

;; flycheck --------------------------------------------------------------------

(with-eval-after-load 'flycheck
  (defhydra basis/hydra-flycheck (:color blue :columns 2)
    ("c"   flycheck-buffer "check buffer")
    ("n"   flycheck-next-error "next error")
    ("p"   flycheck-previous-error "prev error")
    ("l"   flycheck-list-errors "list errors")
    ("s"   flycheck-select-checker "select checker")
    ("C"   flycheck-clear "clear")
    ("SPC" basis/flycheck-check-and-list-errors "check and list")
    ("q"   nil "cancel"))

  (global-set-key (kbd "C-h l") #'basis/hydra-flycheck/body)
  (global-set-key (kbd "<f8>")  #'basis/flycheck-check-and-list-errors)

  (setq flycheck-check-syntax-automatically nil)
  (unless (basis/libxml-available-p)
    (setq flycheck-xml-parser #'flycheck-parse-xml-region))

  ;; Check buffers with errors more frequently than ones without
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            #'basis/adjust-flycheck-idle-change-delay)

  ;; Keys for the errors buffer
  (basis/define-keys flycheck-error-list-mode-map
    ("n" #'flycheck-error-list-next-error)
    ("p" #'flycheck-error-list-previous-error)))

;; python ----------------------------------------------------------------------

(defun basis/init-python-mode ()
  (subword-mode 1)
  (basis/maybe-enable-flycheck)
  (setq fill-column 79)
  (setq tab-width 4)
  (setq-local whitespace-line-column 79)
  (setq-local electric-indent-chars (remove ?: electric-indent-chars)))

(defun basis/init-inferior-python-mode ()
  (subword-mode 1)
  (setq tab-width 4))

(with-eval-after-load 'python
  (basis/define-eval-keys python-mode-map
    (definition #'python-shell-send-defun)
    (buffer     #'python-shell-send-buffer)
    (region     #'python-shell-send-region)
    (something  #'basis/python-send-something)
    (file       #'python-shell-send-file))
  (basis/define-keys python-mode-map
    ("RET"     #'basis/electric-return)
    ("DEL"     #'basis/sp-python-backspace)
    ("C-c C-D" #'python-eldoc-at-point)
    ("C-c C-p" #'basis/run-python)
    ("C-h C-p" #'basis/insert-python-docstring-quotes)
    ("C-c C-j" nil))
  (setq python-fill-docstring-style 'pep-257-nn)
  (add-hook 'python-mode-hook #'basis/init-python-mode)
  (add-hook 'inferior-python-mode-hook #'basis/init-inferior-python-mode)
  (when (and (eq system-type 'windows-nt)
             basis/cygwin-p)
    (advice-add (if (fboundp 'python-shell-calculate-command)
                    'python-shell-calculate-command
                  'python-shell-parse-command)
                :filter-return
                #'basis/fix-bad-cygwin-file-name))
  ;; Jedi has 2 Python dependencies: jedi and epc
  (when (basis/jedi-installed-p)
    (setq jedi:setup-keys t
          jedi:tooltip-method nil)
    (add-hook 'python-mode-hook #'jedi:setup))
  ;; Hopefully-temporary workaround
  (when (and (eq system-type 'windows-nt)
             (boundp 'python-shell-completion-native-enable))
    (setq python-shell-completion-native-enable nil)))

;; haskell ---------------------------------------------------------------------

(defun basis/init-haskell-mode ()
  (turn-on-haskell-indentation)
  (interactive-haskell-mode))

(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook #'basis/init-haskell-mode))

(with-eval-after-load 'interactive-haskell-mode
  (basis/define-keys interactive-haskell-mode-map
    ("C-c C-z" #'haskell-interactive-bring)
    ("C-c C-l" #'haskell-process-load-or-reload)
    ("C-c C-k" #'haskell-interactive-mode-clear)
    ("C-c C-c" #'haskell-process-cabal-build)
    ("C-c c"   #'haskell-process-cabal)
    ("M-."     #'haskell-mode-goto-loc)
    ("M-?"     #'haskell-mode-find-uses)
    ("C-c C-t" #'haskell-mode-show-type-at)))

(autoload 'ghci-script-mode "ghci-script-mode"
  "Major mode for working with .ghci files."
  t)

(add-to-list 'auto-mode-alist '("\\.ghci\\'" . ghci-script-mode))

;; rust ------------------------------------------------------------------------

(defun basis/rust-set-compile-command ()
  (unless (or (file-exists-p "Makefile")
              (file-exists-p "makefile"))
    (setq-local compile-command
                (if (file-exists-p "Cargo.toml")
                    "cargo build"
                  (format "rustc %s"
                          (if buffer-file-name
                              (shell-quote-argument buffer-file-name)
                            ""))))))

(defun basis/init-rust-mode ()
  (subword-mode 1)
  (basis/rust-set-compile-command))

(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "RET") #'basis/electric-return)
  (add-hook 'rust-mode-hook #'basis/init-rust-mode))

;; javascript ------------------------------------------------------------------

(add-to-list 'auto-mode-alist (cons "\\.js\\'" 'js2-mode))

(setq-default js2-show-parse-errors nil
              js2-allow-rhino-new-expr-initializer nil
              js2-strict-inconsistent-return-warning nil
              js2-strict-missing-semi-warning nil
              js2-strict-trailing-comma-warning t)

(setq js2-basic-offset 2)

(defun basis/init-js2-mode ()
  (setq tab-width 4)
  (subword-mode 1)
  (js2-imenu-extras-setup))

(with-eval-after-load 'js2-mode
  (js2r-add-keybindings-with-prefix "C-h m")
  (define-key js2-mode-map (kbd "C-;") #'basis/eol-maybe-semicolon)
  (add-hook 'js2-mode-hook #'basis/init-js2-mode))

;; css-mode --------------------------------------------------------------------

(put 'css-indent-offset 'safe-local-variable #'integerp)

;; skewer ----------------------------------------------------------------------

(skewer-setup) ; hook into js2, html, and css modes

(with-eval-after-load 'skewer-mode
  (basis/define-eval-keys skewer-mode-map
    (last-sexp  #'skewer-eval-last-sexp)
    (definition #'skewer-eval-defun)
    (buffer     #'skewer-load-buffer)))

(with-eval-after-load 'skewer-repl
  (define-key skewer-repl-mode-map (kbd "TAB") #'hippie-expand))

(with-eval-after-load 'skewer-css
  (basis/define-eval-keys skewer-css-mode-map
    (last-sexp  #'skewer-css-eval-current-declaration)
    (definition #'skewer-css-eval-current-rule)
    (buffer     #'skewer-css-eval-buffer)))

;; sql -------------------------------------------------------------------------

(defun basis/init-sql-mode ()
  (sql-set-product "postgres")
  (setq tab-width 4)
  (basis/sql-modify-syntax-table))

(with-eval-after-load 'sql
  ;; But I also work with other products and it's often easier not to switch
  ;; `sql-product' around.
  (let ((more-keywords '("unload" "elsif" "endif" "while")))
    (add-to-list
     'sql-mode-postgres-font-lock-keywords
     (apply #'sql-font-lock-keywords-builder
            'font-lock-keyword-face nil more-keywords)))
  (basis/define-keys sql-mode-map
    ("RET"   #'basis/electric-return)
    ("TAB"   #'basis/sql-indent)
    ("DEL"   #'basis/sql-backspace-dedent)
    ("M-n"   #'basis/sql-forward-clause)
    ("M-p"   #'basis/sql-backward-clause)
    ("C-M-a" #'basis/sql-beginning-of-defun)
    ("C-M-e" #'basis/sql-end-of-defun))
  (add-hook 'sql-mode-hook #'basis/init-sql-mode))

;; When using Emacs as $PSQL_EDITOR, open the files in `sql-mode'
(add-to-list 'auto-mode-alist '("/psql.edit.[0-9]+\\'" . sql-mode))

;; cc-mode ---------------------------------------------------------------------

(defun basis/init-c-base ()
  (setq indent-tabs-mode nil
        c-basic-offset 4)
  (setq-local comment-style 'extra-line)
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

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-j") #'c-context-line-break)
  (add-hook 'c-mode-hook    #'basis/init-c)
  (add-hook 'c++-mode-hook  #'basis/init-c++)
  (add-hook 'java-mode-hook #'basis/init-java))

;; ack and a half --------------------------------------------------------------

(autoload 'ack-and-a-half "ack-and-a-half" "Run ack." t)

(defalias 'ack #'ack-and-a-half)
(defalias 'ack-same #'ack-and-a-half-same)
(defalias 'ack-find-file #'ack-and-a-half-find-file)
(defalias 'ack-find-file-same #'ack-and-a-half-find-file-same)

(when (file-exists-p "~/bin/ack")
  (setq ack-and-a-half-executable "~/bin/ack"))

(with-eval-after-load 'ack-and-a-half
  (basis/define-keys ack-and-a-half-mode-map
    ("n" #'compilation-next-error)
    ("p" #'compilation-previous-error)
    ("]" #'compilation-next-file)
    ("[" #'compilation-previous-file))
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
      org-agenda-files (mapcar (lambda (name)
                                 (expand-file-name name org-directory))
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
  ("C-c a" #'org-agenda)
  ("C-c b" #'org-iswitchb)
  ("C-c c" #'org-capture)
  ("C-c l" #'org-store-link))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "RET") #'org-return-indent))

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
  (fset 'org-babel-execute:clojure #'basis/org-babel-execute:clojure))

;; html ------------------------------------------------------------------------

(defun basis/init-simplezen ()
  (setq-local yas-fallback-behavior
              '(apply simplezen-expand-or-indent-for-tab)))

(defun basis/init-html-mode ()
  (setq tab-width 4)
  (tagedit-mode 1))

(defun basis/sgml-delete-tag-reindent (&rest _ignore)
  "Advice for `sgml-delete-region' to reindent the buffer."
  (indent-region (point-min) (point-max)))

(with-eval-after-load 'sgml-mode
  (require 'tagedit)
  (require 'simplezen)
  (basis/define-keys html-mode-map
    ([remap forward-paragraph]  #'basis/move-to-next-blank-line)
    ([remap backward-paragraph] #'basis/move-to-previous-blank-line)
    ("RET"                      #'basis/html-newline-and-indent)
    ("M-RET"                    #'basis/html-multiline-expand)
    ("C-c C-w"                  #'basis/html-wrap-in-tag)
    ("C-c w"                    #'basis/html-wrap-in-tag))
  (add-hook 'sgml-mode-hook #'basis/init-simplezen)
  (add-hook 'html-mode-hook #'basis/init-html-mode)
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features)
  (advice-add 'sgml-delete-tag :after #'basis/sgml-delete-tag-reindent))

(defun basis/tagedit-toggle-multiline-maybe-forward (function &rest args)
  "Advice for `tagedit-toggle-multiline-tag'.
Move forward by a line and indent if invoked directly between."
  (let ((move-forward-p (and (looking-at-p "<") (basis/looking-back-p ">"))))
    (apply function args)
    (when move-forward-p
      (forward-line 1)
      (indent-according-to-mode))))

(with-eval-after-load 'tagedit
  (advice-add 'tagedit-toggle-multiline-tag
              :around
              #'basis/tagedit-toggle-multiline-maybe-forward))

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
  (when (eq major-mode 'gfm-mode)
    (auto-fill-mode -1)))

(with-eval-after-load 'markdown-mode
  (basis/define-keys markdown-mode-map
    ("DEL"     #'basis/sp-markdown-backspace)
    ("M-n"     #'forward-paragraph)
    ("M-p"     #'backward-paragraph)
    ("C-c r"   #'markdown-insert-reference-link-dwim)
    ("C-c C-r" #'markdown-insert-reference-link-dwim))
  (add-hook 'markdown-mode-hook #'basis/init-markdown-mode))

;; yaml ------------------------------------------------------------------------

(defun basis/init-yaml-mode ()
  (basis/maybe-enable-flyspell-prog-mode)
  (when (basis/yaml-multiple-docs-p)
    (basis/yaml-multi-doc-mode)))

(with-eval-after-load 'yaml-mode
  (add-hook 'yaml-mode-hook #'basis/init-yaml-mode))

;; deft ------------------------------------------------------------------------

(setq deft-extension "md"
      deft-directory "~/Dropbox/deft"
      deft-text-mode  'gfm-mode)

;; gnus ------------------------------------------------------------------------

(setq gnus-use-dribble-file nil
      gnus-always-read-dribble-file nil
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil)

;; debbugs ---------------------------------------------------------------------

(setq debbugs-gnu-persistency-file (basis/emacs-file "var/debbugs"))

;; ssh-config-mode -------------------------------------------------------------

(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))

;; csv-mode --------------------------------------------------------------------

;; Prevent `csv-mode' from being enabled automatically
(dolist (elt auto-mode-alist)
  (pcase elt
    ((and (or `(,rgx . ,sym) `(,rgx ,sym . ,_))
          (guard (eq sym 'csv-mode)))
     (add-to-list 'auto-mode-alist (cons rgx 'text-mode)))))

;; batch-mode ------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))


;;; init.el ends here
