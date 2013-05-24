;; -----------------------------------------------------------------------------
;; ~/.emacs.d/init.el
;; -----------------------------------------------------------------------------

(require 'cl)

;; Disable superfluous UI immediately to prevent momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Define directories for various purposes and ensure they exist
(defvar basis/emacs-dir (file-name-directory load-file-name))
(defvar basis/site-lisp-dir (concat basis/emacs-dir "site-lisp/"))
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

;; Get the package infrastructure going
(require 'init-package)

;; Make dash and s available for use in defuns
(require 'dash)
(require 's)

;; Load custom functions
(let ((defuns-file (expand-file-name "defuns.el" basis/emacs-dir)))
  (when (file-exists-p defuns-file)
    (load defuns-file)))

;; Configure everything
(require 'init-settings)
(require 'init-interface)
(require 'init-uniquify)
(require 'init-keys)
(require 'init-ace-jump)
(require 'init-magit)
(require 'init-ibuffer)
(require 'init-dired)
(require 'init-eshell)
(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-yasnippet)
(require 'init-lisp)
(require 'init-paredit)
(require 'init-slime)
(require 'init-scheme)
(require 'init-autopair)
(require 'init-python)
(require 'init-sql)
(require 'init-c)
(require 'init-yaml-mode)
(require 'init-org)
(require 'init-markdown)
(require 'init-deft)
