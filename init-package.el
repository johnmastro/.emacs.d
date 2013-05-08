;; -----------------------------------------------------------------------------
;; init-package.el
;; -----------------------------------------------------------------------------

(require 'package)

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

;; (add-to-list 'package-archives
;;              '("geiser" . "http://download.savannah.gnu.org/releases/geiser/packages/"))

(package-initialize)

;; Make sure every archive is present in the elpa/archives/ folder
(let* ((archive-folder "~/.emacs.d/elpa/archives/")
       (archive-folders (mapcar #'(lambda (archive)
                                    (concat archive-folder (car archive)))
                                package-archives)))
  (unless (every #'file-exists-p archive-folders)
    (package-refresh-contents)))

(defun basis/maybe-select-archive (archive)
  "Filter all but a single archive from `package-archives`.
If `archive` is nil, `package-arcives` is returned in full."
  (if archive
      (let ((result (assoc-string archive package-archives)))
        (if result
            (list result)
          nil))
    package-archives))

(defun basis/install-package (package)
  "Install `package` if it isn't already installed.
`package` should be a cons with the package name in its car and,
optionally, the name of the archive to use in its cdr."
  (let ((name (car package))
        (repo (cdr package)))
    (unless (package-installed-p name)
      (let ((package-archives (basis/maybe-select-archive repo)))
        (if package-archives
            (package-install name)
          (message "Can't install %s: archive %s not found" name repo))))))

(defun basis/install-packages (packages)
  "Install each of `packages` via `basis/install-package`."
  (package-refresh-contents)
  (mapc #'basis/install-package packages)
  (package-initialize))

(let ((basis/required-packages
       '((paredit . melpa)
         (elisp-slime-nav . melpa)
         (expand-region . melpa)
         (smex . melpa)
         (dired+ . melpa)
         (diminish . melpa)
         (ido-ubiquitous . melpa)
         (undo-tree . melpa)
         (ack-and-a-half . melpa)
         (markdown-mode . melpa)
         (deft . melpa)
         (ace-jump-mode . melpa)
         (jump-char . melpa)
         (magit . melpa)
         (multiple-cursors . melpa)
         (helm . melpa)
         (auto-complete . melpa)
         (ac-slime . melpa)
         (smartparens . melpa)
         (redshank . melpa)
         (yaml-mode . melpa)
         (s . melpa)
         (move-text . melpa)
         (browse-kill-ring . melpa)
         )))
  (basis/install-packages basis/required-packages))


(provide 'init-package)
