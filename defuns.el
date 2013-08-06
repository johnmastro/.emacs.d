;;; defuns.el    -*- coding: utf-8; lexical-binding: t; -*-

(defun basis/occur-show-sections ()
  ;; Use occur to display and navigate the sections in this file
  (interactive)
  (occur ";; .+ -+$"))

;; misc. editing utilities -----------------------------------------------------

(defun beginning-of-line-or-indentation ()
  "Smarter `move-beginning-of-line'.
Go back to the first non-whitespace character or, if already
there, to the beginning of the line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (move-beginning-of-line nil)
    (back-to-indentation)))

(defun basis/comment-or-uncomment ()
  "Comment or uncomment the active region or current line."
  (interactive)
  (let ((reg-p (region-active-p)))
    (save-excursion
      (comment-or-uncomment-region
       (if reg-p (region-beginning) (line-beginning-position))
       (if reg-p (region-end) (line-end-position))))))

(defun basis/insert-enough-dashes (&optional arg)
  "Insert enough dashes to reach a specific column.
With a prefix arg, prompt for the target column. Otherwise use a
default of 80."
  (interactive "P")
  (let* ((goal (if arg (read-number "Target column: " 80) 80))
         (pos (- (point) (line-beginning-position)))
         (enough (- goal pos)))
    (insert (concat (-repeat enough ?-)))))

(defun basis/join-next-line ()
  "Join the next line up to the current one."
  (interactive)
  (join-line -1))

(defun basis/open-line-below ()
  "Open a new line below the current one."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun basis/open-line-above ()
  "Open a new line above the current one."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun basis/other-sexp-delimiter ()
  "Move to a matching delimiter.
Uses either `forward-sexp' or `backward-sexp' depending on the
context at point."
  ;; TODO: can this be made to work with quotes?
  (interactive)
  (cond ((looking-at (rx (syntax open-parenthesis)))  ; includes other brackets
         (forward-sexp))
        ((looking-back (rx (syntax close-parenthesis)))
         (backward-sexp))))

(defun basis/electric-return ()
  "Typical \"electric\" return, similar to that in CC Mode."
  (interactive)
  (when (memql (char-after) '(?\) ?\] ?}))
    (save-excursion (newline-and-indent)))
  (newline-and-indent))

;; kill commands ---------------------------------------------------------------

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun kill-line-backward ()
  "Kill everything before point. Respect indentation."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(defun smart-kill-almost-whole-line ()
  "Like `smart-kill-whole-line' but doesn't kill the newline."
  (interactive)
  (beginning-of-line-or-indentation)
  (kill-line nil))

(defun basis/kill-ring-save-something ()
  "Save the contents of the active region or the current line."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

(defun basis/kill-ring-save-buffer ()
  "Save the buffer's content to the kill ring."
  (interactive)
  (kill-ring-save (point-min) (point-max)))

(defun basis/kill-ring-save-whole-damn-buffer ()
  "Save the buffer's content to the kill ring.
Ignore narrowing but restore it when complete."
  (save-restriction
    (widen)
    (kill-ring-save (point-min) (point-max))))

(defun basis/clipboard-save-string (str)
  "Save STR directly to the system clipboard.
Do not save the string to the the kill ring."
  (funcall interprogram-cut-function str))

(defun basis/clipboard-save-region (beg end)
  "Save the region from BEG to END to the system clipboard.
Do not save the region to the kill ring."
  (interactive "r")
  (basis/clipboard-save-string
   (buffer-substring-no-properties beg end))
  (setq deactivate-mark t))

(defun basis/clipboard-save-buffer ()
  "Save the buffer's content directly to the system clipboard.
Do not save the content to the kill ring."
  (interactive)
  (basis/clipboard-save-region (point-min) (point-max)))

(defun basis/clipboard-save-whole-damn-buffer ()
  "Save the entire buffer's content directly to the clipboard.
Ignore narrowing but restore it when complete."
  (interactive)
  (save-restriction
    (widen)
    (basis/clipboard-save-buffer)))

;; case changing ---------------------------------------------------------------

(defun basis/upcase-something (&optional arg)
  "Upcase either the region or word(s).
This will call `upcase-region' or `upcase-word' depending on
whether the region is active."
  (interactive "p")
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word arg)))

(defun basis/downcase-something (&optional arg)
  "Downcase either the region or word(s).
This will call `downcase-region' or `downcase-word' depending on
whether the region is active."
  (interactive "p")
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word arg)))

(defun basis/capitalize-something (&optional arg)
  "Capitalize either the region or word(s).
This will call `capitalize-region' or `capitalize-word' depending
on whether the region is active."
  (interactive "p")
  (if (region-active-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word arg)))

(defun basis/bounds-of-something (thing)
  ;; Helper for `basis/toggle-case'
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point thing)))

(defun basis/toggle-case ()
  "Toggle the case of the region or the word at point."
  (interactive)
  (-when-let (bounds (basis/bounds-of-something 'word))
    (let ((beg (car bounds))
          (end (cdr bounds))
          (deactivate-mark nil)
          (case-fold-search nil))
      (unless (eq this-command last-command)
        (save-excursion
          (goto-char beg)
          (cond ((looking-at (rx lower lower))
                 (put this-command 'state :downcased))
                ((looking-at (rx upper upper))
                 (put this-command 'state :upcased))
                ((looking-at (rx upper lower))
                 (put this-command 'state :capitalized))
                ((looking-at (rx upper))
                 (put this-command 'state :upcased))
                (t
                 (put this-command 'state :downcased)))))
      (case (get this-command 'state)
        (:downcased
         (capitalize-region beg end)
         (put this-command 'state :capitalized))
        (:capitalized
         (upcase-region beg end)
         (put this-command 'state :upcased))
        (:upcased
         (downcase-region beg end)
         (put this-command 'state :downcased))))))

;; mark commands ---------------------------------------------------------------

(defun push-mark-no-activate ()
  "Pushes `point` to `mark-ring` without activating the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring` order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; buffer cleanup --------------------------------------------------------------

(defun untabify-buffer ()
  "Untabify the current buffer."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(defun indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun cleanup-buffer-safe ()
  "Clean up the whitespace content of a buffer conservatively."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Clean up and indent the current buffer."
  (interactive)
  (cleanup-buffer-save)
  (indent-buffer))

;; file utilities --------------------------------------------------------------

(defun rename-current-buffer-file ()
  "Rename the current buffer and the file it's visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists" new-name)
          (progn
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)
            (message "File '%s' renamed to '%s'"
                     name (file-name-nondirectory new-name))))))))

(defun delete-current-buffer-file ()
  "Kill the current buffer and delete the file it's visiting."
  (interactive)
  (let ((name (buffer-name))
        (buffer (current-buffer))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully deleted" filename)))))

(defun basis/recentf-ido-find-file ()
  "Find recently open files using ido and recentf."
  (interactive)
  (let* ((recent (mapcar #'abbreviate-file-name recentf-list))
         (file (ido-completing-read "Choose recent file: " recent nil t)))
    (when file
      (find-file file))))

(defun file-basename-sans-extension (filename)
  (file-name-sans-extension (file-name-nondirectory filename)))

(defun buffer-file-basename-sans-extension ()
  (file-basename-sans-extension (buffer-file-name)))

(defun basis/kill-all-buffers ()
  "Kill all buffers except *scratch*."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (string= (buffer-name buffer) "*scratch*")
      (kill-buffer buffer)))
  (delete-other-windows))

;; key binding utilities -------------------------------------------------------

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

(defmacro basis/define-hyper-keys (keymap &rest keydefs)
  "Define multiple hyper-modified key bindings for KEYMAP."
  `(progn
     ,@(mapcar #'(lambda (keydef)
                   (let ((key (car keydef))
                         (def (cadr keydef)))
                     `(basis/define-hyper ,keymap ,key ,def)))
               keydefs)))

(put 'basis/define-hyper-keys 'lisp-indent-function 'defun)

(defmacro create-simple-keybinding-command (name key)
  `(defun ,name (def &optional keymap)
     (define-key (or keymap global-map) (read-kbd-macro ,key) def)))

(create-simple-keybinding-command f1 "<f1>")
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

;; eshell ----------------------------------------------------------------------

(defun basis/eshell-kill-line-backward ()
  "Kill the current line backward, respecting Eshell's prompt."
  (interactive)
  (kill-region (save-excursion (eshell-bol) (point))
               (point)))

(defun basis/eshell-kill-whole-line ()
  "Kill the current line, respecting Eshell's prompt."
  (interactive)
  (kill-region (save-excursion (eshell-bol) (point))
               (save-excursion (move-end-of-line 1) (point))))

;; dired -----------------------------------------------------------------------

(defun basis/dired-jump-to-top ()
  "Move point to the first line representing a file."
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 2))

(defun basis/dired-jump-to-bottom ()
  "Move point to the last line representing a file."
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

;; macro utilities -------------------------------------------------------------

(define-minor-mode basis/el-macroexpansion-mode
  "Display Emacs Lisp macro expansions."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'quit-window)
            map))

(defun basis/expand-form (form)
  "Macroexpand FORM and display the result."
  (let ((expansion (macroexpand form))
        (buffer (get-buffer-create "*el-macroexpansion*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (with-output-to-string (pp expansion)))
      (emacs-lisp-mode)
      (basis/el-macroexpansion-mode 1)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun basis/expand-something (thing)
  "Macroexpand the form designated by THING."
  (basis/expand-form (form-at-point thing)))

(defun basis/expand-sexp-at-point ()
  "Display the expansion of the sexp at point."
  (interactive)
  (basis/expand-something 'sexp))

(defun basis/expand-defun ()
  "Display the expansion of the current toplevel form."
  (interactive)
  (basis/expand-something 'defun))

(defmacro with-unique-names (names &rest body)
  "Create unique names for use in a macro definition.
This idea also goes by the name `with-gensyms` in Common Lisp."
  `(let ,(mapcar (lambda (sym)
                   `(,sym (make-symbol (symbol-name ',sym))))
                 names)
     ,@body))

(put 'with-unique-names 'lisp-indent-function 1)

(defmacro looking-at-case (&rest clauses)
  (let ((trues '(t :else otherwise)))
    `(cond
      ,@(mapcar #'(lambda (clause)
                    (let ((expr (car clause)))
                      `(,(if (memq expr trues) t `(looking-at ,expr))
                        ,@(cdr clause))))
                clauses))))

;; window utilities ------------------------------------------------------------

(defun transpose-windows  (arg)
  "Transpose the relative positions of two or more windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win  (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun toggle-window-split ()
  "Toggle between horizontal and vertical window split (for two windows)."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (this-win-edges (window-edges (selected-window)))
           (next-win-edges (window-edges (next-window)))
           (this-win-2nd (not (and (<= (car this-win-edges)
                                       (car next-win-edges))
                                   (<= (cadr this-win-edges)
                                       (cadr next-win-edges)))))
           (splitter
            (if (= (car this-win-edges)
                   (car (window-edges (next-window))))
                'split-window-horizontally
              'split-window-vertically)))
      (delete-other-windows)
      (let ((first-win (selected-window)))
        (funcall splitter)
        (when this-win-2nd
          (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (when this-win-2nd
          (other-window 1))))))

;; misc. defuns ----------------------------------------------------------------

(defmacro after-load (feature &rest body)
  "Evaluate BODY after FEATURE is loaded."
  `(eval-after-load ,feature
     '(progn ,@body)))

(put 'after-load 'lisp-indent-function 1)

(defun basis/set-mode-name (mode name)
  "Set MODE's modeline string to NAME."
  (let ((hook (intern (s-concat (symbol-name mode) "-hook"))))
    (add-hook hook #'(lambda () (setq mode-name name)))))

(defun basis/google ()
  "Run a Google search.
Use the selected region as the search string if any, otherwise
display a prompt."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string
     (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(defun basis/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun basis/goto-line-with-numbers ()
  "Invoke `goto-line' with `linum-mode' temporarily enabled.
If `linum-mode' was already enabled just call `goto-line'."
  (interactive)
  (let ((linum-enabled-p (and (boundp 'linum-mode) linum-mode)))
    (unwind-protect
        (progn
          (unless linum-enabled-p (linum-mode 1))
          (call-interactively #'goto-line))
      (unless linum-enabled-p (linum-mode -1)))))

(defun basis/delete-buffer-file-elc ()
  "Delete the .elc corresponding to the current buffer, if any."
  (interactive)
  (when buffer-file-name
    (let ((elc (concat buffer-file-name "c")))
      (when (file-exists-p elc)
        (delete-file elc)))))

(defun basis/toggle-between-autopair-and-smartparens ()
  "Toggle between autopair and smartparens.
If neither mode is active, do nothing."
  (interactive)
  (cond ((and (boundp 'autopair-mode) autopair-mode)
         (autopair-mode -1)
         (smartparens-mode +1))
        ((and (boundp 'smartparens-mode) smartparens-mode)
         (smartparens-mode -1)
         (autopair-mode +1))))

;; paredit ---------------------------------------------------------------------

(defun basis/paredit-doublequote-space-p (endp delimiter)
  "Don't insert an extraneous space when entering a CL pathname."
  ;; If any of `paredit-space-for-delimiter-predicates' returns nil
  ;; a space isn't inserted.
  (let ((pathname-opening-p
         (and (not endp)
              (eql delimiter ?\")
              (memq major-mode '(lisp-mode common-lisp-mode slime-repl-mode))
              (save-excursion
                (backward-char 2)
                (looking-at "#p")))))
    (not pathname-opening-p)))

(defun basis/paredit-open-something ()
  (interactive)
  (call-interactively
   (if (memq major-mode '(clojure-mode nrepl-mode))
       #'paredit-open-square
     #'paredit-open-round)))

(defun basis/paredit-kill-something ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

(defun basis/paredit-wrap-from-behind (wrapper &optional spacep)
  (paredit-backward)
  (funcall wrapper)
  (when spacep
    (insert " ")
    (forward-char -1)))

(defun basis/paredit-wrap-round-from-behind ()
  (interactive)
  (basis/paredit-wrap-from-behind #'paredit-wrap-round t))

(defun basis/paredit-wrap-square-from-behind ()
  (interactive)
  (basis/paredit-wrap-from-behind #'paredit-wrap-square nil))

(defun basis/paredit-wrap-curly-from-behind ()
  (interactive)
  (basis/paredit-wrap-from-behind #'paredit-wrap-curly nil))

;; smartparens -----------------------------------------------------------------

(defun basis/sp-backward-kill-something (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (sp-backward-kill-word arg)))

;; python ----------------------------------------------------------------------

(defun basis/python-send-something ()
  "Send the active region or the current defun."
  (interactive)
  (if (region-active-p)
      (python-shell-send-region (region-beginning)
                                (region-end))
    (python-shell-send-defun nil)))

(defun basis/python-nav-backward-sexp (&optional arg)
  "Move backward by one sexp."
  (interactive "^p")
  (python-nav-forward-sexp (- arg)))

;; html utilities --------------------------------------------------------------

(defun basis/move-to-next-blank-line ()
  "Move point to the next blank line."
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun basis/move-to-previous-blank-line ()
  "Move point to the previous blank line."
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(defun basis/html-wrap-in-tag (beg end)
  "Wrap the selected region in a tag."
  (interactive "r")
  (let ((oneline? (= (line-number-at-pos beg) (line-number-at-pos end))))
    (deactivate-mark)
    (goto-char end)
    (unless oneline? (newline-and-indent))
    (insert "</div>")
    (goto-char beg)
    (insert "<div>")
    (unless oneline? (newline-and-indent))
    (indent-region beg (+ end 11))
    (goto-char (+ beg 4))))

(defun basis/html-newline-and-indent ()
  (interactive)
  (if (and (looking-at "<") (looking-back ">"))
      (tagedit-toggle-multiline-tag)
    (newline-and-indent)))

(defun basis/html-multiline-expand ()
  (interactive)
  (simplezen-expand)
  (basis/html-newline-and-indent))

;; skewer ----------------------------------------------------------------------

(defun basis/run-skewer ()
  (interactive)
  (let ((httpd-port 8042))
    (httpd-start)
    (message "HTTP server started. Jack in with the bookmarklet.")))

(defun basis/run-skewer-demo ()
  (interactive)
  (let ((httpd-port 8043))
    (httpd-start)
    (browse-url (format "http://localhost:%d/skewer/demo" httpd-port))
    (skewer-repl)))

;; selector stuff --------------------------------------------------------------

(defun insert/sorted (lst item &rest args)
  "Insert ITEM into sorted list LST.
Key and comparison functions can optionally be specified as
quasi-keyword arguments (`:key` and `:cmp` respectively)."
  (let* ((key (or (getf args :key) #'identity))
         (cmp (or (getf args :cmp) #'<))
         (ins? #'(lambda (x y)
                   (funcall cmp
                            (funcall key x)
                            (funcall key y))))
         (ins #'(lambda (lst)
                  (cond ((null lst) (cons item nil))
                        ((funcall ins? item (car lst)) (cons item lst))
                        (t (cons (car lst) (funcall ins (cdr lst))))))))
    (funcall ins lst)))

(defmacro push/sorted (place item &rest args)
  "Insert ITEM into the sorted list stored at PLACE.
Equivalent to (setq place (insert/sorted place item))."
  `(setq ,place (funcall #'insert/sorted ,place ,item ,@args)))

(defun basis/ido-in-dir-for (dir)
  "Return a closure calling `ido-find-file-in-dir' on DIR."
  #'(lambda () (ido-find-file-in-dir dir)))

(defun basis/dired-in-dir-for (dir)
  "Return a closure calling `dired' on DIR."
  #'(lambda () (dired dir)))

(defun basis/ido-tramp-for (host)
  "Return a closure for openning a file on HOST via ido."
  #'(lambda ()
      (find-file (ido-read-file-name "Find file: " (concat "/" host ":~/")))))

(defun basis/selector-quit ()
  #'(lambda () (throw 'quit t)))

;; TODO: abstract out the creation of these tables
(defvar basis/ido-dir-methods
  `((" " ido-find-file)
    ("c" ,(basis/ido-in-dir-for "~/code/"))
    ("d" ,(basis/ido-in-dir-for "~/dotfiles/"))
    ("D" ,(basis/ido-in-dir-for "~/Dropbox/"))
    ("e" ,(basis/ido-in-dir-for "~/.emacs.d/"))
    ("h" ,(basis/ido-in-dir-for "~/"))
    ("q" ,(basis/selector-quit)))
  "Directories to make available via `basis/ido-dir-selector'.")

(defvar basis/dired-dir-methods
  `((" " dired)
    ("c" ,(basis/dired-in-dir-for "~/code/"))
    ("d" ,(basis/dired-in-dir-for "~/dotfiles/"))
    ("D" ,(basis/dired-in-dir-for "~/Dropbox/"))
    ("e" ,(basis/dired-in-dir-for "~/.emacs.d/"))
    ("h" ,(basis/dired-in-dir-for "~/"))
    ("q" ,(basis/selector-quit)))
  "Directories to make available via `basis/dired-dir-selector'.")

(defvar basis/ido-tramp-methods
  ;; This is mainly beneficial on Windows, where  there's no
  ;; completion for hostnames. However, it can save a few keystrokes
  ;; on Unix-likes too.
  `((" " ido-find-file)
    ("a" ,(basis/ido-tramp-for "akira"))
    ("s" ,(basis/ido-tramp-for "sierra"))
    ("h" ,(basis/ido-tramp-for "hera"))
    ("m" ,(basis/ido-tramp-for "mira"))
    ("n" ,(basis/ido-tramp-for "nova"))
    ("q" ,(basis/selector-quit)))
  "Hosts to make available via `basis/ido-tramp-selector'.")

(defun basis/selector (method-table)
  "A simple selector, heavily inspired by `slime-selector'.
METHOD-TABLE should be an alist mapping single-character strings
to functions."
  (message "Select [%s]: "
           (apply #'concat
                  (remove " " (mapcar #'car method-table))))
  (let* ((str (save-window-excursion
                (select-window (minibuffer-window))
                (string (read-char))))
         (record (assoc str method-table)))
    (if record
        (catch 'quit (funcall (cadr record)))
      (message "No entry for '%s'" str)
      (speep-for 1)
      (discard-input)
      (funcall #'basis/selector method-table))))

(defun basis/ido-dir-selector ()
  "Open `ido-find-file-in-dir' in a specified directory."
  (interactive)
  (basis/selector basis/ido-dir-methods))

(defun basis/dired-dir-selector ()
  "Open `dired' in a specified directory."
  (interactive)
  (basis/selector basis/dired-dir-methods))

(defun basis/ido-tramp-selector ()
  "Use ido to find a file on a specified host."
  (interactive)
  (basis/selector basis/ido-tramp-methods))
