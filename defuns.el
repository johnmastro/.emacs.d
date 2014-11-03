;;; defuns.el    -*- coding: utf-8; lexical-binding: t; -*-

(defun basis/occur-show-sections ()
  ;; Use occur to display and navigate the sections in this file
  (interactive)
  (occur ";; .+ -+$"))

;; misc. editing utilities -----------------------------------------------------

(defun basis/beginning-of-line-or-indentation ()
  "Smarter `move-beginning-of-line'.
Go back to the first non-whitespace character or, if already
there, to the beginning of the line."
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    (when (= (point) start)
      (move-beginning-of-line nil))))

(defun basis/comment-or-uncomment ()
  "Comment or uncomment the active region or current line."
  (interactive)
  (let ((reg-p (use-region-p)))
    (comment-or-uncomment-region
     (if reg-p (region-beginning) (line-beginning-position))
     (if reg-p (region-end) (line-end-position)))))

(defun basis/insert-enough-dashes (&optional goal char)
  "Insert enough dashes to reach a specific column.
With a prefix arg, prompt for the target column and the character
to use. Otherwise use a default of 80 and char ?-."
  (interactive
   (when current-prefix-arg
     (list (read-number "Target column: ")
           (read-char "Character: "))))
  (let* ((goal (or goal 80))
         (char (or char ?-))
         (pos (- (point) (line-beginning-position)))
         (enough (- goal pos)))
    (insert (make-string enough char))))

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

(defun basis/electric-return ()
  "Typical \"electric\" return, similar to that in CC Mode."
  (interactive)
  (when (memql (char-after) '(?\) ?\] ?}))
    (save-excursion (newline-and-indent)))
  (newline-and-indent))

(defun basis/eol-maybe-semicolon ()
  (interactive)
  (move-end-of-line)
  (unless (basis/looking-back-p ";")
    (insert ";")))

(defun basis/java-insert-class ()
  (interactive)
  (-if-let (name (buffer-file-name))
      (let ((classname (file-name-sans-extension (file-name-base name))))
        (insert (format "public class %s {" classname))
        (newline 2)
        (insert "}")
        (previous-line)
        (indent-for-tab-command))
    (message "Buffer not visiting a file")))

(defun basis/wrap-in-curlies ()
  (interactive)
  (save-excursion
    (previous-line)
    (move-end-of-line 1)
    (unless (basis/looking-back-p " ")
      (insert " "))
    (insert "{")
    (forward-line)
    (move-end-of-line 1)
    (newline-and-indent)
    (insert "}")
    (indent-for-tab-command)))

(defun basis/insert-blank-below (&optional count)
  "Insert COUNT blank lines below point, without moving point."
  (interactive "p")
  (let ((count (or count 1)))
    (save-excursion
      (move-beginning-of-line 1)
      (forward-line 1)
      (while (> count 0)
        (newline)
        (setq count (1- count))))))

(defun basis/insert-blank-above (&optional count)
  "Insert COUNT blank lines above point, without moving point."
  (interactive "p")
  (let ((count (or count 1)))
    (save-excursion
      (forward-line -1)
      (move-end-of-line 1)
      (while (> count 0)
        (newline)
        (setq count (1- count))))))

(defun basis/insert-files (files &optional buffer)
  "Insert the contents of FILES into BUFFER.
If BUFFER is nil, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer (get-buffer buffer)
      (mapc #'insert-file-contents files))))

(defun basis/insert-directory-files (dir &optional pattern)
  "Insert the contents of DIR's files into BUFFER.
If PATTERN is non-nil, only include matching files (via
`file-expand-wildcards')."
  (interactive
   (list (ido-read-directory-name "Directory: " nil nil t)
         (when current-prefix-arg
           (read-string "Pattern: "))))
  (let ((files (file-expand-wildcards (expand-file-name (or pattern "*")
                                                        dir)
                                      t)))
    (if files
        (basis/insert-files files nil)
      (message "No files matching '%s' in '%s'" pattern dir))))

(defalias 'basis/concat-directory-files 'basis/insert-directory-files)

;; kill commands ---------------------------------------------------------------

(defun basis/kill-region-or-backward-word (arg)
  "Kill the region, or one or more words backward.
If `subword-mode' is active, use `subword-backward-kill'."
  (interactive "p")
  (cond ((use-region-p)
         (kill-region (region-beginning) (region-end)))
        ((bound-and-true-p subword-mode)
         (subword-backward-kill arg))
        (t
         (backward-kill-word arg))))

(defun basis/kill-line-backward ()
  "Kill everything before point. Respect indentation."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun basis/smart-kill-whole-line (&optional arg)
  "A simple wrapper around `kill-whole-line' that respects indentation."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(defun basis/smart-kill-almost-whole-line ()
  "Like `smart-kill-whole-line' but doesn't kill the newline."
  (interactive)
  (basis/beginning-of-line-or-indentation)
  (kill-line nil))

(defun basis/kill-ring-save-something ()
  "Save the contents of the active region or the current line."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

(defun basis/kill-ring-save-buffer ()
  "Save the buffer's content to the kill ring."
  (interactive)
  (kill-ring-save (point-min) (point-max)))

(defun basis/kill-ring-save-buffer-file-name (buffer &optional basename)
  "Save BUFFER's associated file name to the kill ring.
If BASENAME is non-nil, save only its base name. Otherwise save
its full path."
  (interactive
   (list (ido-completing-read "Buffer: "
                              (delq nil (mapcar (lambda (buf)
                                                  (when (buffer-file-name buf)
                                                    (buffer-name buf)))
                                                (buffer-list)))
                              nil
                              t)
         current-prefix-arg))
  (let ((filename (buffer-file-name (get-buffer buffer))))
    (basis/kill-ring-save-string
     (if basename
         (file-name-nondirectory filename)
       filename))))

(defun basis/clipboard-save-string (str)
  "Save STR directly to the system clipboard.
Do not save the string to the the kill ring."
  (funcall interprogram-cut-function str))

(defun basis/clipboard-save-something ()
  "Save the region or buffer to the system clipboard."
  (interactive)
  (if (use-region-p)
      (let ((s (buffer-substring-no-properties (region-beginning)
                                               (region-end))))
        (basis/clipboard-save-string s)
        (setq deactivate-mark t))
    (let ((s (buffer-substring-no-properties (point-min) (point-max))))
      (basis/clipboard-save-string s))))

(defun basis/s-add-indent (s &optional spaces pattern)
  (let ((spaces (or spaces 4))
        (pattern (or pattern "\\S-")))
    (mapconcat (lambda (line)
                 (if (string-match-p pattern line)
                     (concat (make-string spaces ? ) line)
                   line))
               (split-string s "\n")
               "\n")))

(defun basis/buffer-substring-indented (beg end)
  (basis/s-add-indent (buffer-substring-no-properties beg end)))

(defun basis/kill-ring-save-indented ()
  "Save text to the kill ring with four spaces of indentation added.
Save the region if one is currently active, otherwise save the
whole buffer."
  (interactive)
  (if (use-region-p)
      (let ((s (basis/buffer-substring-indented (region-beginning)
                                                (region-end))))
        (kill-new s)
        (setq deactivate-mark t))
    (kill-new (basis/buffer-substring-indented (point-min) (point-max)))))

;; case changing ---------------------------------------------------------------

(defun basis/upcase-something (&optional arg)
  "Upcase either the region or word(s).
This will call `upcase-region' or `upcase-word' depending on
whether the region is active."
  (interactive "p")
  (cond ((use-region-p)
         (upcase-region (region-beginning) (region-end)))
        ((bound-and-true-p subword-mode)
         (subword-upcase arg))
        (t
         (upcase-word arg))))

(defun basis/downcase-something (&optional arg)
  "Downcase either the region or word(s).
This will call `downcase-region' or `downcase-word' depending on
whether the region is active."
  (interactive "p")
  (cond ((use-region-p)
         (downcase-region (region-beginning) (region-end)))
        ((bound-and-true-p subword-mode)
         (subword-downcase arg))
        (t
         (downcase-word arg))))

(defun basis/capitalize-something (&optional arg)
  "Capitalize either the region or word(s).
This will call `capitalize-region' or `capitalize-word' depending
on whether the region is active."
  (interactive "p")
  (cond ((use-region-p)
         (capitalize-region (region-beginning) (region-end)))
        ((bound-and-true-p subword-mode)
         (subword-capitalize arg))
        (t
         (capitalize-word arg))))

;; mark commands ---------------------------------------------------------------

(defun basis/push-mark-no-activate ()
  "Pushes `point` to `mark-ring` without activating the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun basis/jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring` order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun basis/exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; buffer cleanup --------------------------------------------------------------

(defun basis/untabify-buffer ()
  "Untabify the current buffer."
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(defun basis/indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun basis/cleanup-buffer-safe ()
  "Clean up the whitespace content of a buffer conservatively."
  (interactive)
  (basis/untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun basis/cleanup-buffer ()
  "Clean up and indent the current buffer."
  (interactive)
  (basis/cleanup-buffer-safe)
  (basis/indent-buffer))

;; file utilities --------------------------------------------------------------

(defun basis/rename-current-buffer-file (destination)
  "Rename the current buffer's file to DESTINATION."
  (interactive "FDestination: ")
  (let ((name (buffer-name))
        (file (buffer-file-name)))
    (cond ((not (and file (file-exists-p file)))
           (error "Buffer '%s' is not visiting a file" file))
          ((get-buffer destination)
           (error "A buffer named '%s' already exists"))
          (t
           (rename-file file destination 1)
           (rename-buffer destination)
           (set-visited-file-name destination)
           (set-buffer-modified-p nil)
           (message "File '%s' renamed to '%s'"
                    name
                    (file-name-nondirectory destination))))))

(defun basis/delete-current-buffer-file ()
  "Kill the current buffer and delete the file it's visiting."
  (interactive)
  (let ((buffer (current-buffer))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file?")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully deleted" filename)))))

(defun basis/git-commit-msg-p (file)
  "Return true if FILE is a git \"COMMIT_EDITMSG\" file."
  ;; Used in `recentf-exclude'
  (string= "COMMIT_EDITMSG" (file-name-nondirectory file)))

(defun basis/ido-recentf ()
  "Find recently open files using ido and recentf."
  (interactive)
  (let* ((list (mapcar #'abbreviate-file-name recentf-list))
         (file (ido-completing-read "Recent file: " list nil t)))
    (when file
      (find-file file))))

(defun basis/open-file-manager (dir)
  "Open a system file manager at DIR.
If called interactively, use `ido' to read the directory."
  (interactive (list (ido-read-directory-name "Dir: " nil nil t)))
  (if (eq system-type 'windows-nt)
      (w32-shell-execute "explore" dir)
    (shell-command
     (format "%s %s"
             (pcase system-type
               (`gnu/linux  "nautilus")
               (`darwin     "open")
               (_ (error "No file manager known for: " system-type)))
             dir))))

(defun basis/open-file (file)
  "Open FILE in an external program."
  (let ((file (expand-file-name file)))
    (if (file-exists-p file)
        (pcase system-type
          (`windows-nt (let ((f (replace-regexp-in-string "/" "\\" file t t)))
                         (w32-shell-execute "open" f)))
          (`darwin     (shell-command (format "open \"%s\"" file)))
          (`gnu/linux  (let ((process-connection-type nil))
                         (start-process "" nil "xdg-open" file)))
          (_ (error "Don't know how to open files on system type '%s'"
                    system-type)))
      (error "File '%s' does not exist" file))))

(defun basis/open-files (&optional files)
  "Open each of FILES in external programs."
  (mapc #'basis/open-file files))

(defun basis/windows->unix (path)
  "Convert a path from Windows-style to UNIX-style."
  (->> path
    (replace-regexp-in-string "\\\\" "/")
    (replace-regexp-in-string "[a-zA-Z]:" "")))

(defun basis/read-file (file)
  "Read a Lisp form from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

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
     ,@(mapcar (lambda (keydef)
                 (let ((key (if (vectorp (car keydef))
                                (car keydef)
                              (read-kbd-macro (car keydef))))
                       (def (cadr keydef)))
                   `(define-key ,keymap ,key ,def)))
               keydefs)))

(put 'basis/define-keys 'lisp-indent-function 'defun)

(defmacro basis/define-hyper-keys (keymap &rest keydefs)
  "Define multiple hyper-modified key bindings for KEYMAP."
  `(progn
     ,@(mapcar (lambda (keydef)
                 (pcase-let ((`(,key ,def) keydef))
                   `(basis/define-hyper ,keymap ,key ,def)))
               keydefs)))

(put 'basis/define-hyper-keys 'lisp-indent-function 'defun)

(defmacro basis/define-key-translations (&rest keydefs)
  "Define multiple bindings in `key-translation-map'."
  `(progn
     ,@(mapcar (lambda (keydef)
                 (pcase-let ((`(,key ,def) keydef))
                   `(define-key key-translation-map (kbd ,key) (kbd ,def))))
               keydefs)))

(put 'basis/define-key-translations 'lisp-indent-function 'defun)

(defmacro basis/create-simple-keybinding-command (name key)
  `(defun ,name (def &optional keymap)
     (define-key (or keymap global-map) (read-kbd-macro ,key) def)))

(basis/create-simple-keybinding-command f1 "<f1>")
(basis/create-simple-keybinding-command f2 "<f2>")
(basis/create-simple-keybinding-command f3 "<f3>")
(basis/create-simple-keybinding-command f4 "<f4>")
(basis/create-simple-keybinding-command f5 "<f5>")
(basis/create-simple-keybinding-command f6 "<f6>")
(basis/create-simple-keybinding-command f7 "<f7>")
(basis/create-simple-keybinding-command f8 "<f8>")
(basis/create-simple-keybinding-command f9 "<f9>")
(basis/create-simple-keybinding-command f10 "<f10>")
(basis/create-simple-keybinding-command f11 "<f11>")
(basis/create-simple-keybinding-command f12 "<f12>")

;; ibuffer ---------------------------------------------------------------------

(defadvice ibuffer-vc-root (around exclude-emacs-buffers activate)
  "Only associate a buffer with a VC if it's visiting a file."
  (let ((buf (ad-get-arg 0)))
    (when (buffer-file-name buf)
      ad-do-it)))

(defvar basis/ibuffer-grouped-by-vc-p nil
  "Non-nil is grouping by VC root is enabled.")

(defun basis/ibuffer-disable-vc-groups ()
  "Disable grouping by VC root."
  (interactive)
  (ibuffer-assert-ibuffer-mode)
  (setq ibuffer-filter-groups nil)
  (ibuffer-update nil t))

(defun basis/ibuffer-toggle-vc-grouping ()
  "Toggle whether grouping by VC root is enabled."
  (interactive)
  (ibuffer-assert-ibuffer-mode)
  (if basis/ibuffer-grouped-by-vc-p
      (progn (basis/ibuffer-disable-vc-groups)
             (setq basis/ibuffer-grouped-by-vc-p nil))
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (setq basis/ibuffer-grouped-by-vc-p t)))

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
  (dired-next-line (if dired-hide-details-mode 1 2)))

(defun basis/dired-jump-to-bottom ()
  "Move point to the last line representing a file."
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun basis/dired-open-files ()
  "Open marked file(s) in external program(s).
If no files are marked, default to the file under point."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let* ((files (dired-get-marked-files))
             (count (length files)))
        (when (or (<= count 5)
                  (y-or-n-p (format "Really open %s files?" count)))
          (basis/open-files files)))
    (error "Not in a dired-mode buffer")))

(defvar basis/dired-sorting-options
  '(("modification" . "c")
    ("access"       . "u")
    ("size"         . "S")
    ("extension"    . "X")
    ("name"         . ""))
  "Sorting options and their associated ls switches.")

(defun basis/dired-sort-by (what)
  "Sort this `dired-mode' buffer by WHAT.
WHAT must be an option in `dired-sorting-options'."
  (interactive
   (list (ido-completing-read "Sort by: "
                              (mapcar #'car basis/dired-sorting-options)
                              nil
                              t)))
  ;; This assumes we can slap the sort option on the end of
  ;; `dired-listing-switches'. It works with my current setup (and the default
  ;; value) but is fragile and unsatisfactory.
  (let ((opt (cdr (assoc what basis/dired-sorting-options))))
    (if opt
        (dired-sort-other (concat dired-listing-switches opt))
      (error "Don't know how to sort by '%s'" what))))

;; direx -----------------------------------------------------------------------

;; `direx' includes a package `direx-project', which implements its own project
;; root finding. However, since I have `projectile' anyway it makes more sense
;; to use it.

(defun basis/direx-find-project-root-noselect ()
  (when (projectile-project-p)
    (direx:find-directory-noselect (projectile-project-root))))

(defun basis/direx-jump-to-project-root-noselect ()
  (interactive)
  (-if-let (buffer (basis/direx-find-project-root-noselect))
      (progn (direx:maybe-goto-current-buffer-item buffer)
             buffer)
    ;; Or fall back to `default-directory'?
    (error "Not in a project")))

(defun basis/direx-jump-to-project-root ()
  (interactive)
  (switch-to-buffer (basis/direx-jump-to-project-root-noselect)))

(defun basis/direx-jump-to-project-root-other-window ()
  (interactive)
  (switch-to-buffer-other-window (basis/direx-jump-to-project-root-noselect)))

;; emacs lisp ------------------------------------------------------------------

(define-minor-mode basis/elisp-display-mode
  "Display pretty-printed output macro expansions."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'quit-window)
            map))

(defun basis/eval-something ()
  "Eval the active region, if any; otherwise eval the toplevel form."
  (interactive)
  (if (use-region-p)
      (prog1 (eval-region (region-beginning)
                          (region-end))
        (setq deactivate-mark t))
    (eval-defun nil)))

(defun basis/display-elisp (string &optional buffer-or-name)
  (let ((buffer-or-name (or buffer-or-name "*Elisp Display*"))
        (buffer (get-buffer-create buffer-or-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert string)
      (emacs-lisp-mode)
      (basis/elisp-display-mode 1)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun basis/pp-eval-form (form &optional insert)
  "Eval FORM and pretty-print the result.
If INSERT is nil, display the result in a read-only buffer.
Otherwise, insert it into the current buffer."
  (let ((result (pp-to-string (eval form))))
    (if insert
        (insert result)
      (basis/display-elisp result "*PP Eval Output*"))))

(defun basis/pp-eval-last-sexp (arg)
  "Eval the last sexp and pretty-print the result.
If arg is nil, display the result is a read-only buffer.
Otherwise, insert the result into the current buffer."
  (interactive "P")
  (message "Evaluating...")
  (basis/pp-eval-form (pp-last-sexp) arg))

(defun basis/read-expression ()
  "Read an expression from the minibuffer and return it."
  (read-from-minibuffer
   "Eval: " nil read-expression-map t 'read-expression-history))

(defun basis/pp-eval-expression (arg)
  "Read an expression, eval it, and pretty-print the result.
If arg is nil, display the result is a read-only buffer.
Otherwise, insert the result into the current buffer."
  (interactive "P")
  (let ((expr (basis/read-expression)))
    (message "Evaluating... ")
    (basis/pp-eval-form expr arg)))

(defun basis/expand-form (form)
  "Macroexpand FORM and display the result."
  (let* ((expansion (macroexpand form))
         (string (with-output-to-string (pp expansion))))
    (basis/display-elisp string "*Elisp Macroexpansion*")))

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

(defun basis/eval-last-sexp (&optional arg)
  (interactive "P")
  (if arg
      (basis/pp-eval-last-sexp nil)
    (eval-last-sexp nil)))

(defmacro basis/with-unique-names (names &rest body)
  "Create unique names for use in a macro definition.
This idea also goes by the name `with-gensyms` in Common Lisp."
  `(let ,(mapcar (lambda (sym)
                   `(,sym (make-symbol (symbol-name ',sym))))
                 names)
     ,@body))

(put 'basis/with-unique-names 'lisp-indent-function 1)

(defun basis/add-elisp-font-lock-keywords (keywords)
  "Highlight KEYWORDS with `font-lock-keyword-face'."
  (with-eval-after-load 'lisp-mode
    (font-lock-add-keywords
     'emacs-lisp-mode
     `((,(concat "(\\s-*" (regexp-opt keywords 'paren) "\\_>")
        1 font-lock-keyword-face)) 'append)
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (when (and (eq major-mode 'emacs-lisp-mode)
                         (bound-and-true-p font-lock-mode))
                (font-lock-refresh-defaults))))
          (buffer-list))))

;; occur -----------------------------------------------------------------------

(defun basis/active-major-modes ()
  "Return a list of major modes for which a buffer is active."
  (-distinct (mapcar (lambda (buffer)
                       (with-current-buffer buffer
                         major-mode))
                     (buffer-list))))

(defun basis/find-mode-buffers (mode)
  "Return a list of the buffers whose major mode is MODE."
  (-filter (lambda (buffer)
             (with-current-buffer buffer
               (eq mode major-mode)))
           (buffer-list)))

(defun basis/ido-read-mode (prompt &optional choices)
  "Read the name of a major mode.
Optional argument CHOICES should, if provided, be a list of
symbols naming major modes."
  (let ((choices (or choices (basis/active-major-modes))))
    (intern (ido-completing-read "Mode: "
                                 (mapcar #'symbol-name choices)
                                 nil
                                 t))))

(defun basis/multi-occur-by-mode (mode regexp &optional nlines)
  "Run `multi-occur' on all buffers in MODE.
REGEXP and NLINES are passed on to `multi-occur' unchanged."
  (interactive (cons (basis/ido-read-mode "Mode: ")
                     (occur-read-primary-args)))
  (multi-occur (basis/find-mode-buffers mode)
               regexp
               nlines))

(defun basis/multi-occur-this-mode (regexp &optional nlines)
  (interactive (occur-read-primary-args))
  (basis/multi-occur-by-mode major-mode regexp nlines))

;; window utilities ------------------------------------------------------------

(defun basis/transpose-windows (arg)
  "Transpose the relative positions of two or more windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (dotimes (_ (abs arg))
      (let ((this-win  (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector))))))

(defun basis/toggle-window-split ()
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

(defun basis/ack-somewhere (arg default-dir)
  (unless (featurep 'ack-and-a-half)
    (require 'ack-and-a-half nil t))
  (let* ((regexp-p  (if arg
                        (not ack-and-a-half-regexp-search)
                      ack-and-a-half-regexp-search))
         (pattern   (read-from-minibuffer "Ack: "))
         (directory (ido-read-directory-name "Directory: " default-dir nil t)))
    (ack-and-a-half pattern regexp-p directory)))

(defun basis/ack-here (&optional arg)
  "Do an ack search. Prompt for the directory to use.
Default to `default-directory'."
  (interactive "P")
  (basis/ack-somewhere arg default-directory))

(defun basis/set-mode-name (mode name)
  "Set MODE's modeline string to NAME."
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda () (setq mode-name name)))))

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

(defun basis/maybe-enable-flyspell ()
  "Enable `flyspell-mode' if aspell is installed."
  (when ispell-program-name  ; Set to nil if aspell isn't installed
    (flyspell-mode 1)))

(defun basis/maybe-enable-flyspell-prog-mode ()
  "Enable `flyspell-prog-mode' in programming modes for local files."
  (when (and ispell-program-name
             buffer-file-name
             (not (file-remote-p buffer-file-name)))
    (flyspell-prog-mode)))

(defun basis/maybe-enable-whitespace-mode ()
  "Enable `whitespace-mode' in programming modes (but not REPLs)."
  (interactive)
  (unless (or (derived-mode-p 'comint-mode)
              (eq major-mode 'eshell-mode)
              (eq major-mode 'cider-repl-mode))
    (whitespace-mode 1)))

(defun basis/find-mode-keymap (mode)
  "Find (by name) and return the keymap for MODE.
If no keymap is found, return nil."
  (let ((name (intern (concat (symbol-name mode) "-map"))))
    (when (boundp name)
      (symbol-value name))))

(defun basis/isearch-yank-something ()
  "Pull the current region or symbol into the search string."
  (interactive)
  (isearch-yank-string
   (if (use-region-p)
       (let ((str (buffer-substring (region-beginning) (region-end))))
         (deactivate-mark)
         str)
     (or (thing-at-point 'symbol)
         ""))))

(defun basis/kill-this-buffer ()
  "Kill the currently active buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun basis/ido-sort-files-by-modtime ()
  "Sort ido matches my modification time, descending."
  (interactive)
  (when ido-matches
    (let* ((directory ido-current-directory)
           (predicate (lambda (file1 file2)
                        (file-newer-than-file-p
                         (expand-file-name file1 directory)
                         (expand-file-name file2 directory)))))
      (setq ido-cur-list (sort ido-cur-list predicate)
            ido-matches (sort ido-matches predicate)
            ido-rescan nil))))

(defun basis/disable-themes (&optional themes)
  "Disable THEMES (defaults to `custom-enabled-themes')."
  (interactive)
  (mapc #'disable-theme (or themes custom-enabled-themes)))

(defun basis/libxml-available-p ()
  (and (fboundp 'libxml-parse-html-region)
       (with-temp-buffer
         (insert "<html></html>")
         (not (null (libxml-parse-html-region (point-min) (point-max)))))))

(defun basis/yas-expand-or-insert ()
  "Call `yas-expand' or `yas-insert-snippet' depending on context.
If point is after what might be a snippet key, call `yas-expand',
otherwise call `yas-insert-snippet'."
  (interactive)
  (call-interactively
   (if (looking-at-p "\\>") #'yas-expand #'yas-insert-snippet)))

(defun basis/kill-ring-save-string (str)
  "Save STR to the kill ring."
  (with-temp-buffer
    (insert str)
    (kill-ring-save (point-min) (point-max))))

(defun basis/fit-frame-width-to-buffer (&optional frame)
  "Adjust the width of FRAME to display the contents its buffer.
FRAME defaults to the selected frame."
  (interactive)
  (let* ((frame (or frame (window-frame (selected-window))))
         (height (frame-height frame)))
    (fit-frame-to-buffer frame height)))

(defun basis/full-calc-frame ()
  "Create a new frame and run `calc' in a full-size window."
  (interactive)
  (with-selected-frame (make-frame)
    (calc nil t t)))

(defun basis/looking-back-p (regexp)
  "Same as `looking-back' but don't change the match data."
  (save-match-data
    (looking-back regexp)))

(defun basis/find-clang-includes-path (&optional language)
  "Return clang's #include <...> search path."
  ;; This isn't a very satisfactory solution but it's "good enough"
  (-filter #'file-directory-p
           (pcase (or language 'c)
             (`c (let* ((cmd "clang -v -xc -")
                        (str (ignore-errors (shell-command-to-string cmd)))
                        (str (or str ""))
                        (lines (split-string str "\n" t "[[:space:]]+"))
                        (result '()))
                   (catch 'return
                     (while lines
                       (pcase-let ((`(,line . ,more) lines))
                         (if (string= line "#include <...> search starts here:")
                             (dolist (path more)
                               (if (string= path "End of search list.")
                                   (throw 'return (nreverse result))
                                 (push path result)))
                           (setq lines more)))))))
             ;; Hardcoded based on an Ubuntu 12.04 box...
             (`c++ '("/usr/include/c++/4.6"
                     "/usr/include/c++/4.6/i686-linux-gnu"
                     "/usr/include/c++/4.6/backward"
                     "/usr/local/include"
                     "/usr/include/clang/3.3/include"
                     "/usr/include/i386-linux-gnu"
                     "/usr/include")))))

(defun basis/build-clang-args (&optional language)
  (let* ((language (or language 'c))
         (standard (pcase language
                     (`c   "c11")
                     (`c++ "c++11")))
         (includes (basis/find-clang-includes-path language)))
    (when (and standard includes)
      (cons (format "-std=%s" standard)
            (basis/find-clang-includes-path language)))))

(defun basis/enable-company-clang ()
  "Enable `company-clang' for the current buffer."
  (add-to-list 'company-backends 'company-clang))

(defun basis/kill-frame-or-terminal (&optional arg)
  "Kill the current frame or session.
If there is more than one live frame, close the current one.
Otherwise kill the current session. If optional ARG is non-nil,
kill the current session even if there are multiple frames."
  (interactive "P")
  (if (or arg (= (length (frame-list)) 1))
      (save-buffers-kill-terminal)
    (delete-frame)))

(defun basis/in-string-p ()
  "Return non-nil if point is within a string."
  (let ((state (syntax-ppss)))
    (and (nth 3 state) (nth 8 state))))

;; paredit ---------------------------------------------------------------------

(defun basis/paredit-doublequote-space-p (endp delimiter)
  "Don't insert an extraneous space when entering a CL pathname."
  ;; If any of `paredit-space-for-delimiter-predicates' returns nil
  ;; a space isn't inserted.
  (let* ((double-quote 34)
         (pathname-opening-p
          (and (not endp)
               (eql delimiter double-quote)
               (memq major-mode '(lisp-mode common-lisp-mode slime-repl-mode))
               (save-excursion
                 (backward-char 2)
                 (looking-at-p "#p")))))
    (not pathname-opening-p)))

(defun basis/paredit-open-something ()
  (interactive)
  (call-interactively
   (if (memq major-mode '(clojure-mode cider-repl-mode))
       #'paredit-open-square
     #'paredit-open-round)))

(defun basis/paredit-kill-region-or-backward-word ()
  (interactive)
  (if (use-region-p)
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

(defun basis/sp-kill-region-or-backward-word (&optional arg)
  "Call `sp-backward-kill-word' or `kill-region'. "
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (sp-backward-kill-word arg)))

(defmacro basis/with-sp-backward-delete (&rest body)
  "Execute BODY with `sp-backward-delete-char' overriding
`backward-delete-char' and `backward-delete-char-untabify'."
  `(cl-letf (((symbol-function 'backward-delete-char)
              #'sp-backward-delete-char)
             ((symbol-function 'backward-delete-char-untabify)
              #'sp-backward-delete-char))
     ,@body))

(defun basis/sp-python-backspace (arg)
  "Delete a char backward or dedent the current line."
  (interactive "*p")
  (basis/with-sp-backward-delete (python-indent-dedent-line-backspace arg)))

(put 'basis/sp-python-backspace 'delete-selection 'supersede)

(defun basis/sp-markdown-backspace (arg)
  "Delete a char backward or dedent the current line."
  (interactive "*p")
  (basis/with-sp-backward-delete (markdown-exdent-or-delete arg)))

(put 'basis/sp-markdown-backspace 'delete-selection 'supersede)

(defun basis/insert-right-bracket ()
  (interactive)
  (insert "]"))

(defun basis/maybe-sp-forward-sexp (&optional arg)
  (interactive "p")
  (let ((arg (or arg 1)))
    (if (memq major-mode '(python-mode inferior-python-mode))
        (python-nav-forward-sexp arg)
      (sp-forward-sexp arg))))

(defun basis/maybe-sp-backward-sexp (&optional arg)
  (interactive "p")
  (let ((arg (or arg 1)))
    (if (memq major-mode '(python-mode inferior-python-mode))
        (basis/python-nav-backward-sexp arg)
      (sp-backward-sexp arg))))

(defun basis/sp-point-after-word-p (id action context)
  "Like `sp-point-after-word-p' but special-case Python's strings.
Specifically, this handles the \"u\", \"r\", and \"b\" prefixes
used to create Unicode, raw, and byte strings respectively."
  (let ((result (sp-point-after-word-p id action context)))
    (if (and (memq major-mode '(python-mode inferior-python-mode))
             (member id '("'" "\"")))
        (let ((raw-string (concat "\\([^\\sw\\s_]\\)[bru]" (regexp-quote id))))
          (and result (not (looking-back raw-string))))
      result)))

(defun basis/sp-disable-sql-reindent (function &rest args)
  "Advice to disable SP's automatic reindentation in `sql-mode'."
  (if (and (eq major-mode 'sql-mode)
           (eq indent-line-function 'indent-relative))
      (cl-letf (((symbol-function 'indent-according-to-mode)
                 (symbol-function 'ignore)))
        (apply function args))
    (apply function args)))

(advice-add 'sp-kill-word :around #'basis/sp-disable-sql-reindent)
(advice-add 'sp-backward-kill-word :around #'basis/sp-disable-sql-reindent)
(advice-add 'sp-kill-hybrid-sexp :around #'basis/sp-disable-sql-reindent)
(advice-add 'basis/sp-kill-region-or-backward-word
            :around
            #'basis/sp-disable-sql-reindent)

;; scheme/geiser ---------------------------------------------------------------

(defun basis/scheme-send-something ()
  (interactive)
  (if (use-region-p)
      (scheme-send-region (region-beginning)
                          (region-end))
    (scheme-send-definition)))

(defun basis/geiser-eval-something ()
  (interactive)
  (if (use-region-p)
      (geiser-eval-region (region-beginning)
                          (region-end))
    (geiser-eval-definition)))

(defun basis/geiser-eval-something-and-go ()
  (interactive)
  (if (use-region-p)
      (geiser-eval-region-and-go (region-beginning)
                                 (region-end))
    (geiser-eval-definition-and-go)))

(defun basis/geiser-expand-something ()
  (interactive)
  (if (use-region-p)
      (geiser-expand-region (region-beginning)
                            (region-end))
    (geiser-expand-last-sexp)))

;; python ----------------------------------------------------------------------

(defun basis/python-send-something ()
  "Send the active region or the current defun."
  (interactive)
  (if (use-region-p)
      (python-shell-send-region (region-beginning)
                                (region-end))
    (python-shell-send-defun nil)))

(defun basis/python-nav-backward-sexp (&optional arg)
  "Move backward by one sexp."
  (interactive "^p")
  (python-nav-forward-sexp (- arg)))

(defun basis/jedi-installed-p ()
  "Return t if Python, Jedi, and EPC are installed, otherwise nil."
  (when (executable-find "python")
    (let* ((cmd "python -c \"import jedi; import epc; exit()\"")
           (out (shell-command-to-string cmd)))
      (zerop (length out)))))

(defun basis/insert-python-docstring-quotes ()
  "Insert the 6 double quotes for a Python docstring."
  (interactive)
  (let ((double-quote 34))
    (insert (make-string 6 double-quote)))
  (backward-char 3))

(defun basis/run-python ()
  "Adjust the frame's configuration and run Python."
  (interactive)
  (unless (eq major-mode 'python-mode)
    (error "Not in a python-mode buffer"))
  (delete-other-windows)
  (let ((frame (selected-frame)))
    (when (< (frame-width frame) 165)
      (set-frame-width frame 165)))
  (call-interactively #'run-python))

;; slime -----------------------------------------------------------------------

(defun basis/slime-eval-something ()
  "Eval the active region, if any; otherwise eval the toplevel form."
  (interactive)
  (if (use-region-p)
      (slime-eval-region (region-beginning)
                         (region-end))
    (slime-eval-defun)))

(defun basis/slime-expand-defun (&optional repeatedly)
  "Display the macro expansion of the form surrounding point.
Use `slime-expand-1' to produce the expansion."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (slime-expand-1 repeatedly)))

;; clojure ---------------------------------------------------------------------

(defun basis/cider-trust-me ()
  "Run `cider-jack-in' without checking for lein."
  ;; Necessary because `executable-find' can't find lein on my Cygwin box for
  ;; some reason, despite the fact that it's present and works.
  (interactive)
  (unless (featurep 'cider)
    (require 'cider))
  (cl-letf (((symbol-function 'cider--lein-present-p)
             (lambda () t)))
    (call-interactively #'cider-jack-in)))

(defun basis/cider-eval-something (&optional prefix)
  (interactive "P")
  (if (use-region-p)
      (cider-eval-region (region-beginning) (region-end))
    (cider-eval-expression-at-point prefix)))

(defun basis/helm-clj-headlines ()
  (interactive)
  (helm :sources '(((name . "Clojure Headlines")
                    (volatile)
                    (headline "^[;(]")))))

;; gdb -------------------------------------------------------------------------

(defun basis/gdb-a-few-windows ()
  "Create a three-window GDB configuration.
The three buffers are the GUD buffer, the source buffer, and the
locals buffer. Based on `gdb-many-windows', but without quite so
many windows."
  (interactive)
  (when (and gud-comint-buffer
             (buffer-name gud-comint-buffer))
    (ignore-errors
      (basis/gdb-setup-windows))))

(defun basis/gdb-setup-windows ()
  ;; Based on `gdb-setup-windows', but creating a simpler configuration with
  ;; fewer windows (just GUD, source, and locals).
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (let* ((win0 (selected-window))
         (win1 (split-window-right))
         (win2 (with-selected-window win1 (split-window-below))))
    (gdb-set-window-buffer (gdb-locals-buffer-name) nil win2)
    (set-window-buffer
     win1
     (cond (gud-last-last-frame
            (gud-find-file (car gud-last-last-frame)))
           (gdb-main-file
            (gud-find-file gdb-main-file))
           (t
            ;; Put buffer list in window if we can't find a source file.
            (list-buffers-noselect))))
    (setq gdb-source-window win1)
    (select-window win0)))

;; org-mode --------------------------------------------------------------------

(defun basis/process-clojure-output (s)
  (mapconcat (lambda (line)
               (->> line
                 (string-remove-suffix "\r")
                 (concat ";; ")))
             (split-string (string-remove-suffix "\n" s) "\n")
             "\n"))

(defun basis/org-babel-execute:clojure (body params)
  (let* ((result (nrepl-send-string-sync body (cider-current-ns)))
         (value (plist-get result :value))
         (stdout (-when-let (s (plist-get result :stdout))
                   (basis/process-clojure-output s)))
         (stderr (-when-let (s (plist-get result :stderr))
                   (basis/process-clojure-output s)))
         (output (concat stdout
                         (when (and stdout (not (string-suffix-p "\n" stdout)))
                           "\n")
                         stderr)))
    (concat output
            (when (and output
                       (not (string= output ""))
                       (not (string-suffix-p "\n" output)))
              "\n")
            (when value (concat ";;=> " value)))))

(defun basis/org-babel-execute-in-cider-repl ()
  (interactive)
  (let ((body (cadr (org-babel-get-src-block-info))))
    (cider-eval-last-sexp-to-repl body)))

;; sql -------------------------------------------------------------------------

(defvar basis/sql-clause-start-regexp
  (rx word-start
      (or "create" "delete" "drop" "from"
          "having" "insert" "intersect" "into"
          "select" "set" "truncate" "union"
          "update" "where" "with"
          (seq "group" (1+ space) "by")
          (seq "order" (1+ space) "by"))
      word-end)
  "Regexp matching SQL keywords that begin clauses.")

(defun basis/sql-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment."
  (let ((state (syntax-ppss)))
    (or (nth 3 state)
        (nth 4 state))))

(defun basis/sql-forward-clause (&optional n)
  "Move to the start of the next clause of the statement.
With arg N, move forward that many times."
  (interactive "p")
  (let ((n (or n 1)))
    (if (< n 0)
        (basis/sql-backward-clause (- n))
      (let ((start (point)))
        ;; If we're already on a clause starter, move over it
        (when (looking-at basis/sql-clause-start-regexp)
          (goto-char (match-end 0)))
        (while (and (> n 0)
                    (re-search-forward basis/sql-clause-start-regexp nil t))
          (unless (basis/sql-in-string-or-comment-p)
            (setq n (- n 1))))
        ;; If we never moved, the last match will be from something else
        (let ((point (point)))
          (unless (= point start)
            (goto-char (match-beginning 0))
            point))))))

(defun basis/sql-backward-clause (&optional n)
  "Move to the start of the previous clause of the statement.
With arg N, move backward that many times."
  (interactive "p")
  (let ((n (or n 1)))
    (if (< n 0)
        (basis/sql-forward-clause (- n))
      (let ((start (point)))
        (while (and (> n 0)
                    (re-search-backward basis/sql-clause-start-regexp nil t))
          (unless (basis/sql-in-string-or-comment-p)
            (setq n (- n 1))))
        (let ((point (point)))
          (unless (= point start)
            point))))))

(defun basis/sql-beginning-of-defun ()
  "Move to the beginning of the current SQL statement."
  ;; This treats nested statements (e.g. subqueries) as defuns. I find it quite
  ;; convenient at times, but I'm not sure if it's the right thing to do.
  (interactive)
  (let ((state (syntax-ppss)))
    (when (and (> (car state) 0)
               (looking-at-p sql-ansi-statement-starters))
      (backward-up-list)
      (setq state (syntax-ppss)))
    (if (= (car state) 0)
        (while (and (progn (forward-line -1)
                           (back-to-indentation)
                           t)
                    (not (bobp))
                    (not (and (looking-at-p sql-ansi-statement-starters)
                              (save-excursion
                                (forward-line -1)
                                (looking-at-p  "^[[:space:]]*$")))))
          'keep-going)
      (let ((start (point)))
        (while (and (cadr state)
                    (goto-char (cadr state))
                    (basis/sql-forward-clause 1)
                    (> (point) start))
          (goto-char (cadr state))
          (setq state (syntax-ppss)))))))

(defun basis/sql-end-of-defun ()
  "Move to the end of the current SQL statement."
  (interactive)
  (let ((start (point))
        (state (syntax-ppss)))
    (if (= (car state) 0)
        ;; TODO: Lose the assumption that statements never contain blank lines
        (progn
          (while (and (looking-at "^[[:space:]]*$")
                      (not (eobp)))
            (forward-line 1))
          (while (and (not (looking-at "^[[:space:]]*$"))
                      (not (eobp)))
            (forward-line 1)))
      ;; Seems a bit unfortunate to have to find the beginning of the defun in
      ;; order to find its end
      (basis/sql-beginning-of-defun)
      (setq state (syntax-ppss))
      (if (cadr state)
          (progn (goto-char (cadr state))
                 (forward-sexp))
        (goto-char start)))))

(defun basis/recapitalize-sql-buffer (style)
  "Recapitalize the current buffer to STYLE (caps or none)."
  (interactive
   (list (intern (ido-completing-read  "Style: " '("caps" "none") nil t))))
  (unless (memq style '(caps none))
    (error "Unknown capitalization style '%s'" style))
  (when (or (eq major-mode 'sql-mode)
            (y-or-n-p "Not in a SQL buffer. Proceed anyway?"))
    (save-excursion
      (goto-char (point-min))
      (let ((last -1))
        (while (> (point) last)
          (let ((face (get-text-property (point) 'face)))
            (cond ((memq face '(font-lock-builtin-face
                                font-lock-keyword-face
                                font-lock-type-face))
                   (if (eq style 'caps)
                       (upcase-word 1)
                     (downcase-word 1))
                   (backward-word 1))
                  ((null face)
                   (downcase-word 1)
                   (backward-word 1))))
          (setq last (point))
          (forward-word 2)
          (backward-word 1))))))

(defun basis/modify-sql-syntax-table ()
  "Set double quote's syntax to string delimiter.
By default, SQL treats double quote as punctuation. That's
arguably accurate (real strings are delimited with single quotes)
but it's still natural to work with e.g. column names as
strings."
  (let ((buffer (current-buffer)))
    (run-at-time
     "1 sec"
     nil
     (lambda ()
       ;; This doesn't work correctly if run immediately, I think because the
       ;; syntax table for the particular SQL product isn't initialized yet.
       (with-current-buffer buffer
         (modify-syntax-entry 34 "\""))))))

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
  (if (and (looking-at-p "<") (basis/looking-back-p ">"))
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

;; flycheck --------------------------------------------------------------------

(defun basis/flycheck-check-and-list-errors ()
  "Run a check and show the errors, if any."
  (interactive)
  (flycheck-buffer)
  (flycheck-list-errors))

(defun basis/flycheck-enable-automatic-checking ()
  "Enable automatic syntax checking by Flycheck."
  (interactive)
  (setq flycheck-check-syntax-automatically
        '(save idle-change mode-enabled)))

(defun basis/flycheck-disable-automatic-checking ()
  "Disable automatic syntax checking by Flycheck."
  (interactive)
  (setq flycheck-check-syntax-automatically nil))

(defun basis/adjust-flycheck-idle-change-delay ()
  "Adjust Flycheck's idle change delay.
If the last check found errors, set it to 0.5 or 5.0 otherwise."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 5.0)))

;; elfeed ----------------------------------------------------------------------

(defun basis/elfeed-parse-group (group)
  "Parse the feed and tag specification GROUP.
GROUP should be a list whose car contains a list of tags and
whose cdr is a list of feeds to associate with those tags. If
only one tag will be associated with the group, a symbol can be
used rather than a list of symbols."
  (pcase-let* ((`(,tag . ,feeds) group)
               (tags (if (listp tag) tag (list tag))))
    (mapcar (lambda (feed) (cons feed tags))
            feeds)))

(defun basis/elfeed-load-feeds (file)
  "Load feeds FILE. Return a list formatted for `elfeed-feeds'."
  (-mapcat #'basis/elfeed-parse-group (basis/read-file file)))

;; lorem ipsum -----------------------------------------------------------------

(defvar basis/lorem-ipsum-file
  (let ((filename (expand-file-name "~/.emacs.d/lorem-ipsum.txt")))
    (when (file-exists-p filename)
      filename)))

(defvar basis/lorem-ipsum nil)

(defun basis/insert-lorem-ipsum (&optional arg)
  (interactive "P")
  (if (not basis/lorem-ipsum-file)
      (message "No lorem ipsum text found.")
    (when (null basis/lorem-ipsum)
      (setq basis/lorem-ipsum
            (with-temp-buffer
              (insert-file-contents basis/lorem-ipsum-file)
              (split-string (buffer-substring-no-properties 1 (point-max))
                            "\n\n"))))
    (let ((arg (cond ((null arg) 1)
                     ((consp arg) (car arg))
                     (t arg))))
      (insert (mapconcat #'identity
                         (-take arg basis/lorem-ipsum)
                         "\n\n")))))
