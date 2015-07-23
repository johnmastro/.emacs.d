;;; defuns.el    -*- coding: utf-8; lexical-binding: t; -*-

(defun basis/occur-show-sections ()
  ;; Use occur to display and navigate the sections in this file
  (interactive)
  (occur "^;; \\([^ ].+\\) -+$"))

;; misc. editing utilities -----------------------------------------------------

(defun basis/next-line ()
  "Move point to the next line.
Wrapper around `next-line' to let-bind `next-line-add-newlines'
to nil if a keyboard macro is executing."
  (interactive)
  (let ((next-line-add-newlines
         (and next-line-add-newlines (not executing-kbd-macro))))
    (call-interactively #'next-line)))

(defun basis/beginning-of-line-or-indentation ()
  "Smarter `move-beginning-of-line'.
Go back to the first non-whitespace character or, if already
there, to the beginning of the line."
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    (when (= (point) start)
      (move-beginning-of-line nil))))

(defun basis/comment-or-uncomment (beg end)
  "Comment or uncomment the active region or current line."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (comment-or-uncomment-region beg end))

(defun basis/comment-region-lines (beg end &optional arg)
  "Comment out the lines from BEG to END.
With optional prefix ARG, uncomment instead."
  (interactive "*r\nP")
  (pcase-let ((`(,beg . ,end)  ; Rotate if "upside-down"
               (if (> beg end)
                   (cons end beg)
                 (cons beg end))))
    ;; Always comment whole lines
    (let ((beg (save-excursion (goto-char beg)
                               (line-beginning-position)))
          (end (save-excursion (goto-char end)
                               (if (bolp)
                                   (point)
                                 (line-end-position)))))
      (comment-region beg end arg))))

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
    (insert-char char enough)))

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
  (move-end-of-line 1)
  (unless (basis/looking-back-p ";")
    (insert ";")))

(defun basis/wrap-in-curlies ()
  (interactive)
  (save-excursion
    (forward-line -1)
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
   (list (read-directory-name "Directory: " nil nil t)
         (when current-prefix-arg
           (read-string "Pattern: "))))
  (if-let (files (file-expand-wildcards (expand-file-name (or pattern "*")
                                                          dir)
                                        t))
      (basis/insert-files files nil)
    (message "No files matching '%s' in '%s'" pattern dir)))

(defalias 'basis/concat-directory-files #'basis/insert-directory-files)

(defvar-local basis/smart-hyphen-code-only t
  "Whether to only perform hyphen substitutions in code.")

(defvar-local basis/smart-hyphen-style 'snake
  "The style of substitutions to perform: snake or camel.")

(defun basis/smart-hyphen (n)
  "Conditionally insert a hyphen or upcase the next char."
  (interactive "p")
  (if (or (not basis/smart-hyphen-style)
          (and basis/smart-hyphen-code-only
               (memq (get-text-property (point) 'face)
                     '(font-lock-doc-face
                       font-lock-comment-face
                       font-lock-string-face))))
      (self-insert-command n)
    (insert "-")
    (let ((command (key-binding (vector (read-event)))))
      (if (eq command 'self-insert-command)
          (insert (let ((next (elt (this-command-keys) 1)))
                    (if (and (eq ?w (char-syntax next))
                             ;; TODO: Do these next conditions make sense?
                             (>= ?z next ?A)
                             (not (save-excursion
                                    (forward-char -2)
                                    (looking-at-p "[0-9]"))))
                        (pcase basis/smart-hyphen-style
                          (`camel
                           (delete-char -1)
                           (upcase next))
                          (`snake
                           (delete-char -1)
                           (format "_%c" next))
                          (other
                           (error "Unknown smart hyphen style: %s" other)))
                      next)))
        (call-interactively command)))))

(defun basis/swiper-helm ()
  (interactive)
  (let ((helm-move-to-line-cycle-in-source t))
    (call-interactively #'swiper-helm)))

(defun basis/swiper-maybe-yank-something ()
  "Conditionally insert the symbol at point.
If the search text is empty, insert the symbol at point where the
search started. Otherwise, call the command the key is bound to
in the global map."
  (interactive)
  (if (string= ivy-text "")
      (when-let (str (save-window-excursion
                       (with-selected-window swiper--window
                         (goto-char swiper--opoint)
                         (thing-at-point 'symbol))))
        (insert str))
    (when-let (cmd (and (called-interactively-p 'any)
                        (lookup-key global-map (this-command-keys))))
      (call-interactively cmd))))

(defun basis/next-line-no-deactivate-mark (function &rest args)
  "Advice for `next-line'.
Bind `deactivate-mark' so that `next-line-add-newlines' doesn't
cause the mark to be deactivated."
  (let ((deactivate-mark))
    (apply function args)))

;; kill commands ---------------------------------------------------------------

(defun basis/kill-something (arg)
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

(defun basis/kill-ring-save-buffer-file-name (&optional buffer)
  "Save BUFFER's associated file name to the kill ring.
BUFFER defaults to the current buffer, if it is visiting a file."
  (interactive
   (when (or current-prefix-arg
             (not (buffer-file-name (current-buffer))))
     (list (completing-read "Buffer: "
                            (thread-last (buffer-list)
                              (seq-filter #'buffer-file-name)
                              (mapcar #'buffer-name))
                            nil
                            t))))
  (thread-first (or buffer (current-buffer))
    get-buffer
    buffer-file-name
    abbreviate-file-name
    basis/kill-ring-save-string))

(defun basis/clipboard-save-string (str)
  "Save STR directly to the system clipboard.
Do not save the string to the the kill ring."
  (funcall interprogram-cut-function str))

(defun basis/clipboard-save-something (beg end &optional region)
  "Save the region or buffer to the system clipboard."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) t)
                 (list (point-min) (point-max) nil)))
  (basis/clipboard-save-string (buffer-substring-no-properties beg end))
  (when region
    (setq deactivate-mark t)))

(defun basis/kill-ring-save-indented (beg end arg)
  "Save region to the kill ring with ARG spaces of indentation added.
Interactively, default to four spaces of indentation."
  (interactive "r\nP")
  (let ((arg (or arg 4))
        (buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buf beg end)
      (indent-rigidly (point-min) (point-max) arg)
      (kill-ring-save (point-min) (point-max)))))

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

(defun basis/pop-to-mark-ensure-new-pos (function)
  "Advice for `pop-to-mark-command' to repeat until point moves."
  (let ((p (point))
        (n 10))
    ;; (when (eq last-command 'kill-ring-save)
    ;;   (dotimes (_ 3)
    ;;     (funcall function)))
    (while (and (> n 0) (= (point) p))
      (funcall function)
      (setq n (1- n)))))

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
  (let* ((name (buffer-name))
         (file (buffer-file-name))
         (destination (if (file-directory-p destination)
                          (expand-file-name (file-name-nondirectory file)
                                            destination)
                        destination)))
    (if (not (and file (file-exists-p file)))
        (error "Buffer '%s' is not visiting a file" file)
      (rename-file file destination 1)
      (set-visited-file-name destination)
      (set-buffer-modified-p nil)
      (message "File '%s' renamed to '%s'"
               name
               (file-name-nondirectory destination)))))

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

(defun basis/find-file-recentf ()
  "Find recently open files using ido and recentf."
  (interactive)
  (let* ((list (mapcar #'abbreviate-file-name recentf-list))
         (file (completing-read "Recent file: " list nil t)))
    (when file
      (find-file file))))

(defun basis/windows->unix (path)
  "Convert a path from Windows-style to UNIX-style."
  (thread-last path
    (replace-regexp-in-string "\\\\" "/")
    (replace-regexp-in-string "[a-zA-Z]:" "")))

(defun basis/read-file (file)
  "Read a Lisp form from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

;; key binding utilities -------------------------------------------------------

(defmacro basis/define-keys (keymap &rest keydefs)
  "Define multiple key bindings for KEYMAP."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (keydef)
                 (let ((key (if (vectorp (car keydef))
                                (car keydef)
                              (read-kbd-macro (car keydef))))
                       (def (cadr keydef)))
                   `(define-key ,keymap ,key ,def)))
               keydefs)))

(defmacro basis/define-eval-keys (keymap &rest keydefs)
  "Define evaluation key bindings for various units of code.
See `basis/eval-keys'."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (keydef)
                 (let* ((sym (car keydef))
                        (def (cadr keydef))
                        (key (cdr (assq sym basis/eval-keys))))
                   (if key
                       `(define-key ,keymap (kbd ,key) ,def)
                     (error "No eval key for '%s'" sym))))
               keydefs)))

(defmacro basis/define-key-translations (&rest keydefs)
  "Define multiple bindings in `key-translation-map'."
  (declare (indent defun))
  `(progn
     ,@(mapcar (lambda (keydef)
                 (pcase-let ((`(,key ,def) keydef))
                   `(define-key key-translation-map (kbd ,key) (kbd ,def))))
               keydefs)))

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

;; escape mode -----------------------------------------------------------------

;; ESC->escape translation based on that in evil-core.el from `evil-mode'

(defvar basis/esc-mode nil)

(defvar basis/esc-delay 0.01)

(defvar basis/intercept-esc
  (memq (terminal-live-p (frame-terminal)) '(t pc)))

(defvar basis/inhibit-esc nil)

(defun basis/esc-mode (&optional arg)
  "Toggle interception of \\e (escape).
Enable with positive ARG and disable with negative ARG."
  (cond
   ((or (null arg) (eq arg 0))
    (basis/esc-mode (if basis/esc-mode -1 +1)))
   ((> arg 0)
    (unless basis/esc-mode
      (setq basis/esc-mode t)
      (add-hook 'after-make-frame-functions #'basis/init-esc)
      (mapc #'basis/init-esc (frame-list))))
   ((< arg 0)
    (when basis/esc-mode
      (remove-hook 'after-make-frame-functions #'basis/init-esc)
      (mapc #'basis/deinit-esc (frame-list))
      (setq basis/esc-mode nil)))))

(defun basis/init-esc (frame)
  "Update `input-decode-map' in terminal."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (and
             (or (eq basis/intercept-esc 'always)
                 (and basis/intercept-esc
                      (eq (terminal-live-p term) t))) ; only patch tty
             (not (terminal-parameter term 'basis/esc-map)))
        (let ((basis/esc-map (lookup-key input-decode-map [?\e])))
          (set-terminal-parameter term 'basis/esc-map basis/esc-map)
          (define-key input-decode-map [?\e]
            `(menu-item "" ,basis/esc-map :filter ,#'basis/esc-filter)))))))

(defun basis/deinit-esc (frame)
  "Restore `input-decode-map' in terminal."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (terminal-live-p term)
        (let ((basis/esc-map (terminal-parameter term 'basis/esc-map)))
          (when basis/esc-map
            (define-key input-decode-map [?\e] basis/esc-map)
            (set-terminal-parameter term 'basis/esc-map nil)))))))

(defun basis/esc-filter (map)
  "Translate \\e to 'escape if no further event arrives."
  (if (and (not basis/inhibit-esc)
           (equal (this-single-command-keys) [?\e])
           (sit-for basis/esc-delay))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

;; god-mode --------------------------------------------------------------------

(defun basis/god-update-cursor ()
  "Toggle the cursor type to signal whether `god-mode' is active."
  (setq cursor-type
        (if (bound-and-true-p god-local-mode)
            'box
          'bar)))

(defun basis/god-toggle-on-overwrite ()
  "Pause `god-mode' when `overwrite-mode' is active."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(defun basis/god-mode-all ()
  "Enable `god-mode' in all buffers."
  (interactive)
  (setq god-global-mode t)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (god-mode-maybe-activate 1)))
        (buffer-list)))

(defun basis/god-maybe-all ()
  "Conditionally enable `god-local-mode'.
Enable `god-local-mode' if `god-global-mode' is active and the
current buffer is not exempt. Intended for us in
`focus-out-hook'."
  (when (bound-and-true-p god-global-mode)
    (basis/god-mode-all)))

(defun basis/enable-god-mode ()
  "Enable `god-mode'.
Enable `god-global-mode' and `god-local-mode', add related hooks,
and define related key bindings."
  (interactive)
  (setq god-global-mode t)
  (god-local-mode 1)
  (basis/esc-mode 1)
  (global-set-key (kbd "<escape>") #'basis/god-mode-all)
  (define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
  (add-hook 'overwrite-mode-hook #'basis/god-toggle-on-overwrite)
  (add-hook 'focus-out-hook #'basis/god-maybe-all))

(defun basis/disable-god-mode ()
  "Disable `god-mode'.
Disable `god-global-mode' and `god-local-mode'. Also remove
related hooks and key bindings."
  (interactive)
  (setq god-global-mode nil)
  (god-local-mode -1)
  (basis/esc-mode -1)
  (global-set-key (kbd "<escape>") nil)
  (define-key isearch-mode-map (kbd "<escape>") nil)
  (remove-hook 'overwrite-mode-hook #'basis/god-toggle-on-overwrite)
  (remove-hook 'focus-out-hook #'basis/god-maybe-all))

;; ibuffer ---------------------------------------------------------------------

(defun basis/ibuffer-vc-root-files-only (function buf)
  "Advice for `ibuffer-vc-root'.
Only group a buffer with a VC if its visiting a file."
  (when (buffer-file-name buf)
    (funcall function buf)))

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

;; ediff -----------------------------------------------------------------------

(defun basis/ediff-expand-tmp-name (args)
  "Advice for `ediff-make-empty-tmp-file'.
Call `expand-file-name' on the proposed file name. Only necessary
on Windows."
  (pcase-let ((`(,proposed-name . ,rest) args))
    (cons (expand-file-name proposed-name) rest)))

(defun basis/ediff-save-window-config (&rest _ignore)
  "Advice for `ediff-setup'.
Save the current window configuration to register
`:ediff-restore-windows', so that it can be restored on exit."
  (window-configuration-to-register :ediff-restore-windows))

(defun basis/ediff-quit-restore (&rest _args)
  "Advice for `ediff-quit'.
After quitting, restore the previous window configuration."
  (condition-case nil
      (jump-to-register :ediff-restore-windows)
    (error (message "Previous window configuration could not be restored"))))

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

;; helm ------------------------------------------------------------------------

(defun basis/helm-backspace (n)
  "Delete N chars backwards.
If already at the beginning of the field, call
`helm-keyboard-quit'."
  (interactive "p")
  (if (= n 1)
      (condition-case nil
          (backward-delete-char 1)
        (error (helm-keyboard-quit)))
    (backward-delete-char n)))

(defvar basis/helm-w32-shell-operations
  '("open"
    "opennew"
    "openas"
    "print"
    "printto"
    "explore"
    "edit"
    "find"
    "runas"
    "properties"
    "default")
  "List of possible OPERATION arguments to `w32-shell-execute'.")

(defun basis/helm-open-file-w32 (file)
  ;; Used as :override advice to `helm-open-file-externally' on Windows
  (let* ((operation (and helm-current-prefix-arg
                         (helm-comp-read
                          "Operation: "
                          basis/helm-w32-shell-operations
                          :must-match t
                          :name "Open file externally"
                          :del-input nil)))
         (operation (if (string= operation "default")
                        nil
                      operation)))
    (w32-shell-execute operation (expand-file-name file))))

(defun basis/open-file-externally (files)
  "Open FILES externally.
In `dired-mode', open the marked files; in `direx:direx-mode',
open the file at point; otherwise, prompt for the file(s) to open
with `helm-read-file-name'."
  (interactive
   (list (pcase major-mode
           (`dired-mode
            (dired-get-marked-files))
           (`direx:direx-mode
            (basis/direx-file-name-at-point))
           (_
            (helm-read-file-name
             "Open externally: "
             :must-match t
             :marked-candidates t
             :preselect (when-let ((file (buffer-file-name))
                                   (base (file-name-nondirectory file)))
                          (format "^%s$" (regexp-quote base)))
             :persistent-action #'helm-open-file-externally)))))
  (require 'helm-external)
  (let ((helm-current-prefix-arg current-prefix-arg))
    (mapc #'helm-open-file-externally
          (if (listp files) files (list files)))))

(defun basis/helm-ff-run-magit-status ()
  "Run `magit-status' from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action #'magit-status)))

;; dired -----------------------------------------------------------------------

(defun basis/dired-jump-to-top ()
  "Move point to the first line representing a file."
  (interactive)
  (goto-char (point-min))
  (dired-next-line (if dired-hide-details-mode 1 2)))

(defun basis/dired-jump-to-bottom ()
  "Move point to the last line representing a file."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

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
   (list (completing-read "Sort by: "
                          (mapcar #'car basis/dired-sorting-options)
                          nil
                          t)))
  ;; This assumes we can slap the sort option on the end of
  ;; `dired-listing-switches'. It works with my current setup (and the default
  ;; value) but is fragile and unsatisfactory.
  (if-let (opt (cdr (assoc what basis/dired-sorting-options)))
      (dired-sort-other (concat dired-listing-switches opt))
    (error "Don't know how to sort by '%s'" what)))

(defun basis/dired-slurp-files (files buffer)
  "Insert the contents of marked FILES into BUFFER.
If it doesn't exist, BUFFER is created automatically."
  (interactive (list (if (eq major-mode 'dired-mode)
                         (dired-get-marked-files)
                       (error "Buffer not in `dired-mode'"))
                     (read-buffer "Destination buffer: ")))
  (basis/insert-files files (get-buffer-create buffer)))

;; direx -----------------------------------------------------------------------

;; `direx' includes a package `direx-project', which implements its own project
;; root finding. However, since I have `projectile' anyway it makes more sense
;; to use it.

(defun basis/direx-find-project-root-noselect ()
  (when (projectile-project-p)
    (direx:find-directory-noselect (projectile-project-root))))

(defun basis/direx-jump-to-project-root-noselect ()
  (interactive)
  (if-let (buffer (basis/direx-find-project-root-noselect))
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

(defun basis/direx-file-name-at-point (&optional point)
  "Return the absolute file name of the item at POINT."
  (ignore-errors
    (thread-first (direx:item-at-point point)
      (direx:item-name)
      (direx:canonical-filename))))

;; emacs lisp ------------------------------------------------------------------

(defun basis/eval-something ()
  "Eval the active region, if any; otherwise eval the toplevel form."
  (interactive)
  (if (use-region-p)
      (prog1 (call-interactively #'eval-region)
        (setq deactivate-mark t))
    (call-interactively #'eval-defun)))

(defun basis/eval-last-sexp (&optional arg)
  (interactive "P")
  (if (eq arg '-)
      (basis/pp-eval-last-sexp nil)
    (eval-last-sexp arg)))

(defmacro basis/with-unique-names (names &rest body)
  "Create unique names for use in a macro definition.
This idea also goes by the name `with-gensyms` in Common Lisp."
  (declare (indent 1))
  `(let ,(mapcar (lambda (sym)
                   `(,sym (make-symbol (symbol-name ',sym))))
                 names)
     ,@body))

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

(defun basis/elisp-quote (beg end)
  "Put the region from BEG to END in Emacs Lisp-style quotes.
If called interactively without an active region, the symbol at
point is used instead, if any."
  (interactive
   (pcase-let ((`(,beg . ,end)
                (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'symbol))))
     (list beg end)))
  (unless (and beg end)
    (error "Invalid region"))
  (let ((end (move-marker (make-marker) end)))
    (goto-char beg)
    (insert "`")
    (goto-char end)
    (insert "'")))

(defun basis/bug-number-at-point ()
  "Return the bug number at point, if any, as a string."
  (save-excursion
    (skip-syntax-backward "^ " (line-beginning-position))
    (when (looking-at "#?\\([0-9]+\\)")
      (match-string 1))))

(defun basis/browse-emacs-bug (&optional number)
  "Load Emacs bug NUMBER in a web browser."
  (interactive (list (or (basis/bug-number-at-point)
                         (read-string "Bug number: "))))
  (browse-url (format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
                      number)))

;; occur -----------------------------------------------------------------------

(defun basis/occur-dwim (regexp nlines)
  "Like `occur', but REGEXP defaults to the text at point."
  (interactive
   (list (let ((str (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning)
                                                        (region-end))
                      (thing-at-point 'symbol))))
           (read-regexp
            (format "Occur (default %s): " (or str (car regexp-history)))
            (and (not str) 'regexp-history-last)))
         (prefix-numeric-value current-prefix-arg)))
  (occur regexp nlines))

;; window utilities ------------------------------------------------------------

(defun basis/transpose-windows (arg)
  "Transpose the relative positions of two or more windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) #'next-window #'previous-window)))
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
                #'split-window-horizontally
              #'split-window-vertically)))
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

(defun basis/google (string)
  "Run a Google search.
Use the active region or symbol at point, if any, as initial
input."
  (interactive
   (list (let ((default (if (use-region-p)
                            (buffer-substring (region-beginning) (region-end))
                          (thing-at-point 'symbol))))
           (read-string "Google: " default))))
  (browse-url
   (concat "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
           (url-hexify-string string))))

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
  "Invoke `goto-line' with `linum-mode' temporarily enabled."
  (interactive)
  (let ((linum-enabled-p (bound-and-true-p linum-mode)))
    (unwind-protect
        (progn
          (unless linum-enabled-p (linum-mode 1))
          (call-interactively #'goto-line))
      (unless linum-enabled-p (linum-mode -1)))))

(defun basis/file-remote-p (name)
  (and name (or (tramp-tramp-file-p name) (file-remote-p name))))

(defun basis/maybe-enable-flycheck ()
  "Enable `flycheck-mode', except for remote files."
  (unless (basis/file-remote-p buffer-file-name)
    (flycheck-mode)))

(defun basis/maybe-enable-flyspell ()
  "Enable `flyspell-mode', except for remote files."
  (when (and ispell-program-name
             (not (basis/file-remote-p buffer-file-name)))
    (flyspell-mode)))

(defun basis/maybe-enable-flyspell-prog-mode ()
  "Enable `flyspell-prog-mode', except for remote files."
  (when (and ispell-program-name
             (not (basis/file-remote-p buffer-file-name)))
    (flyspell-prog-mode)))

(defun basis/maybe-enable-whitespace-mode ()
  "Enable `whitespace-mode' in programming modes (but not REPLs)."
  (interactive)
  (unless (or (derived-mode-p 'comint-mode)
              (eq major-mode 'eshell-mode)
              (eq major-mode 'cider-repl-mode))
    (whitespace-mode 1)))

(defun basis/isearch-backspace (&optional arg)
  "Delete non-matching text or the last character."
  (interactive "p")
  (if (zerop (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(defun basis/isearch-cancel ()
  "Cancel the current interactive search.
Unlike `isearch-abort' and `isearch-exit', always cancel the
current search, with no context-dependent behavior."
  (interactive)
  (discard-input)
  (setq isearch-success nil)
  (isearch-cancel))

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

(defun basis/ido-selected-file ()
  "Return the current selection during `ido' file completion.
Return the current directory if no text is entered or there are
no matches."
  (if (or (string= ido-text "")
          (null ido-matches))
      default-directory
    (expand-file-name (car ido-matches) default-directory)))

(defun basis/ido-open-file-externally-internal (file)
  (interactive (list (basis/ido-selected-file)))
  (helm-open-file-externally file))

(defun basis/ido-open-file-externally ()
  "Open a file externally during `ido' completion."
  (interactive)
  (setq fallback 'basis/ido-open-file-externally-internal
        ido-exit 'fallback)
  (exit-minibuffer))

(defun basis/disable-themes (&optional themes)
  "Disable THEMES (defaults to `custom-enabled-themes')."
  (interactive)
  (mapc #'disable-theme (or themes custom-enabled-themes)))

(defun basis/reload-solarized (&optional background-mode)
  "Load (or reload) the Solarized theme.
If provided, BACKGROUND-MODE specifies which variant to use:
`dark' or `light'."
  (interactive
   (list (and current-prefix-arg
              (intern (completing-read "Background: " '(dark light) nil t)))))
  (basis/disable-themes)
  (when background-mode
    (set-frame-parameter nil 'background-mode background-mode)
    (set-terminal-parameter nil 'background-mode background-mode))
  (load-theme 'solarized t)
  (load-theme 'solarized-moar t))

(defun basis/libxml-available-p ()
  "Return non-nil if libxml is available."
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
  (seq-filter #'file-directory-p
              (pcase (or language 'c)
                (`c
                 (let* ((cmd "clang -v -xc -")
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
                (`c++
                 '("/usr/include/c++/4.6"
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
  (add-to-list 'company-backends #'company-clang))

(defun basis/kill-frame-or-terminal (&optional arg)
  "Kill the current frame or session.
If there is more than one live frame, close the current one.
Otherwise kill the current session. If optional ARG is non-nil,
kill the current session even if there are multiple frames."
  (interactive "P")
  (if (or arg (null (cdr (frame-list))))
      (save-buffers-kill-terminal)
    (delete-frame)))

(defun basis/in-string-p ()
  "Return non-nil if point is within a string."
  (let ((state (syntax-ppss)))
    (and (nth 3 state) (nth 8 state))))

(defun basis/diff-buffer-with-file (&optional buffer file)
  "View the differences between BUFFER and FILE.
With a prefix arg, prompt for both BUFFER and FILE. Otherwise,
only prompt for BUFFER and use its associated file as FILE."
  (interactive
   (list (read-buffer "Buffer: " (buffer-name) t)
         (unless current-prefix-arg
           (read-file-name "File: " nil nil t))))
  (with-current-buffer (get-buffer (or buffer (current-buffer)))
    (diff (or file buffer-file-name) (current-buffer) nil 'noasync)))

(defun basis/mu4e-action-view-in-browser (msg)
  "View MSG in a browser, via `browse-url'.
For use as a `mu4e' message action."
  (let ((html (or (mu4e-msg-field msg :body-html)
                  (error "This message doesn't have an HTML part")))
        (file (expand-file-name (format "%d.html" (random))
                                temporary-file-directory)))
    (with-temp-file file
      (insert "<html>"
              "<head><meta http-equiv=\"content-type\""
              "content=\"text/html;charset=UTF-8\">"
              html))
    (browse-url (format "file://%s" file))))

(defun basis/count-sloc-region (beg end kind)
  ;; From Stefan Monnier on the help-gnu-emacs list
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) 'region)
     (list (point-min) (point-max) 'buffer)))
  (save-excursion
    (goto-char beg)
    (let ((count 0))
      (while (< (point) end)
        (if (nth 4 (syntax-ppss))
            (let ((pos (point)))
              (goto-char (nth 8 (syntax-ppss)))
              (forward-comment (point-max))
              (when (< (point) pos) (goto-char pos)))
          (forward-comment (point-max)))
        (setq count (1+ count))
        (forward-line))
      (when kind
        (message "SLOC in %s: %d" kind count)))))

(defun basis/shr-html2text ()
  "Convert HTML to plain text in the current buffer using `shr'."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)))

(defun basis/find-tag (symbol)
  "Like `find-tag' but try to avoid prompting."
  (interactive (list (thing-at-point 'symbol)))
  (cond ((equal current-prefix-arg '(4))
         (let ((current-prefix-arg nil))
           (call-interactively #'find-tag)))
        ((or current-prefix-arg (null symbol))
         (call-interactively #'find-tag))
        (t
         (find-tag symbol))))

(defun basis/visit-tags-file-auto ()
  "Automatically find and visit a TAGS file."
  (interactive)
  (when-let ((file (buffer-file-name))
             (tags (locate-dominating-file file "TAGS")))
    (visit-tags-table (expand-file-name "TAGS" tags) t)))

(defun basis/maybe-cygwinize-drive-letter (file)
  "Convert \"c:/foo\" to \"/foo\" or \"e:/foo\" to \"/e/foo\".
Assumes Cygwin's path prefix is \"/\"."
  (cond ((string-match "\\`[Cc]:/" file)
         (replace-match "/" t t file))
        ((string-match "\\`\\([B-Zb-z]\\):/" file)
         (replace-match (concat "/" (match-string 1 file) "/")
                        t
                        t
                        file))
        (t file)))

(defun basis/fix-bad-cygwin-file-name (name)
  "Un-escape the colon drive letter separator in NAME.
For example, given \"c\\:/path/to/file\" return
\"c:/path/to/file\". Used to adjust the result of
`python-shell-calculate-command' and
`with-editor-locate-emacsclient'."
  (if (and name (string-match "\\`[a-zA-Z]\\(\\\\\\):/" name))
      (replace-match "" t t name 1)
    name))

(defun basis/projectile-regenerate-tags ()
  "Copy of `projectile-regenerate-tags' modified for Cygwin paths.
Using a tags file name of e.g. \"c:/foo/TAGS\" causes the
resulting file to contain invalid paths. Use
`basis/maybe-cygwinize-drive-letter' to convert the path to e.g.
\"/foo/TAGS\". This assumes the Cygwin path prefix is \"/\"."
  (interactive)
  (if (and (eq system-type 'windows-nt) basis/cygwin-p)
      (let* ((project-root (projectile-project-root))
             (tags-exclude (projectile-tags-exclude-patterns))
             (default-directory project-root)
             (tags-file (expand-file-name projectile-tags-file-name))
             (tags-file (basis/maybe-cygwinize-drive-letter tags-file))
             (command (format projectile-tags-command tags-file tags-exclude))
             shell-output exit-code)
        (with-temp-buffer
          (setq exit-code
                (call-process-shell-command command nil (current-buffer))
                shell-output (projectile-trim-string
                              (buffer-substring (point-min) (point-max)))))
        (unless (zerop exit-code)
          (error shell-output))
        (visit-tags-table tags-file))
    (call-interactively #'projectile-regenerate-tags)))

(defun basis/count-words ()
  "Count the lines, words, and characters in the active region.
Like `count-words-region', but operate on the current buffer if
the region isn't active."
  (interactive)
  (let ((current-prefix-arg (not (use-region-p))))
    (call-interactively #'count-words-region)))

(defun basis/company-no-completion-in-docstring (function)
  "Advice for `company-auto-begin'.
Work around a bug I haven't figured out yet."
  (unless (and (eq major-mode 'python-mode)
               (basis/in-string-p))
    (funcall function)))

(defun basis/company-no-tramp-completion (function)
  "Advice for `company-auto-begin'.
Work around TRAMP freezes on my Windows machine at work."
  (unless (and (eq major-mode 'shell-mode)
               ;; Skip backward to whitespace and see if we end up on something
               ;; that looks like a TRAMP file name.
               (save-excursion
                 (skip-syntax-backward "^ ")
                 (looking-at-p "\\(\\w+@\\)?\\w\\{2,\\}:/")))
    (funcall function)))

(defun basis/company-sh-no-complete-fi (function)
  "Advice for `company-auto-begin'.
Prevent completion on \"fi\" in `sh-mode'. I find the interaction
between this and electric indentation annoying."
  (unless (and (eq major-mode 'sh-mode)
               (save-excursion
                 (forward-char -2)
                 (looking-at-p "\\_<fi\\_>")))
    (funcall function)))

(defun basis/magit-browse-pull-request-url ()
  "Visit the current branch's PR on GitHub."
  (interactive)
  (browse-url
   (format "https://github.com/%s/compare/%s"
           ;; Or this? "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'"
            "\\1"
            (magit-get "remote" (magit-get-remote) "url"))
           (cdr (magit-get-remote-branch)))))

(defun basis/magit-expand-top-dir (result)
  "Advice for `magit-get-top-dir'.
For use with Cygwin. Call `expand-file-name' on its result, to
make sure its in the same form that Emacs uses (i.e.
\"c:/path/to/somewhere\")."
  (and result (expand-file-name result)))

(defun basis/fix-located-emacsclient-file-name (name)
  "Advice for `with-editor-locate-emacsclient'.
See also `basis/fix-bad-cygwin-file-name'."
  (and name (basis/fix-bad-cygwin-file-name name)))

(defvar basis/ivy-format-selection-text
  (propertize "> " 'face 'font-lock-function-name-face)
  "Text to place before the selection during `ivy' completion.")

(defun basis/ivy-format-function (candidates)
  "Function for use as `ivy-format-function'.
Place a propertized \"> \" before the selected candidate."
  (let ((i -1))
    (mapconcat (lambda (s)
                 (concat (if (eq (cl-incf i) ivy--index)
                             basis/ivy-format-selection-text
                           "  ")
                         s))
               candidates
               "\n")))

(defvar basis/occur-show-note-strings
  (if (require 'hl-todo nil t)
      (mapcar #'car hl-todo-keyword-faces)
    '("HOLD"
      "TODO"
      "NEXT"
      "THEM"
      "PROG"
      "OKAY"
      "DONT"
      "FAIL"
      "DONE"
      "NOTE"
      "KLUDGE"
      "FIXME"
      "XXX"
      "???"))
  "List of strings `basis/occur-show-notes' will search for.")

(defun basis/occur-show-notes ()
  "Search for common \"TODO\"-style notes."
  (interactive)
  (occur (regexp-opt basis/occur-show-note-strings t)))

;; paredit ---------------------------------------------------------------------

(defun basis/paredit-doublequote-space-p (endp delimiter)
  "Don't insert an extraneous space when entering a CL pathname."
  ;; If any of `paredit-space-for-delimiter-predicates' returns nil
  ;; a space isn't inserted.
  (not (and (not endp)
            (eql delimiter ?\")
            (memq major-mode '(lisp-mode common-lisp-mode slime-repl-mode))
            (save-excursion
              (backward-char 2)
              (looking-at-p "#p")))))

(defun basis/paredit-open-something ()
  (interactive)
  (call-interactively
   (if (memq major-mode '(clojure-mode cider-repl-mode))
       #'paredit-open-square
     #'paredit-open-round)))

(defun basis/paredit-kill-something ()
  (interactive)
  (call-interactively (if (use-region-p)
                          #'kill-region
                        #'paredit-backward-kill-word)))

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

(defun basis/sp-backward-delete-no-prefix (args)
  "Advice for `sp-backward-delete-char'.
Do not treat raw universal arguments specially (treat it as a
numeric argument)."
  (pcase-let ((`(,prefix . ,rest) args))
    (cons (prefix-numeric-value prefix) rest)))

(defun basis/sp-kill-something ()
  "Call `sp-backward-kill-word' or `kill-region'. "
  (interactive)
  (call-interactively (if (use-region-p)
                          #'kill-region
                        #'sp-backward-kill-word)))

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

(defun basis/disable-relative-reindent (function &rest args)
  "Advice to prevent relative reindentation by FUNCTION."
  (if (memq indent-line-function '(indent-relative
                                   python-indent-line-function
                                   haskell-indentation-indent-line))
      (cl-letf (((symbol-function 'indent-according-to-mode)
                 #'ignore))
        (apply function args))
    (apply function args)))

(defun basis/disable-relative-reindent-for (functions)
  "Prevent automatic relative reindentation by FUNCTIONS."
  (dolist (function functions)
    (advice-add function :around #'basis/disable-relative-reindent)))

;; scheme/geiser ---------------------------------------------------------------

(defun basis/scheme-send-something ()
  (interactive)
  (call-interactively (if (use-region-p)
                          #'scheme-send-region
                        #'scheme-send-definition)))

(defun basis/geiser-eval-something ()
  (interactive)
  (call-interactively (if (use-region-p)
                          #'geiser-eval-region
                        #'geiser-eval-definition)))

(defun basis/geiser-eval-something-and-go ()
  (interactive)
  (call-interactively (if (use-region-p)
                          #'geiser-eval-region-and-go
                        #'geiser-eval-definition-and-go)))

(defun basis/geiser-expand-something ()
  (interactive)
  (call-interactively (if (use-region-p)
                          #'geiser-expand-region
                        #'geiser-expand-last-sexp)))

;; python ----------------------------------------------------------------------

(defun basis/python-send-something ()
  "Send the active region or the current defun."
  (interactive)
  (call-interactively (if (use-region-p)
                          #'python-shell-send-region
                        #'python-shell-send-defun)))

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
  (insert-char ?\" 6)
  (backward-char 3))

(defun basis/python-find-venv ()
  (when-let ((prj (ignore-errors (projectile-project-root)))
             (env (expand-file-name "env" prj)))
    (and (or (file-exists-p (expand-file-name "bin/activate" env))
             (file-exists-p (expand-file-name "Scripts/activate" env)))
         env)))

(defun basis/pyvenv-activate (dir)
  "Like `pyvenv-activate' but try to guess the directory."
  (interactive
   (list (read-directory-name
          "Activate venv: "
          (or (basis/python-find-venv)
              (ignore-errors (projectile-project-root))))))
  (if (eq major-mode 'python-mode)
      (pyvenv-activate dir)
    (error "Not in a python-mode buffer")))

(defun basis/run-python (&optional _arg)
  "Run an inferior Python process.
If a virtualenv associated with the project is found, prompt to
activate it."
  (interactive "P")
  (unless (eq major-mode 'python-mode)
    (error "Not in a python-mode buffer"))
  (let ((env (basis/python-find-venv)))
    (when (and env (y-or-n-p (format "Activate venv (%s)? " env)))
      (pyvenv-activate env)))
  (call-interactively #'run-python))

(defun basis/python-reformat-module-docstring (beg end)
  "Reformat from BEG to END as a Python module docstring."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  ;; TODO: Handle the module name and underline specially
  (let ((fill-column 68))
    (fill-region beg end))
  (indent-rigidly beg end 4))

;; slime -----------------------------------------------------------------------

(defun basis/slime-eval-something ()
  "Eval the active region, if any; otherwise eval the toplevel form."
  (interactive)
  (call-interactively (if (use-region-p)
                          #'slime-eval-region
                        #'slime-eval-defun)))

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

(defun basis/cider-eval-something ()
  (interactive)
  (call-interactively (if (use-region-p)
                          #'cider-eval-region
                        #'cider-eval-expression-at-point)))

(defun basis/helm-clj-headlines ()
  (interactive)
  (helm :sources '(((name . "Clojure Headlines")
                    (volatile)
                    (headline "^[;(]")))))

;; comint ----------------------------------------------------------------------

(defun basis/comint-input-goto-bottom-if-necessary (function &rest args)
  "Advice for `comint' {previous,next}-input commands.
If an adviced command would signal a \"Not at command line\"
user-error, automatically move point to the command line."
  (when (and comint-scroll-to-bottom-on-input
             (not (comint-after-pmark-p)))
    (goto-char (point-max))))

;; org-mode --------------------------------------------------------------------

(defun basis/process-clojure-output (s)
  (mapconcat (lambda (line)
               (thread-last line
                 (string-remove-suffix "\r")
                 (concat ";; ")))
             (split-string (string-remove-suffix "\n" s) "\n")
             "\n"))

(defun basis/org-babel-execute:clojure (body _params)
  (let* ((result (nrepl-sync-request:eval body (cider-current-ns)))
         (value (plist-get result :value))
         (stdout (when-let (s (plist-get result :stdout))
                   (basis/process-clojure-output s)))
         (stderr (when-let (s (plist-get result :stderr))
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

(defun basis/sql-indent (&optional n)
  "Insert spaces or tabs to the Nth next tab-stop column."
  (interactive "p")
  (let ((n (or n 1)))
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (progn (goto-char (region-end))
                          (point-marker))))
          (goto-char beg)
          (unless (bolp)
            (forward-line 1))
          (while (< (point) end)
            (dotimes (_ n) (tab-to-tab-stop))
            (forward-line 1))
          (move-marker end nil))
      (dotimes (_ n) (tab-to-tab-stop)))))

(defvar basis/sql-backspace-dedent-hungrily t
  "Whether `basis/sql-backspace-dedent' should delete backward hungrily.")

(defun basis/sql-backspace-dedent (&optional n)
  "Delete N characters backward or dedent.
If `basis/sql-backspace-dedent-hungrily' is non-nil, N is null,
and there is non-whitespace before point on the current line,
delete all whitespace backward. Use `sp-backward-delete-char' if
`smartparens-mode' is active."
  (interactive "P")
  (if (use-region-p)
      ;; Just call `backward-delete-char' because it will do the right thing
      ;; based on `delete-active-region'
      (call-interactively #'backward-delete-char)
    (pcase-let ((`(,spaces ,stuff)
                 (let ((start (point)))
                   (save-excursion
                     (skip-syntax-backward " " (line-beginning-position))
                     (list (- start (point)) (not (bolp)))))))
      (if (and basis/sql-backspace-dedent-hungrily (null n) stuff (> spaces 0))
          (delete-horizontal-space t)
        (let ((delete (if (bound-and-true-p smartparens-mode)
                          #'sp-backward-delete-char
                        (lambda (n) (delete-char (- n)))))
              (arg (if (or n (bolp))
                       (prefix-numeric-value n)
                     (max 1 (let* ((column (current-column))
                                   (offset (% column tab-width)))
                              (if (zerop offset)
                                  (min spaces tab-width)
                                (min spaces offset)))))))
          (funcall delete arg))))))

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
      (let* ((start1 (point))
             (start2 start1))
        ;; If we're already on a clause starter, move over it
        (when (looking-at basis/sql-clause-start-regexp)
          (setq start2 (goto-char (match-end 0))))
        (while (and (> n 0)
                    (re-search-forward basis/sql-clause-start-regexp nil t))
          (unless (basis/sql-in-string-or-comment-p)
            (setq n (- n 1))))
        ;; If that didn't get us anywhere, just do `forward-paragraph'
        (cond ((= (point) start1)
               (forward-paragraph n))
              ((= (point) start2)
               (goto-char start1)
               (forward-paragraph n))
              (t
               (goto-char (match-beginning 0))))
        (point)))))

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
        (when (= (point) start)
          (backward-paragraph n))
        (point)))))

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

(defun basis/sql-recapitalize-buffer (style)
  "Recapitalize the current buffer to STYLE (caps or none)."
  (interactive
   (list (intern (completing-read  "Style: " '("caps" "none") nil t))))
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

(defun basis/sql-modify-syntax-table ()
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
         (modify-syntax-entry ?\" "\""))))))

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
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(defun basis/flycheck-disable-automatic-checking ()
  "Disable automatic syntax checking by Flycheck."
  (interactive)
  (setq flycheck-check-syntax-automatically nil))

(defun basis/adjust-flycheck-idle-change-delay ()
  "Adjust Flycheck's idle change delay.
If the last check found errors, set it to 0.5 or 5.0 otherwise."
  (setq flycheck-idle-change-delay (if flycheck-current-errors 0.5 5.0)))

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
  (thread-last file
    (basis/read-file)
    (mapcar #'basis/elfeed-parse-group)
    (apply #'nconc)))

;; yaml ------------------------------------------------------------------------

(defun basis/yaml-multiple-docs-p ()
  "Return non-nil if the buffer contains a multiple-document stream."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^---" nil t)))

(defun basis/yaml-next-document ()
  "Move forward to the next YAML document in the buffer.
This is only effective if the current buffer contains a YAML
multiple-document stream."
  (interactive)
  (let ((start (point)))
    (end-of-line)
    (if (re-search-forward "^---" nil t)
        (goto-char (match-beginning 0))
      (goto-char start))))

(defun basis/yaml-previous-document ()
  "Move backward to the previous YAML document in the buffer.
This is only effective if the current buffer contains a YAML
multiple-document stream."
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (if (re-search-backward "^---" nil t)
        (goto-char (match-beginning 0))
      (goto-char start))))

(define-minor-mode basis/yaml-multi-doc-mode
  "Mode for YAML buffers containing a stream of documents."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-n") #'basis/yaml-next-document)
            (define-key map (kbd "M-p") #'basis/yaml-previous-document)
            map))

;; lorem ipsum -----------------------------------------------------------------

(defvar basis/lorem-ipsum-file
  (let ((filename (expand-file-name (basis/emacs-file "lorem-ipsum.txt"))))
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
                         (seq-take basis/lorem-ipsum arg)
                         "\n\n")))))

