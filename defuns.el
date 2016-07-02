;;; defuns.el    -*- coding: utf-8; lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key binding utilities

(defmacro basis/define-keys (keymap &rest keydefs)
  "Define multiple key bindings for KEYMAP."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (keydef)
                 (let* ((key (car keydef))
                        (def (cadr keydef))
                        (kbd (if (vectorp key) key `(kbd ,key))))
                   `(define-key ,keymap ,kbd ,def)))
               keydefs)))

(defmacro basis/define-map (name args &rest keydefs)
  "Define a prefix map named NAME.
ARGS specifies a key binding for the prefix map and can be of the
form (MAP KEY) or (KEY), in which case the binding is created in
`global-map'."
  (declare (indent 2))
  (pcase-let* ((`(,map ,key)
                (pcase args
                  (`(,map ,key) (list map key))
                  (`(,key)      (list 'global-map key))
                  (`()          (list nil nil))
                  (_            (error "Too many arguments"))))
               (kbd
                (cond ((vectorp key) key)
                      ((stringp key) `(kbd ,key))
                      ((null key) nil)
                      (t (error "Invalid key: %s" key)))))
    `(progn
       (define-prefix-command ',name)
       (basis/define-keys ,name ,@keydefs)
       ,(and key `(define-key ,map ,kbd ',name))
       ',name)))

(defmacro basis/define-eval-keys (keymap &rest keydefs)
  "Define evaluation key bindings for various units of code.
See `basis/eval-keys'."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (keydef)
                 (let* ((sym (car keydef))
                        (def (cadr keydef))
                        (key (cdr (assq sym basis/eval-keys)))
                        (kbd (if (vectorp key) key `(kbd ,key))))
                   (if key
                       `(define-key ,keymap ,kbd ,def)
                     (error "No eval key for `%s'" sym))))
               keydefs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing utilities

(defun basis/bounds-of-region-or-thing (thing)
  "Return the bounds of the region if active or THING."
  ;; Note that whereas `bounds-of-thing-at-point' returns (BEG . END) we return
  ;; (BEG END), because the latter is more convenient to use with `apply' or an
  ;; `interactive' spec.
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (pcase thing
      (`line
       (list (line-beginning-position) (line-end-position)))
      (_
       (when-let ((bounds (bounds-of-thing-at-point thing)))
         (list (car bounds) (cdr bounds)))))))

(defun basis/next-line ()
  "Move point to the next line.
Wrapper around `next-line' to let-bind `next-line-add-newlines'
to nil if a keyboard macro is executing."
  (interactive)
  (let ((next-line-add-newlines
         (and next-line-add-newlines (not executing-kbd-macro))))
    (call-interactively #'next-line)))

(defun basis/beginning-of-line ()
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
  (interactive (basis/bounds-of-region-or-thing 'line))
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

(defvar basis/whitespace-chars "\r\n[:blank:]"
  "Whitespace characters, for e.g. `skip-chars-forward'.")

(defun basis/uncomment-sexp (&optional n)
  "Uncomment the sexp at point.
"
  (interactive "p")
  (let* ((start (point-marker))
         (pos nil)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq pos (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end))
    (goto-char pos)
    ;; Identify the "top-level" sexp inside the comment
    (while (and (ignore-errors (backward-up-list) t)
                (>= (point) beg))
      (skip-chars-backward (rx (syntax expression-prefix)))
      (set-marker pos (point)))
    ;; Re-comment everything before it
    (ignore-errors (comment-region beg pos))
    ;; And everything after it
    (goto-char pos)
    (forward-sexp (or n 1))
    (skip-chars-forward basis/whitespace-chars)
    (if (< (point) end)
        (ignore-errors (comment-region (point) end))
      ;; If this is a closing delimiter, pull it up
      (goto-char end)
      (skip-chars-forward basis/whitespace-chars)
      (when (= (car (syntax-after (point))) 5)
        (delete-indentation)))
    ;; Without a prefix, it's more useful to leave point where it was
    (prog1 (unless n (goto-char start))
      (set-marker start nil)
      (set-marker beg nil)
      (set-marker end nil)
      (set-marker pos nil))))

(defun basis/comment-sexp-raw ()
  "Comment the sexp at point and move over it."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward basis/whitespace-chars)
               (bounds-of-thing-at-point 'sexp)))
    (`(,beg . ,end)
     (goto-char end)
     (skip-chars-forward basis/whitespace-chars)
     (comment-region beg end)
     (skip-chars-forward basis/whitespace-chars))))

(defun basis/comment-or-uncomment-sexp (&optional n)
  "Comment or uncomment the sexp at point.
When commenting, a prefix argument N means comment that many
sexps. When uncommenting, a prefix argument N means move forward
that many sexps before uncommenting."
  (interactive "p")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward basis/whitespace-chars)
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (basis/uncomment-sexp n)
    (dotimes (_ (or n 1))
      (basis/comment-sexp-raw))))

(defun basis/open-line-below (&optional arg)
  "Open a new line below the current one."
  (interactive "*p")
  (let ((arg (or arg 1)))
    (if (< arg 0)
        (basis/open-line-above (- arg))
      (end-of-line)
      (newline arg)
      (indent-according-to-mode))))

(defun basis/open-line-above (&optional arg)
  "Open a new line above the current one."
  (interactive "*p")
  (let ((arg (or arg 1)))
    (if (< arg 0)
        (basis/open-line-below (- arg))
      (beginning-of-line)
      (newline arg)
      (forward-line (- arg))
      (indent-according-to-mode))))

(defun basis/electric-return ()
  "Typical \"electric\" return, similar to that in CC Mode."
  (interactive)
  (when (memq (char-after) '(?\) ?\] ?}))
    (save-excursion (newline-and-indent)))
  (newline-and-indent))

(defun basis/eol-maybe-semicolon ()
  "Move to the end of the line and insert a semicolon."
  (interactive)
  (end-of-line)
  (delete-horizontal-space t)
  (unless (eq (char-before) ?\;)
    (insert ";")))

(defun basis/wrap-in-curlies (beg end)
  "Wrap the current line with curly braces."
  (interactive (basis/bounds-of-region-or-thing 'line))
  (save-excursion
    (goto-char beg)
    (forward-line -1)
    (end-of-line)
    (delete-horizontal-space)
    (insert " {")
    (goto-char end)
    (end-of-line)
    (newline-and-indent)
    (insert "}")
    (indent-for-tab-command)))

(defvar-local basis/smart-hyphen-code-only t
  "If non-nil, only perform hyphen substitutions in code.")

(defvar-local basis/smart-hyphen-style 'snake
  "The style of substitutions to perform.
Valid values are `snake', `camel', and nil.")

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
                           (error "Unknown smart-hyphen style: `%s'" other)))
                      next)))
        (call-interactively command)))))

(defun basis/next-line-no-deactivate-mark (original &rest args)
  "Advice for `next-line'.
Bind `deactivate-mark' so that `next-line-add-newlines' doesn't
cause the mark to be deactivated."
  (let ((deactivate-mark))
    (apply original args)))

(defun basis/next-long-line (&optional threshold message)
  "Move forward to the next overly-long line.
With a prefix arg, read the THRESHOLD to use. Otherwise use
`whitespace-line-column' if `whitespace-mode' is enabled, or 80
if not."
  (interactive (list (and current-prefix-arg (read-number "Threshold: "))
                     t))
  (let ((threshold (or threshold
                       (and (bound-and-true-p whitespace-mode)
                            whitespace-line-column)
                       80))
        (start (point))
        line column)
    (when (eolp) (forward-line 1))
    (catch 'done
      (while (not (eobp))
        (end-of-line)
        (if (> (setq column (current-column)) threshold)
            (progn
              (setq line (line-number-at-pos))
              (when message (message "Line %d is %d columns long" line column))
              (throw 'done (cons line column)))
          (forward-line 1)))
      (goto-char start)
      (when message (message "No long lines found")))))

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
  (basis/beginning-of-line)
  (kill-line nil))

(defun basis/kill-ring-save-something ()
  "Save the contents of the active region or the current line."
  (interactive)
  (apply #'kill-ring-save (basis/bounds-of-region-or-thing 'line)))

(defun basis/kill-ring-save-buffer-file-name (&optional arg)
  "Save BUFFER's associated file name to the kill ring.
Abbreviate the file name, unless called with prefix ARG."
  (interactive "P")
  (if-let ((file (buffer-file-name)))
      (kill-new (message "%s" (if arg file (abbreviate-file-name file))))
    (user-error "Current buffer is not visiting a file")))

(defun basis/clipboard-save-string (str)
  "Save STR directly to the system clipboard.
Do not save the string to the the kill ring."
  (funcall interprogram-cut-function str))

(defvar-local basis/clipboard-default-save-thing 'buffer
  "Default \"thing\" to save with `basis/clipboard-save-something'.
Any symbol that `bounds-of-thing-at-point' knows about can be
used here.")

(defun basis/clipboard-save-something (thing beg end)
  "Save the region or buffer to the system clipboard."
  (interactive
   (let ((thing basis/clipboard-default-save-thing))
     (cons thing (basis/bounds-of-region-or-thing thing))))
  (basis/clipboard-save-string (buffer-substring-no-properties beg end))
  (setq deactivate-mark t)
  (when thing
    (message "Copied %s to clipboard" thing)))

(defun basis/kill-ring-save-indented (arg beg end)
  "Save region to the kill ring with ARG spaces of indentation added.
Interactively, default to four spaces of indentation."
  (interactive
   (cons (or current-prefix-arg 4)
         (basis/bounds-of-region-or-thing 'buffer)))
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer beg end)
      (indent-rigidly (point-min) (point-max) arg)
      (kill-ring-save (point-min) (point-max))))
  (setq deactivate-mark t))

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

(defun basis/pop-to-mark-ensure-new-pos (original)
  "Advice for `pop-to-mark-command' to repeat until point moves."
  (let ((p (point))
        (n 0))
    (while (and (= p (point))
                (< (prog1 n (setq n (1+ n)))
                   10))
      (funcall original))))

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

(defun basis/count-words ()
  "Count the lines, words, and characters in the active region.
Like `count-words-region', but operate on the current buffer if
the region isn't active."
  (interactive)
  (let ((current-prefix-arg (not (use-region-p))))
    (call-interactively #'count-words-region)))

(defun basis/count-sloc-region (beg end)
  "Count the \"SLOC\" in the region from BEG to END.
If no region is active, examine the full buffer."
  (interactive (basis/bounds-of-region-or-thing 'buffer))
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
      (when (called-interactively-p 'interactive)
        (message "SLOC: %d" count)))))

(defun basis/cycle-spacing-fast (&optional n)
  "Invoke `cycle-spacing' in `fast' MODE."
  (interactive "*p")
  (cycle-spacing n nil 'fast))

(defun basis/fill-or-unfill-paragraph ()
  (interactive)
  (let ((fill-column (if (eq last-command 'basis/fill-or-unfill-paragraph)
                         (progn (setq this-command nil)
                                (point-max))
                       fill-column)))
    (call-interactively #'fill-paragraph)))

(defun basis/quote-thing (beg end open close)
  "Surround the region from BEG to END in OPEN and CLOSE quotes."
  (interactive
   (pcase-let* ((`(,beg . ,end)
                 (let (bounds)
                   (cond ((use-region-p)
                          (cons (region-beginning) (region-end)))
                         ((setq bounds (bounds-of-thing-at-point 'symbol))
                          bounds)
                         (t
                          (cons (point) (point))))))
                (`(,open . ,close)
                 (let ((char (read-char "Quote type: ")))
                   (if current-prefix-arg
                       (cons char char)
                     (pcase char
                       (?\` '(?` . ?'))
                       (?\' '(?‘ . ?’))
                       (?\" '(?“ . ?”))
                       (c   `(,c . ,c)))))))
     (list beg end open close)))
  (if (eq beg end)
      (progn (goto-char beg)
             (insert open close)
             (forward-char -1))
    (let ((end (copy-marker end)))
      (goto-char beg)
      (insert open)
      (goto-char end)
      (insert close)
      (set-marker end nil))))

(defun basis/insert-parentheses (&optional arg)
  "Like `insert-parenthesis' but treat `C-u' specially.
With `C-u', wrap as may sexps as possible, until reaching
whitespace or an end-of-line. With any other prefix argument, or
if the region is active, defer to `insert-parenthesis'."
  ;; The idea is to be able to wrap the entirety of an expression like
  ;; foo.bar(1, 2)
  (interactive "P")
  (if (and (equal arg '(4))
           (not (use-region-p)))
      (let ((beg (point)))
        (skip-syntax-forward " ")
        (while (not (looking-at-p "[[:space:]]\\|$"))
          (forward-sexp 1))
        (insert ")")
        (goto-char beg)
        (insert "("))
    (call-interactively #'insert-parentheses)))

(defun basis/delete-indentation (&optional arg)
  "Like `delete-indentation' but delete extra comments and whitespace."
  (interactive "*P")
  (delete-indentation arg)
  (when (and (nth 4 (syntax-ppss))
             (looking-at (format "\\(\\s<\\|%c\\|\\s-\\)+"
                                 (elt comment-start 0))))
    (replace-match " ")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search

(defun basis/isearch-backspace (&optional _arg)
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

(defun basis/occur-dwim (regexp nlines)
  "Like `occur', but REGEXP defaults to the text at point."
  (interactive
   (list (pcase-let* ((bnd (basis/bounds-of-region-or-thing 'symbol))
                      (str (when bnd
                             (apply #'buffer-substring-no-properties bnd)))
                      (`(,default ,history)
                       (if str
                           (list str nil)
                         (list (car regexp-history) 'regexp-history-last))))
           (read-regexp (format "Occur (default %s): " default)
                        default
                        history))
         (prefix-numeric-value current-prefix-arg)))
  (occur regexp nlines))

(defvar basis/occur-show-note-regexp
  (regexp-opt '("TODO" "DONE" "NOTE" "KLUDGE" "FIXME" "FAIL" "XXX" "???")
              t)
  "Regexp used by `basis/occur-show-notes'")

(defun basis/occur-show-notes ()
  "Search for common \"TODO\"-style notes."
  (interactive)
  (occur basis/occur-show-note-regexp))

(defun basis/push-mark-noactivate (&rest _args)
  "Advice to push the mark (without activating it)."
  (push-mark nil t nil))

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
      (when-let ((str (save-window-excursion
                        (with-ivy-window
                          (goto-char swiper--opoint)
                          (thing-at-point 'symbol)))))
        (insert str))
    (when-let ((cmd (and (called-interactively-p 'any)
                         (lookup-key global-map (this-command-keys)))))
      (call-interactively cmd))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion

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
  (setq fallback 'basis/ido-open-file-externally-internal)
  (setq ido-exit 'fallback)
  (exit-minibuffer))

(defun basis/company-maybe-block-completion (&rest _args)
  "Prevent `company-auto-begin' from running in some circumstances."
  ;; Used as `:before-until' advice, so returning non-nil prevents completion.
  (pcase major-mode
    (`python-mode
     (basis/in-string-p))
    (`shell-mode
     (and (eq system-type 'windows-nt)
          (save-excursion
            (skip-syntax-backward "^ ")
            (looking-at-p "\\(\\w+@\\)?\\w\\{2,\\}:"))))
    (`sh-mode
     (save-excursion
       (forward-char -2)
       (looking-at-p "\\_<fi\\_>")))))

(defun basis/company-no-srv-completion (&rest _args)
  "Advice for `company-auto-begin'.
For use on a particular host - prevent completion of directories
under \"/srv/\" (ugh)."
  (and (eq major-mode 'shell-mode)
       (save-excursion
         (skip-syntax-backward "^ ")
         (looking-at-p "/srv/"))))

(defun basis/maybe-enable-company-clang ()
  "Conditionally enable `company-clang' for the current buffer."
  (when (and (buffer-file-name)
             (not (basis/file-remote-p (buffer-file-name)))
             (executable-find "clang"))
    (add-to-list 'company-backends #'company-clang)))

(defvar basis/ivy-format-selection-text
  (propertize "> " 'face 'font-lock-function-name-face)
  "Text to place before the selection during `ivy' completion.")

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

(defun basis/helm-find-files (&optional arg)
  "Slightly augmented version of `helm-find-files'.
If called with a negative argument, temporarily invert
`helm-ff-skip-boring-files'. Otherwise, behave the same as
`helm-find-files.'"
  (interactive "P")
  (pcase-let ((`(,helm-ff-skip-boring-files ,current-prefix-arg)
               (if (eq arg '-)
                   (list (not helm-ff-skip-boring-files) nil)
                 (list helm-ff-skip-boring-files current-prefix-arg))))
    (call-interactively #'helm-find-files)))

(defun basis/helm-run-bookmarks ()
  "Run `helm-bookmarks'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action (lambda (&rest _) (helm-bookmarks)))))

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
In `dired-mode', open the marked files; otherwise, prompt for the
file(s) to open with `helm-read-file-name'."
  (interactive
   (list (pcase major-mode
           (`dired-mode
            (dired-get-marked-files))
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
    (helm-exit-and-execute-action #'magit-status)))

(defun basis/helm-pages-get-next-header ()
  "Alternative implementation of `helm-pages-get-next-header'.
Like the above but skip over lines that contain only whitespace
or comment starters."
  (with-helm-current-buffer
    (save-restriction
      (save-excursion
        (narrow-to-page)
        (beginning-of-line)
        (while (and (not (eobp))
                    (looking-at-p (format "^\\(\\s<\\|%c\\|\\s-\\)*$"
                                          (elt comment-start 0))))
          (forward-line))
        (buffer-substring-no-properties (line-beginning-position)
                                        (line-end-position))))))

(defun basis/yas-expand-or-insert ()
  "Call `yas-expand' or `yas-insert-snippet' depending on context.
If point is after what might be a snippet key, call `yas-expand',
otherwise call `yas-insert-snippet'."
  (interactive)
  (call-interactively
   (if (looking-at-p "\\>") #'yas-expand #'yas-insert-snippet)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming modes

(defun basis/eval-something ()
  "Eval the active region, if any; otherwise eval the toplevel form."
  (interactive)
  (if (use-region-p)
      (prog1 (call-interactively #'eval-region)
        (setq deactivate-mark t))
    (call-interactively #'eval-defun)))

(defmacro basis/with-unique-names (names &rest body)
  "Create unique names for use in a macro definition.
This idea also goes by the name `with-gensyms` in Common Lisp."
  (declare (indent 1))
  `(let ,(mapcar (lambda (sym)
                   `(,sym (make-symbol (symbol-name ',sym))))
                 names)
     ,@body))

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

(defun basis/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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
  (with-temp-buffer
    (let ((inhibit-message t)
          (message-log-max nil))
      (zerop (shell-command "python -c \"import jedi; import epc; exit()\""
                            (current-buffer))))))

(defun basis/insert-python-docstring-quotes ()
  "Insert the 6 double quotes for a Python docstring."
  (interactive)
  (insert-char ?\" 6)
  (backward-char 3))

(defun basis/python-find-venv (&optional dir)
  (when-let ((dir (or dir (ignore-errors (projectile-project-root))))
             (env (expand-file-name "env" dir)))
    (and (or (file-exists-p (expand-file-name "bin/activate" env))
             (file-exists-p (expand-file-name "Scripts/activate" env)))
         env)))

(defun basis/pyvenv-activate (dir)
  "Like `pyvenv-activate' but try to guess the directory."
  (interactive
   (list (read-directory-name
          "Activate venv: "
          (when-let ((root (ignore-errors (projectile-project-root))))
            (or (basis/python-find-venv root)
                root)))))
  (if (eq major-mode 'python-mode)
      (pyvenv-activate dir)
    (error "Not in a python-mode buffer")))

(defun basis/run-python (&optional arg)
  "Run an inferior Python process.
If a (Projectile) project root is found, start the Python process
with it as `default-directory'. If a virtualenv associated with
the project is found, prompt to activate it. However, stick to
the base `run-python' functionality with a prefix arg."
  (interactive "P")
  (unless (eq major-mode 'python-mode)
    (error "Not in a python-mode buffer"))
  (if arg
      (call-interactively #'run-python)
    (let* ((prj (ignore-errors (projectile-project-root)))
           (env (basis/python-find-venv prj))
           (default-directory (or prj default-directory)))
      (when env
        (pyvenv-activate env)
        (message "Activated venv `%s'" env))
      (call-interactively #'run-python))))

(defun basis/python-mark-docstring (mark-inside)
  "Mark the docstring around point."
  (unless (python-info-docstring-p)
    (user-error "Not in a docstring"))
  (require 'expand-region)
  (require 'python-el-fgallina-expansions)
  (er/mark-python-string mark-inside))

(defun basis/python-fill-module-docstring (beg end)
  "Fill from BEG to END as a Python module docstring."
  (interactive
   (progn
     (unless (use-region-p)
       (basis/python-mark-docstring t))
     (list (region-beginning) (region-end))))
  (let ((text (delete-and-extract-region beg end)))
    (with-temp-buffer
      (insert text)
      (indent-rigidly (point-min) (point-max) most-negative-fixnum)
      (goto-char (point-min))
      (forward-line 3)               ; Move over the header
      (let ((fill-column 68))
        (fill-region (point) (point-max)))
      (indent-rigidly (point-min) (point-max) 4)
      (setq text (delete-and-extract-region (point-min) (point-max))))
    (insert text)))

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

(defun basis/cider-jack-in ()
  "Run `cider-jack-in' without checking for lein."
  ;; Necessary because `executable-find' can't find lein on my Cygwin box for
  ;; some reason, despite the fact that it's present and works.
  (interactive)
  (require 'cider)
  (cl-letf (((symbol-function 'cider--lein-present-p)
             (lambda () t)))
    (call-interactively #'cider-jack-in)))

(defun basis/cider-eval-something ()
  (interactive)
  (call-interactively (if (use-region-p)
                          #'cider-eval-region
                        #'cider-eval-defun-at-point)))

(defun basis/helm-clj-headlines ()
  (interactive)
  (helm :sources '(((name . "Clojure Headlines")
                    (volatile)
                    (headline "^[;(]")))))

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
          (set-marker end nil))
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
  (if (or (bound-and-true-p multiple-cursors-mode)
          (use-region-p))
      (call-interactively #'backward-delete-char)
    (pcase-let ((`(,spaces ,stuff)
                 (let ((start (point)))
                   (save-excursion
                     (skip-syntax-backward " " (line-beginning-position))
                     (list (- start (point)) (not (bolp)))))))
      (if (and basis/sql-backspace-dedent-hungrily (null n) stuff (> spaces 0))
          (progn (delete-horizontal-space t)
                 (unless (= spaces 1) (insert " ")))
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
    (error "Unknown capitalization style `%s'" style))
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

;; There's probably a better way to do this
(defvar basis/transact-sql-regexps
  '("^\\(use[[:space:]]+\[?[\\sw_]+]?;?\\|go\\)[[:space:]]*$"
    "\\(^\\|[[:space:]]\\)object_id(")
  "Regular expressions to identify Transact-SQL constructs.")

(defun basis/sql-guess-product ()
  "Try to guess the SQL product for the current buffer."
  (if (save-excursion
        (seq-some (lambda (regexp)
                    (goto-char (point-min))
                    (re-search-forward regexp nil t))
                  basis/transact-sql-regexps))
      'ms
    ;; Default to PostgreSQL because I use it the most
    'postgres))

(defun basis/sql-set-product (&optional product)
  "Call `sql-set-product' based on file content."
  (interactive
   (list (intern (completing-read "SQL product: "
                                  (mapcar (lambda (p) (symbol-name (car p)))
                                          sql-product-alist)
                                  nil
                                  t))))
  (sql-set-product (or product (basis/sql-guess-product))))

(defun basis/sql-modify-syntax-table (&rest _)
  "Modify a couple syntax table entries for SQL.
Make double quote a string delimiter and period a symbol
constituent. This is most effective when run as `:after' on
`sql-highlight-product'."
  (let ((table (make-syntax-table (syntax-table))))
    (modify-syntax-entry ?\" "\"\"  " table)
    (modify-syntax-entry ?. "_   " table)
    (set-syntax-table table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text, markup, and configuration modes

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
         (stdout (when-let ((s (plist-get result :stdout)))
                   (basis/process-clojure-output s)))
         (stderr (when-let ((s (plist-get result :stderr)))
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
  (cider-eval-last-sexp-to-repl (cadr (org-babel-get-src-block-info))))

(defun basis/move-to-next-blank-line ()
  "Move point to the next blank line."
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (re-search-forward "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun basis/move-to-previous-blank-line ()
  "Move point to the previous blank line."
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (re-search-backward "^\\s *$" nil t)
      (goto-char (point-min)))))

(defun basis/html-wrap-in-tag (tag beg end)
  "Wrap the selected region in TAG.
If the region is not active, wrap the current line."
  (interactive
   (cons (read-string "Tag: ")
         (if (use-region-p)
             (list (region-beginning) (region-end))
           (list (save-excursion (back-to-indentation) (point))
                 (line-end-position)))))
  (let ((beg (copy-marker beg))
        (end (copy-marker end))
        (one (= (line-number-at-pos beg) (line-number-at-pos end))))
    (goto-char beg)
    (insert "<" tag ">")
    (unless one (newline-and-indent))
    (goto-char end)
    (unless one (newline-and-indent))
    (insert "</" tag ">")
    (indent-region beg end)
    (goto-char beg)
    (setq deactivate-mark t)
    (set-marker beg nil)
    (set-marker end nil)))

(defun basis/html-newline-and-indent ()
  (interactive)
  (if (and (eq (char-before) ?>) (eq (char-after) ?<))
      (tagedit-toggle-multiline-tag)
    (newline-and-indent)))

(defun basis/html-multiline-expand ()
  (interactive)
  (simplezen-expand)
  (basis/html-newline-and-indent))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Brackets

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

(defun basis/sp-backward-up (&optional arg _interactive)
  "Like `sp-backward-up-sexp' but augmented in `python-mode'.
In `python-mode', fall back to `python-nav-backward-up-list' if
`sp-backward-up-sexp' doesn't move point."
  (interactive "^p\np")
  (if (eq major-mode 'python-mode)
      (catch 'done
        (let ((arg (or arg 1)))
          (while (> arg 0)
            (let ((point (point)))
              (sp-backward-up-sexp 1 1)
              (when (= (point) point)
                (python-nav-backward-up-list 1))
              (when (= (point) point)
                (throw 'done t))
              (setq arg (1- arg))))))
    (call-interactively #'sp-backward-up-sexp)))

(defun basis/sp-point-after-word-p (id action context)
  "Like `sp-point-after-word-p' but handle Python and SQL Unicode strings."
  (let ((result (sp-point-after-word-p id action context))
        (regexp (pcase (cons major-mode id)
                  (`(,(or `python-mode `inferior-python-mode) . ,(or "'" "\""))
                   (concat "\\(^\\|[^\\sw\\s_]\\)[bru]" (regexp-quote id)))
                  (`(sql-mode . "'")
                   (concat "\\(^\\|[^\\sw\\s_]\\)N" (regexp-quote id))))))
    (if regexp
        (let ((case-fold-search
               (memq major-mode '(python-mode inferior-python-mode))))
          (and result (not (looking-back regexp (line-beginning-position)))))
      result)))

(defvar basis/sp-inhibit-cleanup-list
  '(indent-relative
    indent-relative-maybe
    python-indent-line-function
    haskell-indentation-indent-line)
  "Indentation functions for which to inhibit smartparens's cleanup.")

(defsubst basis/sp-inhibit-cleanup-p ()
  "Return non-nil if smartparens's cleanup should be inhibited.
See `basis/sp-inhibit-cleanup-list'."
  (memq indent-line-function basis/sp-inhibit-cleanup-list))

(defun basis/sp-unwrap-no-cleanup (args)
  "Advice for `sp--unwrap-sexp' to inhibit problematic cleanup."
  (if (basis/sp-inhibit-cleanup-p)
      (list (car args) t)
    args))

(defun basis/sp-cleanup-maybe-not (original &rest args)
  "Advice for `sp--cleanup-after-kill' to inhibit problematic cleanup."
  (unless (basis/sp-inhibit-cleanup-p)
    (apply original args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error checking

(defun basis/maybe-enable-flycheck ()
  "Enable `flycheck-mode', except for remote files."
  (unless (basis/file-remote-p buffer-file-name)
    (flycheck-mode)))

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

(defun basis/maybe-enable-flyspell ()
  "Enable `flyspell-mode', except for remote files."
  (require 'ispell)
  (when (and ispell-program-name
             (not (basis/file-remote-p buffer-file-name)))
    (flyspell-mode)))

(defun basis/maybe-enable-flyspell-prog-mode ()
  "Enable `flyspell-prog-mode', except for remote files."
  (require 'ispell)
  (when (and ispell-program-name
             (not (basis/file-remote-p buffer-file-name)))
    (flyspell-prog-mode)))

(defun basis/maybe-enable-whitespace-mode ()
  "Enable `whitespace-mode' in programming modes (but not REPLs)."
  (interactive)
  (unless (derived-mode-p 'comint-mode 'cider-repl-mode 'eshell-mode)
    (whitespace-mode)))

(defun basis/ispell-init-process (original &rest args)
  "Advice for `ispell-init-process' on Cygwin.
Let-bind `ispell-current-personal-dictionary' to a
Cygwin-friendly name so that the personal dictionary works."
  (let ((ispell-current-personal-dictionary
         (basis/maybe-cygwinize-file-name ispell-current-personal-dictionary)))
    (apply original args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diffing

(defun basis/diff-buffer-with-file (buffer file)
  "View the differences between BUFFER and FILE.
With a prefix arg, prompt for both BUFFER and FILE. Otherwise,
use the current buffer and prompt for FILE."
  (interactive (list (if current-prefix-arg
                         (read-buffer "Buffer: " nil t)
                       (current-buffer))
                     (read-file-name "File: " nil nil t)))
  (diff file buffer nil 'noasync))

(defun basis/ediff-expand-tmp-name (args)
  "Advice for `ediff-make-empty-tmp-file'.
Call `expand-file-name' on the proposed file name. Only necessary
on Windows."
  (cons (expand-file-name (car args))
        (cdr args)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit & other git things

(defun basis/magit-browse-pull-request-url ()
  "Visit the current branch's PR on GitHub."
  (interactive)
  (if-let ((regexp "\\`.+github\\.com:\\(.+\\)\\.git\\'")
           (remote (magit-get "remote" (magit-get-remote) "url"))
           (repo (and (string-match regexp remote)
                      (match-string 1 remote))))
      ;; Or this? "https://github.com/%s/pull/new/%s"
      (browse-url (format "https://github.com/%s/compare/%s"
                          repo
                          (magit-get-upstream-branch)))
    (error "No repo or remote associated with current buffer")))

(defun basis/magit-expand-toplevel (result)
  "Advice for `magit-toplevel'.
For use with Cygwin. Call `expand-file-name' on its result, to
make sure its in the same form that Emacs uses (i.e.
\"c:/path/to/somewhere\")."
  (and result (expand-file-name result)))

(defun basis/magit-list-repos-uniquely (result)
  "Advice for `magit-list-repos'."
  ;; Only necessary on my Cygwin setup
  (delete-dups (mapcar #'abbreviate-file-name result)))

(defun basis/magit-maybe-delete-other-windows ()
  "Give `magit-status-mode' buffers the whole frame."
  (when (eq major-mode 'magit-status-mode)
    (delete-other-windows)))

(defun basis/magit-cygwin-save-repository-buffers (&optional arg)
  "Alternative `magit-save-repository-buffers'.
Use `expand-file-name' to canonicalize file names to Emacs's
representation before comparing them."
  (interactive "P")
  (when-let ((topdir (magit-rev-parse-safe "--show-toplevel"))
             (topdir (expand-file-name topdir)))
    (save-some-buffers
     arg
     (lambda ()
       (and buffer-file-name
            ;; Avoid needlessly connecting to unrelated remotes.
            (string-prefix-p topdir buffer-file-name)
            (equal (when-let ((dir (magit-rev-parse-safe "--show-toplevel"))
                              (dir (expand-file-name dir)))
                     dir)
                   topdir))))))

(defun basis/magit-stash-save (message &optional include-untracked)
  "Run \"git stash save\"."
  (interactive (magit-stash-read-args))
  (if (magit-git-success "stash" "save" (and include-untracked "-u") message)
      (magit-refresh)
    (error "Unable to stash")))

(defun basis/magit-stash-snapshot (&optional include-untracked)
  "Run \"git stash save\" with a generated message."
  (interactive (magit-snapshot-read-args))
  (basis/magit-stash-save (concat "WIP on " (magit-stash-summary))
                          include-untracked))

(defun basis/magit-stash-pop (stash)
  "Run \"git stash pop\"."
  (interactive (list (magit-read-stash "Apply pop" t)))
  (if (magit-git-success "stash" "pop" stash)
      (magit-refresh)
    (error "Unable to pop stash")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Processes and shells

(defun basis/make-tags (arg)
  "Regenerate and reload TAGS via `make tags'.
Of course, this only works if a Makefile with a \"TAGS\" target
is available."
  (interactive "P")
  (let* ((project-root (ignore-errors (projectile-project-root)))
         (default-directory (if (and (not arg) project-root)
                                project-root
                              default-directory))
         (buffer (with-current-buffer (get-buffer-create "*make-TAGS*")
                   (let ((inhibit-read-only t))
                     (erase-buffer))
                   (special-mode)
                   (current-buffer))))
    (set-process-sentinel
     (start-process "make-TAGS" buffer "make" "TAGS")
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (pcase (process-exit-status proc)
           (0 (message "Regenerated TAGS")
              (unless (eq arg '-) (visit-tags-table "TAGS")))
           (_ (pop-to-buffer buffer))))))))

(defun basis/comint-input-goto-bottom-if-necessary (&rest _args)
  "Advice for `comint' {previous,next}-input commands.
If an adviced command would signal a \"Not at command line\"
user-error, automatically move point to the command line."
  (when (and comint-scroll-to-bottom-on-input
             (not (comint-after-pmark-p)))
    (goto-char (point-max))))

(defun basis/colorize-compilation ()
  "Colorize from `compilation-filter-start' to point."
  (unless (string-match-p "\\*grep\\*" (buffer-name))
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(defun basis/eshell-kill-line-backward ()
  "Kill the current line backward, respecting Eshell's prompt."
  (interactive)
  (kill-region (save-excursion (eshell-bol) (point))
               (point)))

(defun basis/eshell-kill-whole-line ()
  "Kill the current line, respecting Eshell's prompt."
  (interactive)
  (kill-region (save-excursion (eshell-bol) (point))
               (line-end-position)))

(defvar basis/clang-program "clang"
  "The clang program used by `basis/find-clang-includes-path'.")

(defun basis/find-clang-includes-path (&optional language)
  "Return clang's #include <...> search path."
  ;; This isn't a very satisfactory solution but it's "good enough"
  (let ((language (or language 'c)))
    (unless (memq language '(c c++))
      (error "Unknown language `%s'" language))
    (with-temp-buffer
      (apply #'call-process basis/clang-program nil t nil
             (list "-E" (if (eq language 'c++) "-xc++" "-xc") "-" "-v"))
      (goto-char (point-min))
      (re-search-forward "^#include <\\.\\.\\.> search starts here:$")
      (forward-line 1)
      (let ((str (buffer-substring-no-properties
                  (line-beginning-position)
                  (progn (re-search-forward "^End of search list\\.$")
                         (line-beginning-position)))))
        (seq-filter #'file-directory-p
                    (split-string str "\n" t "[[:space:]]+"))))))

(defun basis/build-clang-args (&optional language)
  (let ((language (or language 'c)))
    (unless (memq language '(c c++))
      (error "Unknown language `%s'" language))
    (when-let ((std (if (eq language 'c++) "c++11" "c11"))
               (inc (ignore-errors (basis/find-clang-includes-path language))))
      (cons (format "-std=%s" std) inc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File utilities

(defun basis/rename-current-buffer-file (buffer destination)
  "Rename BUFFER and the file it's visiting to DESTINATION."
  (interactive
   (list (current-buffer)
         (read-file-name "Destination: ")))
  (with-current-buffer buffer
    (let* ((src (buffer-file-name))
           (dst (if (file-directory-p destination)
                    (expand-file-name (file-name-nondirectory src)
                                      destination)
                  destination)))
      (unless (and src (file-exists-p src))
        (error "Buffer `%s' is not visiting a file" src))
      (rename-file src dst 1)
      (set-visited-file-name dst)
      (set-buffer-modified-p nil)
      (apply #'message
             "File `%s' renamed to `%s'"
             (if (file-in-directory-p dst (file-name-directory src))
                 (list (file-name-nondirectory src)
                       (file-relative-name dst (file-name-directory src)))
               (list (abbreviate-file-name src)
                     (abbreviate-file-name dst)))))))

(defun basis/delete-current-buffer-file ()
  "Kill the current buffer and delete the file it's visiting."
  (interactive)
  (let* ((buffer (current-buffer))
         (file (buffer-file-name))
         (abbr (abbreviate-file-name file)))
    (if (and file (file-exists-p file))
        (when (y-or-n-p (format "Delete file `%s'?" abbr))
          (delete-file file)
          (kill-buffer buffer)
          (message "File `%s' deleted" abbr))
      (unless (and (buffer-modified-p)
                   (not (y-or-n-p (format "Buffer `%s' modified; kill anyway?"
                                          (buffer-name)))))
        (kill-buffer)))))

(defun basis/find-file-recentf ()
  "Find recently open files using ido and recentf."
  (interactive)
  (let* ((list (mapcar #'abbreviate-file-name recentf-list))
         (file (completing-read "Recent file: " list nil t)))
    (when file
      (find-file file))))

(defun basis/windows->unix (filename)
  "Convert FILENAME from Windows-style to UNIX-style."
  (thread-last filename
    (replace-regexp-in-string "\\\\" "/")
    (replace-regexp-in-string "\\`[a-zA-Z]:" "")))

(defun basis/maybe-cygwinize-file-name (file)
  "Convert e.g. \"c:/foo\" to \"/c/foo\"."
  (if (and file (string-match "\\`\\([A-Za-z]\\):\\(/.*\\)" file))
      (let ((drive (match-string 1 file))
            (name (match-string 2 file)))
        (if (member drive '("c" "C"))
            name
          (concat "/" drive name)))
    file))

(defun basis/cygwin-shell-quote-argument (args)
  "Advice for `shell-quote-argument' on machines with Cygwin.
Quote file names appropriately for POSIX-like shells."
  ;; Used as :filter-args advice
  (list (basis/maybe-cygwinize-file-name (car args))))

(defun basis/read-file (file)
  "Read a Lisp form from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun basis/file-remote-p (name)
  (and name (or (tramp-tramp-file-p name) (file-remote-p name))))

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

(defun basis/insert-files (files &optional buffer)
  "Insert the contents of FILES into BUFFER.
If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (mapc #'insert-file-contents files)))

(defun basis/dired-slurp-files (files buffer)
  "Insert the contents of marked FILES into BUFFER.
If it doesn't exist, BUFFER is created automatically."
  (interactive (list (if (eq major-mode 'dired-mode)
                         (dired-get-marked-files)
                       (error "Buffer not in `dired-mode'"))
                     (read-buffer "Destination buffer: ")))
  (basis/insert-files files (get-buffer-create buffer)))

(defun basis/dired-rsync (files destination)
  "Rsync FILES to DESTINATION.
When called interactively, FILES is the list of marked files."
  (interactive (list (dired-get-marked-files nil current-prefix-arg)
                     (thread-first "Rsync to: "
                       (read-file-name (dired-dwim-target-directory))
                       (expand-file-name))))
  (when (null files)
    (error "No files selected"))
  (pcase-let ((`(,destination . ,files)
               (if (eq basis/system-type 'windows+cygwin)
                   (mapcar #'basis/windows->unix (cons destination files))
                 (cons destination files))))
    (let ((cmd (mapconcat #'identity
                          (list "rsync -arvz --progress"
                                (mapconcat #'shell-quote-argument files " ")
                                (shell-quote-argument destination))
                          " ")))
      (async-shell-command cmd "*rsync*")
      (other-window 1))))

(defun basis/download-file (url destination &optional visit)
  "Download URL to DESTINATION.
If VISIT is non-nil, visit the file after downloading it."
  (interactive
   (let* ((url (read-string "URL: " (thing-at-point 'url)))
          (name (thread-first url
                  url-generic-parse-url
                  url-path-and-query
                  car
                  (split-string "/")
                  last
                  car)))
     (list url (read-file-name "Destination: " nil name) current-prefix-arg)))
  (url-copy-file url destination 0)
  (when visit (find-file destination)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project management

(defun basis/xref-next-group (&optional n)
  "Move forward to the N-th next group (i.e. file)."
  (interactive "p")
  (let ((n (or n 1)))
    (forward-line n)
    (while (and (not (eobp))
                (get-text-property (point) 'xref-item))
      (forward-line n))))

(defun basis/xref-prev-group (&optional n)
  "Move backward to the N-th previous group (i.e. file)."
  (interactive "p")
  (basis/xref-next-group (- (or n 1))))

(defun basis/projectile-regenerate-tags (original &rest args)
  "Advice for `projectile-regenerate-tags'.
Call `save-some-buffers' for buffers visiting files inside the
project before regenerating tags. On Cygwin, use Cygwin-style
paths when calling the tags command."
  (let* ((root (projectile-project-root))
         (pred (lambda ()
                 (string-prefix-p root (file-truename (buffer-file-name))))))
    (save-some-buffers nil pred)
    (pcase basis/system-type
      (`windows+cygwin
       ;; Duplicate the logic from `projectile-regenerate-tags' here because
       ;; there's no good way to "cygwnize" the tags file's name
       (let* ((default-directory root)
              (tags-file (expand-file-name projectile-tags-file-name))
              (command (format projectile-tags-command
                               (basis/maybe-cygwinize-file-name tags-file)
                               (projectile-tags-exclude-patterns))))
         (with-temp-buffer
           (unless (zerop (call-process-shell-command command
                                                      nil
                                                      (current-buffer)))
             (thread-last (buffer-substring (point-min) (point-max))
               string-trim
               (error "%s"))))
         (visit-tags-table tags-file)))
      (_
       (apply original args)))))

(defun basis/ibuffer-vc-root-files-only (original buf)
  "Advice for `ibuffer-vc-root'.
Only group a buffer with a VC if its visiting a file."
  (when (buffer-file-name buf)
    (funcall original buf)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window utilities

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interface

(defun basis/disable-themes (&optional themes)
  "Disable THEMES (defaults to `custom-enabled-themes')."
  (interactive)
  (mapc #'disable-theme (or themes custom-enabled-themes)))

(defun basis/maybe-set-emoji-font (&optional frame)
  "Set a system-specific font for symbols (including emojis)."
  (when-let ((font-spec (pcase system-type
                          (`darwin (font-spec :family "Apple Color Emoji")))))
    (set-fontset-font t 'symbol font-spec frame 'prepend)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous

(defun basis/read-choice (prompt choices)
  "Prompt to select one of CHOICES and return the result.
CHOICES is a list of (CHAR DESCRIPTION)."
  (let ((cursor-in-echo-area t)
        (prompt (if (or (not prompt)
                        (get-text-property 0 'face prompt))
                    prompt
                  (propertize prompt
                              'face
                              'minibuffer-prompt))))
    (save-window-excursion
      (pop-to-buffer " *Read choice*" t t)
      (fundamental-mode)
      (setq cursor-type nil)
      (erase-buffer)
      (pcase-dolist (`(,char ,description) choices)
        (unless (and (characterp char)
                     (<= 33 char 126))
          ;; TODO: Add support for any key
          (user-error "%s is not a valid character"
                      (key-description (vector char))))
        (insert (propertize (string char)
                            'face
                            'font-lock-variable-name-face)
                (format " %s\n" description)))
      (goto-char (point-min))
      (fit-window-to-buffer)
      (let ((choice (read-key prompt)))
        (or (assoc choice choices)
            (user-error "Invalid selection: %s"
                        (key-description (vector choice))))))))

(defun basis/google (string)
  "Run a Google search for STRING.
Use the active region or symbol at point, if any, as the default
search term."
  (interactive
   (list (if-let ((bounds (basis/bounds-of-region-or-thing 'symbol))
                  (string (apply #'buffer-substring-no-properties bounds)))
             (read-string (format "Google (default %s) " string)
                          nil nil
                          string)
           (read-string "Google: "))))
  (browse-url
   (concat "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
           (url-hexify-string string))))

(defun basis/goto-line-with-numbers ()
  "Invoke `goto-line' with `linum-mode' temporarily enabled."
  (interactive)
  (let ((enabled (bound-and-true-p linum-mode)))
    (unwind-protect
        (progn
          (unless enabled (linum-mode 1))
          (call-interactively #'goto-line))
      (unless enabled (linum-mode -1)))))

(defun basis/libxml-available-p ()
  "Return non-nil if libxml is available."
  (and (fboundp 'libxml-parse-html-region)
       (with-temp-buffer
         (insert "<html></html>")
         (not (null (libxml-parse-html-region (point-min) (point-max)))))))

(defun basis/full-calc-frame ()
  "Create a new frame and run `calc' in a full-size window."
  (interactive)
  (with-selected-frame (make-frame)
    (calc nil t t)))

(defun basis/looking-back-p (regexp &optional limit)
  "Same as `looking-back' but don't change the match data."
  (save-match-data (looking-back regexp limit)))

(defun basis/kill-buffer (&optional buffer)
  "Kill BUFFER.
Like `kill-buffer' but, when called interactively, kill the
current buffer without pompting, unless called with a prefix
argument."
  (interactive
   (list (and current-prefix-arg
              (read-buffer "Kill buffer: " (current-buffer) t))))
  (kill-buffer (or buffer (current-buffer))))

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
  (nth 3 (syntax-ppss)))

(defun basis/shr-html2text ()
  "Convert HTML to plain text in the current buffer using `shr'."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)))

(defun basis/visit-tags-file-auto ()
  "Automatically find and visit a TAGS file."
  (interactive)
  (when-let ((file (buffer-file-name))
             (tags (locate-dominating-file file "TAGS")))
    (visit-tags-table (expand-file-name "TAGS" tags) t)))

(defun basis/flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun basis/time-function (fn)
  "Return the execution time in seconds of calling FN.
Byte-compile FN before calling it."
  (setq fn (byte-compile fn))
  (let ((beg (float-time)))
    (funcall fn)
    (/ (truncate (* (- (float-time (current-time))
                       beg)
                    10000))
       10000.0)))

(defmacro basis/time-forms (&rest body)
  "Return the execution time in seconds of evaluating BODY.
Wrap BODY in a lambda and use `basis/time-function' so that the
forms are byte-compiled."
  `(basis/time-function (lambda () (progn ,@body))))

(defun basis/toggle-echo-area-messages ()
  "Toggle whether the \"*Messages*\" buffer is shown."
  (interactive)
  (let* ((buffer (messages-buffer))
         (window (get-buffer-window buffer 'visible)))
    (if window
        (if (eq window (selected-window))
            (bury-buffer)
          (delete-window window))
      (with-current-buffer buffer
        (goto-char (point-max))
        (display-buffer (current-buffer))))))

(defun basis/emacs-Q (emacs home)
  "Run `EMACS -Q' with its home in HOME.
If called interactively, use the current emacs and /tmp by
default, unless called with a prefix argument."
  ;; This is only really useful on Windows, where I use a Cygwin shell but a
  ;; native Windows Emacs, so `emacs -Q' in my shell doesn't cut it.
  (interactive
   (pcase-let ((`(,self ,tmp)
                (if (eq system-type 'windows-nt)
                    (list (expand-file-name "runemacs.exe" invocation-directory)
                          "e:\\tmp")
                  (list (expand-file-name invocation-name invocation-directory)
                        "/tmp"))))
     (if current-prefix-arg
         (list (read-file-name "Emacs: " nil self t nil #'file-executable-p)
               (read-directory-name "HOME: " nil tmp t))
       (list self tmp))))
  (let ((emacs (expand-file-name emacs))
        (process-environment (cons (concat "HOME=" home) process-environment)))
    (start-process "emacs-Q" nil emacs "-Q")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Applications

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
  (seq-mapcat #'basis/elfeed-parse-group (basis/read-file file)))

(defun basis/define-word (arg)
  (interactive "P")
  (call-interactively (if arg #'define-word #'define-word-at-point)))

(defun basis/delete-cookies ()
  (when (fboundp 'url-cookie-delete-cookies)
    (url-cookie-delete-cookies)))

(defun basis/sx-display-full-screen ()
  "Like `sx-display' but also delete any other windows."
  (interactive)
  (call-interactively #'sx-display)
  (delete-other-windows))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Email and news

(defvar basis/message-delete-sig-dashes nil
  "If non-nil, delete `message-mode' signature dashes.")

(defun basis/message-maybe-delete-sig-dashes (&rest _)
  "Advice for `message-insert-signature' to remove sig dashes."
  ;; Used as :after advice
  (when basis/message-delete-sig-dashes
    (save-excursion
      (beginning-of-line)
      (forward-line -2)
      (when (looking-at-p "^-- $")
        (kill-whole-line)))))

(defun basis/mu4e-action-view-in-browser (msg)
  "View MSG in a browser, via `browse-url'.
For use as a `mu4e' message action."
  (if-let ((html (mu4e-msg-field msg :body-html))
           (file (make-temp-file "" nil ".html")))
      (with-temp-buffer
        (insert html)
        (write-region nil nil file)
        (browse-url (concat "file://" file)))
    (error "This message doesn't have an HTML part")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lorem ipsum

(defvar basis/lorem-ipsum-file (basis/emacs-file "lorem-ipsum.txt")
  "File containing \"lorem ipsum\" placeholder text.")

(defun basis/insert-lorem-ipsum (&optional arg)
  "Insert ARG paragraphs of \"lorem ipsum\" text at point."
  (interactive "p")
  (unless (file-exists-p basis/lorem-ipsum-file)
    (error "Lorem ipsum file does not exist: `%s'" basis/lorem-ipsum-file))
  (let* ((arg (or arg 1))
         (str (with-temp-buffer
                (insert-file-contents basis/lorem-ipsum-file)
                (goto-char (point-min))
                (forward-paragraph arg)
                (buffer-substring-no-properties (point-min) (point)))))
    (insert str)))


;;; defuns.el ends here
