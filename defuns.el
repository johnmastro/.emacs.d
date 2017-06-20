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
  "Define key bindings for evaluating various units of code.
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

(defvar-local basis/beginning-of-buffer-function #'ignore
  "Function to move to the logical beginning of a buffer.")

(defvar-local basis/end-of-buffer-function #'ignore
  "Function to move to the logical end of a buffer.")

(defun basis/beginning-of-buffer (&optional arg)
  (interactive "^P")
  (if (null arg)
      (let ((pos (point)))
        (or (region-active-p) (push-mark))
        (funcall basis/beginning-of-buffer-function)
        (and (eq (point) pos) (goto-char (point-min))))
    (call-interactively #'beginning-of-buffer)))

(defun basis/end-of-buffer (&optional arg)
  (interactive "^P")
  (if (null arg)
      (let ((pos (point)))
        (or (region-active-p) (push-mark))
        (funcall basis/end-of-buffer-function)
        (and (eq (point) pos) (goto-char (point-max))))
    (call-interactively #'end-of-buffer)))

(defun basis/next-line ()
  "Move point to the next line.
Wrapper around `next-line' to let-bind `next-line-add-newlines'
to nil if a keyboard macro is executing. Also let-bind
`deactivate-mark' so that the mark isn't deactivated when
newlines are entered."
  (interactive)
  (let ((next-line-add-newlines
         (and next-line-add-newlines
              (not executing-kbd-macro)))
        (deactivate-mark))
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

(defvar basis/blank-line-regexp "^[[:blank:]]*$"
  "Regexp matching a blank line.")

(defun basis/next-blank-line (n)
  "Move point to the Nth next blank line."
  (interactive "p")
  (let* ((back  (< n 0))
         (count (abs n)))
    (while (and (> count 0) (not (if back (bobp) (eobp))))
      (and (save-excursion (beginning-of-line)
                           (looking-at basis/blank-line-regexp))
           (forward-line (if back -1 1)))
      (or (and (funcall (if back #'re-search-backward #'re-search-forward)
                        basis/blank-line-regexp nil t)
               (goto-char (match-end 0)))
          (goto-char (if back (point-min) (point-max))))
      (setq count (1- count)))))

(defun basis/previous-blank-line (n)
  "Move point to the Nth previous blank line."
  (interactive "p")
  (basis/next-blank-line (- n)))

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

(defvar basis/whitespace-chars "\f\r\n[:blank:]"
  "Whitespace characters, for e.g. `skip-chars-forward'.")

(defun basis/uncomment-sexp (&optional n)
  "Uncomment the sexp at point."
  (interactive "*p")
  (let* ((start (point-marker))
         (pos nil)
         (end (save-excursion
                (when (nth 4 (syntax-ppss))
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
  (interactive "*p")
  (if (or (nth 4 (syntax-ppss))
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
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
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
  "If non-nil, do not perform substitutions in strings or comments.")

(defvar-local basis/smart-hyphen-style 'snake
  "The substitution style to use (`snake', `camel', or nil).")

(defun basis/smart-hyphen (n)
  "Conditionally insert a hyphen or upcase the next char."
  (interactive "*p")
  (unless (memq basis/smart-hyphen-style '(snake camel nil))
    (error "Unknown smart-hyphen style: `%s'" basis/smart-hyphen-style))
  (if (or (not basis/smart-hyphen-style)
          (and basis/smart-hyphen-code-only
               (let ((state (syntax-ppss)))
                 (or (nth 3 state)
                     (nth 4 state)))))
      (self-insert-command n)
    (insert "-")
    (let ((command (key-binding (vector (read-event)))))
      (if (eq command 'self-insert-command)
          (insert (let ((next (elt (this-command-keys) 1)))
                    (if (and (memq (char-syntax next) '(?w ?_))
                             ;; Don't perform the replacement if the preceding
                             ;; expression is a literal number or simple numeric
                             ;; expression (e.g. arithmetic)
                             (save-excursion
                               (skip-chars-backward "[:digit:][:punct:]()")
                               (not (looking-at-p "\\_<\\|("))))
                        (pcase basis/smart-hyphen-style
                          (`camel
                           (delete-char -1)
                           (upcase next))
                          (`snake
                           (delete-char -1)
                           (format "_%c" next)))
                      next)))
        (call-interactively command)))))

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
                       (and (bound-and-true-p auto-fill-function)
                            fill-column)
                       80))
        (start (point))
        (line (line-number-at-pos))
        result)
    (when (eolp)
      (forward-line 1)
      (setq line (1+ line)))
    (while (not (or result (eobp)))
      (end-of-line)
      (let ((column (current-column)))
        (if (> column threshold)
            (let ((chars (- (point) (line-beginning-position))))
              (when message
                (message "Line %d is %d columns (%d chars) long"
                         line column chars))
              (setq result (cons line column)))
          (forward-line 1)
          (setq line (1+ line)))))
    (unless result
      (goto-char start)
      (when message (message "No long lines found")))
    result))

(defun basis/kill-something (arg)
  "Kill the region, or one or more words backward.
If `subword-mode' is active, use `subword-backward-kill'."
  (interactive "*p")
  (cond ((use-region-p)
         (kill-region (region-beginning) (region-end)))
        ((bound-and-true-p subword-mode)
         (subword-backward-kill arg))
        (t
         (backward-kill-word arg))))

(defun basis/smart-kill-whole-line (&optional arg)
  "Variant of `kill-whole-line'.
Kill the current line and move point to the first non-whitespace
character of the next line."
  (interactive "*P")
  (kill-whole-line arg)
  (back-to-indentation))

(defun basis/duplicate-line (n)
  "Duplicate the line at point.
With an argument N, duplicate that many lines."
  (interactive "*p")
  (let ((col (current-column))
        (beg (line-beginning-position))
        (end (progn (forward-line n) (point))))
    (insert-buffer-substring (current-buffer) beg end)
    (forward-line (- n))
    (move-to-column col)))

(defun basis/kill-ring-save-something (beg end &optional indent-by)
  "Save the contents of the active region or the current line."
  (interactive
   (pcase-let ((`(,beg . ,end)
                (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (cons (line-beginning-position)
                        (save-excursion (forward-line 1) (point)))))
               (arg current-prefix-arg))
     (list beg end (and arg (prefix-numeric-value arg)))))
  (if indent-by
      ;; It arguably makes more sense to add indentation on yank, but adding it
      ;; on save has two upsides: the indented code or text can then be pasted
      ;; into another program, and `yank' already takes an argument.
      (let ((buffer (current-buffer)))
        (with-temp-buffer
          (insert-buffer-substring buffer beg end)
          (indent-rigidly (point-min) (point-max) indent-by)
          (kill-ring-save (point-min) (point-max)))
        (setq deactivate-mark t))
    (kill-ring-save beg end)))

(defun basis/kill-ring-save-as-fenced-code-block (beg end &optional indent)
  "Save the region from BEG to END as a fenced code block."
  (interactive
   (pcase-let* ((`(,beg . ,end)
                 (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (cons (point-min) (point-max))))
                (indent
                 (cond ((integerp current-prefix-arg)
                        current-prefix-arg)
                       ((consp current-prefix-arg)
                        (- (indent-rigidly--current-indentation beg end))))))
     (list beg end indent)))
  (let* ((buffer (current-buffer))
         (mode-name (symbol-name major-mode))
         (language (replace-regexp-in-string "-mode\\'" "" mode-name)))
    (with-temp-buffer
      (insert-buffer-substring buffer beg end)
      (goto-char (point-min))
      (when indent
        (indent-rigidly (point-min) (point-max) indent))
      (when (looking-at "^\\(\\([[:blank:]]*\n\\)+\\)")
        (delete-region (match-beginning 1) (match-end 1)))
      (insert "```" language "\n")
      (if (re-search-forward "^\\([[:blank:]\n]+\\)\\'" nil t)
          (delete-region (match-beginning 1) (match-end 1))
        (goto-char (point-max)))
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (insert "```")
      (kill-ring-save (point-min) (point-max)))
    (setq deactivate-mark t)))

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

(defun basis/clipboard-save-something (beg end &optional thing)
  "Save the region or buffer to the system clipboard."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) 'region)
     (list (point-min) (point-max) 'buffer)))
  (basis/clipboard-save-string (buffer-substring-no-properties beg end))
  (setq deactivate-mark t)
  (when thing (message "Copied %s to clipboard" thing)))

(defmacro basis/def-case-op (op)
  (let ((region-op  (intern (format "%s-region" op)))
        (word-op    (intern (format "%s-word" op)))
        (subword-op (intern (format "subword-%s" op))))
    `(defun ,(intern (format "basis/%s-something" op)) (&optional count)
       ,(format "%s the region or COUNT words." (capitalize (symbol-name op)))
       (interactive "*p")
       (cond ((use-region-p)
              (,region-op (region-beginning) (region-end)))
             ((bound-and-true-p subword-mode)
              (,subword-op count))
             (t
              (,word-op count))))))

(basis/def-case-op upcase)
(basis/def-case-op downcase)
(basis/def-case-op capitalize)

(defun basis/downcase-almost-everything (arg)
  "Downcase everything not in a string or comment."
  (interactive "P")
  (let ((start (point))
        (count 0)
        (case-fold-search nil))
    (and arg (goto-char (point-min)))
    (while (re-search-forward "[[:upper:]]+" nil t)
      (let ((state (syntax-ppss)))
        (cond ((nth 3 state)  ; Inside a string
               (goto-char (nth 8 state))
               (forward-sexp 1))
              ((nth 4 state)  ; Inside a comment
               (goto-char (nth 8 state))
               (forward-comment (buffer-size)))
              (t
               (replace-match (downcase (match-string 0)) t t)
               (setq count (1+ count))))))
    (and (zerop count) (goto-char start))
    (message "Made %d replacements" count)))

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
  (save-excursion (untabify (point-min) (point-max))))

(defun basis/indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max))))

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
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
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
  "Fill or un-fill the paragraph at or after point."
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
                       (?\` '(?\` . ?\'))
                       (?\' '(?\‘ . ?\’))
                       (?\" '(?\“ . ?\”))
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

(defun basis/delete-some-whitespace ()
  "Delete whitespace around point."
  (interactive)
  (let* ((pos (point))
         (tab (progn (skip-chars-backward "[:blank:]\f")
                     (and indent-tabs-mode (eq (char-after) ?\t))))
         (beg (constrain-to-field nil pos))
         (end (progn (skip-chars-forward "[:blank:]\f")
                     (constrain-to-field nil pos t)))
         (cnt (- end beg)))
    (cond ((> cnt 1)
           (delete-region beg end)
           (insert (if tab ?\t ?\s)))
          ((= cnt 1)
           (delete-region beg end)
           (when tab (insert ?\s)))
          ((or (bolp) (eolp))
           (let* ((beg (if (re-search-backward "[^[:blank:]\f\r\n]" nil t)
                           (progn (forward-line 1)
                                  (point))
                         (point-min)))
                  (end (progn (or (re-search-forward "[^[:blank:]\f\r\n]" nil t)
                                  (goto-char (point-max)))
                              (beginning-of-line)
                              (max (point) beg)))
                  (cnt (count-lines beg end))
                  (pos (copy-marker pos (= pos end))))
             (delete-region beg end)
             (when (> cnt 1) (insert ?\n))
             (goto-char pos)
             (prog1 nil (set-marker pos nil)))))))

(defun basis/delete-indentation (&optional arg)
  "Augmented version of `delete-indentation'.
Like `delete-indentation', but also delete redundant comment
characters and, if joining to an empty line, re-indent."
  (interactive "*P")
  (if arg (forward-line) (beginning-of-line))
  (let ((pos (point-marker)))
    (when comment-start
      (comment-normalize-vars)
      (let* ((beg-blk (or (and (derived-mode-p 'sql-mode)
                               "/\\*")
                          (and (boundp 'c-block-comment-start-regexp)
                               c-block-comment-start-regexp)))
             (end-blk (or (and (derived-mode-p 'sql-mode)
                               "\\*/")
                          (and (boundp 'c-block-comment-ender-regexp)
                               c-block-comment-ender-regexp)))
             (beg-rxp (concat "\\(\\s<+\\|"
                              comment-start-skip
                              (and beg-blk (concat "\\|" beg-blk))
                              "\\)"))
             (con-str (or (and (> (length comment-continue) 0)
                               comment-continue)
                          (and (derived-mode-p 'sql-mode)
                               " * ")
                          (and (boundp 'c-block-comment-prefix)
                               (stringp c-block-comment-prefix)
                               c-block-comment-prefix)))
             (con-rxp (and con-str (regexp-quote con-str)))
             (end-rxp (concat "\\(\\s>+\\|"
                              comment-end-skip
                              (and end-blk (concat "\\|" end-blk))
                              "\\)")))
        (cond ((and (progn (skip-chars-forward " \t")
                           (looking-at beg-rxp))
                    (save-excursion
                      (save-match-data
                        (let ((bol (progn (forward-line -1) (point)))
                              (eol (progn (end-of-line) (point))))
                          (or (and (progn (skip-chars-backward " \t")
                                          (looking-back end-rxp bol))
                                   (progn (delete-region (match-beginning 0)
                                                         eol)
                                          t))
                              (progn (goto-char bol)
                                     (looking-at beg-rxp)))))))
               (delete-region pos (match-end 0)))
              ((let ((beg (and con-rxp (nth 8 (syntax-ppss)))))
                 (when (and beg (> (point) beg))
                   (progn (skip-chars-forward " \t")
                          (let ((n (length con-str))
                                (i 0))
                            (while (and (< i n)
                                        (not (bolp))
                                        (memq (aref con-str i) '(?\s ?\t)))
                              (forward-char -1)
                              (setq i (1+ i))))
                          (looking-at con-rxp))))
               (delete-region pos (match-end 0))))))
    (delete-indentation)
    (when (bolp) (indent-according-to-mode))
    (prog1 nil (set-marker pos nil))))

(defun basis/narrow-or-widen-dwim (arg)
  "Widen if buffer is narrowed, otherwise narrow.
When narrowing, narrow to the region if active, otherwise to the
current defun. With prefix ARG, never widen; narrow even if the
buffer is already narrowed."
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not arg))
         (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        (t
         (narrow-to-defun))))

(defun basis/next-trailing-whitespace ()
  "Move point to the next occurrence of trailing whitespace."
  (interactive)
  (let ((start (point)))
    (skip-chars-forward "[:blank:]")
    (if (re-search-forward "[[:blank:]]+$" nil t)
        (goto-char (match-beginning 0))
      (goto-char start)
      (message "No trailing whitespace"))))

(defun basis/find-non-text-character ()
  "Find a non-text character, if any, in the current buffer.
\"Non-text\" is not a well-defined term, but in this case it
means anything that's not a printing character, a whitespace
character, or newline."
  (interactive)
  (if (re-search-forward "[^[:print:][:space:]\n]" nil t)
      (forward-char -1)
    (message "No non-text characters found")))

(defun basis/abbrev-insert-undo-boundary (&rest _)
  "Advice for `abbrev-insert'.
When triggered by `self-insert-command', insert an undo boundary
after inserting the character."
  (when (eq this-command 'self-insert-command)
    (let ((abbrev-mode nil))
      (call-interactively #'self-insert-command)
      (undo-boundary))))

(defvar basis/file-preamble-lines
  '((emacs-lisp-mode ";; -*- coding: utf-8; lexical-binding: t; -*-")
    (python-mode     "#!/usr/bin/env python" "# -*- coding: utf-8 -*-"))
  "Association list mapping major mode symbols to file preambles.")

(defun basis/insert-file-preamble ()
  "Insert a mode-specific file \"preamble\"."
  (interactive)
  (let ((start (point))
        (lines (seq-some (lambda (elt)
                           (and (derived-mode-p (car elt))
                                (cdr elt)))
                         basis/file-preamble-lines)))
    (if lines
        (progn (goto-char (point-min))
               (dolist (line lines) (insert line "\n"))
               (insert "\n")
               (unless (eq start (point-min)) (goto-char start)))
      (message "No file preamble defined for `%s'" major-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement

(defun basis/ace-window-kludge (original arg)
  "Advice for `ace-window'.
Ensure it always works with two windows, even when one (or both)
is read-only and empty."
  (if (and (eq aw-scope 'frame)
           (= (length (window-list)) 2))
      (pcase arg
        (4  (basis/transpose-windows 1))
        (16 (delete-other-windows))
        (_  (other-window 1)))
    (funcall original arg)))

(defun basis/imenu-dwim (arg)
  "Invoke `imenu'."
  (interactive "P")
  (call-interactively
   (or (and arg #'imenu-list-minor-mode)
       (and (bound-and-true-p helm-mode) #'helm-imenu)
       #'idomenu)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search

(defun basis/isearch-meta-del-char ()
  "Delete non-matching text or the last character."
  (interactive)
  (let ((pos (isearch-fail-pos)))
    (isearch-del-char (or (and pos (- (length isearch-string) pos))
                          1))))

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
       (progn (setq deactivate-mark t)
              (buffer-substring (region-beginning) (region-end)))
     (or (current-word t) ""))))

(defun basis/occur-beginning-of-buffer ()
  (interactive)
  (goto-char (point-min))
  (occur-next 1))

(defun basis/occur-end-of-buffer ()
  (interactive)
  (goto-char (point-max))
  (occur-prev 1))

(defun basis/occur-dwim (regexp nlines)
  "Like `occur', but use the symbol at point as the default REGEXP."
  (interactive
   (let* ((history nil)
          (default (cond ((use-region-p)
                          (buffer-substring (region-beginning) (region-end)))
                         ((current-word t))
                         (t (setq history 'regexp-history-last)
                            (car regexp-history)))))
     (list (read-regexp (if default
                            (format "Occur (default %s): " default)
                          "Occur: ")
                        default
                        history)
           (prefix-numeric-value current-prefix-arg))))
  (occur regexp nlines))

(defvar basis/occur-show-notes-regexp
  (regexp-opt '("TODO" "DONE" "NOTE" "KLUDGE" "FIXME" "FAIL" "XXX" "???") t)
  "Regexp for `basis/occur-show-notes'.")

(defun basis/occur-show-notes ()
  "Search for common \"TODO\"-style notes."
  (interactive)
  (occur basis/occur-show-notes-regexp))

(defun basis/push-mark-noactivate (&rest _)
  "Push the mark without activating it.
Used as :after advice for `avy-push-mark'."
  (push-mark nil t nil))

(defun basis/swiper-helm ()
  (interactive)
  (let ((helm-move-to-line-cycle-in-source t))
    (call-interactively #'swiper-helm)))

(defmacro basis/with-ivy-window (&rest body)
  ;; Copy of `with-ivy-window' so this file can be compiled without `ivy' loaded
  (declare (indent 0) (debug t))
  `(with-selected-window (ivy--get-window ivy-last)
     ,@body))

(defun basis/swiper-maybe-yank-something ()
  "Conditionally insert the symbol at point.
If the search text is empty, insert the symbol at point where the
search started. Otherwise, call the command the key is bound to
in the global map."
  (interactive)
  (if (string= ivy-text "")
      (when-let ((str (save-window-excursion
                        (basis/with-ivy-window
                          (goto-char swiper--opoint)
                          (current-word t)))))
        (insert str))
    (when-let ((cmd (and (called-interactively-p 'any)
                         (lookup-key global-map (this-command-keys)))))
      (call-interactively cmd))))

(defun basis/grep-use-bash (original &rest args)
  "Advice for `lgrep'.
Invoke grep via bash, since zsh signals an error if there are any
non-matching patterns. See bug #23590."
  (let ((shell-file-name (or (executable-find "bash")
                             shell-file-name)))
    (apply original args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion

(defun basis/describe-function (function)
  "Display the full documentation of FUNCTION."
  (interactive
   (let ((default (function-called-at-point))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Describe function: "
                                    obarray
                                    (lambda (sym)
                                      (or (get sym 'function-documentation)
                                          (fboundp sym)))
                                    t nil nil
                                    (and (symbolp default)
                                         (not (null default))
                                         (symbol-name default)))))))
  (describe-function function))

(defun basis/describe-variable (variable &optional buffer frame)
  "Display the full documentation of VARIABLE."
  (interactive
   (let ((default (variable-at-point))
	 (enable-recursive-minibuffers t))
     (list (intern (completing-read "Describe variable: "
                                    obarray
                                    (lambda (sym)
                                      (or (get sym 'variable-documentation)
                                          (and (boundp sym)
                                               (not (keywordp sym)))))
                                    t nil nil
                                    (and (symbolp default)
                                         (not (keywordp default))
                                         (symbol-name default)))))))
  (describe-variable variable buffer frame))

(defun basis/describe-face (face &optional frame)
  "Display the properties of face FACE on FRAME."
  (interactive
   (let ((default (symbol-at-point))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Describe face: "
                                    obarray #'facep t nil nil
                                    (and (symbolp default)
                                         (symbol-name default)))))))
  (describe-face face frame))

(defun basis/ido-selected-file ()
  "Return the current selection during `ido' file completion.
Return the current directory if no text is entered or there are
no matches."
  (if (or (equal ido-text "")
          (null ido-matches))
      default-directory
    (expand-file-name (car ido-matches))))

(defun basis/ido-open-file-externally-1 (file)
  (interactive (list (basis/ido-selected-file)))
  (basis/open-file-externally file))

(defun basis/ido-open-file-externally ()
  "Open a file externally during `ido' completion."
  (interactive)
  (setq fallback 'basis/ido-open-file-externally-1)
  (setq ido-exit 'fallback)
  (exit-minibuffer))

(defun basis/company-maybe-block-completion (&rest _)
  "Prevent `company-auto-begin' from running in some circumstances."
  ;; Used as `:before-until' advice, so returning non-nil prevents completion.
  (pcase major-mode
    (`python-mode
     (nth 3 (syntax-ppss)))
    (`shell-mode
     (and (eq system-type 'windows-nt)
          (save-excursion
            (skip-chars-backward "^[:blank:]")
            (looking-at-p tramp-file-name-regexp))))
    (`sh-mode
     (save-excursion
       (forward-char -2)
       (looking-at-p "\\_<fi\\_>")))))

(defun basis/company-no-srv-candidates (args)
  "Advice for `company-update-candidates'.
Ignore all potential candidates under \"/srv/\"."
  ;; For use on a particular host, because reasons.
  (if (and (eq major-mode 'shell-mode)
           (let ((elt (caar args)))
             (and (> (length elt) 5)
                  (eq (compare-strings "/srv/" nil nil elt nil 5) t))))
      '(nil)
    args))

(defun basis/maybe-enable-company-clang ()
  "Conditionally enable `company-clang' for the current buffer."
  (when (and (buffer-file-name)
             (not (file-remote-p (buffer-file-name)))
             (bound-and-true-p company-clang-executable))
    (add-to-list 'company-backends #'company-clang)))

(defvar basis/ivy-format-selection-text
  (propertize "> " 'face 'font-lock-function-name-face)
  "Text to place before the selection during `ivy' completion.")

(defun basis/helm-backspace (n)
  "Delete N chars backwards.
If already at the beginning of the field, call
`helm-keyboard-quit'."
  (interactive "*p")
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
  (if (bound-and-true-p helm-alive-p)
      (helm-exit-and-execute-action (lambda (&rest _) (helm-bookmarks)))
    (error "Running helm command outside of context")))

(defun basis/helm-open-file-w32 (file)
  ;; Used as :override advice to `helm-open-file-externally' on Windows
  (w32-shell-execute "open" (expand-file-name file)))

(defun basis/helm-ff-run-magit-status ()
  "Run `magit-status' from `helm-source-find-files'."
  (interactive)
  (if (bound-and-true-p helm-alive-p)
      (helm-exit-and-execute-action #'magit-status)
    (error "Running helm command outside of context")))

(defun basis/insert-file-name (file)
  "Read FILE with `helm' and insert it in the current buffer.
Intended for use in `minibuffer-local-shell-command-map'."
  (interactive (list (helm-read-file-name "File: ")))
  (insert (abbreviate-file-name (if (file-directory-p file)
                                    (file-name-as-directory file)
                                  file))))

(defun basis/helm-pages-get-next-header ()
  "Alternative implementation of `helm-pages-get-next-header'.
Like the above but skip over lines that contain only whitespace
or comment starters."
  (save-restriction
    (save-excursion
      (narrow-to-page)
      (beginning-of-line)
      (let ((regexp (format "^\\(\\s<\\|%c\\|\\s-\\)*$" (elt comment-start 0))))
        (while (and (not (eobp))
                    (looking-at-p regexp))
          (forward-line)))
      (buffer-substring-no-properties (point) (line-end-position)))))

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
  (call-interactively
   (if (use-region-p) #'scheme-send-region #'scheme-send-definition)))

(defun basis/geiser-eval-something ()
  (interactive)
  (call-interactively
   (if (use-region-p) #'geiser-eval-region #'geiser-eval-definition)))

(defun basis/geiser-eval-something-and-go ()
  (interactive)
  (call-interactively (if (use-region-p)
                          #'geiser-eval-region-and-go
                        #'geiser-eval-definition-and-go)))

(defun basis/geiser-expand-something ()
  (interactive)
  (call-interactively
   (if (use-region-p) #'geiser-expand-region #'geiser-expand-last-sexp)))

(defun basis/python-send-something ()
  "Send the active region or the current defun."
  (interactive)
  (call-interactively
   (if (use-region-p) #'python-shell-send-region #'python-shell-send-defun)))

(defun basis/python-nav-backward-sexp (&optional arg)
  "Move backward by one sexp."
  (interactive "^p")
  (python-nav-forward-sexp (- arg)))

(defun basis/jedi-installed-p ()
  "Return non-nil if Python, Jedi, and EPC are installed."
  (condition-case nil
      (with-temp-buffer
        (let ((inhibit-message t)
              (message-log-max nil))
          (zerop (call-process python-shell-interpreter nil nil nil
                               "-c" "import epc; import jedi; exit()"))))
    (file-error nil)))

(defun basis/python-insert-triple-quotes ()
  "Insert Python triple-quotes and move point to the center."
  (interactive)
  (let* ((keys (this-command-keys))
         (char (elt keys (1- (length keys)))))
    (insert-char char 6)
    (forward-char -3)))

(defun basis/python-find-virtual-env (&optional dir)
  (when-let ((dir (or dir (ignore-errors (projectile-project-root))))
             (env (expand-file-name "env" dir)))
    (and (or (file-exists-p (expand-file-name "bin/activate" env))
             (file-exists-p (expand-file-name "Scripts/activate" env)))
         env)))

(defun basis/python-activate-virtual-env (dir)
  "Like `pyvenv-activate' but try to guess the directory."
  (interactive
   (list (read-directory-name
          "Activate virtual environment: "
          (when-let ((root (ignore-errors (projectile-project-root))))
            (or (basis/python-find-virtual-env root)
                root)))))
  (if (eq major-mode 'python-mode)
      (pyvenv-activate dir)
    (error "Not in a python-mode buffer")))

(defun basis/run-python (&optional arg)
  "Run an inferior Python process.
If a (Projectile) project root is found, start the Python process
with it as `default-directory'. If a virtualenv associated with
the project is found, prompt to activate it. However, when called
with a prefix argument, invoke plain `run-python' directly."
  (interactive "P")
  (let (proc)
    (cond ((not (derived-mode-p 'python-mode))
           (user-error "Not in a python-mode buffer"))
          (arg
           (call-interactively #'run-python))
          ((setq proc (python-shell-get-process))
           (pop-to-buffer (process-buffer proc) nil t))
          (t
           (let* ((prj (ignore-errors (projectile-project-root)))
                  (env (basis/python-find-virtual-env prj))
                  (default-directory (or prj default-directory)))
             (when env
               (pyvenv-activate env)
               (message "Activated virtual environment `%s'"
                        (abbreviate-file-name env)))
             (call-interactively #'run-python))))))

(defun basis/python-string-bounds (&optional inside)
  "Return the bounds of the Python string around point.
If INSIDE is non-nil, return the bounds of the area inside the
string delimiters, otherwise return bounds including the
delimiters."
  (save-excursion
    (let (beg end)
      (goto-char (nth 8 (syntax-ppss)))
      (setq beg (save-excursion
                  (when inside (skip-chars-forward "'\""))
                  (point)))
      (goto-char (scan-sexps (point) 1))
      (when inside (skip-chars-backward "'\""))
      (setq end (point))
      (list beg end))))

(defun basis/python-fill-module-docstring (beg end)
  "Fill from BEG to END as a Python module docstring.
This assumes a possibly-idiosyncratic module docstring style."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (basis/python-string-bounds t)))
  (let* ((buf (current-buffer))
         (new (with-temp-buffer
                (setq fill-column 64)
                (insert-buffer-substring buf beg end)
                (indent-rigidly (point-min) (point-max) most-negative-fixnum)
                (goto-char (point-min))
                ;; If there's a header, move over it
                (when (save-excursion
                        (and (> (count-lines (point-min) (point-max)) 3)
                             (progn (forward-line 2)
                                    (looking-at-p "^[ *~_=-]*$"))))
                  (forward-line 3))
                (fill-region (point) (point-max))
                (indent-rigidly (point-min) (point-max) 4)
                (buffer-string))))
    (delete-region beg end)
    (insert new)))

(defun basis/slime-eval-something ()
  "Eval the active region, if any; otherwise eval the toplevel form."
  (interactive)
  (call-interactively
   (if (use-region-p) #'slime-eval-region #'slime-eval-defun)))

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
  (call-interactively
   (if (use-region-p) #'cider-eval-region #'cider-eval-defun-at-point)))

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
  (interactive "*p")
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
  (interactive "*P")
  (if (or (bound-and-true-p multiple-cursors-mode)
          (use-region-p))
      (call-interactively #'backward-delete-char)
    (pcase-let ((`(,spaces ,stuff)
                 (let ((start (point)))
                   (save-excursion
                     (skip-chars-backward "[:blank:]" (line-beginning-position))
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
          (let ((state (syntax-ppss)))
            (unless (or (nth 3 state) (nth 4 state))
              (setq n (- n 1)))))
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
          (let ((state (syntax-ppss)))
            (unless (or (nth 3 state) (nth 4 state))
              (setq n (- n 1)))))
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
                                (looking-at-p "^[[:blank:]]*$")))))
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
          (while (and (looking-at "^[[:blank:]]*$")
                      (not (eobp)))
            (forward-line 1))
          (while (and (not (looking-at "^[[:blank:]]*$"))
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

(defun basis/sql-toggle-column-alias-format ()
  "Toggle a column alias at point between two formats."
  ;; This is obviously far from a general solution but it does manage to be
  ;; useful with the styles I work with most often.
  (interactive)
  (let ((new (cond ((looking-at "\\(\\sw\\|\\s_\\)+")
                    (thread-last (match-string 0)
                      (replace-regexp-in-string "_" " ")
                      (replace-regexp-in-string "\\_<no\\_>" "#")
                      (replace-regexp-in-string "\\_<pct\\_>" "%")
                      (replace-regexp-in-string "\\_<dollars\\_>" "$")
                      (capitalize)
                      (format "\"%s\"")))
                   ((looking-at "\".+\"")
                    (thread-last (match-string 0)
                      (replace-regexp-in-string "\"" "")
                      (replace-regexp-in-string "\\s-" "_")
                      (replace-regexp-in-string "#" "no")
                      (replace-regexp-in-string "%" "pct")
                      (replace-regexp-in-string "\\$" "dollars")
                      (downcase))))))
    (if new
        (replace-match new)
      (user-error "No column name at point"))))

(defun basis/sql-modify-syntax-table ()
  "Modify a couple syntax table entries for SQL.
Make double quote a string delimiter and period a symbol
constituent."
  (let ((table (make-syntax-table (syntax-table))))
    (modify-syntax-entry ?\" "\"\"  " table)
    (modify-syntax-entry ?. "_   " table)
    (set-syntax-table table)))

(defun basis/sql-after-highlight-product (&rest _)
  "Advice for `sql-highlight-product'."
  (basis/sql-modify-syntax-table)
  (when (bound-and-true-p whitespace-mode)
    ;; Re-enable `whitespace-mode'. Otherwise it will be "on" but have no effect
    (whitespace-turn-on)))

(defun basis/c-backspace (&rest args)
  "Smartparens-aware value for `c-backspace-function'."
  (interactive)
  (apply (if (bound-and-true-p smartparens-mode)
             #'sp-backward-delete-char
           #'backward-delete-char-untabify)
         args))

(defun basis/c-delete (&rest args)
  "Smartparens-aware value for `c-delete-function'."
  (interactive)
  (apply (if (bound-and-true-p smartparens-mode)
             #'sp-delete-char
           #'delete-char)
         args))

(eval-when-compile (require 'cc-engine))

(defun basis/c-align-macro-slashes (spacing)
  "Align the line-continuation backslashes of a C macro."
  (interactive "*p")
  (let ((beg (c-save-buffer-state ()
               (c-beginning-of-macro)
               (point)))
        (end (c-save-buffer-state ()
               (c-end-of-macro)
               (point))))
    (align-regexp beg end "\\(\\s-*\\)\\\\" 1 spacing)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text, markup, and configuration modes

(defvar basis/org-todo-keyword-regexp nil ; Initialized in init.el
  "Regexp matching org TODO keywords.")

(defun basis/org-maybe-beginning-of-todo-keyword (original &rest args)
  "Advice for `org-beginning-of-line'.
When `org-special-ctrl-a/e' is non-nil, include the beginning of
TODO keywords in the positions cycled between."
  (let ((start (point)))
    (unless (and org-special-ctrl-a/e
                 (progn (skip-chars-backward " ")
                        (skip-chars-backward "[A-Z]")
                        (looking-at basis/org-todo-keyword-regexp)))
      (goto-char start)
      (apply original args))))

(defun basis/sgml-delete-tag-reindent (&rest _)
  "Advice for `sgml-delete-region' to reindent the buffer."
  (indent-region (point-min) (point-max)))

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
        (one (< end (progn (goto-char beg) (forward-line 1) (point)))))
    (goto-char beg)
    (insert "<" tag ">")
    (unless one (newline-and-indent))
    (goto-char end)
    (unless one (newline-and-indent))
    (insert "</" tag ">")
    (indent-region beg end)
    (goto-char beg)
    (set-marker beg nil)
    (set-marker end nil)
    (setq deactivate-mark t)))

(defun basis/html-newline-and-indent ()
  (interactive)
  (if (and (eq (char-before) ?>) (eq (char-after) ?<))
      (tagedit-toggle-multiline-tag)
    (newline-and-indent)))

(defun basis/html-multiline-expand ()
  (interactive)
  (simplezen-expand)
  (basis/html-newline-and-indent))

(defun basis/tagedit-toggle-multiline-maybe-forward (original &rest args)
  "Advice for `tagedit-toggle-multiline-tag'.
Move forward by a line and indent if invoked directly between."
  (let ((forward (and (eq (char-before) ?>) (eq (char-after) ?<))))
    (apply original args)
    (when forward
      (forward-line 1)
      (indent-according-to-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Brackets

(defun basis/paredit-doublequote-space-p (endp delimiter)
  ;; For use as a member of `paredit-space-for-delimiter-predicates'
  (not (and (not endp)
            (eq delimiter ?\")
            (derived-mode-p 'lisp-mode 'common-lisp-mode 'inferior-lisp-mode
                            'slime-repl-mode)
            (let ((point (point)))
              (and (eq (char-before point) ?p)
                   (eq (char-before (1- point)) ?#))))))

(defun basis/paredit-splicing-unquote-p (endp delimiter)
  ;; For use as a member of `paredit-space-for-delimiter-predicates'
  (not (and (not endp)
            (eq delimiter ?\()
            (derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode
                            'lisp-mode 'common-lisp-mode 'inferior-lisp-mode
                            'slime-repl-mode)
            (let ((point (point)))
              (and (eq (char-before point) ?@)
                   (eq (char-before (1- point)) ?,))))))

(defun basis/paredit-open-something ()
  (interactive)
  (call-interactively
   (if (memq major-mode '(clojure-mode cider-repl-mode))
       #'paredit-open-square
     #'paredit-open-round)))

(defun basis/paredit-kill-something ()
  (interactive)
  (call-interactively
   (if (use-region-p) #'kill-region #'paredit-backward-kill-word)))

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
  (cons (prefix-numeric-value (car args))
        (cdr args)))

(defun basis/sp-kill-something ()
  "Call `sp-backward-kill-word' or `kill-region'. "
  (interactive)
  (call-interactively
   (if (use-region-p) #'kill-region #'sp-backward-kill-word)))

(defun basis/sp-kill-sexp (&optional arg)
  "Variant of `sp-kill-sexp'.
If called inside a symbol, only kill to the end of the
symbol (like `kill-sexp')."
  (interactive "P")
  (if (and (not arg)
           (looking-at-p "\\sw\\|\\s_")
           (not (looking-at-p "\\_<")))
      (kill-sexp 1)
    (sp-kill-sexp arg)))

(defmacro basis/def-sp-backspace-command (name command)
  (declare (indent defun))
  (let ((doc (ignore-errors (documentation (eval command) t))))
    `(progn
       (defun ,name ()
         ,@(and doc (list doc))
         (interactive)
         (cl-letf (((symbol-function 'backward-delete-char)
                    #'sp-backward-delete-char)
                   ((symbol-function 'backward-delete-char-untabify)
                    #'sp-backward-delete-char))
           (call-interactively ,command)))
       (put ',name 'delete-selection 'supersede)
       ',name)))

(basis/def-sp-backspace-command basis/sp-python-backspace
  #'python-indent-dedent-line-backspace)

(basis/def-sp-backspace-command basis/sp-yaml-backspace
  #'yaml-electric-backspace)

(basis/def-sp-backspace-command basis/sp-markdown-backspace
  #'markdown-exdent-or-delete)

(defun basis/sp-comint-delchar-or-maybe-eof (arg)
  "Delete ARG characters or send an EOF to subprocess."
  ;; Copy the code of `comint-delchar-or-maybe-eof' rather than using advice
  ;; and/or `cl-letf' to avoid recursion errors.
  (interactive "p")
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and (eobp) proc (= (point) (marker-position (process-mark proc))))
	(comint-send-eof)
      (sp-delete-char arg))))

(put 'basis/sp-comint-delchar-or-maybe-eof 'delete-selection 'supersede)

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
  "Augmented version of `sp-point-after-word-p'.
Handle the special string literals in Python and SQL."
  (and (sp-point-after-word-p id action context)
       (let ((regexp (pcase major-mode
                       ((or `python-mode `inferior-python-mode)
                        "\\_<[BbRrUu]")
                       ((or `sql-mode `sql-interactive-mode)
                        "\\_<[BbNnXx]"))))
         (or (null regexp) ; No change for this mode
             (not (and (member id '("'" "\""))
                       (save-excursion (forward-char -2)
                                       (looking-at-p regexp))))))))

(defvar basis/sp-inhibit-cleanup-list
  '(indent-relative
    indent-relative-maybe
    python-indent-line-function
    haskell-indentation-indent-line)
  "Indentation functions for which to inhibit smartparens's cleanup.")

(defun basis/sp-unwrap-no-cleanup (args)
  "Advice for `sp--unwrap-sexp' to inhibit problematic cleanup."
  (if (memq indent-line-function basis/sp-inhibit-cleanup-list)
      (list (car args) t)
    args))

(defun basis/sp-cleanup-maybe-not (&rest _)
  "Advice for `sp--cleanup-after-kill' to inhibit problematic cleanup."
  ;; Used as `:before-until' advice, so returning non-nil prevents cleanup
  (memq indent-line-function basis/sp-inhibit-cleanup-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error checking

(defun basis/maybe-enable-flycheck ()
  "Enable `flycheck-mode', except for remote files."
  (unless (and buffer-file-name (file-remote-p buffer-file-name))
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
             (not (and buffer-file-name (file-remote-p buffer-file-name))))
    (flyspell-mode)))

(defun basis/maybe-enable-flyspell-prog-mode ()
  "Enable `flyspell-prog-mode', except for remote files."
  (require 'ispell)
  (when (and ispell-program-name
             (not (and buffer-file-name (file-remote-p buffer-file-name))))
    (flyspell-prog-mode)))

(defun basis/maybe-enable-whitespace-mode ()
  "Enable `whitespace-mode' in programming modes (but not REPLs)."
  (interactive)
  (unless (derived-mode-p 'comint-mode 'cider-repl-mode 'eshell-mode)
    (whitespace-mode)))

(defun basis/maybe-enable-bug-reference-mode ()
  (require 'bug-reference)
  (when (stringp bug-reference-url-format)
    (if (derived-mode-p 'prog-mode)
        (bug-reference-prog-mode)
      (bug-reference-mode))))

(defun basis/ispell-init-process (original &rest args)
  "Advice for `ispell-init-process' on Cygwin.
Let-bind `ispell-current-personal-dictionary' to a
Cygwin-friendly name so that the personal dictionary works."
  (let ((ispell-current-personal-dictionary
         (basis/cygwinize-file-name ispell-current-personal-dictionary)))
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

(defun basis/ediff-save-window-config (&rest _)
  "Advice for `ediff-setup'.
Save the current window configuration to register
`:ediff-restore-windows', so that it can be restored on exit."
  (window-configuration-to-register :ediff-restore-windows))

(defun basis/ediff-quit-restore (&rest _)
  "Advice for `ediff-quit'.
After quitting, restore the previous window configuration."
  (with-demoted-errors "Couldn't restore window configuration: %S"
    (jump-to-register :ediff-restore-windows)))

(defun basis/ediff-files (file1 file2)
  "Use `ediff' to compare FILE1 and FILE2."
  (interactive
   (if (eq major-mode 'dired-mode)
       (let ((marked (dired-get-marked-files)))
         (pcase (length marked)
           (1 (cons (read-file-name "File: " (dired-dwim-target-directory))
                    marked))
           (2 marked)
           (_ (error "Either one or two files should be marked"))))
     (let* ((file1 (read-file-name "File 1: " nil nil t))
            (file2 (read-file-name "File 2: "
                                   (file-name-directory file1)
                                   nil t)))
       (list file1 file2))))
  (if (file-newer-than-file-p file1 file2)
      (ediff-files file2 file1)
    (ediff-files file1 file2)))


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

(defun basis/magit-cygwin-save-repository-buffers (&optional arg)
  "Alternative `magit-save-repository-buffers'.
Use `expand-file-name' to canonicalize file names to Emacs's
representation before comparing them."
  (interactive "P")
  (when-let ((topdir (magit-rev-parse-safe "--show-toplevel"))
             (topdir (expand-file-name topdir)))
    (let ((remote (file-remote-p topdir)))
      (save-some-buffers
       arg
       (lambda ()
         (and buffer-file-name
              ;; Avoid needlessly connecting to unrelated remotes.
              (equal (file-remote-p buffer-file-name) remote)
              (string-prefix-p topdir (file-truename buffer-file-name))
              (equal (when-let ((dir (magit-rev-parse-safe "--show-toplevel"))
                                (dir (expand-file-name dir)))
                       dir)
                     topdir)))))))

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

(defun basis/magit-show-commit (hash project-directory)
  "Show the commit with HASH in PROJECT-DIRECTORY.
When called interactively, HASH defaults to the hash at point (if
any) and PROJECT-DIRECTORY defaults to the current `projectile'
project."
  (interactive
   (list (if (save-excursion (skip-chars-backward "[:xdigit:]")
                             (looking-at "\\<\\([[:xdigit:]]\\{4,40\\}\\)\\>"))
             (match-string-no-properties 1)
           (user-error "No commit hash at point"))
         (if (or current-prefix-arg (not (projectile-project-p)))
             (completing-read "Project: " (projectile-relevant-known-projects))
           (projectile-project-root))))
  (let ((default-directory project-directory))
    (magit-show-commit hash (car (magit-diff-arguments)))))

(defun basis/magit-shorten-hash (n)
  "Shorten the hash at point to N characters.
N must be between 4 and 40 and defaults to the result of calling
`magit-abbrev-length'."
  (interactive (list (cond (current-prefix-arg
                            (prefix-numeric-value current-prefix-arg))
                           ((fboundp 'magit-abbrev-length)
                            (magit-abbrev-length))
                           (t 7))))
  (unless (<= 4 n 40)
    (user-error "Hash must be between 4 and 40 characters"))
  (save-excursion
    (skip-chars-backward "[:xdigit:]")
    (if (looking-at "\\<\\([[:xdigit:]]\\{4,40\\}\\)\\>")
        (let* ((val (match-string 1))
               (len (length val)))
          (if (> len n)
              (replace-match (substring val 0 n) 'fixedcase)
            (user-error "Desired hash length is not less than current")))
      (user-error "No hash at point"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Processes and shells

(defun basis/default-program-for-file (_)
  ;; TODO: Use `mailcap'?
  (if (memq system-type '(darwin windows-nt))
      "open"
    "xdg-open"))

(defun basis/open-file-externally-1 (program file)
  "Use PROGRAM to open FILE externally."
  (let ((file (expand-file-name file)))
    (cond ((eq system-type 'windows-nt)
           (w32-shell-execute program file))
          ((member program '("xdg-open" "gvfs-open" "gnome-open"))
           (call-process program nil 0 nil file))
          (t
           (start-process "*open externally*" nil program file)))))

(defun basis/open-file-externally (file)
  "Open FILE in an external application."
  (interactive (list (or (and (eq major-mode 'dired-mode)
                              (ignore-errors (dired-get-file-for-visit)))
                         (read-file-name "Open externally: " nil nil t))))
  (if-let ((exe (basis/default-program-for-file file)))
      (basis/open-file-externally-1 exe file)
    (error "No external program defined for `%s'" (abbreviate-file-name file))))

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

(defun basis/comint-input-goto-bottom-if-necessary (&rest _)
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

(defun basis/find-clang-program ()
  "Return the clang program to be used by `company-clang'."
  (or (executable-find "clang")
      (let ((regexp "\\`clang-\\([0-9.]+\\)\\(alpha\\|beta\\|pre\\)?\\'")
            (clangs ()))
        (dolist (dir exec-path)
          (when (file-directory-p dir)
            (dolist (file (directory-files dir t))
              (when (and (file-executable-p file)
                         (string-match regexp file))
                (push (cons file (concat (match-string 1 file)
                                         (match-string 2 file)))
                      clangs)))))
        (caar (sort (nreverse clangs)   ; Maintain relative order from exec-path
                    (lambda (a b) (version< (cdr b) (cdr a))))))))

(defun basis/find-clang-includes-path (language)
  "Return clang's #include <...> search path."
  (unless (memq language '(c c++))
    (error "Unknown language `%s'" language))
  (let ((clang (or (and (boundp 'company-clang-executable)
                        (symbol-value 'company-clang-executable))
                   (basis/find-clang-program)
                   (error "Clang executable not found")))
        (path ()))
    (with-temp-buffer
      (call-process clang nil t nil
                    "-E" (if (eq language 'c++) "-xc++" "-xc") "-" "-v")
      (goto-char (point-min))
      (re-search-forward "^#include <\\.\\.\\.> search starts here:$")
      (forward-line 1)
      (while (not (looking-at "^End of search list\\.$"))
        (skip-chars-forward "[:blank:]")
        (let* ((str (buffer-substring (point) (line-end-position)))
               (dir (expand-file-name str)))
          (when (and (file-directory-p dir)
                     (not (member dir path)))
            (push dir path)))
        (forward-line 1))
      (nreverse path))))

(defun basis/build-clang-args (language)
  (unless (memq language '(c c++))
    (error "Unknown language `%s'" language))
  (when-let ((std (if (eq language 'c++) "c++11" "c11"))
             (inc (ignore-errors (basis/find-clang-includes-path language))))
    (cons (format "-std=%s" std) inc)))

(defun basis/eshell-cygwin-path-env ()
  (setq eshell-path-env (replace-regexp-in-string ":" ";" eshell-path-env)))

(defvar basis/ido-man-topics nil)

(defun basis/ido-man-init (&optional arg)
  (require 'woman)
  (when (or arg (null basis/ido-man-topics))
    (setq woman-expanded-directory-path
          (woman-expand-directory-path woman-manpath woman-path))
    (setq woman-topic-all-completions
          (woman-topic-all-completions woman-expanded-directory-path))
    (setq basis/ido-man-topics (mapcar #'car woman-topic-all-completions))))

(defun basis/read-manual-page-file-name ()
  (let* ((topic (completing-read "Manual entry: " basis/ido-man-topics nil t))
         (files (mapcar #'car (woman-file-name-all-completions topic))))
    (if (cdr files)
        (completing-read "Manual file: " files nil t)
      (car files))))

(defun basis/ido-man (arg)
  "Select a manual page via `ido' and show it in a buffer.
If ARG is non-nil, reinitialize the cache of topics."
  (interactive "P")
  (basis/ido-man-init arg)
  (manual-entry (concat "-l " (basis/read-manual-page-file-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File utilities

(defun basis/rename-buffer-file (buffer destination)
  "Rename BUFFER and the file it's visiting to DESTINATION."
  (interactive
   (list (current-buffer)
         (substring-no-properties (read-file-name "Destination: "))))
  (with-current-buffer buffer
    (when (and (buffer-modified-p)
               (y-or-n-p (format-message "Buffer `%s' modified; save?"
                                         (current-buffer))))
      (save-buffer))
    (let ((src (buffer-file-name)))
      (unless (and src (file-exists-p src))
        (user-error "Buffer `%s' is not visiting a file" (buffer-name)))
      (pcase-let ((`(,dst . ,dir)
                   (if (or (file-directory-p destination)
                           (directory-name-p destination))
                       (cons (expand-file-name (file-name-nondirectory src)
                                               destination)
                             destination)
                     (cons destination (file-name-directory destination)))))
        (unless (file-directory-p dir)
          (if (y-or-n-p (format-message
                         "Directory `%s' does not exist; create it?"
                         dir))
              (make-directory dir t)
            (user-error "Directory `%s' does not exist" dir)))
        (rename-file src dst 1)
        (set-visited-file-name dst)
        (set-buffer-modified-p nil)
        (message "File `%s' renamed to `%s'"
                 (file-name-nondirectory src)
                 (if (file-in-directory-p dst (file-name-directory src))
                     (file-relative-name dst (file-name-directory src))
                   (abbreviate-file-name dst)))))))

(defun basis/delete-buffer-file (buffer)
  "Kill BUFFER and delete the file it's visiting."
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (if-let ((file (buffer-file-name))
             (abbr (abbreviate-file-name file)))
        (when (y-or-n-p (format-message "Delete file `%s'?" abbr))
          (delete-file file)
          (kill-buffer buffer)
          (message "File `%s' deleted" abbr))
      (when (or (zerop (buffer-size))
                (y-or-n-p (format-message
                           "Buffer `%s' is not visiting a file; kill it?"
                           (buffer-name))))
        (kill-buffer)))))

(defun basis/find-file-recentf ()
  "Find recently open files using ido and recentf."
  (interactive)
  (find-file (completing-read "Recent file: "
                              (mapcar #'abbreviate-file-name recentf-list)
                              nil t)))

(defun basis/cygwinize-file-name (name)
  "Convert NAME for use with Cygwin."
  ;; This is obviously not a general solution
  (if (string-match "\\`\\([A-Za-z]\\):\\(/\\|\\\\\\)" name)
      (let* ((drive (match-string 1 name))
             (sep   (match-string 2 name))
             (more  (substring name (match-end 2))))
        (concat (if (member drive '("c" "C")) "" (concat "/" drive))
                "/"
                (if (equal sep "\\")
                    (replace-regexp-in-string "\\\\" "/" more)
                  more)))
    name))

(defun basis/cygwin-shell-quote-argument (args)
  "Advice for `shell-quote-argument' on machines with Cygwin.
Quote file names appropriately for POSIX-like shells."
  ;; Used as :filter-args advice
  (let* ((arg (car args))
         (new (basis/cygwinize-file-name arg)))
    (if (eq new arg)
        args
      (list new))))

(defun basis/read-file (file)
  "Read a Lisp form from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun basis/dired-beginning-of-buffer ()
  (interactive)
  (goto-char (point-min))
  (while (not (ignore-errors (dired-get-filename)))
    (dired-next-line 1)))

(defun basis/dired-end-of-buffer ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun basis/insert-files (files buffer &optional interactive)
  "Insert the contents of FILES into BUFFER."
  (interactive
   (let ((default-directory (read-directory-name "Directory: ")))
     (list (file-expand-wildcards (read-string "Wildcard: ") t)
           (current-buffer)
           t)))
  (with-current-buffer (or buffer (current-buffer))
    (mapc #'insert-file-contents files))
  (when interactive
    (message "Inserted %d files:\n%s"
             (length files)
             (mapconcat #'abbreviate-file-name files "\n"))))

(defun basis/dired-slurp-files (files buffer)
  "Insert the contents of marked FILES into BUFFER."
  (interactive (list (if (eq major-mode 'dired-mode)
                         (dired-get-marked-files)
                       (error "Buffer not in `dired-mode'"))
                     (read-buffer "Destination buffer: ")))
  (let ((buffer (get-buffer-create buffer)))
    (basis/insert-files files buffer)
    (pop-to-buffer buffer)))

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
                   (mapcar #'basis/cygwinize-file-name (cons destination files))
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
   (let* ((def (thing-at-point 'url t))
          (url (if def
                   (read-string (format "URL (default %s): " def)
                                nil nil
                                def)
                 (read-string "URL: ")))
          (name (thread-first url
                  url-generic-parse-url
                  url-path-and-query
                  car
                  file-name-nondirectory)))
     (list url (read-file-name "Destination: " nil name) current-prefix-arg)))
  (url-copy-file url destination 0)
  (when visit (find-file destination)))

(defvar basis/xdg-user-dirs nil
  "Association list of xdg user directory keys and values.")

(defun basis/xdg-user-dir (name)
  "Look up the file name for XDG user directory NAME."
  (or (cdr (assq name basis/xdg-user-dirs))
      (when (executable-find "xdg-user-dir")
        (let* ((str (upcase (symbol-name name)))
               (dir (with-temp-buffer
                      (call-process "xdg-user-dir" nil t nil str)
                      (goto-char (point-min))
                      (skip-chars-forward "[:blank:]")
                      (buffer-substring (point) (line-end-position)))))
          ;; xdg-user-dir returns the user's home directory for non-existing
          ;; NAME values, so convert that to nil
          (unless (or (equal dir "")
                      (file-equal-p dir (expand-file-name "~")))
            (push (cons name dir) basis/xdg-user-dirs)
            dir)))))

(defun basis/apply-patch (beg end dir)
  "Apply the patch between BEG and END in DIR."
  (interactive
   (pcase-let ((`(,beg ,end) (if (use-region-p)
                                 (list (region-beginning) (region-end))
                               (list (point-min) (point-max)))))
     (list beg end (read-directory-name "Directory: "))))
  (let ((cmd (format "patch -d %s -p1" (shell-quote-argument dir))))
    (shell-command-on-region beg end cmd)))

(defun basis/ffap-more-file-at-point (original &rest args)
  (or (apply original args)
      (ffap-file-exists-string
       (save-excursion
         (skip-chars-backward "[:alnum:][:punct:]")
         (buffer-substring-no-properties
          (point)
          (progn (skip-chars-forward "[:alnum:][:punct:]")
                 (point)))))
      (and (nth 3 (syntax-ppss))
           (ignore-errors
             (ffap-file-exists-string
              (save-excursion
                (goto-char (nth 8 (syntax-ppss)))
                (buffer-substring-no-properties
                 (1+ (point))
                 (1- (progn (forward-sexp 1) (point))))))))))


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
       ;; there's no good way to "cygwinize" the tags file name
       (let* ((default-directory root)
              (tags-file (expand-file-name projectile-tags-file-name))
              (command (format projectile-tags-command
                               (basis/cygwinize-file-name tags-file)
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

(defun basis/ibuffer-beginning-of-buffer ()
  (interactive)
  (goto-char (point-min))
  (ibuffer-forward-line 1))

(defun basis/ibuffer-end-of-buffer ()
  (interactive)
  (goto-char (point-max))
  (ibuffer-backward-line 1))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous

(defun basis/read-choice (prompt choices &optional must-match)
  "Prompt to select one of CHOICES and return the result.
CHOICES is a list of (KEY DESCRIPTION). Each KEY can be any value
supported by `single-key-description'; each DESCRIPTION can be
any printable value."
  (let ((cursor-in-echo-area t)
        (prompt (if (or (not prompt)
                        (get-text-property 0 'face prompt))
                    prompt
                  (propertize prompt 'face 'minibuffer-prompt)))
        key elt)
    (save-window-excursion
      (with-current-buffer (get-buffer-create " *Read choice*")
        (erase-buffer)
        (fundamental-mode)
        (setq cursor-type nil)
        (pcase-dolist (`(,key ,description) choices)
          (let ((str (propertize (single-key-description key)
                                 'face 'font-lock-variable-name-face)))
            (insert str " " description "\n")))
        (align-regexp (point-min) (point-max) "\\(\\s-*\\) ")
        (goto-char (point-min))
        (pop-to-buffer (current-buffer) t t)
        (fit-window-to-buffer)
        (while (and (null (progn (setq key (read-key prompt))
                                 (setq elt (assq key choices))))
                    must-match)
          (if (memq key '(?\C-g ?\C-\[))
              (keyboard-quit)
            (let (cursor-in-echo-area)
              (message "Invalid selection: %s" (single-key-description key))
              (sit-for 0.5))))
        elt))))

(defun basis/google (string)
  "Run a Google search for STRING.
Use the active region or symbol at point, if any, as the default
search term."
  (interactive
   (let ((default (if (use-region-p)
                      (buffer-substring (region-beginning) (region-end))
                    (current-word t))))
     (list (read-string (if default
                            (format "Google (default %s): " default)
                          "Google: ")
                        nil nil
                        default))))
  (browse-url
   (concat "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
           (url-hexify-string string))))

(defun basis/goto-line-with-numbers ()
  "Invoke `goto-line' with `nlinum-mode' temporarily enabled."
  (interactive)
  (if (bound-and-true-p nlinum-mode)
      (call-interactively #'goto-line)
    (unwind-protect (progn (nlinum-mode 1)
                           (call-interactively #'goto-line))
      (nlinum-mode -1))))

(defun basis/libxml-available-p ()
  "Return non-nil if libxml is available."
  (and (fboundp 'libxml-parse-html-region)
       (with-temp-buffer
         (insert "<html></html>")
         (not (null (libxml-parse-html-region (point-min) (point-max)))))))

(defun basis/looking-back-p (regexp &optional limit)
  "Same as `looking-back' but don't change the match data."
  (save-match-data (looking-back regexp limit)))

(defun basis/kill-buffer (&optional buffer)
  "Kill BUFFER.
Like `kill-buffer' but, when called interactively, kill the
current buffer without pompting, unless called with a prefix
argument."
  (interactive (list (and current-prefix-arg
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

(defun basis/measure-time* (fn)
  "Return the execution time in seconds of calling FN."
  (let* ((fn (byte-compile fn))
         (t0 (progn (garbage-collect)
                    (float-time))))
    (funcall fn)
    (/ (truncate (* (- (float-time (current-time))
                       t0)
                    10000))
       10000.0)))

(defmacro basis/measure-time (&rest body)
  "Return the execution time in seconds of evaluating BODY."
  (declare (indent defun))
  `(basis/measure-time* (lambda () ,@body)))

(defun basis/toggle-echo-area-messages (arg)
  "Toggle whether the \"*Messages*\" buffer is shown."
  (interactive "P")
  (let ((buffer (messages-buffer)))
    (if-let ((window (get-buffer-window buffer 'visible)))
        (if (eq window (selected-window))
            (bury-buffer)
          (delete-window window))
      (with-current-buffer buffer
        (goto-char (point-max))
        (if arg
            (pop-to-buffer-same-window (current-buffer) t)
          (display-buffer (current-buffer)))))))

(defun basis/emacs-Q (emacs home args)
  "Run \"EMACS -Q ARGS\" with its home in HOME.
EMACS defaults to the current Emacs executable. HOME defaults to
\"/tmp\"."
  ;; This is only really useful on Windows, where I use a Cygwin shell but a
  ;; native Windows Emacs, so `emacs -Q' in my shell doesn't cut it.
  (interactive (list nil nil nil))
  (let* ((emacs (or emacs (expand-file-name (if (eq system-type 'windows-nt)
                                                "runemacs.exe"
                                              invocation-name)
                                            invocation-directory)))
         (home (or home (if (eq system-type 'windows-nt)
                            "e:\\tmp"
                          "/tmp")))
         (penv (or basis/pre-cygwin-process-environment
                   process-environment))
         (process-environment (cons (concat "HOME=" home) penv)))
    (apply #'start-process "emacs-Q" nil (expand-file-name emacs)
           "-Q" "--eval" "(setq debug-on-error t)"
           args)))

(defun basis/kill-scratch-query-function ()
  ;; For use as a member of `kill-buffer-query-functions'
  (if (equal (buffer-name) "*scratch*")
      (y-or-n-p "Really kill the *scratch* buffer?")
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Applications

(defun basis/browse-url-url-at-point-https (original &rest args)
  "Advice for `browse-url-url-at-point'. Return HTTPS URLs."
  (let ((result (apply original args)))
    (if (stringp result)
        (replace-regexp-in-string "\\`http://" "https://" result t t)
      result)))

(defun basis/elfeed-parse-group (group)
  "Parse the feed and tag specification GROUP.
GROUP should be a list whose car contains a tag (a symbol) or a
list of tags and whose cdr is a list of feeds to associate with
those tags."
  (pcase-let* ((`(,tag . ,feeds) group)
               (tags (if (listp tag) tag (list tag))))
    (mapcar (lambda (feed) (cons feed tags))
            feeds)))

(defun basis/elfeed-load-feeds (file)
  "Load feeds FILE. Return a list formatted for `elfeed-feeds'."
  (seq-mapcat #'basis/elfeed-parse-group (basis/read-file file)))

(defun basis/define-word (word)
  (interactive
   (let ((default (if (use-region-p)
                      (buffer-substring (region-beginning) (region-end))
                    (current-word nil t))))
     (list (read-string (if default
                            (format "Define word (default %s): " default)
                          "Define word: ")
                        nil nil
                        default))))
  (define-word word))

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

(defun basis/mu4e-action-view-in-eww (msg)
  "View mu4e MSG with `eww'."
  (let ((browse-url-browser-function #'eww-browse-url))
    (mu4e-action-view-in-browser msg)))

(defun basis/compose-message (arg)
  "Jump to a `message-mode' buffer."
  (interactive "P")
  (let ((name "*unsent message*"))
    (pop-to-buffer-same-window
     (or (and (not arg) (get-buffer name))
         (with-current-buffer (generate-new-buffer name)
           (message-mode)
           (current-buffer))))))

(defun basis/mml-attach-at-eob (original &rest args)
  "Advice for `mml-attach-file' et al.
Always add attachments at the end of the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (apply original args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lorem ipsum

(defvar basis/lorem-ipsum-file (basis/emacs-file "lorem-ipsum.txt")
  "File containing \"lorem ipsum\" placeholder text.")

(defun basis/insert-lorem-ipsum (&optional arg)
  "Insert ARG paragraphs of \"lorem ipsum\" text at point."
  (interactive "*p")
  (let ((buffer (get-buffer-create " *Lorem Ipsum*"))
        beg end)
    (with-current-buffer buffer
      (when (zerop (buffer-size))
        (insert-file-contents basis/lorem-ipsum-file))
      (goto-char (point-min))
      (setq beg (point))
      (forward-paragraph arg)
      (setq end (point)))
    (let ((pos (point)))
      (insert-buffer-substring buffer beg end)
      (when auto-fill-function
        (fill-region pos (point))))))


;;; defuns.el ends here
