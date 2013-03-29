;; -----------------------------------------------------------------------------
;; init-utils.el
;; -----------------------------------------------------------------------------

;; macro utilities -------------------------------------------------------------

(defun macroexpand-sexp-at-point ()
  "Macroexpand the s-expression at point."
  (macroexpand (sexp-at-point)))

(defun macroexpand-point (sexp)
  "Macroexpand the s-expression at point.
Print the result to a temp buffer."
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp))))

(defmacro with-unique-names (names &rest body)
  "Create unique names for use in a macro definition.
This idea also goes by the name `with-gensyms` in Common Lisp."
  (declare (indent defun))
  (let ((make-varlist (lambda (syms)
                        (loop for s in syms
                           collect `(,s (make-symbol (symbol-name ',s)))))))
    `(let ,(funcall make-varlist names)
       ,@body)))

;; editing ---------------------------------------------------------------------

(defun beginning-of-line-or-indentation ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (move-beginning-of-line nil)
      (back-to-indentation)))

(defun kill-region-or-backward-word ()
  ;; from github.com/magnars/.emacs.d
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))

;; miscellaneous ---------------------------------------------------------------

(defun basis/google ()
  "Run a Google search.
Use the selected region as the search string if any, otherwise
display a prompt."
  ;; from emacsredux.com/blog/2013/03/28/google/
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string
     (if mark-active
         (buffer-substring (region-beginning) (region-end))
         (read-string "Google: "))))))

;; mark commands ---------------------------------------------------------------

(defun push-mark-no-activate ()
  "Pushes `point` to `mark-ring` without activating the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled."
  ;; from masteringemacs.com
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring` order.
This is the same as using \\[set-mark-command] with the prefix argument."
  ;; from masteringemacs.com
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  ;; from masteringemacs.com
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map
    [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; window commands -------------------------------------------------------------

(defun transpose-windows  (arg)
  "Transpose the relative positions of two or more windows."
  ;; From whattheemacsd.com
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
  ;; From whattheemacsd.com
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

;; ido directory selector ------------------------------------------------------

(defun insert/sorted (lst item &rest args)
  "Insert `item` into sorted list `lst`.
Key and comparison functions can optionally be specified as
quasi-keyword arguments (`key` and `cmp` respectively)."
  (let ((key (or (getf args :key) #'identity))
        (cmp (or (getf args :cmp) #'<)))
    (labels ((ins? (x y)
               (funcall cmp
                        (funcall key x)
                        (funcall key y)))
             (ins (lst)
               (cond ((null lst) (cons item nil))
                     ((ins? item (car lst)) (cons item lst))
                     (t (cons (car lst) (ins (cdr lst)))))))
      (ins lst))))

(defmacro push/sorted (place item &rest args)
  "Insert `item` into the sorted list stored at `place`.
Equivalent to (setq place (insert/sorted place item))."
  `(setq ,place (funcall #'insert/sorted ,place ,item ,@args)))

(defmacro basis/ido-in-dir-for (dir)
  "Return a `lambda` form calling `ido-find-file-in-dir`."
  `#'(lambda () (ido-find-file-in-dir ,dir)))

(defmacro basis/dired-in-dir-for (dir)
  "Return a `lambda` form calling `dired`."
  `#'(lambda () (dired ,dir)))

;; TODO: abstract out the creation of these tables
(defvar basis/ido-dir-methods
  `((" " ido-find-file)
    ("c" ,(basis/ido-in-dir-for "~/code/"))
    ("d" ,(basis/ido-in-dir-for "~/dotfiles/"))
    ("D" ,(basis/ido-in-dir-for "~/Dropbox/"))
    ("e" ,(basis/ido-in-dir-for "~/.emacs.d/"))
    ("h" ,(basis/ido-in-dir-for "~/"))
    ("q" (lambda () (throw 'quit t))))
  "Directories to make available via `basis/ido-dir-selector`.")

(defvar basis/dired-dir-methods
  `((" " dired)
    ("c" ,(basis/dired-in-dir-for "~/code/"))
    ("d" ,(basis/dired-in-dir-for "~/dotfiles/"))
    ("D" ,(basis/dired-in-dir-for "~/Dropbox/"))
    ("e" ,(basis/dired-in-dir-for "~/.emacs.d/"))
    ("h" ,(basis/dired-in-dir-for "~/"))
    ("q" (lambda () (throw 'quit t))))
  "Directories to make available via `basis/dired-dir-selector`.")

(defun basis/selector (method-table)
  "A simple selector, heavily inspired by `slime-selector`.
Parameter `method-table` should be an alist mapping
single-character strings to functions."
  (message "Select [%s]: "
           (apply #'concat
                  (remove " " (mapcar #'car method-table))))
  (let* ((str (save-window-excursion
                (select-window (minibuffer-window))
                (string (read-char))))
         (record (assoc str method-table)))
    (cond (record (catch 'quit
                    (funcall (cadr record))))
          (t (message "No entry for '%s'" str)
             (sleep-for 1)
             (discard-input)
             (funcall #'basis/selector method-table)))))

(defun basis/ido-dir-selector ()
  "Open `ido-find-file-in-dir` in a specified directory."
  (interactive)
  (basis/selector basis/ido-dir-methods))

(defun basis/dired-dir-selector ()
  "Open `dired` in a specified directory."
  (interactive)
  (basis/selector basis/dired-dir-methods))

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
  ;; From https://gist.github.com/3048595
  (interactive)
  (cleanup-buffer-save)
  (indent-buffer))


(provide 'init-utils)
