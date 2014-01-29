;;; cycle-spacing, copied here from GNU Emacs 24.4's simple.el.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(defvar cycle-spacing--context nil
  "Store context used in consecutive calls to `cycle-spacing' command.
The first time this function is run, it saves the original point
position and original spacing around the point in this
variable.")

(defun cycle-spacing (&optional n preserve-nl-back single-shot)
  "Manipulate whitespace around point in a smart way.
In interactive use, this function behaves differently in successive
consecutive calls.

The first call in a sequence acts like `just-one-space'.
It deletes all spaces and tabs around point, leaving one space
\(or N spaces).  N is the prefix argument.  If N is negative,
it deletes newlines as well, leaving -N spaces.
\(If PRESERVE-NL-BACK is non-nil, it does not delete newlines before point.)

The second call in a sequence (or the first call if the above does
not result in any changes) deletes all spaces.

The third call in a sequence restores the original whitespace (and point).

If SINGLE-SHOT is non-nil, it only performs the first step in the sequence."
  (interactive "*p")
  (let ((orig-pos        (point))
        (skip-characters (if (and n (< n 0)) " \t\n\r" " \t"))
        (n               (abs (or n 1))))
    (skip-chars-backward (if preserve-nl-back " \t" skip-characters))
    (constrain-to-field nil orig-pos)
    (cond
     ;; Command run for the first time or single-shot is non-nil.
     ((or single-shot
          (not (equal last-command this-command))
          (not cycle-spacing--context))
      (let* ((start (point))
             (n     (- n (skip-chars-forward " " (+ n (point)))))
             (mid   (point))
             (end   (progn
                      (skip-chars-forward skip-characters)
                      (constrain-to-field nil orig-pos t))))
        (setq cycle-spacing--context ;; Save for later.
              ;; Special handling for case where there was no space at all.
              (unless (= start end)
                (cons orig-pos (buffer-substring start (point)))))
        ;; If this run causes no change in buffer content, delete all spaces,
        ;; otherwise delete all excess spaces.
        (delete-region (if (and (not single-shot) (zerop n) (= mid end))
                           start mid) end)
        (insert (make-string n ?\s))))

     ;; Command run for the second time.
     ((not (equal orig-pos (point)))
      (delete-region (point) orig-pos))

     ;; Command run for the third time.
     (t
      (insert (cdr cycle-spacing--context))
      (goto-char (car cycle-spacing--context))
      (setq cycle-spacing--context nil)))))

(provide 'cycle-spacing)
