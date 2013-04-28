;; -----------------------------------------------------------------------------
;; init-python.el
;; -----------------------------------------------------------------------------

(defun basis/python-setup ()
  (define-key python-mode-map "\C-m" 'newline-and-indent))

(add-hook 'python-mode-hook 'basis/python-setup)

(defun basis/insert-python-header (&optional arg)
  "Insert a Python coding cookie and modeline.
By default insert at the beginning of the buffer. With
a prefix arg insert wherever point is."
  (interactive "P")
  (save-excursion
    (unless arg
      (goto-char (point-min)))
    (insert (concatenate 'string
                         "# -*- coding: utf-8 -*-\n"
                         "#!/usr/bin/env python\n"
                         "\n"))))

(provide 'init-python)
