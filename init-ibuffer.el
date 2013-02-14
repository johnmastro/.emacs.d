;; -----------------------------------------------------------------------------
;; init-ibuffer.el
;; -----------------------------------------------------------------------------

(eval-after-load 'ibuffer
  '(progn
     (define-ibuffer-column size-h
       ;; a more readable size column
       ;; from github.com/purcell/emacs.d/blob/master/init-ibuffer.el
       (:name "Size" :inline t)
       (cond ((> (buffer-size) 1000000)
              (format "%7.1fM" (/ (buffer-size) 1000000.0)))
             ((> (buffer-size) 1000)
              (format "%7.1fk" (/ (buffer-size) 1000.0)))
             (t
              (format "%8d" (buffer-size)))))))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(provide 'init-ibuffer)
