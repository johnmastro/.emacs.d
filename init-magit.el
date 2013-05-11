;; -----------------------------------------------------------------------------
;; init-magit.el
;; -----------------------------------------------------------------------------

(defadvice magit-status (around magit-fullscreen activate)
  ;; from http://whattheemacsd.com/setup-magit.el-01.html
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(eval-after-load 'magit
  '(define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(provide 'init-magit)
