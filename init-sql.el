;; -----------------------------------------------------------------------------
;; init-sql.el
;; -----------------------------------------------------------------------------

(defun sql-product-is-probably-postgres ()
  (sql-set-product "postgres"))

(add-hook 'sql-mode-hook 'sql-product-is-probably-postgres)

(provide 'init-sql)
