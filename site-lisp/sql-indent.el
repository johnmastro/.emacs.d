;;; sql-indent.el --- indentation of SQL statements  -*- lexical-binding: t -*-

;; Copyright (C) 2000  Alex Schroeder

;; Authors: Alex Schroeder <alex@gnu.org>
;;          Matt Henry <mcthenry+gnu@gmail.com>
;; Maintainer: Matt Henry <mcthenry+gnu@gmail.com>
;; Version: $Id: sql-indent.el,v 1.10 2009/03/25 22:52:25 mhenry Exp $  

;; Keywords: languages
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?SqlIndent

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Indent SQL statements.

;; As the indentation of SQL statements depends not only on the previous
;; line but also on the current line, empty lines cannot always be
;; indented correctly.

;; Usage note: Loading this file will make all SQL mode buffers created
;; from then on use `sql-indent-line' for indentation.  A possible way
;; to install sql-indent.el would be to add the following to your
;; .emacs:

;; (eval-after-load "sql"
;;   '(load-library "sql-indent"))

;; Thanks:
;; Arcady Genkin <antipode@thpoon.com>


;;; History:
;; 2009-03-22*
;;     * mhenry
;;             Added `sql-indent-buffer' for efficient full buffer processing.
;;             Modified `sql-indent' to be savvy to comments and strings.
;;             Removed "and", "or" and "exists" from `sql-indent-first-column-regexp'
;;             Added "create", "drop" and "truncate" to `sql-indent-first-column-regexp'

;;; Code:

(require 'sql)

(defcustom sql-indent-first-column-regexp
  (concat "^\\s-*"
          (regexp-opt '("select" "update" "insert" "delete"
                        "union" "intersect"
                        "from" "where" "into" "group" "having" "order"
                        "set"
                        "create" "drop" "truncate")
                      t)
          "\\(\\b\\|\\s-\\)")
  "Regexp matching keywords relevant for indentation.
The regexp matches lines which start SQL statements and it matches lines
that should be indented at the same column as the start of the SQL
statement.  The regexp is created at compile-time.  Take a look at the
source before changing it.  All lines not matching this regexp will be
indented by `sql-indent-offset'."
  :type 'regexp
  :group 'SQL)

(defcustom sql-indent-offset 4
  "*Offset for SQL indentation."
  :type 'number
  :group 'SQL)

(defvar sql-indent-debug nil
  "If non-nil, `sql-indent-line' will output debugging messages.")

(defun sql-indent-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment."
  (let ((parse-state (syntax-ppss)))
    (or (nth 3 parse-state)             ; String
        (nth 4 parse-state))))          ; Comment

(defun sql-indent-last-line-start ()
  "Return the beginning position and indentation of the last non-blank line."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (or (looking-at-p "^\\s-*$")
                    (sql-indent-in-string-or-comment-p)))
      (forward-line -1))
    (list (point) (current-indentation))))

(defun sql-indent-level-delta (&optional prev-start prev-indent)
  "Return a list of the level change and the previous indentation.
Calculate the change in level from the previous non-blank line.
Given the optional parameters PREV-START and PREV-INDENT, assume
those to be the previous non-blank line."
  (save-excursion
    ;; Go back to the previous non-blank line
    (pcase-let* ((`(,prev-start ,prev-indent) (if (and prev-start prev-indent)
                                                  (list prev-start prev-indent)
                                                (sql-indent-last-line-start)))
                 (curr-start (progn (beginning-of-line)
                                    (point)))
                 (paren (car (parse-partial-sexp prev-start curr-start))))
      ;; Add opening or closing parens. If the current line starts with a
      ;; keyword statement (e.g. SELECT, FROM, ...), back up one level. If the
      ;; previous line starts with a keyword statement then add one level
      (list (+ paren
               (if (progn (goto-char prev-start)
                          (looking-at-p sql-indent-first-column-regexp))
                   1
                 0)
               (if (progn (goto-char curr-start)
                          (looking-at-p sql-indent-first-column-regexp))
                   -1
                 0))
            prev-indent))))

(defun sql-indent-buffer ()
  "Indent the current buffer's SQL statements."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let* ((start (point))
           (indent (if (looking-at-p "^\\s-*$")
                       0
                     (current-indentation))))
      (while (/= (point) (point-max))
        (forward-line)
        (pcase-let* ((`(,level ,indent) (sql-indent-level-delta start indent))
                     (this-indent (max 0 (* sql-indent-offset
                                            (if (< level 0)
                                                0
                                              level)))))
          (when sql-indent-debug
            (setq line (1+ line))
            (message "Line %3d; level %3d; indent was %3d; at %d"
                     line level indent (point)))
          (beginning-of-line)
          (when (and (not (looking-at-p "^\\s-*$"))
                     (not (sql-indent-in-string-or-comment-p))
                     (/= this-indent (current-indentation)))
            (indent-line-to this-indent))
          (end-of-line))))))

(defun sql-indent-line ()
  "Indent current line in an SQL statement."
  (interactive)
  (pcase-let* ((`(,level ,indent) (sql-indent-level-delta))
               (this-indent (max 0 (+ indent
                                      (* sql-indent-offset
                                         level)))))
    (when sql-indent-debug
      (message "SQL Indent: level delta: %3d; prev: %3d; this: %3d"
               level indent this-indent))
    (save-excursion
      (beginning-of-line)
      (if (and (not (looking-at-p "^\\s-*$"))
               (not (sql-indent-in-string-or-comment-p))
               (/= this-indent (current-indentation)))
          (indent-line-to this-indent)))))

(defun sql-indent-enable-locally ()
  "Enable `sql-indent' in the current buffer."
  (interactive)
  (set (make-local-variable 'indent-line-function)
       'sql-indent-line))

(defun sql-indent-enable ()
  "Enable `sql-indent' in `sql-mode' buffers."
  (interactive)
  (sql-indent-enable-locally)
  (add-hook 'sql-mode-hook 'sql-indent-enable-locally))

(provide 'sql-indent)

;;; sql-indent.el ends here
