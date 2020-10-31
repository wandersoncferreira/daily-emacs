;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; * sql indent mode
;; - History
;;   -  2020-08-17 Created
(when (bk/add-load-path "langs/sql" "emacs-sql-indent")
  (bk-auto-loads "sql-indent" #'sqlind-minor-mode)
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
