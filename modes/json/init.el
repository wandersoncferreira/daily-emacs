;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; * json-mode
;; - https://github.com/joshwnj/json-mode
;; - History
;;   -  2020-09-04 Created
(when (bk/add-load-path "modes/json" "json-mode")
  (bk-auto-loads "json-mode" '("\\.json\\'" . json-mode)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
