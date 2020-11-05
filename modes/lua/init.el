;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; * lua-mode
;; - https://github.com/immerrr/lua-mode
;; - History
;;   -  2020-11-03 Created

(when (bk/add-load-path "modes/lua" "lua-mode")
  (bk-auto-loads "lua-mode" '("\\.lua\\'" . lua-mode)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
