;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 00:03:26 (wand)>

;;; Code:

;; * haskell-mode
;; - https://github.com/haskell/haskell-mode
;; - History
;;   -  2020-09-02 Created
(when (bk/add-load-path "langs/haskell" "haskell-mode")
  (bk-auto-loads "haskell" '("\\.hs\\'" . haskell-mode))
  (bk-auto-loads "haskell-interactive-mode" #'interactive-haskell-mode)
  (bk-auto-loads "haskell-doc" #'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
