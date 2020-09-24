;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 00:04:44 (wand)>

;;; Code:

;; * markdown mode
;; - History
;;   -  2020-08-17 Created
(when (bk/add-load-path "modes/markdown" "markdown-mode")
  (bk-auto-loads "markdown-mode"
                 '("\\.md\\'" . markdown-mode)
                 '("\\.markdown\\'" . markdown-mode)
                 '("README\\.md" . gfm-mode))
  (with-eval-after-load 'markdown-mode
    (setq markdown-command "pandoc")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here