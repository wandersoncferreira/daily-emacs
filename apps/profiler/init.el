;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-29 06:36:59 (wand)>

;;; Code:


(when (bk/add-load-path "apps/profiler" "esup")
  (bk-auto-loads "esup" #'esup)
  (setq esup-depth 0))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
