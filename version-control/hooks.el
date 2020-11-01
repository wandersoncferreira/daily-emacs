;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-11-01 01:05:16 (wand)>

;;; Code:

(defun bk/magit-status-hook ()
  (yas-minor-mode 0))

(defun bk/magit-commit-hook ()
  (setq magit-ignore-huge-commits nil))

(defun bk/magit-log-hook ())

;;; hooks.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
