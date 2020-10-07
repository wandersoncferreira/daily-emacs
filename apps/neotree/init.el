;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-07 06:34:06 (wand)>

;;; Code:

;; * emacs-neotree
;; - https://github.com/jaypei/emacs-neotree
;; - History
;;   -  2020-10-07 Created
(when (bk/add-load-path "apps/neotree" "emacs-neotree")
  (bk-auto-loads "neotree" #'neotree-projectile-action #'neotree-toggle)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-smart-open t)
  (global-set-key [f8] 'neotree-toggle))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
