;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-19 23:15:54 (wand)>

;;; Code:

(defun bk-setup-feature-elfeed ()
  "Customizations for elfeed."
  (interactive)

  )

(when (bk/add-load-path "apps/elfeed" "elfeed")
  (bk-auto-loads "elfeed" #'elfeed))

(defun bk-setup-feature-elfeed-org ()
  "Customizations for elfeed-org."
  (interactive)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/apps/elfeed/feeds.org")
        rmh-elfeed-org-tree-id "elfeed")
  (elfeed-org))

(when (bk/add-load-path "apps/elfeed" "elfeed-org")
  (bk-auto-loads "elfeed-org" #'elfeed-org)
  (eval-after-load 'elfeed
    '(bk-setup-feature-elfeed-org)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
