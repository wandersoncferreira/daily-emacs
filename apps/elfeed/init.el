;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-22 22:06:07 (wand)>

;;; Code:

(defvar bk-elfeed-path "~/.emacs.d/apps/elfeed")

(load-file (format "%s/functions.el" bk-elfeed-path))

(defun bk-setup-feature-elfeed ()
  "Customizations for elfeed."
  (define-key elfeed-search-mode-map "v" #'ambrevar/elfeed-play-with-mpv)
  (define-key elfeed-show-mode-map "b" #'bk/elfeed-bongo-insert-item)
  (define-key elfeed-search-mode-map "b" #'bk/elfeed-bongo-insert-item)
  (define-key elfeed-search-mode-map "h" #'bk/elfeed-bongo-switch-to-playlist))

(when (bk/add-load-path "apps/elfeed" "elfeed")
  (bk-auto-loads "elfeed" #'elfeed)
  (eval-after-load 'elfeed
    '(bk-setup-feature-elfeed)))

(defun bk-setup-feature-elfeed-org ()
  "Customizations for elfeed-org."
  (setq rmh-elfeed-org-files (list (format "%s/feeds.org" bk-elfeed-path))
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
