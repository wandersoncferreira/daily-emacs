;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-19 23:24:40 (wand)>

;;; Code:

(defun ambrevar/elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(defun bk-setup-feature-elfeed ()
  "Customizations for elfeed."
  (define-key elfeed-search-mode-map "v" #'ambrevar/elfeed-play-with-mpv))

(when (bk/add-load-path "apps/elfeed" "elfeed")
  (bk-auto-loads "elfeed" #'elfeed)
  (eval-after-load 'elfeed
    '(bk-setup-feature-elfeed)))

(defun bk-setup-feature-elfeed-org ()
  "Customizations for elfeed-org."
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
