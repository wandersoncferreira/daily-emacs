;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 00:11:35 (wand)>

;;; Code:

(let* ((current-dir (expand-file-name "modes" user-emacs-directory)))
  (dolist (dir (directory-files current-dir nil directory-files-no-dot-files-regexp))
    (when (file-directory-p dir)
      (load-file (expand-file-name
                  (concat "modes/" dir "/init.el")
                  user-emacs-directory)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; loader.el ends here
