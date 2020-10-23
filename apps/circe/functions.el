;;; functions.el --- Entry point for functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-22 23:26:37 (wand)>

;;; Code:

(defun my-nickserv-password (_)
  "Get the password for IRC Freenode."
  (with-temp-buffer
    (insert-file-contents-literally "~/.emacs.d/apps/circe/etc/password.el")
    (plist-get (read (buffer-string)) :nickserv-password)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; functions.el ends here
