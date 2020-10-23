;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-22 23:16:04 (wand)>

;;; Code:

;; * circe.el
;; - https://github.com/Silex/circe.el
;; - History
;;   -  2020-09-04 Created

(defun my-nickserv-password (_)
  "Get the password for IRC Freenode."
  (with-temp-buffer
    (insert-file-contents-literally "~/.emacs.d/apps/circe/etc/password.el")
    (plist-get (read (buffer-string)) :nickserv-password)))

(when (bk/add-load-path "apps/circe" "circe")
  (bk-auto-loads "circe" #'circe)
  (setq circe-network-options
        '(("Freenode"
           :tls t
           :nick "bartuka"
           :sasl-username "bartuka"
           :sasl-password my-nickserv-password
           :channels (:after-auth "#emacs" "#clojure")))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
