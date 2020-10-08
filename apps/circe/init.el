;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-07 07:13:14 (wand)>

;;; Code:

;; * circe.el
;; - https://github.com/Silex/circe.el
;; - History
;;   -  2020-09-04 Created
(when (bk/add-load-path "apps/circe" "circe")
  (bk-auto-loads "circe" #'circe)
  (setq circe-network-options
      '(("Freenode"
         :tls t
         :nick "bartuka"
         :sasl-username "bartuka"
         :sasl-password "lkli3210"
         :channels ("#emacs")))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
