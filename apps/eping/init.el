;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:
(defun bk-setup-feature-eping ()
  "Customizations for eping."
  (interactive)
  (setq eping-domain-options '("google.com" "haskell.org")
        eping-number-pings-options '("1" "10" "20")))

(when (bk/add-load-path "apps/eping" "eping")
  (bk-auto-loads "eping" #'eping)
  (bk-setup-feature-eping))

(provide 'init.el)
;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
