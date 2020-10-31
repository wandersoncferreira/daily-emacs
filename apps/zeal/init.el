;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;;; you need to install zeal
(defun bk/has-package? (pkg-name)
  "Verify if the PKG-NAME is installed in your box."
  (let* ((r (shell-command-to-string (format "which %s" pkg-name))))
    (when (not (string-match "which:" r)) t)))

(when (not (bk/has-package? "zeal"))
  (user-error "Error, because: %s" "Missing Zeal package"))

(when (bk/add-load-path "apps/zeal" "zeal-at-point")
  (bk-auto-loads "zeal-at-point" #'zeal-at-point)
  (global-set-key (kbd "C-c C-d") 'zeal-at-point)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq zeal-at-point-docset "clojure"))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
