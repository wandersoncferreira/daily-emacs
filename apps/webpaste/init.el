;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(defun bk-setup-feature-webpaste ()
  "Customizations for webpaste."
  (setq webpaste-paste-confirmation t
        webpaster-provider-priority '("gist.github.com" "ix.io" "dpaste.org")))

(when (bk/add-load-path "apps/webpaste" "webpaste.el")
  (bk-auto-loads "webpaste" #'webpaste-paste-buffer-or-region)
  (add-hook 'after-init-hook #'bk-setup-feature-webpaste))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
