;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(defun bk-setup-feature-go-mode ()
  "Customizations for golang."
  (interactive)
  (setq gofmt-command "goimports")
  (add-to-list 'exec-path "/usr/bin/go")
  (setenv "GOPATH" "/usr/bin/go"))

(when (bk/add-load-path "langs/golang" "go-mode.el")
  (bk-auto-loads "go-mode" '("\\.go\\'" . go-mode))
  (add-hook 'go-mode-hook #'bk-setup-feature-go-mode))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
