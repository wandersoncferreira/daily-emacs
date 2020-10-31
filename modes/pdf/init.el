;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(when (bk/add-load-path "modes/pdf" "pdf-tools/lisp")
  (bk-auto-loads "pdf-view" '("\\.pdf" . pdf-view-mode))
  (bk-auto-loads "pdf-tools" #'pdf-tools-install)
  (bk-auto-loads "pdf-occur" #'pdf-occur-global-minor-mode)
  (eval-after-load 'pdf '(progn (pdf-tools-install))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
