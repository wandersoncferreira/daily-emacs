;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-28 23:10:32 (wand)>

;;; Code:

;; * telega
;; - History
;;   -  2020-09-25 Created
(when (bk/add-load-path "apps/telega" "telega.el")
  (bk-auto-loads "telega" #'telega #'telega-chat-mode-hook)
  (eval-after-load 'telega
    '(telega-notifications-mode +1))
  (add-hook 'telega-chat-mode-hook
            (lambda ()
              (company-mode -1)))
  (add-hook 'telega-chat-mode-hook
            (lambda ()
              (emojify-mode-line-mode +1))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
