;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-29 00:52:16 (wand)>

;;; Code:

;; * telega
;; - History
;;   -  2020-09-25 Created
(when (bk/add-load-path "apps/telega" "telega.el")
  (bk-auto-loads "telega" #'telega #'telega-chat-mode-hook)
  (eval-after-load 'telega
    '(progn
       (setq telega-user-use-avatars nil
             telega-use-tracking-for '(any pin unread)
             telega-emoji-use-images t
             telega-completing-read-function #'ivy-completing-read
             telega-msg-rainbow-title nil
             telega-chat-fill-column 75)
       (telega-notifications-mode +1)))
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
