;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(load-file "~/.emacs.d/apps/gnus/functions.el")

;; * gnus
;; - History
;;   - 2020/11/01 Created
(defun bk-setup-feature-gnus ()
  "Customizations for GNUS."
  (interactive)
  (setq gnus-select-method '(nntp "news.gmane.io")
        user-full-name "bartuka"
        user-mail-address "bartuka.clj@gmail.com"
        message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-use-cache t
        epa-file-cache-passphrase-for-symmetric-encryption t))

(eval-after-load 'gnus
  '(progn
     (add-to-list 'gnus-secondary-select-methods
                  '(nnimap "gmail"
                           (nnimap-address "imap.gmail.com")
                           (nnimap-server-port 993)
                           (nnimap-stream ssl)
                           (nnir-search-engine imap)
                           (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                           (nnmail-expiry-wait 90)))
     
     (bk-setup-feature-gnus)
     
     ;;; tree view for groups
     (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)))

;; (setq gnus-thread-sort-functions
;;       '(gnus-thread-sort-by-most-recent-date
;;         (not gnus-thread-sort-by-number)))

;; ;;; threads!
;; (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
;;       gnus-thread-ignore-subject t
;;       gnus-thread-hide-subtree t)

;; (require 'spam)
;; (spam-initialize)
;; (setq spam-install-hooks t)

;; (spam-stat-install-hooks-function)

;; (setq spam-use-move t
;;       spam-split-group "spam")

;; (setq gnus-spam-process-destinations
;;       '((".*" "nnimap+gmail:[Gmail]/Spam")))

;; (setq gnus-ham-process-destinations
;;       '(("nnimap+gmail:[Gmail]/Spam" "nnimap+gmail:[Gmail]/Reclassify")))

;; (setq spam-junk-mailgroups '("nnimap+gmail:[Gmail]/Spam"))
;; (setq spam-split-group "nnimap+gmail:[Gmail]/Spam")

;; (setq nnimap-split-fancy '(|
;;                            (: spam-split 'spam-use-regex-headers)
;;                            (any "ding" "ding")
;;                            (: spam-split)
;;                            "nnimap:gmail:INBOX"
;;                            ))


;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
