;;; functions.el --- Entry point for functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-11-03 09:24:02 (wand)>

;;; Code:

(defun bk/gmail-report-spam ()
  "Report the current or marked mails as spam."
  (interactive)
  (gnus-summary-move-article nil "nnimap+gmail:[Gmail]/Spam"))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; functions.el ends here
