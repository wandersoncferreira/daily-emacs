;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 21:06:05 (wand)>

;;; Code:

(defun bk/kill-all-comment ()
  "Function to kill all comments in a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (comment-kill (save-excursion
                    (goto-char (point-max))
                    (line-number-at-pos)))))

;;; functions.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
