;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 21:33:12 (wand)>

;;; Code:

(defun bk/magit-cursor-fix ()
  "Fix the cursor position inside magit buffers."
  (goto-char (point-min))
  (when (looking-at "#")
    (forward-line 2)))

(defun bk/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote" (magit-get-remote) "url"))
           (magit-get-current-branch))))

;;; functions.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
