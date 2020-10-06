;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-01 15:41:26 (wand)>

;;; Code:

;; * magit
;; - https://github.com/magit/magit
;; - History
;;   -  2020-08-15 Created
(defun bk/magit-cursor-fix ()
  "Fix the cursor position inside magit buffers."
  (goto-char (point-min))
  (when (looking-at "#")
    (forward-line 2)))

(when (bk/add-load-path "version-control" "magit/lisp")
  (bk-auto-loads "magit" #'magit-status)
  (global-set-key (kbd "C-c g s") #'magit-status)
  (with-eval-after-load 'magit
    (setq magit-refresh-status-buffer nil)
    (set-default 'magit-revert-buffers 'silent)
    (set-default 'magit-no-confirm '(stage-all-changes
                                     unstage-all-changes))))

;; * gist.el
;; - https://github.com/defunkt/gist.el
;; - History
;;   -  2020-10-01 Created
(when (bk/add-load-path "version-control" "gist.el")
  (bk-auto-loads "gist" #'gist-buffer-private #'gist-list))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
