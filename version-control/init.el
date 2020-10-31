;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(load-file "~/.emacs.d/version-control/functions.el")

;; * magit
;; - https://github.com/magit/magit
;; - History
;;   -  2020-08-15 Created
(defun bk-setup-feature-magit ()
  "Customizations to magit."
  (interactive)
  (setq magit-diff-refine-hunk t
        magit-revert-buffers 'silent
        magit-commit-arguments '("--verbose")
        magit-process-popup-time 10
        magit-refresh-status-buffer nil)
  
  (set-default 'magit-revert-buffers 'silent)
  (set-default 'magit-no-confirm '(stage-all-changes
                                   unstage-all-changes)))

(when (bk/add-load-path "version-control" "magit/lisp")
  (bk-auto-loads "magit" #'magit-status)
  (bk-auto-loads "magit"
                 #'magit-get
                 #'magit-get-remote
                 #'magit-get-current-branch)
  (global-set-key (kbd "C-c g s") #'magit-status)
  (with-eval-after-load 'magit (bk-setup-feature-magit)))

;; * gist.el
;; - https://github.com/defunkt/gist.el
;; - History
;;   -  2020-10-01 Created
(when (bk/add-load-path "version-control" "gist.el")
  (bk-auto-loads "gist" #'gist-buffer-private #'gist-list
                 #'gist-region #'gist-region-private))
;; * git-modes
;; - https://github.com/magit/git-modes
;; - History
;;   - 2020-10-12 Created
(when (bk/add-load-path "version-control" "git-modes")
  (bk-auto-loads "gitconfig-mode" #'gitconfig-mode)
  (bk-auto-loads "gitignore-mode" #'gitignore-mode)
  (add-hook 'after-init-hook (lambda () (require 'gitconfig-mode)))
  (add-hook 'after-init-hook (lambda () (require 'gitignore-mode))))

;; * git-timemachines
;; - https://github.com/emacsmirror/git-timemachine
;; - History
;;   - 2020-10-12 Created
(when (bk/add-load-path "version-control" "git-timemachine")
  (bk-auto-loads "git-timemachine" #'git-timemachine)
  (global-set-key (kbd "C-c g t") 'git-timemachine))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
