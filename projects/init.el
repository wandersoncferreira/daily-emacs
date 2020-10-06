;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-06 05:47:11 (wand)>

;;; Code:

;; * projectile mode
;; - History
;;   -  2020-08-14 Created
;;   -  2020-08-28 Changing completion system to `ivy'
(when (bk/add-load-path "projects" "projectile")
  (bk-auto-loads "projectile" #'projectile-mode)
  (add-hook 'after-init-hook #'projectile-mode)
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy
          projectile-cache-file (concat user-emacs-directory "projectile.cache")
          projectile-auto-discover nil
          projectile-globally-ignored-files '(".DS_Store"
                                              "TAGS"
                                              "ido.last"
                                              "recentf"
                                              "smex-items")
          projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
          projectile-enable-caching t
          projectile-indexing-method 'hybrid
          projectile-kill-buffers-filter 'kill-only-files
          projectile-ignored-projects '("~/" "/tmp")
          projectile-mode-line-prefix " Prj")
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

;; * counsel-projectile
;; - https://github.com/ericdanan/counsel-projectile
;; - History
;;   -  2020-08-28 Created
(when (bk/add-load-path "projects" "counsel-projectile")
  (bk-auto-loads "counsel-projectile" #'counsel-projectile-mode)
  (add-hook 'after-init-hook #'counsel-projectile-mode))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
