;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

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
          projectile-cache-file "~/.emacs.d/projects/etc/projectile.cache"
          projectile-known-projects-file "~/.emacs.d/projects/etc/projectile-bookmarks.eld"
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

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
