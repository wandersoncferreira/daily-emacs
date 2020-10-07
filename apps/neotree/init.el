
;; * emacs-neotree
;; - https://github.com/jaypei/emacs-neotree
;; - History
;;   -  2020-10-07 Created
(when (bk/add-load-path "apps/neotree" "emacs-neotree")
  (bk-auto-loads "neotree" #'neotree-projectile-action)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-smart-open t))
