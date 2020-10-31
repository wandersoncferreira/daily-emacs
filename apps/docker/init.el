;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(load-file "~/.emacs.d/apps/docker/functions.el")

;; * docker.el
;; - https://github.com/Silex/docker.el
;; - History
;;   -  2020-09-04 Created
(when (bk/add-load-path "apps/docker" "docker.el")
  (bk-auto-loads "docker" #'docker)
  (global-set-key (kbd "C-c d") #'docker))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
