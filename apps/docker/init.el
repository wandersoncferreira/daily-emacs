;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 00:36:49 (wand)>

;;; Code:

;;; docker
(defun bk/docker-compose-custom-envs ()
  "Add usual env variables to Emacs environment."
  (interactive)
  (let* ((idu (shell-command-to-string "id -u"))
         (idg (shell-command-to-string "id -g"))
         (uid (string-join (vector (string-trim idu) ":" (string-trim idg)))))
    (setenv "WEBSERVER_PORT" "3000")
    (setenv "GRAPHQL_PORT" "4000")
    (setenv "CURRENT_UID" uid)
    (message "setenv WEBSERVER_PORT=3000 CURRENT_UID=$(id -u):$(id -g) done!")
    (docker)))

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
