;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 21:21:32 (wand)>

;;; Code:

(defun bk/docker-cleanup-buffers ()
  "Delete all the docker buffers created."
  (interactive)
  (kill-matching-buffers "docker" nil t))

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

;;; functions.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
