;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(load-file "~/.emacs.d/apps/docker/functions.el")

(defun bk-setup-feature-docker ()
  "Customizations for Docker."
  (interactive)
  (require 'tramp)
  (push
   (cons
    "docker"
    '((tramp-login-program "docker")
      (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-i") ("-c"))))
   tramp-methods)
  
  (defadvice tramp-completion-handle-file-name-all-completions
      (around dotemacs-completion-docker activate)
    "returns a list of active Docker container names, followed by colons."
    (if (equal (ad-get-arg 1) "/docker:")
        (let* ((dockernames-raw (shell-command-to-string "docker ps | aws '$NF != \"NAMES\" { print $NF \":\" }'"))
               (dockernames (cl-remove-if-not
                             #'(lambda (dockerline) (string-match ":$" dockerline))
                             (split-string dockernames-raw "\n"))))
          (setq ad-return-value dockernames))
      ad-do-it)))

;; * docker.el
;; - https://github.com/Silex/docker.el
;; - History
;;   -  2020-09-04 Created
(when (bk/add-load-path "apps/docker" "docker.el")
  (bk-auto-loads "docker" #'docker)
  (global-set-key (kbd "C-c d") #'docker)
  (add-hook 'after-init-hook #'bk-setup-feature-docker))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
