;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-08 07:32:03 (wand)>

;;; Code:

;;; get my current ip address
(defvar url-http-end-of-headers)
(defun bk/ip ()
  "Find my current public IP address."
  (interactive)
  (let* ((endpoint "https://api.ipify.org")
         (myip (with-current-buffer (url-retrieve-synchronously endpoint)
                 (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))
    (kill-new myip)
    (message "IP: %s" myip)))

;;; edit using sudo
(defun bk/sudo-edit (&optional arg)
  "Function to edit file with super-user with optional ARG."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun iwb ()
  "Ident whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "<f7>") 'iwb)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
