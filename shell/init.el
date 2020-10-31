;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;;; open eshell more easily
(global-set-key (kbd "C-c e") 'eshell)

;;; eshell clear buffer key binding
(defun eshell-clear-buffer ()
  "Clear the terminal buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;;; eshell aliases
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")))

;;; define default shell
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  "BEFORE FORCE-BASH docs."
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))

(setq eshell-history-size 10000
      eshell-buffer-maximum-lines 10000
      eshell-hist-ignoredups t
      eshell-highlight-promp t
      eshell-scroll-to-bottom-on-input t)

;; * eshell-z
;; - https://github.com/xuchunyang/eshell-z
;; - History
;;   - 2020/10/29 Created
(when (bk/add-load-path "shell" "eshell-z")
  (bk-auto-loads "eshell-z" #'eshell/pushd #'eshell/pwd)
  (add-hook 'eshell-mode-hook (lambda () (require 'eshell-z)))
  (add-hook 'eshell-z-change-dir-hook (lambda () (eshell/pushd (eshell/pwd)))))

;; * eshell-syntax-highlighting
;; - https://github.com/akreisher/eshell-syntax-highlighting
;; - History
;;   - 2020/10/29 Created
(when (bk/add-load-path "shell" "eshell-syntax-highlighting")
  (bk-auto-loads "eshell-syntax-highlighting" #'eshell-syntax-highlighting-global-mode)
  (add-hook 'after-init-hook #'eshell-syntax-highlighting-global-mode))

;; * esh-autosugest
;; - https://github.com/dieggsy/esh-autosuggest
;; - History
;;   - 2020/10/29 Created
(defun bk-setup-feature-esh-autosuggest ()
  "Customizations for esh-autosugest."
  (interactive)
  (esh-autosuggest-mode +1)
  (setq esh-autosuggest-delay 0.5)
  (set-face-attribute 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))
  
(when (bk/add-load-path "shell" "esh-autosuggest")
  (bk-auto-loads "esh-autosuggest" #'esh-autosuggest-mode)
  (add-hook 'eshell-mode-hook #'bk-setup-feature-esh-autosuggest))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
