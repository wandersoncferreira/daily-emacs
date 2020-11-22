;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;;; load emacs-nox specifics
(load-file "~/.emacs.d/terminal-emulators/emacs-nox.el")

;;; load tmux translations
(load-file "~/.emacs.d/terminal-emulators/tmux-translations.el")

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

;; * emamux
;; - https://github.com/emacsorphanage/emamux
;; - History
;;   - 2020-11-15 Created
(defun bk-setup-feature-tmux ()
  "Customizations for emamux."
  (interactive)
  (require 'emamux)
  (global-set-key (kbd "C-c t") emamux:keymap))

(when (bk/add-load-path "shell" "emamux")
  (add-hook 'after-init-hook #'bk-setup-feature-tmux))

;; * tramp-mode
;; - History
;;; tramp mode
(require 'tramp)
(setq explicit-shell-file-name "/bin/bash"
      tramp-histfile-override "/dev/null"
      remote-file-name-inhibit-cache nil
      vc-ignore-dir-regexp (format "%s\\|%s"
                                   vc-ignore-dir-regexp
                                   tramp-file-name-regexp)

      tramp-default-method "ssh"
      tramp-auto-save-directory temporary-file-directory
      tramp-verbose 10
      tramp-ssh-controlmaster-options nil
      disable-tramp-backups nil)

(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-hook 'minibuffer-inactive-mode-hook (lambda () (auto-revert-mode -1)))

(setq auto-revert-remote-files nil)
(setenv "PAGER" "cat")

(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))

;;; don't generate backups for remote files opened as root (security hazard)
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (let ((method (file-remote-p name 'method)))
                    (when (stringp method)
                      (member method '("su" "sudo"))))))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
