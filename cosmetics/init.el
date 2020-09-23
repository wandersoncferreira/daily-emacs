;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 00:21:22 (wand)>

;;; Code:

;; - History
;; - 2020-09-17 - Enabling Zenburn
;;; change font
(defun bk/set-monaco-font ()
  "Define the Monaco font."
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco" :height 110)))

;;; change themes
(defun bk/light-theme ()
  "Define custom light theme."
  (interactive)
  (bk/set-monaco-font)
  (set-face-attribute 'lazy-highlight nil :background "light green")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))

;; * themes
;; - History
;; - 2020-09-17 Added zenburn
(setq custom-theme-directory (concat user-emacs-directory "themes")
      custom-safe-themes t)

(dolist (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(add-hook 'after-init-hook
          (lambda ()
            (bk/light-theme)))

;;; supress unecessary things
;; (put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name))

(add-hook
 'after-init-hook
 (lambda ()
   (menu-bar-mode -1)
   (tool-bar-mode -1)
   (scroll-bar-mode -1)
   (column-number-mode)
   (size-indication-mode)))

;; * whitespace
;; - History
;; - 2020-09-15 Created
;;; whitespace-mode config
(defun bk-setup-feature-whitespace ()
  "Customizations for `whitespace-mode'."
  (setq whitespace-line-column 80
        whitespace-style '(face tabs empty trailing lines-tail))
  (whitespace-mode +1)
  (diminish 'whitespace-mode))

(add-hook 'after-init-hook #'bk-setup-feature-whitespace)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
