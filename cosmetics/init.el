;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-11 23:42:40 (wand)>

;;; Code:

;;; activate highlight parenthesis
(add-hook 'after-init-hook 'show-paren-mode)

;; - History
;; - 2020-09-17 - Enabling Zenburn
;;; change font
(defun bk/set-monaco-font ()
  "Define the Monaco font."
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco" :height 100)))

(defun bk/increase-font (size)
  "Increase the SIZE of the current font."
  (set-face-attribute 'default nil :height size))

(defun bk/light-theme ()
  "Define custom light theme."
  (interactive)
  (bk/increase-font 120)
  (set-face-attribute 'lazy-highlight nil :background "light green")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))

;; * themes
;; - History
;; - 2020-09-17 Added zenburn
(setq custom-theme-directory (concat user-emacs-directory "cosmetics/themes")
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
   (size-indication-mode)
   (global-prettify-symbols-mode +1)))

;; text mode
(add-hook 'text-mode-hook #'auto-fill-mode)

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

;; * outline
;; - History
;; - 2020-09-13 Created
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (outline-minor-mode +1)
            (diminish 'outline-minor-mode)
            (setq outline-blank-line t)
            (setq-local outline-regexp ";; \\*")))

;; * diminish
;; - https://github.com/emacsmirror/diminish
;; - History
;;   -  2020-08-15 Created
(when (bk/add-load-path "cosmetics" "diminish")
  (bk-auto-loads "diminish" #'diminish)
  (global-eldoc-mode +1)
  (diminish 'eldoc-mode))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
