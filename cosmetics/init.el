;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(load-file "~/.emacs.d/cosmetics/functions.el")

;;; activate highlight parenthesis
(add-hook 'after-init-hook 'show-paren-mode)

;; * themes
;; - History
;; - 2020-09-17 Added zenburn
(setq custom-theme-directory (concat user-emacs-directory "cosmetics/themes")
      custom-safe-themes t)

(dolist (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

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
   (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
   (column-number-mode)
   (size-indication-mode)
   (global-prettify-symbols-mode +1)))

;;; my WSL2 installation on Windows
(when (string-equal (system-name) "DESKTOP-77T99NU")
  (bk/set-consolas-font))

;; text mode
(add-hook 'text-mode-hook #'auto-fill-mode)

;; * whitespace
;; - History
;; - 2020-09-15 Created
;;; whitespace-mode config
(defun bk-setup-feature-whitespace ()
  "Customizations for `whitespace-mode'."
  (setq whitespace-line-column 80
        whitespace-style '(trailing tabs tab-mark))
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

;; * emojify
(defun bk-setup-feature-emojify ()
  "Customizations for emojify-mode."
  (interactive)
  (when (display-graphic-p)
    (setq emojify-display-style 'image
          emojify-emoji-set "emojione-v2.2.6")))

;; * highlight-numbers
;; - History
;;   - 2020/10/29 Created
(when (bk/add-load-path "cosmetics" "highlight-numbers")
  (bk-auto-loads "highlight-numbers" #'highlight-numbers-mode)
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; * volatile-highlights
;; - https://github.com/k-talo/volatile-highlights.el
;; - History
;;   - 2020/11/01 Created
(when (bk/add-load-path "cosmetics" "volatile-highlights.el")
  (bk-auto-loads "volatile-highlights" #'volatile-highlights-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (volatile-highlights-mode +1)
                              (diminish 'volatile-highlights-mode))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
