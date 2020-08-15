;;; init.el --- Personal configuration ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;;; timing
(let ((t0 (float-time)))
  (defun f-msg (msg)
    "MSG with time since start."
    (message "%s. Time elapsed: %.3fs" msg (- (float-time) t0))))

;;; quick startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 800000
		  gc-cons-percentage 0.1)))

(defvar bk--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist bk--file-name-handler-alist)))

;;; helper functions to perform lazy loading of packages

(defvar bk-external-packages-dir "~/.emacs.d/external/")

;;; load lazy-loading helper functions
(load-file (expand-file-name "lazy-loading.el" user-emacs-directory))

;;; load dependencies
(load-file (expand-file-name "dependencies.el" user-emacs-directory))

;;; write customizations in the custom.el file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; customizations
;; basics
(setq tab-always-indent 'complete)

;;; change font
(defun bk/set-monaco-font ()
  "Define the Monaco font."
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco" :height 110)))

(bk/set-monaco-font)

;;; change themes
(defun bk/light-theme ()
  "Define custom light theme."
  (interactive)
  (bk/set-monaco-font)
  (set-face-attribute 'lazy-highlight nil :background "light green")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))

(bk/light-theme)

;;; supress unecessary things
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; redefine key
(defun bk/eval-buffer ()
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'bk/eval-buffer)

;; * Paredit
;; - History
;;   - 2020-08-14 Create
(when (bk-load-path-add "paredit")
  (bk-auto-loads "paredit" #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

;; * Ido
;; - History
;;   -  2020-08-14 Create
(setq ido-use-virtual-buffers t
      ido-enable-flex-matching t)
(ido-mode +1)

;; * Clojure mode
;; - History
;;   -  2020-08-14 Create
(when (bk-load-path-add "clojure-mode")
  (bk-auto-loads "clojure-mode" '("\\.clj$" . clojure-mode)))

;; * CIDER mode
;; - History
;;   -  2020-08-14 Create
(when (bk-load-path-add "cider")
  (bk-auto-loads "cider" #'cider-jack-in #'cider-connect))

;; * PROJECTILE mode
;; - History
;;   -  2020-08-14 Create
(when (bk-load-path-add "projectile")
  (bk-auto-loads "projectile" #'projectile-mode)
  (add-hook 'after-init-hook #'projectile-mode)
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ido
	  projectile-enable-caching t
	  projectile-indexing-method 'hybrid
	  projectile-sort-order 'access-time)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

;; * change-inner
;; - https://github.com/magnars/change-inner.el
;; - History
;;   -  2020-08-15 Create
(when (bk-load-path-add "change-inner.el")
  (bk-auto-loads "change-inner" #'change-inner #'change-outer)
  (global-set-key (kbd "M-i") #'change-inner)
  (global-set-key (kbd "M-o") #'change-outer))

;;; improve scroll functions
(defun bk/scroll-up ()
  "Scroll only specific amount of lines. I don't like the defaults of whole screen."
  (interactive)
  (scroll-up-command 8))

(defun bk/scroll-down ()
  "Scroll only specific amount of lines. I don't like the defaults of whole screen."
  (interactive)
  (scroll-down-command 8))

(global-set-key (kbd "C-v") #'bk/scroll-up)
(global-set-key (kbd "M-v") #'bk/scroll-down)

;;; go back to last marked place
(global-set-key (kbd "C-x p") 'pop-to-mark-command)

;;; open eshell more easily
(global-set-key (kbd "C-c e") 'eshell)

;;; open dired in the current file
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; * magit
;; - https://github.com/magit/magit
;; - History
;;   -  2020-08-15 Create
(when (bk-load-path-add "magit/lisp")
  (bk-auto-loads "magit" #'magit-status)
  (global-set-key (kbd "C-c g s") #'magit-status))



;;; End of file
(f-msg "Loaded init.el!")
