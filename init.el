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
(setq tab-always-indent 'complete
      vc-follow-symlinks t)

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
  "Provide some feedback."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'bk/eval-buffer)

;; * diminish
;; - https://github.com/emacsmirror/diminish
;; - History
;;   -  2020-08-15 Create
(when (bk-load-path-add "diminish")
  (bk-auto-loads "diminish" #'diminish))

;; * Paredit
;; - History
;;   - 2020-08-14 Create
(when (bk-load-path-add "paredit")
  (bk-auto-loads "paredit" #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (paredit-mode)
	      (diminish 'paredit-mode))))

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
	  projectile-mode-line-prefix " Prj"
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
  "Scroll only specific amount of lines."
  (interactive)
  (scroll-up-command 8))

(defun bk/scroll-down ()
  "Scroll only specific amount of lines."
  (interactive)
  (scroll-down-command 8))

(global-set-key (kbd "C-v") #'bk/scroll-up)
(global-set-key (kbd "M-v") #'bk/scroll-down)

;;; go back to last marked place
(global-set-key (kbd "C-x p") 'pop-to-mark-command)

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
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;;; open dired in the current file
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; * magit
;; - https://github.com/magit/magit
;; - History
;;   -  2020-08-15 Create
(when (bk-load-path-add "magit/lisp")
  (bk-auto-loads "magit" #'magit-status)
  (global-set-key (kbd "C-c g s") #'magit-status))

;; * Ledger mode
;; - https://github.com/ledger/ledger-mode
;; - History
;;   -  2020-08-15 Create
(defun bk/clean-ledger ()
  "Bring back timeline structure to the whole file."
  (interactive)
  (if (eq major-mode 'ledger-mode)
      (let ((curr-line (line-number-at-pos)))
        (ledger-mode-clean-buffer)
        (line-move (- curr-line 1)))))

(defun bk-setup-feature-ledger ()
  "Customizations for ledger."
  (setq ledger-reports
	'(("netcash" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -R -X R$ --current bal ^assets:bank liabilities:card")
          ("networth" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -X R$ --current bal ^assets:bank liabilities equity:apartment")
          ("spent-vs-earned" "ledger [[ledger-mode-flags]] -f /home/wand/.ledger bal -X BRL --period=\"last 4 weeks\" ^Expenses ^Income --invert -S amount")
          ("budget" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -X R$ --current bal ^assets:bank:checking:budget liabilities:card")
          ("bal" "%(binary) -f %(ledger-file) bal")
          ("reg" "%(binary) -f %(ledger-file) reg")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
          ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(when (bk-load-path-add "ledger-mode")
  (bk-auto-loads "ledger-mode"
		 '("\\ledger$" . ledger-mode)
		 '("\\.ledger$" . ledger-mode))
  (set-register ?l '(file . "~/.ledger"))
  (with-eval-after-load 'ledger
    (bk-setup-feature-ledger)))

;; * smex
;; - https://github.com/nonsequitur/smex
;; - History
;;   -  2020-08-15 Create
(when (bk-load-path-add "smex")
  (bk-auto-loads "smex" #'smex)
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "C-x C-m") 'smex)

  (with-eval-after-load 'smex
    (smex-initialize)))

;;; when two buffers have the same name, we need a way to distinguish them
(setq uniquify-buffer-name-style 'post-forward)


;;; delete selected text with a key..
(add-hook 'after-init-hook 'delete-selection-mode)

;;; auto revert buffers if the file underneath it gets modified
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; * flycheck
;; - https://github.com/flycheck/flycheck
;; - History
;;   -  2020-08-15 Create
(defun bk-setup-feature-flycheck ()
  "Customizations for flycheck."
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch)
	flycheck-display-errors-delay 0.25))

(when (bk-load-path-add "flycheck")
  (bk-auto-loads "flycheck" #'flycheck-mode)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (with-eval-after-load 'flycheck
    (bk-setup-feature-flycheck)))




;;; End of file
(f-msg "Loaded init.el!")

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

(provide init.el)
;;; init.el ends here
