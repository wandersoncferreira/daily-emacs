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

;;; change column automatic wrap
(setq-local fill-column 70)

;;; activate highlight parenthesis
(add-hook 'after-init-hook 'show-paren-mode)

;;; open this file easily
(set-register ?e '(file . "~/.emacs.d/init.el"))

;;; change font
(defun bk/set-monaco-font ()
  "Define the Monaco font."
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco" :height 110)))

(add-hook 'after-init-hook #'bk/set-monaco-font)

;;; change themes
(defun bk/light-theme ()
  "Define custom light theme."
  (interactive)
  (bk/set-monaco-font)
  (set-face-attribute 'lazy-highlight nil :background "light green")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))

(add-hook 'after-init-hook #'bk/light-theme)

;;; supress unecessary things
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name))

(add-hook
 'after-init-hook
 (lambda ()
   (menu-bar-mode -1)
   (tool-bar-mode -1)
   (scroll-bar-mode -1)))

;;; redefine key
(defun bk/eval-buffer ()
  "Provide some feedback."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'bk/eval-buffer)

;; * exec-path-from-shell
;; - https://github.com/purcell/exec-path-from-shell
;; - History
;;   -  2020-08-16 Create
(when (bk-load-path-add "exec-path-from-shell")
  (bk-auto-loads "exec-path-from-shell" #'exec-path-from-shell-initialize)
  (add-hook 'after-init-hook #'exec-path-from-shell-initialize))

;; * expand-region.el
;; - https://github.com/magnars/expand-region.el
;; - History
;;   -  2020-08-16 Create
(when (bk-load-path-add "expand-region.el")
  (bk-auto-loads "expand-region" #'er/expand-region)
  (global-set-key (kbd "C-'") #'er/expand-region))

;; * diminish
;; - https://github.com/emacsmirror/diminish
;; - History
;;   -  2020-08-15 Create
(when (bk-load-path-add "diminish")
  (bk-auto-loads "diminish" #'diminish))

;; * Paredit
;; - History
;;   - 2020-08-14 Create
(defun bk-setup-feature-paredit ()
  "Customizations for paredit."
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "M-?") nil))

(when (bk-load-path-add "paredit")
  (bk-auto-loads "paredit" #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (paredit-mode)
	      (bk-setup-feature-paredit)
	      (diminish 'paredit-mode))))

;; * Ido
;; - History
;;   -  2020-08-14 Create
(add-hook 'after-init-hook #'ido-mode)
(with-eval-after-load 'ido
  (setq ido-use-virtual-buffers t
      ido-enable-flex-matching t))

;; * Clojure mode
;; - History
;;   -  2020-08-14 Create
(when (bk-load-path-add "clojure-mode")
  (bk-auto-loads "clojure-mode"
		 '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
                 '("\\.cljc\\'" . clojurec-mode)
                 '("\\.cljx\\'" . clojurex-mode)
                 '("\\.cljs\\'" . clojurescript-mode)
                 '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)))

;; * CIDER mode
;; - History
;;   -  2020-08-14 Create
(when (bk-load-path-add "cider")
  (bk-auto-loads "cider"
		 #'cider-jack-in
		 #'cider-connect
		 #'cider-jack-in-clj&cljs))

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
(defun bk/magit-cursor-fix ()
  "Fix the cursor position inside magit buffers."
  (goto-char (point-min))
  (when (looking-at "#")
    (forward-line 2)))

(when (bk-load-path-add "magit/lisp")
  (bk-auto-loads "magit" #'magit-status)
  (global-set-key (kbd "C-c g s") #'magit-status)
  (with-eval-after-load 'magit
    (set-default 'magit-revert-buffers 'silent)
    (set-default 'magit-no-confirm '(stage-all-changes
				     unstage-all-changes))))

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

;; * org
;; - History
;;   -  2020-08-16 Create
(defun bk-setup-feature-org ()
  "Customizations for org mode."
  (setq org-return-follows-link t)
  (require 'ob-plantuml))

(with-eval-after-load 'org
  (bk-setup-feature-org))

;; * plantuml
;; - https://github.com/skuro/plantuml-mode
;; - History
;;   -  2020-08-16 Create
(defvar bk-plantuml-path "~/plantuml.jar")
(when (bk-load-path-add "plantuml-mode")
  (bk-auto-loads "plantuml-mode" '("\\.plantuml$" . plantuml-mode))
  (with-eval-after-load 'plantuml
    (setq org-plantuml-jar-path bk-plantuml-path)))

;; * org-roam
;; - https://github.com/org-roam/org-roam
;; - History
;;   -  2020-08-16 Create
(defvar bk-org-roam-directory "~/all/permanent")
(when (bk-load-path-add "org-roam")
  (bk-auto-loads "org-roam"
		 #'org-roam-capture
		 #'org-roam-dailies-today
		 #'org-roam-find-file
		 #'org-roam-insert
		 #'org-roam)
  (global-set-key (kbd "C-c n c") #'org-roam-capture)
  (global-set-key (kbd "C-c n t") #'org-roam-dailies-today)
  (global-set-key (kbd "C-c n f")  #'org-roam-find-file)
  (global-set-key (kbd "C-c n i") #'org-roam-insert)
  (global-set-key (kbd "C-c n r") #'org-roam)
  (with-eval-after-load 'org-roam
    (setq org-roam-directory bk-org-roam-directory)
    (setq org-roam-completion-system 'ido)
    (org-roam-mode +1)))

;; * org-roam-server
;; - https://github.com/org-roam/org-roam-server
;; - History
;;   -  2020-08-16 Create
(when (bk-load-path-add "org-roam-server")
  (bk-auto-loads "org-roam-server" #'org-roam-server-mode)
  (with-eval-after-load 'org-roam
    (setq org-roam-server-enable-access-to-local-files t
      org-roam-server-webserver-prefix "/home/wand"
      org-roam-server-webserver-address "127.0.0.1:8887/"
      org-roam-server-webserver-supported-extensions '("pdf" "mp4" "ogv" "mkv"))
    (org-roam-server-mode +1)))

;; * company-mode
;; - https://github.com/company-mode/company-mode
;; - History
;;   -  2020-08-16 Create
(when (bk-load-path-add "company-mode")
  (bk-auto-loads "company" #'global-company-mode)
  (add-hook 'after-init-hook #'global-company-mode)
  (with-eval-after-load 'company
    (setq company-show-numbers t
	  company-idle-delay 0.25
	  company-minimum-prefix-length 2
	  company-tooltip-limit 14
	  company-tooltip-align-annotations t
	  company-require-match 'never
	  company-frontends '(company-pseudo-tooltip-frontend
                              company-echo-metadata-frontend)
	  company-backends '(company-capf)
	  company-auto-complete-chars nil
	  company-dabbrev-other-buffers nil
	  company-dabbrev-ignore-case nil
	  company-dabbrev-downcase nil)
    (diminish 'company-mode)))

;; * company-org-roam
;; - https://github.com/org-roam/company-org-roam
;; - History
;;   -  2020-08-16 Create
(when (bk-load-path-add "company-org-roam")
  (bk-auto-loads "company-org-roam" #'company-org-roam)
  (with-eval-after-load 'org-roam
    (push 'company-org-roam company-backends)))

;; * switch-window
;; - https://github.com/dimitri/switch-window
;; - History
;;   -  2020-08-16 Create
(when (bk-load-path-add "switch-window")
  (bk-auto-loads "switch-window" #'switch-window)
  (global-set-key (kbd "C-x o") 'switch-window)
  (with-eval-after-load 'switch-window
    (setq-default switch-window-shortcut-style 'alphabet
                  switch-window-timeout nil)))

;; * multiple-cursors.el
;; - https://github.com/magnars/multiple-cursors.el
;; - History
;;   -  2020-08-16 Create
(when (bk-load-path-add "multiple-cursors.el")
  (bk-auto-loads "multiple-cursors" #'mc/mark-next-like-this #'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))


;;; allow ad-handle-redefinition
;;; got from here https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;;; I dont like that warning telling me that ad-handle-definition was changed.. that's ok.
(setq ad-redefinition-action 'accept)


;; End of file
(f-msg "Loaded init.el!")

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

(provide 'init.el)
;;; init.el ends here
