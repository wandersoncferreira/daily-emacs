;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-20 00:39:49 (wand)>

;;; Code:

(load-file "~/.emacs.d/modes/prog/functions.el")

;; * paredit
;; - History
;;   - 2020-08-14 Created
;;   - 2020-08-16 Add clojure(script) modes
(defun bk-setup-feature-paredit ()
  "Customizations for paredit."
  (paredit-mode)
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "M-?") nil)
  (diminish 'paredit-mode))

(when (bk/add-load-path "modes/prog" "paredit")
  (bk-auto-loads "paredit" #'paredit-mode)
  (add-hook 'lisp-mode-hook #'bk-setup-feature-paredit)
  (add-hook 'emacs-lisp-mode-hook #'bk-setup-feature-paredit)
  (add-hook 'clojure-mode-hook #'bk-setup-feature-paredit)
  (add-hook 'clojurescript-mode-hook #'bk-setup-feature-paredit)
  (add-hook 'cider-repl-mode-hook #'bk-setup-feature-paredit))

;; * hl-todo
;; - https://github.com/tarsius/hl-todo
;; - History
;;  - 2020-09-15 Created
(when (bk/add-load-path "modes/prog" "hl-todo")
  (bk-auto-loads "hl-todo" #'hl-todo-mode)
  (add-hook 'prog-mode-hook #'hl-todo-mode))

;; * yasnippet-snippets
;; - https://github.com/AndreaCrotti/yasnippet-snippets
;; - History
;;   - 2020-10-09 Created
(when (bk/add-load-path "modes/prog" "yasnippet-snippets")
  (bk-auto-loads "yasnippet" #'yas-describe-tables)
  (setq yas-snippet-dirs
        '("/home/wand/.emacs.d/langs/python/pkgs/elpy/snippets/"
          "/home/wand/.emacs.d/modes/prog/pkgs/yasnippet-snippets/snippets/")))

;; * yasnippet
;; - https://github.com/joaotavora/yasnippet
;; - History
;;   -  2020-08-18 Created
(when (bk/add-load-path "modes/prog" "yasnippet")
  (bk-auto-loads "yasnippet" #'yas-global-mode)
  (add-hook 'after-init-hook (lambda ()
                               (yas-global-mode +1)
                               (diminish 'yas-minor-mode)
                               (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand))))

;; * flycheck
;; - https://github.com/flycheck/flycheck
;; - History
;;   -  2020-08-15 Created
(defun bk-setup-feature-flycheck ()
  "Customizations for flycheck."
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch)
        flycheck-display-errors-delay 0.25))

(when (bk/add-load-path "modes/prog" "flycheck")
  (bk-auto-loads "flycheck" #'flycheck-mode)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (with-eval-after-load 'flycheck
    (bk-setup-feature-flycheck)))

;; * toggle-test
;; - History
;;   -  2020-08-17 Created
(when (bk/add-load-path "modes/prog" "toggle-test")
  (bk-auto-loads "toggle-test" #'tgt-toggle)
  (global-set-key (kbd "s-t") #'tgt-toggle)
  (setq tgt-open-in-new-window nil)
  (put 'tgt-projects 'safe-local-variable #'listp))

;; * quickrun
;; - https://github.com/emacsorphanage/quickrun
;; - History
;;   - 2020-10-12 Created
(when (bk/add-load-path "modes/prog" "quickrun")
  (bk-auto-loads "quickrun" #'quickrun #'quickrun-region))

;; * column-enforce-mode
;; - https://github.com/jordonbiondo/column-enforce-mode
;; - History
;;   - 2020-10-14 Created
(when (bk/add-load-path "modes/prog" "column-enforce-mode")
  (bk-auto-loads "column-enforce-mode" #'column-enforce-mode)
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq column-enforce-column 100)
              (column-enforce-mode +1)
              (diminish 'column-enforce-mode))))

;; * package-lint
;; - History
;;   - 2020-10-18 Created
(when (bk/add-load-path "modes/prog" "package-lint")
  (bk-auto-loads "package-lint" #'package-lint-current-buffer))

(provide 'init.el)
;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
