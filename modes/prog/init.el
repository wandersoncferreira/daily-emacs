;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 08:25:25 (wand)>

;;; Code:

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

;; * yasnippet
;; - https://github.com/joaotavora/yasnippet
;; - History
;;   -  2020-08-18 Created
(when (bk/add-load-path "modes/prog" "yasnippet")
  (bk-auto-loads "yasnippet" #'yas-global-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (yas-global-mode +1)
                              (diminish 'yas-minor-mode))))

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

;; * flycheck-posframe
;; - https://github.com/alexmurray/flycheck-posframe
;; - History
;;   -  2020-09-15 Created
(when (bk/add-load-path "modes/prog" "flycheck-posframe")
  (bk-auto-loads "flycheck-posframe" #'flycheck-posframe-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; * toggle-test
;; - History
;;   -  2020-08-17 Created
(when (bk/add-load-path "modes/prog" "toggle-test")
  (bk-auto-loads "toggle-test" #'tgt-toggle)
  (global-set-key (kbd "s-t") #'tgt-toggle)
  (setq tgt-open-in-new-window nil)
  (put 'tgt-projects 'safe-local-variable #'listp))


(provide 'init.el)
;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
