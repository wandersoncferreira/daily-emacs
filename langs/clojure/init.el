;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(load-file "~/.emacs.d/langs/clojure/functions.el")

;; * clojure-mode
;; - History
;;   -  2020-08-14 Created
(defun bk-setup-feature-clojure ()
  "Set up clojure-y things."
  (subword-mode 1)
  (setq company-backends
        '(company-capf company-dabbrev-code company-keywords company-files))
  (diminish 'subword-mode))

(when (bk/add-load-path "langs/clojure" "clojure-mode")
  (bk-auto-loads "clojure-mode"
                 '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
                 '("\\.cljc\\'" . clojurec-mode)
                 '("\\.cljx\\'" . clojurex-mode)
                 '("\\.cljs\\'" . clojurescript-mode)
                 '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode))
  (add-hook 'clojure-mode-hook #'bk-setup-feature-clojure))

;; * cider mode
;; - History
;;   -  2020-08-14 Created
;;   -  2020-08-18 Adding key binding to cider-jack-in
(when (bk/add-load-path "langs/clojure" "cider")
  (bk-auto-loads "cider"
                 #'cider-jack-in
                 #'cider-connect
                 #'cider-jack-in-clj&cljs)
  (setq nrepl-use-ssh-fallback-for-remote-hosts t)
  (bk-auto-loads "cider-macroexpansion" #'cider-macroexpand-1)
  (bk-auto-loads "cider-find" #'cider-find-var)
  (bk-auto-loads "cider-scratch" #'cider-scratch)
  
  (add-hook 'cider-mode-hook 'eldoc-mode)
  
  (with-eval-after-load 'clojure-mode
    (setq cider-save-file-on-load t
          cider-auto-select-error-buffer t
          cider-mode-line-show-connection nil
          cider-font-lock-dynamically nil
          cider-auto-select-test-report-buffer nil
          cider-repl-pop-to-buffer-on-connect nil)
    (defalias 'cquit 'cider-quit)
    (define-key clojure-mode-map (kbd "C-c M-j") #'cider-jack-in)
    (define-key clojure-mode-map (kbd "C-x C-e") 'bk/nrepl-warn-when-not-connected)
    (define-key clojure-mode-map (kbd "C-c C-k") 'bk/nrepl-warn-when-not-connected)
    (define-key clojure-mode-map (kbd "C-c C-z") 'bk/nrepl-warn-when-not-connected)
    (diminish 'cider-mode))

  (eval-after-load "cider-repl"
    '(progn
       (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
       (define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-return)
       (setq cider-repl-tab-command #'company-indent-or-complete-common)))

  (with-eval-after-load 'cider
    (define-key cider-mode-map (kbd "C-c C-a") 'cider-eval-n-defuns)))

;; * clj-refactor.el
;; - https://github.com/clojure-emacs/clj-refactor.el
;; - History
;;   -  2020-08-18 Created
;;   -  2020-09-02 Create function to setup and hook in clojure mode
(defun bk-setup-feature-clj-refactor ()
  "Customizations for Clj refactor."
  (setq cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures]]"
        cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures]]"
        cljr-eagerly-build-asts-on-startup nil
        cljr-warn-on-eval nil)
  (clj-refactor-mode +1)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (diminish 'clj-refactor-mode))

(when (bk/add-load-path "langs/clojure" "clj-refactor.el")
  (bk-auto-loads "clj-refactor.el" #'clj-refactor-mode #'cljr-add-keybindings-with-prefix)
  (add-hook 'clojure-mode-hook #'bk-setup-feature-clj-refactor))

;; * cljr-ivy
;; - https://github.com/wandersoncferreira/cljr-ivy
;; - History
;;   -  2020-09-03 Created
(when (bk/add-load-path "langs/clojure" "cljr-ivy")
  (bk-auto-loads "cljr-ivy" #'cljr-ivy)
  (with-eval-after-load 'clojure-mode
    (define-key clojure-mode-map (kbd "C-c C-r") #'cljr-ivy)))

;; * flycheck-clj-kondo
;; - https://github.com/borkdude/flycheck-clj-kondo
;; - History
;;   -  2020-08-18 Created
(when (bk/add-load-path "langs/clojure" "flycheck-clj-kondo")
  (bk-auto-loads "flycheck-clj-kondo" #'flycheck-clj-kondo)
  (with-eval-after-load 'clojure-mode
    (require 'flycheck-clj-kondo)))

;; * clj-decompiler
;; - https://github.com/bsless/clj-decompiler.el
;; - History
;;   - 2020-10-12 Created
(when (bk/add-load-path "langs/clojure" "clj-decompiler.el")
  (bk-auto-loads "clj-decompiler" #'clj-decompiler-setup)
  (eval-after-load 'cider '(clj-decompiler-setup)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
