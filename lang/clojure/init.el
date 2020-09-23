;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-22 22:02:43 (wand)>

;;; Code:

;; * clojure mode
;; - History
;;   -  2020-08-14 Created
(when (bk/add-load-path "lang/clojure" "clojure-mode")
  (bk-auto-loads "clojure-mode"
                 '("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
                 '("\\.cljc\\'" . clojurec-mode)
                 '("\\.cljx\\'" . clojurex-mode)
                 '("\\.cljs\\'" . clojurescript-mode)
                 '("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)))

;; * cider mode
;; - History
;;   -  2020-08-14 Created
;;   -  2020-08-18 Adding key binding to cider-jack-in
(defun bk/nrepl-warn-when-not-connected ()
  "Function to warn me to start the REPL."
  (interactive)
  (message "Oops! You're not connected to an nREPL server.
Please run M-x cider or M-x cider-jack-in to connect"))

(when (bk/add-load-path "lang/clojure" "cider")
  (bk-auto-loads "cider"
                 #'cider-jack-in
                 #'cider-connect
                 #'cider-jack-in-clj&cljs)
  (bk-auto-loads "cider-macroexpansion" #'cider-macroexpand-1)
  (bk-auto-loads "cider-find" #'cider-find-var)
  (with-eval-after-load 'clojure-mode
    (setq cider-save-file-on-load t
          cider-auto-select-error-buffer t
          cider-auto-select-test-report-buffer nil
          cider-repl-pop-to-buffer-on-connect nil)
    (defalias 'cquit 'cider-quit)
    (define-key clojure-mode-map (kbd "C-c M-j") #'cider-jack-in)
    (define-key clojure-mode-map (kbd "C-x C-e") 'bk/nrepl-warn-when-not-connected)
    (define-key clojure-mode-map (kbd "C-c C-k") 'bk/nrepl-warn-when-not-connected)
    (define-key clojure-mode-map (kbd "C-c C-z") 'bk/nrepl-warn-when-not-connected)
    (diminish 'cider-mode)))

;; * clj-refactor.el
;; - https://github.com/clojure-emacs/clj-refactor.el
;; - History
;;   -  2020-08-18 Created
;;   -  2020-09-02 Create function to setup and hook in clojure mode
(defun bk-setup-feature-clj-refactor ()
  "Customizations for Clj refactor."
  (clj-refactor-mode +1)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (diminish 'clj-refactor-mode))

(when (bk/add-load-path "lang/clojure" "clj-refactor.el")
  (bk-auto-loads "clj-refactor.el" #'clj-refactor-mode #'cljr-add-keybindings-with-prefix)
  (add-hook 'clojure-mode-hook #'bk-setup-feature-clj-refactor))

;; * cljr-ivy
;; - https://github.com/wandersoncferreira/cljr-ivy
;; - History
;;   -  2020-09-03 Created
(when (bk/add-load-path "lang/clojure" "cljr-ivy")
  (bk-auto-loads "cljr-ivy" #'cljr-ivy)
  (with-eval-after-load 'clojure-mode
    (define-key clojure-mode-map (kbd "C-c C-r") #'cljr-ivy)))

;; * flycheck-clj-kondo
;; - https://github.com/borkdude/flycheck-clj-kondo
;; - History
;;   -  2020-08-18 Created
(when (bk/add-load-path "lang/clojure" "flycheck-clj-kondo")
  (bk-auto-loads "flycheck-clj-kondo" #'flycheck-clj-kondo)
  (with-eval-after-load 'clojure-mode
    (require 'flycheck-clj-kondo)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
