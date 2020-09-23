;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-22 22:03:15 (wand)>

;;; Code:

;; * scala
;; - https://github.com/hvesalai/emacs-scala-mode
;; - History
;;  - 2020/08/27 Created
(when (bk/add-load-path "lang/scala" "emacs-scala-mode")
  (bk-auto-loads "scala-mode"
                 '("\\.s\\(cala\\|bt\\)$" . scala-mode)))

;; * sbt
;; - https://github.com/hvesalai/emacs-sbt-mode
;; - History
;;  - 2020/08/27 Created
(when (bk/add-load-path "lang/scala" "emacs-sbt-mode")
  (bk-auto-loads "sbt-mode" #'sbt-start #'sbt-command)
  (with-eval-after-load 'scala-mode
    (setq sbt:program-options '("-Dsbt.supershell=false"))))

;; * lsp-mode
;; - History
;;  - 2020/08/27 Created
(when (bk/add-load-path "lang/scala" "lsp-mode")
  (bk-auto-loads "lsp-mode" #'lsp #'lsp-lens-mode)
  (bk-auto-loads "lsp-modeline" #'lsp-modeline-diagnostics-mode)
  (add-hook 'scala-mode-hook #'lsp)
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (with-eval-after-load 'lsp-mode
    (setq lsp-prefer-flymake nil)))

;; * dap-mode
;; - History
;;  - 2020/08/27 Created
(when (bk/add-load-path "lang/scala" "dap-mode")
  (bk-auto-loads "dap-mode" #'dap-mode)
  (bk-auto-loads "dap-ui" #'dap-ui-mode)
  (bk-auto-loads "dap-mouse" #'dap-tooltip-mode)
  (with-eval-after-load 'lsp-mode
    (dap-mode +1)
    (dap-ui-mode +1)))

;; * treemacs
;; - History
;;  - 2020/08/27 Created
(when (bk/add-load-path "lang/scala" "treemacs/src/elisp")
  (bk-auto-loads "treemacs" #'treemacs))

;; * lsp-metals
;; - History
;;  - 2020/08/27 Created
(when (bk/add-load-path "lang/scala" "lsp-metals")
  (bk-auto-loads "lsp-metals" #'lsp-metals)
  (bk-auto-loads "lsp-metals-treeview" #'lsp-metals-treeview-mode))

;; * lsp-Treemacs
;; - History
;;  - 2020/08/27 Created
(when (bk/add-load-path "lang/scala" "lsp-treemacs")
  (bk-auto-loads "lsp-treemacs")
  (with-eval-after-load 'lsp-mode
    (lsp-metals-treeview-mode +1)
    (setq lsp-metals-treeview-show-when-views-received t)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
