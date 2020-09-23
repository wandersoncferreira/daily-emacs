;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 00:23:28 (wand)>

;;; Code:

;; * pyvenv
;; - History
;; - 2020-09-21 Created
(when (bk/add-load-path "langs/python" "pyvenv")
  (bk-auto-loads "pyvenv" #'pyvenv-activate))

;; * highlight-identation
;; - History
;; - 2020-09-21 Created
(when (bk/add-load-path "langs/python" "Highlight-Indentation-for-Emacs")
  (bk-auto-loads "highlight-identation"))

;; * elpy
;; - History:
;; - 2020-09-21 Created
(defun bk-setup-feature-elpy ()
  "Customizations for elpy."
  (pyvenv-activate "~/miniconda3")
  (delete `elpy-module-django elpy-modules)
  (delete `elpy-module-highlight-indentation elpy-modules))

(when (bk/add-load-path "langs/python" "elpy")
  (bk-auto-loads "elpy" #'elpy-enable #'elpy-modules))

(with-eval-after-load 'python
  (elpy-enable)
  (bk-setup-feature-elpy))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
