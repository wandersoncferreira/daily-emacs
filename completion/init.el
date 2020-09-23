;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-22 21:54:54 (wand)>

;;; Code:

;; * company-mode
;; - https://github.com/company-mode/company-mode
;; - History
;;   -  2020-08-16 Created
;;   -  2020-08-17 Remap C-M-S and C-s to filter list of results
(when (bk/add-load-path "completion" "company-mode")
  (bk-auto-loads "company" #'global-company-mode)
  (add-hook 'after-init-hook #'global-company-mode)
  (with-eval-after-load 'company

    (setq company-show-numbers t
          company-idle-delay 0.25
          company-minimum-prefix-length 2
          company-tooltip-limit 10
          company-tooltip-flip-when-above t
          company-tooltip-align-annotations t
          company-require-match 'never
          company-global-modes '(not org-mode))

    (define-key company-active-map [(control) (meta) ?s] 'company-search-candidates)
    (define-key company-active-map "\C-s" 'company-filter-candidates)
    (define-key company-active-map (kbd "C-/") 'counsel-company)
    (diminish 'company-mode)))

;; * company-postframe
;; - https://github.com/tumashu/company-posframe
;; - History
;;   -  2020-08-18 Created
(when (bk/add-load-path "completion" "company-posframe")
  (bk-auto-loads "company-posframe" #'company-posframe-mode)
  (with-eval-after-load 'company
    (setq company-posframe-show-indicator nil
          company-posframe-show-metadata nil
          company-posframe-show-params nil)
    (company-posframe-mode 1)
    (diminish 'company-posframe-mode)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
