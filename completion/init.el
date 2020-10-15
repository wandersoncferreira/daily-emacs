;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-14 22:49:43 (wand)>

;;; Code:

(load-file "~/.emacs.d/completion/functions.el")

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
          company-echo-delay 0.5
          company-minimum-prefix-length 2
          company-dabbrev-downcase nil
          company-tooltip-limit 12
          company-transformers '(company-sort-by-occurrence)
          company-selection-wrap-around t
          company-tooltip-flip-when-above t
          company-tooltip-align-annotations t
          company-require-match 'never
          company-global-modes '(not org-mode))

    (define-key company-active-map [(control) (meta) ?s] 'company-search-candidates)
    (define-key company-active-map "\C-s" 'company-filter-candidates)
    (define-key company-active-map (kbd "C-/") 'counsel-company)
    (diminish 'company-mode)))

;; * ivy
;; - https://github.com/abo-abo/swiper
;; - History
;;   -  2020-08-28 Created
;;   -  2020-09-14 Add swiper binding
(when (bk/add-load-path "completion" "swiper")
  (bk-auto-loads "ivy" #'ivy-mode)
  (bk-auto-loads "counsel" #'counsel-find-file)
  (bk-auto-loads "swiper" #'swiper)
  (add-hook 'after-init-hook #'ivy-mode)
  (with-eval-after-load 'ivy

    (setq ivy-use-virtual-buffers t
          ivy-case-fold-search-default t
          enable-recursive-minibuffers t
          ivy-count-format "(%d/%d) "
          ivy-re-builders-alist '((t . ivy--regex-plus))
          ivy-initial-inputs-alist nil)

    (global-set-key (kbd "C-s") #'swiper)
    (global-set-key (kbd "C-r") #'swiper)
    (global-set-key (kbd "C-x C-f") #'counsel-find-file)
    (global-set-key (kbd "C-x B") #'ivy-switch-buffer-other-window)

    (diminish 'ivy-mode)))

;; * counsel
;; - https://github.com/abo-abo/swiper
;; - History
;;   -  2020-08-28 Created
(when (bk/add-load-path "completion" "swiper")
  (bk-auto-loads "counsel" #'counsel-M-x)
  (global-set-key (kbd "M-x") #'counsel-M-x)
  (global-set-key (kbd "C-x C-m") 'counsel-M-x)
  (global-set-key (kbd "C-c g p") 'counsel-git-grep))

;; * smex
;; - https://github.com/nonsequitur/smex
;; - History
;;   -  2020-08-15 Created
(when (and (bk/add-load-path "completion" "smex") not-disabled?)
  (bk-auto-loads "smex" #'smex)
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "C-x C-m") 'smex)
  (setq smex-save-file "~/.emacs.d/completion/etc/smex-items")
  (with-eval-after-load 'smex
    (smex-initialize)))

;; * occur
;; - History
;;   - 2020-10-12 Created

(global-set-key (kbd "M-s o") 'bk/occur-dwim)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
