;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 00:08:11 (wand)>

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

;; * ivy
;; - https://github.com/abo-abo/swiper
;; - History
;;   -  2020-08-28 Created
;;   -  2020-09-14 Add swiper binding
(when (bk/add-load-path "completion" "swiper")
  (bk-auto-loads "ivy" #'ivy-mode)
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
  (global-set-key (kbd "C-x C-m") 'counsel-M-x))

;; * ivy-postframe
;; - https://github.com/tumashu/ivy-posframe
;; - History
;;   -  2020-08-18 Created
(when (bk/add-load-path "completion" "ivy-posframe")
  (bk-auto-loads "ivy-posframe" #'ivy-posframe-mode)
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-bottom-left)))
  (setq ivy-posframe-height-alist '((t . 13)))
  (setq ivy-posframe-hide-minibuffer t
        ivy-posframe-border-width 3)
  (setq ivy-posframe-parameters '((left-fringe . 8)
                                  (right-fringe . 8)))
  (add-hook 'after-init-hook
            (lambda ()
              (ivy-posframe-mode)
              (diminish 'ivy-posframe-mode))))

;; * smex
;; - https://github.com/nonsequitur/smex
;; - History
;;   -  2020-08-15 Created
(when (and (bk/add-load-path "completion" "smex") not-disabled?)
  (bk-auto-loads "smex" #'smex)
  (global-set-key (kbd "M-x") #'smex)
  (global-set-key (kbd "C-x C-m") 'smex)
  (with-eval-after-load 'smex
    (smex-initialize)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
