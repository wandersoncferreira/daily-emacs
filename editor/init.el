;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(load-file "~/.emacs.d/editor/functions.el")

;; * toggle map
;; extracted from Malabarba's http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(define-prefix-command 'bk/toggle-map)
(define-key ctl-x-map "t" 'bk/toggle-map)

;; * expand-region.el
;; - https://github.com/magnars/expand-region.el
;; - History
;;   -  2020-08-16 Created
(when (bk/add-load-path "editor" "expand-region.el")
  (bk-auto-loads "expand-region" #'er/expand-region)
  (global-set-key (kbd "C-'") #'er/expand-region))

;; * change-inner
;; - https://github.com/magnars/change-inner.el
;; - History
;;   -  2020-08-15 Created
(when (bk/add-load-path "editor" "change-inner.el")
  (bk-auto-loads "change-inner" #'change-inner #'change-outer)
  (global-set-key (kbd "M-i") #'change-inner)
  (global-set-key (kbd "M-o") #'change-outer))

;; * multiple-cursors.el
;; - https://github.com/magnars/multiple-cursors.el
;; - History
;;   -  2020-08-16 Created
(when (bk/add-load-path "editor" "multiple-cursors.el")
  (bk-auto-loads "multiple-cursors" #'mc/mark-next-like-this #'mc/mark-previous-like-this)
  (setq mc/list-file "~/.emacs.d/editor/etc/.mc-lists.el")
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

;; * fix-word
;; - https://github.com/mrkkrp/fix-word
;; - History
;;   -  2020-08-17 Created
(when (bk/add-load-path "editor" "fix-word")
  (bk-auto-loads "fix-word"
                 #'fix-word-upcase
                 #'fix-word-downcase #'fix-word-capitalize)
  (global-set-key (kbd "M-u") #'fix-word-upcase)
  (global-set-key (kbd "M-l") #'fix-word-downcase)
  (global-set-key (kbd "M-c") #'fix-word-capitalize))

;; * easy-kill
;; - https://github.com/leoliu/easy-kill
;; - History
;;   -  2020-09-15 Created
(when (bk/add-load-path "editor" "easy-kill")
  (bk-auto-loads "easy-kill" #'easy-kill #'easy-mark)
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; * highlight-symbol.el
;; - https://github.com/nschum/highlight-symbol.el
;; - History
;;   -  2020-10-07 Created
(when (bk/add-load-path "editor" "highlight-symbol.el")
  (bk-auto-loads "highlight-symbol" #'highlight-symbol #'highlight-symbol-next #'highlight-symbol-prev)
  (global-set-key [(control f1)] 'highlight-symbol)
  (global-set-key [f1] 'highlight-symbol-next)
  (global-set-key [(shift f1)] 'highlight-symbol-prev))

;; * goto-chg
;; - https://github.com/emacs-evil/goto-chg
;; - History
;;   - 2020-10-12 Created
(when (bk/add-load-path "editor" "goto-chg")
  (bk-auto-loads "goto-chg" #'goto-last-change #'goto-last-change-reverse)
  (global-set-key (kbd "C-c ,") 'goto-last-change)
  (global-set-key (kbd "C-c .") 'goto-last-change-reverse))

;; * vimish-fold
;; - https://github.com/matsievskiysv/vimish-fold
;; - History
;;   - 2020-11-16 Created
(defun bk-setup-feature-fold ()
  "Customizations for Vimish fold."
  (interactive)
  (vimish-fold-mode +1)
  (bk/vimish-fold-all-defn))

(when (bk/add-load-path "editor" "vimish-fold")
  (bk-auto-loads "vimish-fold" #'vimish-fold-toggle #'vimish-fold-mode))

(add-hook 'clojure-mode-hook #'bk-setup-feature-fold)
(define-key bk/toggle-map "f" #'vimish-fold-toggle)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
