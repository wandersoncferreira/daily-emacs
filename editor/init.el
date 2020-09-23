;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-22 23:47:52 (wand)>

;;; Code:

;; * expand-region.el
;; - https://github.com/magnars/expand-region.el
;; - History
;;   -  2020-08-16 Created
(when (bk-load-path-add "expand-region.el")
  (bk-auto-loads "expand-region" #'er/expand-region)
  (global-set-key (kbd "C-'") #'er/expand-region))

;; * change-inner
;; - https://github.com/magnars/change-inner.el
;; - History
;;   -  2020-08-15 Created
(when (bk-load-path-add "change-inner.el")
  (bk-auto-loads "change-inner" #'change-inner #'change-outer)
  (global-set-key (kbd "M-i") #'change-inner)
  (global-set-key (kbd "M-o") #'change-outer))

;; * multiple-cursors.el
;; - https://github.com/magnars/multiple-cursors.el
;; - History
;;   -  2020-08-16 Created
(when (bk-load-path-add "multiple-cursors.el")
  (bk-auto-loads "multiple-cursors" #'mc/mark-next-like-this #'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

;; * fix-word
;; - https://github.com/mrkkrp/fix-word
;; - History
;;   -  2020-08-17 Created
(when (bk-load-path-add "fix-word")
  (bk-auto-loads "fix-word"
                 #'fix-word-upcase
                 #'fix-word-downcase #'fix-word-capitalize)
  (global-set-key (kbd "M-u") #'fix-word-upcase)
  (global-set-key (kbd "M-l") #'fix-word-downcase)
  (global-set-key (kbd "M-c") #'fix-word-capitalize))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
