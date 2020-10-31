;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

;; * wgrep
;; - History
;;  - 2020/09/20 Created
(when (bk/add-load-path "search" "wgrep")
  (bk-auto-loads "wgrep" #'wgrep-change-to-wgrep-mode))

;; * grep
;; - History
;; - 2020/09/20 Created
(defun bk-setup-feature-grep ()
  "Customizations for grep."
  (let ((blocked-dirs '("tmp" "target" "elpa" "workspace" ".cache"
                        "data" "node_modules"))
        (blocked-files '("ido.last" "smex-items" "recentf" "dmenu-items")))
    (eval-after-load 'grep
      '(progn
         (dolist (it blocked-dirs)
           (add-to-list 'grep-find-ignored-directories it))
         (dolist (et blocked-files)
           (add-to-list 'grep-find-ignored-files et))))))

(add-hook 'after-init-hook #'bk-setup-feature-grep)

;; * avy
;; - History
;;  - 2020/08/27 Created
(when (bk/add-load-path "search" "avy")
  (bk-auto-loads "avy" #'avy-goto-char)
  (global-set-key (kbd "C-;") #'avy-goto-char))

;; * jump-char
;; - https://github.com/lewang/jump-char
;; - History
;;   -  2020-08-16 Created
(when (bk/add-load-path "search" "jump-char")
  (bk-auto-loads "jump-char" #'jump-char-forward #'jump-char-backward)
  (global-set-key (kbd "M-n") 'jump-char-forward)
  (global-set-key (kbd "M-p") 'jump-char-backward))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
