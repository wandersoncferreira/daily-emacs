;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 21:29:21 (wand)>

;;; Code:

(load-file "~/.emacs.d/window/functions.el")

;;; auto revert buffers if the file underneath it gets modified
(add-hook 'after-init-hook 'global-auto-revert-mode)

;;; allow ad-handle-redefinition
;;; got from here https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;;; I dont like that warning telling me that ad-handle-definition was changed.. that's ok.
(setq ad-redefinition-action 'accept)

;; * switch-window
;; - https://github.com/dimitri/switch-window
;; - History
;;   -  2020-08-16 Created
(when (bk/add-load-path "window" "switch-window")
  (bk-auto-loads "switch-window" #'switch-window)
  (global-set-key (kbd "C-x o") 'switch-window)
  (with-eval-after-load 'switch-window
    (setq-default switch-window-shortcut-style 'alphabet
                  switch-window-timeout nil)))

;; * ace-window
;; - History
;;  - 2020/08/27 Created
(when (bk/add-load-path "window" "ace-window")
  (bk-auto-loads "ace-window" #'ace-window))

;; * zoom-frm
;; - https://github.com/emacsmirror/zoom-frm
;; - History
;; - 2020-09-15 Created
(when (bk/add-load-path "window" "zoom-frm")
  (bk-auto-loads "zoom-frm" #'zoom-in #'zoom-out))

;;; kill current buffer
(global-set-key (kbd "C-x k") 'bk/kill-buffer)

;; * winner
;; - History
;;   - 2020-10-12 Adding configurations
(defun bk-setup-feature-winner ()
  "Customizations to winner."
  (interactive)
  (setq winner-dont-bind-my-keys t
        winner-boring-buffers
        '("*Completions*"
	  "*Compile-Log*"
	  "*inferior-lisp*"
	  "*Fuzzy Completions*"
	  "*Apropos*"
	  "*Help*"
	  "*cvs*"
	  "*Buffer List*"
	  "*Ibuffer*"
	  "*esh command on file*"))
  (winner-mode +1))

(add-hook 'after-init-hook #'bk-setup-feature-winner)
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 U") 'winner-redo)

;; * windresize
;; - History
;;   -  2020-08-17 Created
(when (bk/add-load-path "window" "windresize")
  (bk-auto-loads "windresize" #'windresize))

;;; improve split windows
(global-set-key (kbd "C-x 2") 'bk/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'bk/hsplit-last-buffer)

;;; use shift+arrow keys to switch between visible buffers
(add-hook 'after-init-hook
          (lambda ()
            (windmove-default-keybindings)))

;;; toggle window from
;;; Window A ++++++++ Window B
;;; Window A + Window B

(global-set-key (kbd "C-c |") 'bk/toggle-window-split)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
