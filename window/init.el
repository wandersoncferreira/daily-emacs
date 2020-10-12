;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 10:40:16 (wand)>

;;; Code:

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
(defun bk/kill-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'bk/kill-buffer)

;;; winner mode is a global minor mode that records
;;; the changes in the window configuration
(add-hook 'after-init-hook #'winner-mode)
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 U") 'winner-redo)

;; * windresize
;; - History
;;   -  2020-08-17 Created
(when (bk/add-load-path "window" "windresize")
  (bk-auto-loads "windresize" #'windresize))

;;; improve split windows
(defun bk/vsplit-last-buffer ()
  "Split the window vertically and display the previous buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun bk/hsplit-last-buffer ()
  "Split the window horizontally and display the previous buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'bk/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'bk/hsplit-last-buffer)

;;; use shift+arrow keys to switch between visible buffers
(add-hook 'after-init-hook
          (lambda ()
            (windmove-default-keybindings)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
