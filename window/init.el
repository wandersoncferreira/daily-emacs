;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 21:03:24 (wand)>

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

;;; toggle window from
;;; Window A ++++++++ Window B
;;; Window A + Window B
(defun bk/toggle-window-split ()
  "Toggle window."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c |") 'bk/toggle-window-split)

(defun bk/kill-buffer-and-file (buffer-name)
  "Remove file connected to current buffer and kill the BUFFER-NAME."
  (interactive "bKill buffer and its file: ")
  (let* ((buffer (get-buffer buffer-name))
         (filename (buffer-file-name buffer)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" buffer-name)
      (delete-file filename)
      (kill-buffer buffer))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
