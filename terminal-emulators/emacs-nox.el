;;; Emacs-nox --- Setup to run emacs inside a terminal
;;; Commentary:
;;; Code:

;; * emacs running inside a terminal emulator
(setq save-interprogram-paste-before-kill t
      x-select-enable-clipboard t
      select-enable-clipboard t)

(when (bk/add-load-path "core" "xclip")
  (bk-auto-loads "xclip" #'xclip-mode)
  (add-hook 'after-init-hook #'xclip-mode))

(defun bk/windmove (windmove-fn tmux-param)
  (interactive)
  (condition-case nil
      (funcall windmove-fn)
    (error (shell-command (concat "tmux select-pane " tmux-param)))))

(defun bk/windmove-up ()
  (interactive)
  (bk/windmove 'windmove-up "-U"))

(defun bk/windmove-down ()
  (interactive)
  (bk/windmove 'windmove-down "-D"))

(defun bk/windmove-left ()
  (interactive)
  (bk/windmove 'windmove-left "-L"))

(defun bk/windmove-right ()
  (interactive)
  (bk/windmove 'windmove-right "-R"))

(global-set-key (kbd "S-<down>") 'bk/windmove-down)
(global-set-key (kbd "S-<up>") 'bk/windmove-up)
(global-set-key (kbd "S-<left>") 'bk/windmove-left)
(global-set-key (kbd "S-<right>") 'bk/windmove-right)

(when (bk/add-load-path "core" "term-keys")
  (bk-auto-loads "term-keys" #'term-keys-mode)
  (add-hook 'after-init-hook #'term-keys-mode))

(defun bk/setup-Xresources ()
  "Setup Xresources to work properly with emacs-nox."
    (interactive)
    (require 'term-keys-urxvt)
    (with-temp-buffer
      (insert (term-keys/urxvt-xresources))
      (append-to-file (point-min) (point-max) "~/.Xresources")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; emacs-nox.el ends here
