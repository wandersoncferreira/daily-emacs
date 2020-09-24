;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-24 00:45:20 (wand)>

;;; Code:

;; * custom functions
;;; smart beg/end of line
(defun bk/beginning-of-line ()
  "Go back at the first non-whitespace character."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun bk/end-of-line ()
  "Go to the end of the last non-whitespace character."
  (interactive)
  (move-end-of-line nil)
  (re-search-backward "^\\|[^[:space:]]")
  (forward-char))

(global-set-key (kbd "C-a") 'bk/beginning-of-line)
(global-set-key (kbd "C-e") 'bk/end-of-line)

(defun bk/eval-buffer ()
  "Provide some feedback after evaluating the buffer."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'bk/eval-buffer)

;; * basic customizations
;; basics
(setq tab-always-indent 'complete
      vc-follow-symlinks t
      create-lockfiles nil
      backup-directory-alist `(("." . ,(expand-file-name
                                        (concat user-emacs-directory "backups")))))

;;; don't use tabs to indent
(setq-default indent-tabs-mode nil)

;;; newline at the end of file
(setq require-final-newline t)

(setq blink-matching-paren nil)

;;; reduce the frequency of garbage collection
(setq gc-cons-threshold 50000000)

;;; timestamps
(setq time-stamp-active t
      time-stamp-line-limit 10
      time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S (%u)")

(add-hook 'write-file-functions 'time-stamp)

;;; abbreviate yes-or-no questions
(fset 'yes-or-no-p 'y-or-n-p)

;;; change column automatic wrap
(setq-local fill-column 70)

;;; improve scroll functions
(defun bk/scroll-up ()
  "Scroll only specific amount of lines."
  (interactive)
  (scroll-up-command 8))

(defun bk/scroll-down ()
  "Scroll only specific amount of lines."
  (interactive)
  (scroll-down-command 8))

(global-set-key (kbd "C-v") #'bk/scroll-up)
(global-set-key (kbd "M-v") #'bk/scroll-down)

;;; go back to last marked place
(global-set-key (kbd "C-x p") 'pop-to-mark-command)

;;; delete selected text with a key..
(add-hook 'after-init-hook 'delete-selection-mode)

;;; supress the warning about cl package
;;; right now, the following files are not updated with the new cl-lib:
;;; - sesman-test.el
;;; - change-inner.el
;;; - ert.el
;;; - dash.el
;;; - flycheck-test.el
;;; - queue.el
;;; - multiple-cursors-steps.el
(setq byte-compile-warnings '(cl-functions))

;;; set a register at a specific point
(defun bk/point-to-register ()
  "Store cursor position in a register."
  (interactive)
  (point-to-register 8)
  (message "Point set"))

(global-set-key (kbd "C-c r p") 'bk/point-to-register)

;;; jump to register
(defun bk/jump-to-register ()
  "Switch between current position and pos stored."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(global-set-key (kbd "C-c r j") 'bk/jump-to-register)

;; * line numbers (disabled)
;; - History
;; - 2020-09-13 - Disabling it
(when not-disabled?
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))


;; * abbreviations
(defun bk/add-region-local-abbrev (start end)
  "Move from START to END and add the selected text to a local abbrev."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
        (add-mode-abbrev num-words)
        (deactivate-mark))
    (message "No selected region!")))

(defun bk/add-region-global-abbrev (start end)
  "Go from START to END and add the selected text to global abbrev."
  (interactive "r")
  (if (use-region-p)
      (let ((num-words (count-words-region start end)))
        (add-abbrev global-abbrev-table "Global" num-words)
        (deactivate-mark))
    (message "No selected region!")))

(define-abbrev-table 'global-abbrev-table
  '(
    ("reuslt" "result" nil 0)
    ("requie" "require" nil 0)
    ("requier" "require" nil 0)
    ))

(setq abbrev-file-name "~/.emacs.d/core/etc/abbrev_defs")

(add-hook 'prog-mode-hook
          (lambda ()
            (abbrev-mode +1)
            (diminish 'abbrev-mode)))

;; * which-key
;; - https://github.com/justbur/emacs-which-key
;; - History
;;   -  2020-08-28 Created
(defun bk-setup-feature-which-key ()
  "Customizations to which-key mode."
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode)
  (diminish 'which-key-mode))

(when (bk/add-load-path "core" "emacs-which-key")
  (bk-auto-loads "which-key" #'which-key-mode)
  (add-hook 'after-init-hook #'bk-setup-feature-which-key))

;; * dired
;; - History
;; - 2020/09/20 Created
;; xdg-open
(defun bk/dired-xdg-open ()
  "Open the file at point with xdg-open."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "O") 'bk/dired-xdg-open))

;;; open dired in the current file
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; * exec-path-from-shell
;; - https://github.com/purcell/exec-path-from-shell
;; - History
;;   -  2020-08-16 Created
(when (bk/add-load-path "core" "exec-path-from-shell")
  (bk-auto-loads "exec-path-from-shell" #'exec-path-from-shell-initialize)
  (add-hook 'after-init-hook #'exec-path-from-shell-initialize))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
