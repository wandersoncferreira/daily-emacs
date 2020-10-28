;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-28 15:26:51 (wand)>

;;; Code:

(load-file "~/.emacs.d/core/functions.el")

(global-set-key (kbd "C-a") 'bk/beginning-of-line)
(global-set-key (kbd "C-e") 'bk/end-of-line)

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

(global-set-key (kbd "C-c r p") 'bk/point-to-register)
(global-set-key (kbd "C-c r j") 'bk/jump-to-register)

;; * line numbers (disabled)
;; - History
;; - 2020-09-13 - Disabling it
(when not-disabled?
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

;; * abbreviations
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
(defun bk-setup-feature-dired ()
  "Customizations for dired."
  (interactive)
  (require 'dired)
  (require 'dired-x)

  (setq dired-dwim-target t
        dired-omit-files "^\\...+$")

  ;;; open dired in the current file
  (global-set-key (kbd "C-x C-j") 'dired-jump)
  
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (eval-after-load 'dired '(define-key dired-mode-map (kbd "O") 'bk/dired-xdg-open)))

(add-hook 'after-init-hook #'bk-setup-feature-dired)

;; * exec-path-from-shell
;; - https://github.com/purcell/exec-path-from-shell
;; - History
;;   -  2020-08-16 Created
;;   -  2020-10-15 Setup function
(defun bk-setup-feature-path ()
  "Customizations for path variable."
  (interactive)
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(when (bk/add-load-path "core" "exec-path-from-shell")
  (bk-auto-loads "exec-path-from-shell" #'exec-path-from-shell-initialize)
  (add-hook 'after-init-hook #'bk-setup-feature-path))


(define-advice goto-line (:before (&rest _) preview-line-number)
  "Preview line number when prompting for goto-line."
  (interactive
   (lambda (spec)
     (if (and (boundp 'display-line-numbers)
              (not display-line-numbers))
         (unwind-protect
             (progn (display-line-numbers-mode)
                    (advice-eval-interactive-spec spec))
           (display-line-numbers-mode -1))
       (advice-eval-interactive-spec spec)))))

;; say you copied a link from your web browser, then switched to Emacs
;; to paste it somewhere. Before you do that, you notice something you want
;; to kill. Doing that will place the last kill to the clipboard, thus
;; overriding the thing you copied earlier. We can have a kill ring solution
(setq save-interprogram-paste-before-kill t)

;; * recentf
;; - History
;;   - 2020-10-12 Created
(defun bk-setup-feature-recentf ()
  "Customizations to recentf."
  (interactive)
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 15
        recentf-save-file "~/.emacs.d/core/etc/recentf"
        recentf-show-file-shortcuts-flag nil
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(add-hook 'after-init-hook #'bk-setup-feature-recentf)

;; * savehist
;; - History
;;   - 2020-10-12 Created
(defun bk-setup-feature-savehist ()
  "Customizations to savehist."
  (interactive)
  (setq savehist-file "~/.emacs.d/core/etc/savehist"
        history-length 30000
        history-delete-duplicates nil
        savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-save-minibuffer-history t)
  (savehist-mode +1))

(add-hook 'after-init-hook #'bk-setup-feature-savehist)

;;; when two buffers have the same name, we need a way to distinguish them
;; * uniquify
;; - History
;;   - 2020-10-12 Created
(defun bk-setup-feature-uniquify ()
  "Customizations to uniquify."
  (interactive)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-separator " * "
        uniquify-after-kill-buffer-p t
        uniquify-strip-common-suffix t
        uniquify-ignore-buffers-re "^\\*"))

(add-hook 'after-init-hook #'bk-setup-feature-uniquify)

;; * imenu
;; - History
;;   - 2020-10-12 Created
(defun bk-setup-feature-imenu ()
  "Customizations to imenu."
  (interactive)
  (setq imenu-auto-rescan t
        imenu-auto-rescan-maxout 600000
        imenu-max-item-length 600
        imenu-use-markers t
        imenu-max-items 200))

(global-set-key (kbd "C-c i") 'counsel-imenu)

;;; bookmarks
(setq bookmark-default-file "~/.emacs.d/core/etc/bookmarks")

;; pop to mark
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(when (bk/add-load-path "core" "scratch-el")
  (bk-auto-loads "scratch" #'scratch #'scratch-create-buffer-hook)
  (add-hook 'scratch-create-buffer-hook 'prot/scratch-buffer-setup)
  (global-set-key (kbd "C-c s") 'scratch))

(when (bk/add-load-path "core" "system-packages")
  (bk-auto-loads "system-packages"
                 #'system-packages-search
                 #'system-packages-update)
  (setq system-packages-use-sudo t))

(defvar prot/window-configuration nil
  "Current window configuration.")

(define-minor-mode bk/window-single-toggle
  "Toggle between multiple windows and single window.
This is the equivalent of maximising a window. Got from Protesilaos."
  :lighter " [M]"
  :global nil
  (if (one-window-p)
      (when prot/window-configuration
        (set-window-configuration prot/window-configuration))
    (setq prot/window-configuration (current-window-configuration))
    (delete-other-windows)))

(global-set-key (kbd "s-m") 'bk/window-single-toggle)

;;; utf-8 support
(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      locale-coding-system 'utf-8)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
