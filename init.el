;;; init.el --- Personal configuration ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-09-23 00:15:01 (wand)>

;;; Code:

;; * startup tricks
;; - History
;; - 2020-09-13 Organized into outlines
(let ((t0 (float-time)))
  (defun f-msg (msg)
    "MSG with time since start."
    (message "%s. Time elapsed: %.3fs" msg (- (float-time) t0))))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

(defvar bk--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist bk--file-name-handler-alist)))

(defvar not-disabled? nil)

;; * lazy loading
;; - History
;; - 2020-09-13 Organized into outlines

;;; helper functions to perform lazy loading of packages
(defvar bk-external-packages-dir "~/.emacs.d/external/")

;;; load lazy-loading helper functions
(load-file (expand-file-name "lazy-loading.el" user-emacs-directory))

;;; load dependencies
(load-file (expand-file-name "dependencies.el" user-emacs-directory))

;; * extra packages
;; - History
;; - 2020-09-22 Added clojure pack
(dolist (pkg '("langs"
               "modes"))
  (let* ((pkg-name (concat pkg "/loader.el"))
         (pkg-name (expand-file-name pkg-name user-emacs-directory)))
    (load-file pkg-name)))

(dolist (module '("completion"
                  "editor"
                  "projects"
                  "search"
                  "functions"))
  (let* ((module-name (concat module "/init.el"))
         (module-name (expand-file-name module-name user-emacs-directory)))
    (load-file module-name)))

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

(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

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

;;; use shift+arrow keys to switch between visible buffers
(add-hook 'after-init-hook
          (lambda ()
            (windmove-default-keybindings)))

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

;;; activate highlight parenthesis
(add-hook 'after-init-hook 'show-paren-mode)

;;; open this file easily
(set-register ?e '(file . "~/.emacs.d/init.el"))

;;; cheatsheet
(set-register ?c '(file . "~/.emacs.d/cheatsheet.org"))

;; * theming and Fonts
;; - History
;; - 2020-09-17 - Enabling Zenburn
;;; change font
(defun bk/set-monaco-font ()
  "Define the Monaco font."
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco" :height 110)))

;;; change themes
(defun bk/light-theme ()
  "Define custom light theme."
  (interactive)
  (bk/set-monaco-font)
  (set-face-attribute 'lazy-highlight nil :background "light green")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))

;; * themes
;; - History
;; - 2020-09-17 Added zenburn
(setq custom-theme-directory (concat user-emacs-directory "themes")
      custom-safe-themes t)

(dolist (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(add-hook 'after-init-hook
          (lambda ()
            (bk/light-theme)))

;;; supress unecessary things
;; (put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name))

(add-hook
 'after-init-hook
 (lambda ()
   (menu-bar-mode -1)
   (tool-bar-mode -1)
   (scroll-bar-mode -1)
   (column-number-mode)
   (size-indication-mode)))

;; * outline
;; - History
;; - 2020-09-13 Created
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (outline-minor-mode +1)
            (diminish 'outline-minor-mode)
            (setq outline-blank-line t)
            (setq-local outline-regexp ";; \\*")))


;; * whitespace
;; - History
;; - 2020-09-15 Created
;;; whitespace-mode config
(defun bk-setup-feature-whitespace ()
  "Customizations for `whitespace-mode'."
  (setq whitespace-line-column 80
        whitespace-style '(face tabs empty trailing lines-tail))
  (whitespace-mode +1)
  (diminish 'whitespace-mode))

(add-hook 'after-init-hook #'bk-setup-feature-whitespace)

;; * exec-path-from-shell
;; - https://github.com/purcell/exec-path-from-shell
;; - History
;;   -  2020-08-16 Created
(when (bk-load-path-add "exec-path-from-shell")
  (bk-auto-loads "exec-path-from-shell" #'exec-path-from-shell-initialize)
  (add-hook 'after-init-hook #'exec-path-from-shell-initialize))

;; * diminish
;; - https://github.com/emacsmirror/diminish
;; - History
;;   -  2020-08-15 Created
(when (bk-load-path-add "diminish")
  (bk-auto-loads "diminish" #'diminish)
  (global-eldoc-mode +1)
  (diminish 'eldoc-mode))

;; * paredit
;; - History
;;   - 2020-08-14 Created
;;   - 2020-08-16 Add clojure(script) modes
(defun bk-setup-feature-paredit ()
  "Customizations for paredit."
  (paredit-mode)
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "M-?") nil)
  (diminish 'paredit-mode))

(when (bk-load-path-add "paredit")
  (bk-auto-loads "paredit" #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'bk-setup-feature-paredit)
  (add-hook 'clojure-mode-hook #'bk-setup-feature-paredit)
  (add-hook 'clojurescript-mode-hook #'bk-setup-feature-paredit)
  (add-hook 'cider-repl-mode-hook #'bk-setup-feature-paredit))

;; * ido (disabled)
;; - History
;;   -  2020-08-14 Created
;;   -  2020-08-18 Enable ido-everywhere
;;   -  2020-08-28 Disabled
(when not-disabled?
  (add-hook 'after-init-hook #'ido-mode)
  (with-eval-after-load 'ido
    (setq ido-use-virtual-buffers t
          ido-enable-flex-matching t)
    (ido-everywhere +1)))

;; * ido completing-read-plus (disabled)
;; - https://github.com/DarwinAwardWinner/ido-completing-read-plus
;; - History
;;   -  2020-08-18 Created
;;   -  2020-08-28 Disabled
(when (and (bk-load-path-add "ido-completing-read-plus") not-disabled?)
  (bk-auto-loads "ido-completing-read+" #'ido-ubiquitous-mode)
  (with-eval-after-load 'ido
    (ido-ubiquitous-mode +1)))

;; * counsel-projectile
;; - https://github.com/ericdanan/counsel-projectile
;; - History
;;   -  2020-08-28 Created
(when (bk-load-path-add "counsel-projectile")
  (bk-auto-loads "counsel-projectile" #'counsel-projectile-mode)
  (add-hook 'after-init-hook #'counsel-projectile-mode))

;; * pyvenv
;; - History
;; - 2020-09-21 Created
(when (bk-load-path-add "pyvenv")
  (bk-auto-loads "pyvenv" #'pyvenv-activate))

;; * highlight-identation
;; - History
;; - 2020-09-21 Created
(when (bk-load-path-add "Highlight-Indentation-for-Emacs")
  (bk-auto-loads "highlight-identation"))

;; * elpy
;; - History:
;; - 2020-09-21 Created
(defun bk-setup-feature-elpy ()
  "Customizations for elpy."
  (pyvenv-activate "~/miniconda3")
  (delete `elpy-module-django elpy-modules)
  (delete `elpy-module-highlight-indentation elpy-modules))

(when (bk-load-path-add "elpy")
  (bk-auto-loads "elpy" #'elpy-enable #'elpy-modules))

(with-eval-after-load 'python
  (elpy-enable)
  (bk-setup-feature-elpy))

;; * hl-todo
;; - https://github.com/tarsius/hl-todo
;; - History
;;  - 2020-09-15 Created
(when (bk-load-path-add "hl-todo")
  (bk-auto-loads "hl-todo" #'hl-todo-mode)
  (add-hook 'prog-mode-hook #'hl-todo-mode))

;; * ace-window
;; - History
;;  - 2020/08/27 Created
(when (bk-load-path-add "ace-window")
  (bk-auto-loads "ace-window" #'ace-window))

;; * zoom-frm
;; - https://github.com/emacsmirror/zoom-frm
;; - History
;; - 2020-09-15 Created
(when (bk-load-path-add "zoom-frm")
  (bk-auto-loads "zoom-frm" #'zoom-in #'zoom-out))

;; * hydra
;; - https://github.com/abo-abo/hydra
;; - History
;;   -  2020-08-18 Created
(when (bk-load-path-add "hydra")
  (bk-auto-loads "hydra" #'hydra))

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

(when (bk-load-path-add "emacs-which-key")
  (bk-auto-loads "which-key" #'which-key-mode)
  (add-hook 'after-init-hook #'bk-setup-feature-which-key))

;; * yasnippet
;; - https://github.com/joaotavora/yasnippet
;; - History
;;   -  2020-08-18 Created
(when (bk-load-path-add "yasnippet")
  (bk-auto-loads "yasnippet" #'yas-global-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (yas-global-mode +1)
                              (diminish 'yas-minor-mode))))

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

;;; open eshell more easily
(global-set-key (kbd "C-c e") 'eshell)

;;; eshell clear buffer key binding
(defun eshell-clear-buffer ()
  "Clear the terminal buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;;; eshell aliases
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")))

;;; define default shell
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  "BEFORE FORCE-BASH docs."
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;;; open dired in the current file
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; * magit
;; - https://github.com/magit/magit
;; - History
;;   -  2020-08-15 Created
(defun bk/magit-cursor-fix ()
  "Fix the cursor position inside magit buffers."
  (goto-char (point-min))
  (when (looking-at "#")
    (forward-line 2)))

(when (bk-load-path-add "magit/lisp")
  (bk-auto-loads "magit" #'magit-status)
  (global-set-key (kbd "C-c g s") #'magit-status)
  (with-eval-after-load 'magit
    (setq magit-refresh-status-buffer nil)
    (set-default 'magit-revert-buffers 'silent)
    (set-default 'magit-no-confirm '(stage-all-changes
                                     unstage-all-changes))))

;; * ledger mode
;; - https://github.com/ledger/ledger-mode
;; - History
;;   -  2020-08-15 Created
;;   -  2020-08-18 Fix eval-after-load to ledger-modex
(defun bk/clean-ledger ()
  "Bring back timeline structure to the whole file."
  (interactive)
  (if (eq major-mode 'ledger-mode)
      (let ((curr-line (line-number-at-pos)))
        (ledger-mode-clean-buffer)
        (line-move (- curr-line 1)))))

(defun bk-setup-feature-ledger ()
  "Customizations for ledger."
  (setq ledger-reports
        '(("netcash" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -R -X R$ --current bal ^assets:bank liabilities:card")
          ("networth" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -X R$ --current bal ^assets:bank liabilities equity:apartment")
          ("spent-vs-earned" "ledger [[ledger-mode-flags]] -f /home/wand/.ledger bal -X BRL --period=\"last 4 weeks\" ^Expenses ^Income --invert -S amount")
          ("budget" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -X R$ --current bal ^assets:bank:checking:budget liabilities:card")
          ("creta" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -X R$ --current bal ^expenses:car: ^equity:car")
          ("taxes" "ledger [[ledger-mode-flags]] -f /home/wand/private/finance/ledger -R -X R$ --current bal ^expenses:taxes")
          ("bal" "%(binary) -f %(ledger-file) bal")
          ("reg" "%(binary) -f %(ledger-file) reg")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
          ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(when (bk-load-path-add "ledger-mode")
  (bk-auto-loads "ledger-mode"
                 '("\\ledger$" . ledger-mode)
                 '("\\.ledger$" . ledger-mode))
  (set-register ?l '(file . "~/.ledger"))
  (with-eval-after-load 'ledger-mode
    (bk-setup-feature-ledger)))

;;; when two buffers have the same name, we need a way to distinguish them
(setq uniquify-buffer-name-style 'post-forward)

;;; delete selected text with a key..
(add-hook 'after-init-hook 'delete-selection-mode)

;;; auto revert buffers if the file underneath it gets modified
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; * flycheck
;; - https://github.com/flycheck/flycheck
;; - History
;;   -  2020-08-15 Created
(defun bk-setup-feature-flycheck ()
  "Customizations for flycheck."
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch)
        flycheck-display-errors-delay 0.25))

(when (bk-load-path-add "flycheck")
  (bk-auto-loads "flycheck" #'flycheck-mode)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (with-eval-after-load 'flycheck
    (bk-setup-feature-flycheck)))

;; * flycheck-posframe
;; - https://github.com/alexmurray/flycheck-posframe
;; - History
;;   -  2020-09-15 Created
(when (bk-load-path-add "flycheck-posframe")
  (bk-auto-loads "flycheck-posframe" #'flycheck-posframe-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; * switch-window
;; - https://github.com/dimitri/switch-window
;; - History
;;   -  2020-08-16 Created
(when (bk-load-path-add "switch-window")
  (bk-auto-loads "switch-window" #'switch-window)
  (global-set-key (kbd "C-x o") 'switch-window)
  (with-eval-after-load 'switch-window
    (setq-default switch-window-shortcut-style 'alphabet
                  switch-window-timeout nil)))

;;; allow ad-handle-redefinition
;;; got from here https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;;; I dont like that warning telling me that ad-handle-definition was changed.. that's ok.
(setq ad-redefinition-action 'accept)

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

;;; cheatsheet
(set-register ?k '(file . "~/private/cheatsheet.org"))

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
(when (bk-load-path-add "windresize")
  (bk-auto-loads "windresize" #'windresize))

;; * sql indent mode
;; - History
;;   -  2020-08-17 Created
(when (bk-load-path-add "emacs-sql-indent")
  (bk-auto-loads "sql-indent" #'sqlind-minor-mode)
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))

;; * toggle-test
;; - History
;;   -  2020-08-17 Created
(when (bk-load-path-add "toggle-test")
  (bk-auto-loads "toggle-test" #'tgt-toggle)
  (global-set-key (kbd "s-t") #'tgt-toggle)
  (setq tgt-open-in-new-window nil)
  (put 'tgt-projects 'safe-local-variable #'listp))

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

;;; docker
(defun bk/docker-compose-custom-envs ()
  "Add usual env variables to Emacs environment."
  (interactive)
  (let* ((idu (shell-command-to-string "id -u"))
         (idg (shell-command-to-string "id -g"))
         (uid (string-join (vector (string-trim idu) ":" (string-trim idg)))))
    (setenv "WEBSERVER_PORT" "3000")
    (setenv "GRAPHQL_PORT" "4000")
    (setenv "CURRENT_UID" uid)
    (message "setenv WEBSERVER_PORT=3000 CURRENT_UID=$(id -u):$(id -g) done!")
    (docker)))

;; * docker.el
;; - https://github.com/Silex/docker.el
;; - History
;;   -  2020-09-04 Created
(when (bk-load-path-add "docker.el")
  (bk-auto-loads "docker" #'docker)
  (global-set-key (kbd "C-c d") #'docker))

;; * line numbers (disabled)
;; - History
;; - 2020-09-13 - Disabling it
(when not-disabled?
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

;; * custom
;; - History
;; - 2020-09-13 Organized into outlines
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; * server
(require 'server)
(when (not (server-running-p))
  (server-start))

;; * organizer
(load-file (expand-file-name "organizer.el" user-emacs-directory))

;;; abbreviations
(load-file (expand-file-name "abbreviations.el" user-emacs-directory))

;; End of file
(f-msg "Loaded init.el!")

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

(provide 'init.el)
;;; init.el ends here
