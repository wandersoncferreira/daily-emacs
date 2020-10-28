;;; init.el --- Personal configuration ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-28 15:24:53 (wand)>

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

(setq inhibit-compacting-font-caches t)

(defvar bk--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist bk--file-name-handler-alist)))

(defvar not-disabled? nil)

;; * lazy loading
;; - History
;; - 2020-09-13 Organized into outlines

(defun bk/add-load-path (pkg subdir)
  "If PKG/SUBDIR exist add it to `load-path'.
Return non-nil if successful."
  (let* ((path (concat (file-name-as-directory
			(expand-file-name pkg user-emacs-directory))
		       (concat "pkgs/" subdir))))
    (when (file-readable-p path)
      (add-to-list 'load-path path))))

(defun bk-auto-loads (file &rest func-or-ext-with-func)
  "`autoload' and `auto-mode-alist' for packages in the FILE.
FUNC-OR-EXT-WITH-FUNC are the triggers of the package activation.
After any of the functions is called, the whole package is loaded in memory."
  (dolist (x func-or-ext-with-func)
    (autoload
      (if (consp x) (cdr x) x)
      file
      "Undocumented `autoload'."
      t)
    (when (consp x) (add-to-list 'auto-mode-alist x))))

;; * extra packages
;; - History
;; - 2020-09-22 Added clojure pack
(load-file (expand-file-name "dependencies/init.el" user-emacs-directory))

(dolist (module '("completion"
                  "editor"
                  "projects"
                  "search"
                  "cosmetics"
                  "window"
                  "org"
		  "core"
                  "version-control"
                  "shell"
		  "apps/docker"
                  "apps/bongo"
                  "apps/elfeed"
                  "apps/telega"
		  "apps/ledger"
                  "apps/circe"
                  "apps/webpaste"
                  "apps/eping"
                  "apps/zeal"
                  "apps/oblique-strategies"
		  "langs/clojure"
		  "langs/haskell"
		  "langs/python"
                  "langs/golang"
		  "langs/scala"
                  "langs/sql"
                  "langs/common-lisp"
		  "modes/json"
		  "modes/prog"
		  "modes/markdown"))
  (let* ((module-name (concat module "/init.el"))
         (module-name (expand-file-name module-name user-emacs-directory)))
    (load module-name)))

;;; open this file easily
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?c '(file . "~/.emacs.d/core/etc/cheatsheet.org"))

;; * custom
;; - History
;; - 2020-09-13 Organized into outlines
(setq custom-file "~/.emacs.d/core/etc/custom.el")
(load custom-file)

;; * server
(require 'server)
(when (not (server-running-p))
  (server-start))

;; End of file
(f-msg "Loaded init.el!")

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

(provide 'init.el)
;;; init.el ends here
