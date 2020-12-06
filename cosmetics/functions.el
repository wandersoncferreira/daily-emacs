;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-12-06 10:46:16 (wanderson)>

;;; Code:

(defun bk/set-monaco-font ()
  "Define the Monaco font."
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco" :height 100)))

(defun bk/set-consolas-font ()
  "Define the Consolas font."
  (when (member "Consolas" (font-family-list))
    (set-face-attribute 'default nil :font "Consolas" :height 140)))

(defun bk/set-fira-font ()
  "Define the Fira Code font."
  (when (member "Fira Code" (font-family-list))
    (set-face-attribute 'default nil :font "Fira Code Retina" :height 130)))

(defun bk/increase-font (size)
  "Increase the SIZE of the current font."
  (set-face-attribute 'default nil :height size))

(defun bk/light-theme ()
  "Define custom light theme."
  (interactive)
  (set-face-attribute 'lazy-highlight nil :background "light green")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))

(defun bk/slate-grey-theme ()
  "Nice composition."
  (interactive)
  (set-background-color "DarkSlateGray")
  (set-face-background 'mode-line "Wheat")
  (set-face-foreground 'mode-line "DarkSlateGray")
  (set-foreground-color "Wheat"))

(defun bk/monokai-theme ()
  "Customizations for Monokai."
  (interactive)
  (load-theme 'monokai t))

;;; functions.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
