;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-14 00:50:18 (wand)>

;;; Code:

(defun bk/set-monaco-font ()
  "Define the Monaco font."
  (when (member "Monaco" (font-family-list))
    (set-face-attribute 'default nil :font "Monaco" :height 100)))

(defun bk/increase-font (size)
  "Increase the SIZE of the current font."
  (set-face-attribute 'default nil :height size))

(defun bk/light-theme ()
  "Define custom light theme."
  (interactive)
  (bk/increase-font 120)
  (set-face-attribute 'lazy-highlight nil :background "light green")
  (set-face-attribute 'isearch nil :background "khaki1")
  (set-face-attribute 'region nil :background "khaki1"))

(defun bk/slate-grey ()
  "Nice composition."
  (interactive)
  (set-background-color "DarkSlateGray")
  (set-face-background 'mode-line "Wheat")
  (set-face-foreground 'mode-line "DarkSlateGray")
  (set-foreground-color "Wheat"))

;;; functions.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
