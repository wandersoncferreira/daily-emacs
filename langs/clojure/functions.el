;;; init.el --- Entry point for functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 21:46:05 (wand)>

;;; Code:

(defun bk/nrepl-warn-when-not-connected ()
  "Function to warn me to start the REPL."
  (interactive)
  (message "Oops! You're not connected to an nREPL server.
Please run M-x cider or M-x cider-jack-in to connect"))

(defun cider-eval-n-defuns (n)
  "N times."
  (interactive "P")
  (cider-eval-region (car (bounds-of-thing-at-point 'defun))
                     (save-excursion
                       (dotimes (i (or n 2))
                         (end-of-defun))
                       (point))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; functions.el ends here
