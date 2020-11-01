;;; init.el --- Entry point for functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-11-01 01:28:00 (wand)>

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

(defun bk/repl ()
  "Start an interactive repl in a temp project."
  (interactive)
  (cider-jack-in '(:project-dir "/home/wand/temp"))
  (add-hook 'cider-connected-hook
            (lambda ()
              (cider-repl-set-ns "user")
              (cider-nrepl-sync-request:eval "(require '[clj-time.core :as t])")
              (cider-nrepl-sync-request:eval "(require '[cheshire.core :as json])")
              (cider-nrepl-sync-request:eval "(require '[clojure.core.async :as a])")
              (cider-switch-to-repl-buffer))))

(defun bk/cider-quit-all ()
  "Interate over all CIDER connections and close them all."
  (interactive)
  (let ((repl-buffers (seq-filter (lambda (b)
                                    (with-current-buffer b
                                      (eq major-mode 'cider-repl-mode)))
                                  (buffer-list))))
    (dolist (buf repl-buffers)
      (cider--close-connection buf))
    (message "All CIDER connections closed")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; functions.el ends here
