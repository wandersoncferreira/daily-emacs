;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 21:22:32 (wand)>

;;; Code:

(defun bk/occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; functions.el ends here
