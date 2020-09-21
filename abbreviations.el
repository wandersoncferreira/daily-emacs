;;; Abbreviations --- Abbrev setup  ;; -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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

(add-hook 'prog-mode-hook 'abbrev-mode)


(provide 'abbreviations)
;;; abbreviations.el ends here
