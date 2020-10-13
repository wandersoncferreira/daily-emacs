;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 21:25:27 (wand)>

;;; Code:

(defun bk/kill-inner-word ()
  "Kill the entire word your cursor is in.  Equivalent to ciw in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "C-c k w") 'bk/kill-inner-word)

(defun bk/copy-whole-line ()
  "Copy a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))

(global-set-key (kbd "C-c y l") 'bk/copy-whole-line)

(defun bk/unfill-paragraph ()
  "Takes a multi-line paragraph and make it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun duplicate-region (num &optional start end)
  "Duplicate the region bounded by START and END NUM times."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char start)
    (dotimes (_ num)
      (insert region))))

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (when (eq (point-at-eol) (point-max))
    (goto-char (point-max))
    (newline)
    (forward-char -1))
  (duplicate-region num (point-at-bol) (1+ (point-at-eol))))

(defun bk/duplicate-current-line-or-region (arg)
  "Duplicate the current line or region ARG times."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (duplicate-region arg)
      (duplicate-current-line arg))))

(global-set-key (kbd "C-c 2") #'bk/duplicate-current-line-or-region)

(defun iwb ()
  "Ident whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "<f7>") 'iwb)

;;; functions.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
