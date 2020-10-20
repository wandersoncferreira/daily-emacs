;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-19 22:40:14 (wand)>

;;; Code:

(defun lgm/clock-in-when-started ()
  "Automatically clock in a task when status is changed to STARTED."
  (when (string= org-state "STARTED")
    (org-clock-in)))

(defun bk/clock-out-when-waiting ()
  "Clock out when the task change to WAIT."
  (when (and (string= org-state "WAIT")
             (not (string= org-last-state org-state)))
    (org-clock-out)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun scheduled-or-not (resp)
  "Improve the schedule view in agenda mode when there is no RESP available."
  (interactive)
  (if resp
      (concat "In " (number-to-string (org-time-stamp-to-now resp)) " day(s)")
    '"Not Scheduled"))

(defun bk/second-brain-server ()
  "Start my second brain server."
  (interactive)
  (bk-setup-feature-org-roam-server))

(require 'map)
(require 'org)
(require 'seq)

(defvar company-org-block-bol-p t "If t, detect completion when at
beginning of line, otherwise detect completion anywhere.")

(defvar company-org--regexp "<\\([^ ]*\\)")

(defun company-org-block (command &optional arg &rest ignored)
  "Complete org babel languages into source blocks."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-block))
    (prefix (when (derived-mode-p 'org-mode)
              (company-org-block--grab-symbol-cons)))
    (candidates (company-org-block--candidates arg))
    (post-completion
     (company-org-block--expand arg))))

(defun company-org-block--candidates (prefix)
  "Return a list of org babel languages matching PREFIX."
  (seq-filter (lambda (language)
                (string-prefix-p prefix language))
              ;; Flatten `org-babel-load-languages' and
              ;; `org-structure-template-alist', join, and sort.
              (seq-sort
               #'string-lessp
               (append
                (mapcar #'prin1-to-string
                        (map-keys org-babel-load-languages))
                (map-values org-structure-template-alist)))))

(defun company-org-block--template-p (template)
  (seq-contains (map-values org-structure-template-alist)
                template))

(defun company-org-block--expand (insertion)
  "Replace INSERTION with actual source block."
  (delete-region (point) (- (point) (1+ ;; Include "<" in length.
                                     (length insertion))))
  (if (company-org-block--template-p insertion)
      (company-org-block--wrap-point insertion
                                     ;; May be multiple words.
                                     ;; Take the first one.
                                     (nth 0 (split-string insertion)))
    (company-org-block--wrap-point (format "src %s" insertion)
                                   "src")))

(defun company-org-block--wrap-point (begin end)
  "Wrap point with block using BEGIN and END.  For example:
#+begin_BEGIN
  |
#+end_END"
  (insert (format "#+begin_%s\n" begin))
  (insert (make-string org-edit-src-content-indentation ?\s))
  ;; Saving excursion restores point to location inside code block.
  (save-excursion
    (insert (format "\n#+end_%s" end))))

(defun company-org-block--grab-symbol-cons ()
  "Return cons with symbol and t whenever prefix of < is found.
For example: \"<e\" -> (\"e\" . t)"
  (when (looking-back (if company-org-block-bol-p
                          (concat "^" company-org--regexp)
                        company-org--regexp)
                      (line-beginning-position))
    (cons (match-string-no-properties 1) t)))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-org-block))

(defcustom company-org-block-edit-mode 'auto
  "Customize whether edit mode, post completion was inserted."
  :type '(choice
          (const :tag "nil: no edit after insertion" nil)
          (const :tag "prompt: ask before edit" prompt)
          (const :tag "auto edit, no prompt" auto)))

;;; functions.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
