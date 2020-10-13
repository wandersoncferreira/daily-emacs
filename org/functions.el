;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;; Time-stamp: <2020-10-12 21:30:58 (wand)>

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

;;; functions.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
