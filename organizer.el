;;; organizer.el --- Org-mode setup ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be more dragons! Spice ones...

;; Time-stamp: <2020-09-16 09:13:15 (wand)>

;;; Code:

;;; load lazy-loading helper functions
(load-file (expand-file-name "lazy-loading.el" user-emacs-directory))

;;; load dependencies
(load-file (expand-file-name "dependencies.el" user-emacs-directory))

;; org
;; basic functionalities of org
;; - History
;;   -  2020-08-16 Created

(defun bk-setup-feature-org ()
  "Customizations for org mode."
  (setq org-return-follows-link t
        org-use-speed-commands t)
  (require 'ob-plantuml))

(with-eval-after-load 'org
  (bk-setup-feature-org))

;; * org-agenda
;; - History
;; - 2020-09-15 Created
(require 'org-agenda)

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

(defun bk-setup-feature-org-agenda ()
  "Customizations for `org-agenda'."
  (setq org-agenda-files '("/home/wand/all/agenda/todo.org"))

  (set-register ?t '(file . "~/all/agenda/todo.org"))

  ;; add closing time when changing to DONE
  (setq org-log-done 'time)

  ;; clock out when moving task to done
  (setq org-clock-out-when-done t)

  (add-hook 'org-after-todo-state-change-hook 'lgm/clock-in-when-started)
  (add-hook 'org-after-todo-state-change-hook 'bk/clock-out-when-waiting)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w!)" "STARTED(s!)" "|"
                    "DONE(d)" "CANCELED(c@)" "INACTIVE(i@)" "FAIL(f@)")))

  (setq org-agenda-block-separator " "
        org-agenda-span 'day
        org-agenda-skip-function-global
        '(org-agenda-skip-entry-if 'todo '("ACTED" "CANCELED")))
  
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "/home/wand/all/agenda/todo.org" "Task")
           "* TODO [#D] %^{Title}\n :PROPERTIES:\n :CAPTURED: %U\n :END:\n\n %i %l"
           :clock-in t :clock-resume t)))

  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and NEXTs!"
           (;; deadlines in the next 45 days
            (agenda ""
                    ((org-agenda-time-grid nil)
                     (org-agenda-span 'day)
                     (org-deadline-warning-days 45)
                     (org-agenda-entry-types '(:deadline))
                     (org-agenda-sorting-strategy '(deadline-up))
                     (org-agenda-overriding-header "Deadlines in the next 45 days:")))

            ;; week tasks and deadlines
            (agenda ""
                    ((org-agenda-time-grid nil)
                     (org-agenda-span 'week)
                     (org-agenda-start-on-weekday 0)
                     (org-deadline-warning-days 0)
                     (org-deadline-past-days 0)
                     (org-scheduled-past-days 0)
                     (org-agenda-entry-types '(:deadline :scheduled))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                     (org-agenda-overriding-header "Week tasks completed:")))
            
            ;; high priority tasks
            (tags "PRIORITY=\"A\"&-TODO=\"DONE\""
                  ((org-agenda-overriding-header "High priority tasks:")
                   (org-agenda-skip-function '(air-org-skip-subtree-if-habit))))

            ;; work
            (tags "+work+TODO=\"NEXT\"|+work+TODO=\"STARTED\"|+work+TODO=\"WAIT\""
                  ((org-agenda-overriding-header "Next task in WORK:")
                   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")))

            ;; next study
            (tags "+study+TODO=\"NEXT\"|+study+TODO=\"STARTED\"|+study+TODO=\"WAIT\""
                  ((org-agenda-overriding-header "Next task in STUDY:")
                   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")))

            ;; next life
            (tags "+life+"
                  ((org-agenda-overriding-header "Next task in LIFE:")
                   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")))

            ;; next projects
            (tags "+project+TODO=\"NEXT\"|+project+TODO=\"STARTED\"|+project+TODO=\"WAIT\""
                  ((org-agenda-overriding-header "Next task in PROJECTS:")
                   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")))
            
            ))))

  ;; compact only day view
  (add-to-list 'org-agenda-custom-commands
               '("l" "Compact today"
                 agenda "" ((org-agenda-ndays 5)
                            (org-agenda-span 'day)
                            (org-deadline-warning-days 0)
                            (org-agenda-skip-scheduled-delay-if-deadline t)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-scheduled-leaders '("" ""))
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-overriding-header "Today Agenda:"))))

  ;; daily habits
  (add-to-list 'org-agenda-custom-commands
               '("h" "Daily habits"
                 agenda ""
                 ((org-agenda-show-log t)
                  (org-agenda-ndays 7)
                  (org-agenda-log-mode-items '(state))
                  (org-agenda-skip-function '(org-agenda-skip-if 'notregexp ":daily:")))))

  ;; enable the usage of two agenda views at the same time
  (org-toggle-sticky-agenda))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(add-hook 'after-init-hook #'bk-setup-feature-org-agenda)

;; * org-roam
;; - https://github.com/org-roam
;; - History
;;   -  2020-08-16 Created
;;   -  2020-09-12 Improve find notes by adding a prefix `f` in the Org roam chords
(setq bk-org-roam-directory "~/all/zettelkasten")
(when (bk-load-path-add "org-roam")
  (bk-auto-loads "org-roam"
                 #'org-roam-capture
                 #'org-roam-dailies-today
                 #'org-roam-random-note
                 #'org-roam-find-file
                 #'org-roam-dailies-find-today
                 #'org-roam-dailies-find-next-note
                 #'org-roam-dailies-find-previous-note
                 #'org-roam-insert
                 #'org-roam)
  
  (global-set-key (kbd "C-c n c") #'org-roam-capture)
  (global-set-key (kbd "C-c n t") #'org-roam-dailies-today)
  (global-set-key (kbd "C-c n i") #'org-roam-insert)
  (global-set-key (kbd "C-c n r") #'org-roam-random-note)
  (global-set-key (kbd "C-c n R") #'org-roam)

  (global-set-key (kbd "C-c n f f")  #'org-roam-find-file)
  (global-set-key (kbd "C-c n f t") #'org-roam-dailies-find-today)
  (global-set-key (kbd "C-c n f n") #'org-roam-dailies-find-next-note)
  (global-set-key (kbd "C-c n f p") #'org-roam-dailies-find-previous-note)
  
  (with-eval-after-load 'org-roam
    (setq org-roam-directory bk-org-roam-directory)
    (setq org-roam-completion-system 'ivy)
    (setq org-roam-dailies-capture-templates
          '(("d" "daily" plain (function org-roam-capture--get-point) ""
             :file-name "daily/%<%Y-%m-%d>"
             :unnarrowed t
             :head "#+TITLE: %<%Y-%m-%d>\n#+STARTUP: showall\n#+roam_tags: fleeting\n#+Time-stamp: <>")))

    (setq org-roam-capture-templates
          '(("p" "permanent" plain #'org-roam-capture--get-point "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+title: ${title}\n#+created_at: %U\n#+STARTUP: showall\n#+Time-stamp: <>"
             :unnarrowed t)))
    (org-roam-mode +1)
    (diminish 'org-roam-mode)))

(define-derived-mode orgr-mode org-mode "orgr"
  "Major mode to segregate configs of Org-roam from Org-mode."
  
  (make-local-variable 'company-backends)
  (make-local-variable 'company-idle-delay)
  (make-local-variable 'company-minimum-prefix-length)
  (setq company-backends '(company-org-roam))
  (setq company-idle-delay 0.20
        company-minimum-prefix-length 2))

(setq org-roam-file-extensions '("orgr"))
(add-to-list 'auto-mode-alist '("\\.orgr\\'" . orgr-mode))

;;; * org-roam-protocol
(with-eval-after-load 'org-roam
  (require 'org-roam-protocol))

;; * org-roam-server
;; - https://github.com/org-roam/org-roam-server
;; - History
;;   -  2020-08-16 Created
;;   -  2020-08-31 Setup feature org-roam-server function created
;;   -  2020-09-12 Remove automatic startup and provide manual alternative
;;   -  2020-09-13 Update with upstream
(defun bk-setup-feature-org-roam-server ()
  "Customizations for `org-roam-server'."
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-serve-files t
        org-roam-server-default-exclude-filters (json-encode (list (list (cons 'id "fleeting") (cons 'parent "tags"))))
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv" "mkv"))
  (org-roam-server-mode +1))

(defun bk/second-brain-server ()
  "Start my second brain server."
  (interactive)
  (bk-setup-feature-org-roam-server))

(when (bk-load-path-add "org-roam-server")
  (bk-auto-loads "org-roam-server" #'org-roam-server-mode))

;; * plantuml
;; - https://github.com/skuro/plantuml-mode
;; - History
;;   -  2020-08-16 Created
;;   -  2020-08-17 Change after-load to enable usage in org-mode
(defvar bk-plantuml-path "~/plantuml.jar")
(when (bk-load-path-add "plantuml-mode")
  (bk-auto-loads "plantuml-mode" '("\\.plantuml$" . plantuml-mode))
  (with-eval-after-load 'org
    (setq org-plantuml-jar-path bk-plantuml-path)))

;; * company-org-roam
;; - https://github.com/org-roam/company-org-roam
;; - History
;;   -  2020-08-16 Created
(when (bk-load-path-add "company-org-roam")
  (bk-auto-loads "company-org-roam" #'company-org-roam))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

(provide 'organizer.el)
;;; organizer.el ends here
