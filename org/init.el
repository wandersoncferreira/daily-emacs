;;; init.el --- Entry point for configurations ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(load-file "~/.emacs.d/org/functions.el")

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

(defun bk-setup-feature-org-agenda ()
  "Customizations for `org-agenda'."
  (setq org-agenda-files '("/home/wand/all/agenda/todo.org"))

  (set-register ?t '(file . "~/all/agenda/todo.org"))

  ;; add closing time when changing to DONE
  (setq org-log-done 'time)

  ;; clock out when moving task to done
  (setq org-clock-out-when-done t)

  (setq org-agenda-log-mode-items '(closed clock state))

  (add-hook 'org-after-todo-state-change-hook 'lgm/clock-in-when-started)
  (add-hook 'org-after-todo-state-change-hook 'bk/clock-out-when-waiting)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w!)" "STARTED(s!)" "|"
                    "DONE(d)" "CANCELED(c@)" "INACTIVE(i@)" "FAIL(f@)")))

  (require 'ob-clojure)
  (setq org-babel-clojure-backend 'cider)
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (clojure . t)
          (python . t)))

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
            (tags "+life+TODO=\"NEXT\"|+life+TODO=\"STARTED\"|+life+TODO=\"WAIT\""
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
(when (bk/add-load-path "org" "org-roam")
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
    (setq org-roam-db-location "/home/wand/all/zettelkasten/org-roam.db")
    (setq org-roam-completion-system 'ivy)
    (setq org-roam-dailies-capture-templates
          '(("d" "daily" plain (function org-roam-capture--get-point) ""
             :file-name "daily/%<%Y-%m-%d>"
             :unnarrowed t
             :head "#+TITLE: %<%Y-%m-%d>\n#+STARTUP: showall\n#+HUGO_BASE_DIR: ~/open-source/braindump\n#+roam_tags: fleeting\n#+Time-stamp: <>")))

    (setq org-roam-capture-templates
          '(("p" "permanent" plain #'org-roam-capture--get-point "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+title: ${title}\n#+created_at: %U\n#+STARTUP: showall\n#+HUGO_BASE_DIR: ~/open-source/braindump\n#+Time-stamp: <>"
             :unnarrowed t)))

    (add-hook 'org-roam-mode-hook 'toggle-truncate-lines)
    (org-roam-mode +1)
    (diminish 'org-roam-mode)))

;; * ox-hugo
;; - https://github.com/kaushalmodi/ox-hugo
;; - History
;;   - 2020-10-18 Created
(when (bk/add-load-path "org" "ox-hugo")
  (bk-auto-loads "ox-hugo" #'ox-hugo #'org-hugo-export-wim-to-md)
  (bk-auto-loads "org-hugo-auto-export-mode.el" #'org-hugo-auto-export-mode)
  (with-eval-after-load 'ox
    (require 'ox-hugo)))

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
(add-hook 'orgr-mode-hook 'org-hugo-auto-export-mode)


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
        )
  (org-roam-server-mode +1))

(when (bk/add-load-path "org" "org-roam-server")
  (bk-auto-loads "org-roam-server" #'org-roam-server-mode))

;; * plantuml
;; - https://github.com/skuro/plantuml-mode
;; - History
;;   -  2020-08-16 Created
;;   -  2020-08-17 Change after-load to enable usage in org-mode
(defvar bk-plantuml-path "~/plantuml.jar")
(when (bk/add-load-path "org" "plantuml-mode")
  (bk-auto-loads "plantuml-mode" '("\\.plantuml$" . plantuml-mode))
  (with-eval-after-load 'org
    (setq org-plantuml-jar-path bk-plantuml-path)))

;; * company-org-roam
;; - https://github.com/org-roam/company-org-roam
;; - History
;;   -  2020-08-16 Created
(when (bk/add-load-path "org" "company-org-roam")
  (bk-auto-loads "company-org-roam" #'company-org-roam))

(defun bk/org-roam--backlinks-list (file)
  "Find links referring to FILE."
  (if (org-roam--org-roam-file-p file)
      (--reduce-from
       (concat acc
               (let ((fld (car (split-string (file-relative-name (car it) org-roam-directory) ".orgr"))))
                 (format "- [[braindump:%s][%s]]\n"
                         fld
                         (org-roam--get-title-or-slug (car it)))))
       "" (org-roam-db-query [:select [from] :from links :where (= to $s1)] file))
    ""))

(defun bk/org-export-preprocessor (backend)
  "BACKEND passed by `org-export-before-processing-hook'."
  (let ((links (bk/org-roam--backlinks-list (buffer-file-name))))
    (save-excursion
      (goto-char (point-max))
      (insert (concat "\n* Links to this note\n"))
      (insert links))))

(defun bk/replace-file-handle (_backend)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(file:\\).+\.org" nil t)
      (replace-match "braindump:" nil nil nil 1))))

(defvar bk/braindump-org-roam-dir "~/all/zettelkasten")
(defvar bk/braindump-org-ext ".orgr")
(defvar bk/braindump-base-dir "~/open-source/braindump")

(defun bk/process-file (f)
  (save-excursion
    (find-file f)
    (goto-char (point-min))
    (forward-line)
    (insert "tmp\n")
    (forward-line -1)
    (kill-whole-line)
    (when (not (re-search-forward "HUGO_BASE_DIR:" nil t))
      (forward-line)
      (insert (format "#+HUGO_BASE_DIR: %s\n" bk/braindump-base-dir)))
    
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun bk/hugo-base-dir-zettelkasten ()
  "Parse all files for personal blog."
  (interactive)
  (mapc 'bk/process-file
        (directory-files bk/braindump-org-roam-dir t
                         (format "%s$" bk/braindump-org-ext))))

(defun bk/hugo-base-dir-daily-files ()
  "Add the header HUGO_BASE_DIR to the daily files."
  (interactive)
  (mapc 'bk/process-file
        (directory-files (format "%s/daily" bk/braindump-org-roam-dir) t
                         (format "%s$" bk/braindump-org-ext))))

(defun bk-setup-feature-org-braindump ()
  "Customizations for braindump."
  (org-add-link-type "braindump" nil
                     '(lambda (path desc frmt)
                        (format "[%s]({{< relref \"/posts/%s\" >}} \"%s\")"
                                desc path desc)))

  (add-hook 'org-export-before-processing-hook 'bk/org-export-preprocessor)
  (add-hook 'org-export-before-processing-hook 'bk/replace-file-handle))

(eval-after-load 'org #'bk-setup-feature-org-braindump)

(when (bk/add-load-path "org" "org-download")
  (bk-auto-loads "org-download" #'org-download-clipboard))

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

;; * org-present
;; - https://github.com/rlister/org-present
;; - History
;;   - 2020/10/28 Created
(when (bk/add-load-path "org" "org-present")
  (bk-auto-loads "org-present" #'org-present)
  (eval-after-load "org-present"
    '(progn
       (add-hook 'org-present-mode-hook
                 (lambda ()
                   (org-present-big)
                   (org-display-inline-images)
                   (org-present-hide-cursor)
                   (org-present-read-only)))
       (add-hook 'org-present-mode-quit-hook
                 (lambda ()
                   (org-present-small)
                   (org-remove-inline-images)
                   (org-present-show-cursor)
                   (org-present-read-write))))))

;; * org-bullets-mode
;; - https://github.com/integral-dw/org-bullets
;; - History
;;   - 2020/10/29 Created
(when (bk/add-load-path "org" "org-bullets")
  (bk-auto-loads "org-bullets" #'org-bullets-mode)
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  (add-hook 'org-mode-hook #'org-bullets-mode))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
