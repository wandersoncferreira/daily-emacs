;;; organizer.el --- Org-mode setup ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be more dragons! Spice ones...

;; Time-stamp: <2020-09-15 21:40:51 (wand)>

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
  (setq org-return-follows-link t)
  (setq org-use-speed-commands t)
  (require 'ob-plantuml))

(with-eval-after-load 'org
  (bk-setup-feature-org))

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
