;;; init.el --- Entry point for custom functions ;; -*- lexical-binding: t -*-

;;; Commentary:

;; Here be dragons

;;; Code:

(defun bk/beginning-of-line ()
  "Go back at the first non-whitespace character."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun bk/end-of-line ()
  "Go to the end of the last non-whitespace character."
  (interactive)
  (move-end-of-line nil)
  (re-search-backward "^\\|[^[:space:]]")
  (forward-char))

(defun bk/eval-buffer ()
  "Provide some feedback after evaluating the buffer."
  (interactive)
  (eval-buffer)
  (message "Buffer evaluated!"))

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

(defun bk/dired-xdg-open ()
  "Open the file at point with xdg-open."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

(defun bk/scroll-up ()
  "Scroll only specific amount of lines."
  (interactive)
  (scroll-up-command 8))

(defun bk/scroll-down ()
  "Scroll only specific amount of lines."
  (interactive)
  (scroll-down-command 8))

(defun bk/jump-to-register ()
  "Switch between current position and pos stored."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

(defun bk/point-to-register ()
  "Store cursor position in a register."
  (interactive)
  (point-to-register 8)
  (message "Point set"))

(defun bk/insert-date-today ()
  "Insert today date as YYYY/MM/DD."
  (interactive)
  (insert (format-time-string "%Y/%m/%d")))

(defvar url-http-end-of-headers)
(defun bk/ip ()
  "Find my current public IP address."
  (interactive)
  (let* ((endpoint "https://api.ipify.org")
         (myip (with-current-buffer (url-retrieve-synchronously endpoint)
                 (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))
    (kill-new myip)
    (message "IP: %s" myip)))

;;; edit using sudo
(defun bk/sudo-edit (&optional arg)
  "Function to edit file with super-user with optional ARG."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun bk/generate-password ()
  "Generate a 16-digit password."
  (interactive)
  (let* ((sym "!@#$%^&*()_+-=[]{}|")
         (i (% (abs (random)) (length sym)))
         (beg (substring sym i (1+ i))))
    (kill-new
     (format "%s%s" beg (string-trim (shell-command-to-string
                                      " openssl rand -base64 32 | tr -d /=+ | cut -c -16"))))
    (message "Password in kill ring!")))

(defun bk/diff-last-two-kills (&optional ediff?)
  "Diff last things in the `kill-ring' with prefix EDIFF? open ediff."
  (interactive "P")
  (let ((old "/tmp/old-kill")
        (new "/tmp/new-kill"))
    (with-temp-file new
      (insert (current-kill 0 t)))
    (with-temp-file old
      (insert (current-kill 1 t)))
    (if ediff?
        (ediff old new)
      (diff old new "-u" t))))

(defun prot/scratch-buffer-setup ()
  "Add contents to `scratch' buffer and name it accordingly."
  (let* ((mode (format "%s" major-mode))
         (string (concat "Scratch buffer for: " mode "\n\n")))
    (when scratch-buffer
      (save-excursion
        (insert string)
        (goto-char (point-min))
        (comment-region (point-at-bol) (point-at-eol)))
      (forward-line 2))
    (rename-buffer (concat "*Scratch for " mode "*") t)))

(defun bk/select-buffers-same-major-mode (&optional arg)
  "Select buffers that match the current buffer's major mode.
With \\[universal-argument] produce an `ibuffer' filtered
accordingly."
  (interactive "P")
  (let* ((major major-mode)
         (prompt "Buffers for ")
         (mode-string (format "%s" major))
         (mode-string-pretty (propertize mode-string 'face 'success)))
    (if arg
        (ibuffer t (concat "*" prompt mode-string "*")
                 (list (cons 'used-mode major)))
      (switch-to-buffer
       (read-buffer
        (concat prompt mode-string-pretty ": ") nil t
        (lambda (pair)
          (with-current-buffer (cdr pair) (derived-mode-p major))))))))

(global-set-key (kbd "M-s b") 'bk/select-buffers-same-major-mode)

(defun bk/search-subdirs ()
  "Search for directories in VC root or PWD."
  (interactive)
  (let* ((vc (vc-root-dir))
         (dir (expand-file-name (or vc default-directory)))
         (regexp (read-regexp
                  (format "Subdirectories matching REGEXP in %s: "
                          (propertize dir 'face 'bold))))
         (names (process-lines "fd" "-i" "-H"
                               "-a" "-t" "d" "-c" "never"
                               regexp dir))
         (buf "*FD Dired*"))
    (dired (cons (generate-new-buffer-name buf) names))))

(global-set-key (kbd "M-s d") 'bk/search-subdirs)

(defun bk/os-uptime ()
  "Uptime of the box."
  (interactive)
  (message (shell-command-to-string "uptime -p")))

(defun bk/days-since ()
  "Number of days since DATE to today."
  (interactive)
  (let* ((date (read-from-minibuffer "Inicial date: "))
         (date-zoned (format "%s +0300" date)))
    (message (format "%s days ago" (days-between
                                    (current-time-string)
                                    date-zoned)))))

(defun bk/update-emacs ()
  "Update this Emacs configuration using git."
  (interactive)
  (async-shell-command "~/.emacs.d/bin/update"))

(defun bk/get-pkg-list (dir)
  "Get list of packages installed in a specific DIR (pkgs) subfolder."
  (cl-remove-if (lambda (d)
                  (let* ((path-length (length d))
                         (ending (substring d (- path-length 1))))
                    (or (equal "." ending)
                        (equal ".." ending))))
                (directory-files dir t)))

(defun bk/pkg-candidates (&rest _u)
  "Get list of candidates to ivy."
  (let* ((pkg-dirs (split-string
                    (shell-command-to-string
                     "find ~/.emacs.d/ -type d -iname pkgs") "\n" t)))
    (-mapcat #'bk/get-pkg-list pkg-dirs)))

(defun bk/pkg-execute-removal (pkg-path)
  "Execute the remove command from git-submodule for PKG-PATH."
  (message (shell-command-to-string
            (format "~/.emacs.d/bin/remove %s" pkg-path))))

(defun bk/pkgs-invoke ()
  "Invoke the ivy completion function."
  (ivy-read "Remove package: " #'bk/pkg-candidates
            :action (lambda (entry)
                      (bk/pkg-execute-removal entry))
            :caller #'bk/remove-package))

(defun bk/remove-package ()
  "Remove a package installed in the current box."
  (interactive)
  (bk/pkgs-invoke))

(defvar bk--content-candidates-cache nil)
(defvar bk--register-cache register-alist)

(defun bk/get-content-candidates-dir (dir)
  "Get content from a DIR."
  (let (yanked)
    (setq yanked (list))
    (with-current-buffer (find-file-noselect dir)
      (goto-char (point-min))
      (while (search-forward ";; *" nil t)
        (kill-line)
        (yank)
        (setq subject (string-trim (substring-no-properties (car kill-ring))))
        (setq yanked (cons subject yanked))
        (point-to-register (intern subject))
        (let ((inhibit-message t))
          (save-buffer))))
    yanked))

(defun bk/get-content-candidates (&rest _u)
  "Get content candidates."
  (if bk--content-candidates-cache
      bk--content-candidates-cache
    (let* ((inits (split-string
                   (shell-command-to-string
                    "find ~/.emacs.d/ -type f -iname init.el") "\n" t))
           (res (-mapcat #'bk/get-content-candidates-dir inits)))
      (setq bk--content-candidates-cache res)
      res)))

(defun bk/goto-content (entry)
  "Move to the content pointed by ENTRY."
  (jump-to-register (intern entry)))

(defun bk/get-content (u)
  "Search for content and move to there.

If U is provided, clear the caching."
  (interactive "p")
  (when (equal u 4)
    (setq bk--content-candidates-cache nil)
    (setq register-alist bk--content-candidates-cache))
  
  (ivy-read "Get content: " #'bk/get-content-candidates
            :action (lambda (entry)
                      (bk/goto-content entry))))

(defun bk/qtd-pkgs ()
  "Return the amount of packages installed."
  (interactive)
  (let* ((candidates (bk/pkg-candidates))
         (total (length candidates))
         (deps (seq-filter
                (lambda (v)
                  (string-match "dependencies" v))
                candidates))
         (total-deps (length deps)))
    (message (format "Total: %s\nDependencies: %s/%s\nFeatures: %s"
                     total total-deps total (- total total-deps)))))

;;; functions.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
