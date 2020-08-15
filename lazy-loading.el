(defun bk-load-path-add (subdirectory)
  "If DIRECTORY with SUBDIRECTORY exist add it to `load-path'.
Return non-nil if successful. Used for external packages."
  (let* ((path (concat (file-name-as-directory
				   (expand-file-name bk-external-packages-dir))
		       subdirectory)))
    (when (file-readable-p path)
      (add-to-list 'load-path path))))

(defun bk-auto-loads (file &rest func-or-ext-with-func)
  "`autoload' and `auto-mode-alist' for packages."
  (dolist (x func-or-ext-with-func)
    (autoload
      (if (consp x) (cdr x) x)
      file
      "Undocumented `autoload'."
      t)
    (when (consp x) (add-to-list 'auto-mode-alist x))))
