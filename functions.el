;; some helper functions to have available in init.el

(defun get-string-from-file (filePath)
  "Return filePath's file content.
  Credit: http://ergoemacs.org/emacs/elisp_read_file_content.html
  Note: (file-readable-p \".\") returns t so.. wtf
"
  (condition-case nil
      (with-temp-buffer
	(insert-file-contents filePath)
	(buffer-string))
    ((message error) "")))
(defun md5-file (filePath)
  (md5 (get-string-from-file filePath)))

(defun load-org-cached (rel-path)
  "Loading an org file (using a relative path from the init file)
- If the tangled .el file exists and the md5 checksum hasn't changed, then the .el file is loaded without parsing the .org file.
- Otherwise, the .org file is parsed and loaded."
  (let* ((path (relative-from-init rel-path))
	 ;; Note: make-symbol is not gonna work
	 (symbol (intern (concat "md5_" path)))
	 (checksum (md5-file path))
	 (stored-checksum (if (boundp symbol) (symbol-value symbol) nil ))
	 (el-path (concat (file-name-sans-extension path)
			  ".el")))
    (if (and
	 (equal checksum stored-checksum)
	 (file-exists-p el-path))
	(progn
	  (message "Loading cached .el file %s " el-path)
	  (load-file el-path)
	  )
      (progn
	(message "Checksum mismatch, or .el not found, loading .org file %s" path)
	(require 'org)
	(org-babel-load-file path)
	)
      )
    (when (not (equal checksum stored-checksum))
      (message "Saving new checksum for %s" path)
      ;; after all went good, store the checksum
      (customize-save-variable symbol checksum))))

(defun aod.org/insert-src (mode)
  "Insert a new source block and start editing it in its own editor
  TODO
  - see auto-mode-alist : would be nice to have completion for the mode"
  (interactive "sMode:")
  (message (format "selected mode %s" mode))
  (indent-according-to-mode)
  (insert (format "#+BEGIN_SRC %s" mode))
  (newline)
  (save-excursion
    (newline-and-indent)
    (insert "#+END_SRC"))
  (org-edit-special))

(defun aod.org/goto-end-of-src-block ()
  (interactive)
  (search-forward "#+END_SRC")
  (indent-new-comment-line))

(defun aod-org/execute-elisp ()
  "Executes current org-babel src block without asking.
  Plus, it doesn't use the save-window-excursion that C-c C-c does"
  (interactive)
  (let* ((info (org-babel-get-src-block-info))
	 (body (nth 1 info)))
    (eval (read body))))

(defun save-as (new-filename)
  "Save current buffer to a new file.
  Credits https://stackoverflow.com/questions/5168262/emacs-write-buffer-to-new-file-but-keep-this-file-open"
  (interactive "FFilename:")
  (save-restriction
    (widen)
    (write-region (point-min) (point-max) new-filename))
  (find-file-noselect new-filename)
  )
