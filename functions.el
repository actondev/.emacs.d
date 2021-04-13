;; some helper functions to have available in init.el

(defun get-file-contents (filePath)
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
  (md5 (get-file-contents filePath)))

(defun load-org-cached (rel-path)
  (load-org-cached-mod-time rel-path))

(defun load-org-cached-md5 (rel-path)
  "Loading an org file (using a relative path from the init file)
- If the tangled .el file exists and the md5 checksum hasn't changed, then the .el file is loaded without parsing the .org file.
- Otherwise, the .org file is parsed and loaded.

TODO could just use last mod date to check .org and .el modified date?
that would enable manually tagling"
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
	  (load-file el-path))
      (progn
	(message "Checksum mismatch, or .el not found, loading .org file %s" path)
	(require 'org)
	(org-babel-load-file path)))
    (when (not (equal checksum stored-checksum))
      (message "Saving new checksum for %s" path)
      ;; after all went good, store the checksum
      (customize-save-variable symbol checksum))))

(defun load-org-cached-mod-time (rel-path)
  "Loading an org file (using a relative path from the init file)
- If the tangled .el file exists and the .org isn't last mod date is NOT newer,
then the .el file is loaded without parsing the .org file.
- Otherwise, the .org file is parsed and loaded."
  (let* ((path (relative-from-init rel-path))
	 (el-path (concat (file-name-sans-extension path)
			  ".el")))
    (if (and
	 (file-exists-p el-path)
	 (not (file-newer-than-file-p path el-path)))
	(progn
	  (message "Loading tangled .el file %s " el-path)
	  (load-file el-path))
      (progn
	(message ".el not found, org .org is modified: loading .org file %s" path)
	(require 'org)
	(org-babel-load-file path)))))

(defun aod/shell-command-disown (command)
  (interactive "sCommand: ")
  (shell-command  (format "(%s &> /dev/null &)" command) nil nil))
