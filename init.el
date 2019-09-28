(defun relative-from-init (file)
  "Returns a complete file path relative from the init file.
  eg if loaded ~/.emacs.d/init.el and you pass config.org you get
  ~/.emacs.d/config.org"
  (format "%s%s" (file-name-directory user-init-file) file))

(package-initialize)
(org-babel-load-file (relative-from-init "config.org")) ;; loading relative, instead of "~/.emacs.d/config.org")

(setq custom-file (relative-from-init ".emacs-custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(message (format "%s%s" "Startup time: " (emacs-init-time)))

