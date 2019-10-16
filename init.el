;; to be able to load with custom init.el location
;; see https://emacs.stackexchange.com/questions/4253/how-to-start-emacs-with-a-custom-user-emacs-directory
(setq user-init-file (or load-file-name (buffer-file-name)))
;; (setq user-emacs-directory (file-name-directory user-init-file))

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

