(setq start-time (float-time))

(defun msg-time (text)
  (message (format "%s : %f" text (- (float-time) start-time))))

;; WIP: startup time
(setq gc-cons-threshold 64000000
      inhibit-startup-screen t)

(add-hook 'after-init-hook
	  #'(lambda ()
	      (setq gc-cons-threshold 800000)))
;; startup time end


;; to be able to load with custom init.el location
;; see https://emacs.stackexchange.com/questions/4253/how-to-start-emacs-with-a-custom-user-emacs-directory
(setq user-init-file (or load-file-name (buffer-file-name)))
;; (setq user-emacs-directory (file-name-directory user-init-file))

(defun relative-from-init (file)
  "Returns a complete file path relative from the init file.
  eg if loaded ~/.emacs.d/init.el and you pass config.org you get
  ~/.emacs.d/config.org"
  (format "%s%s" (file-name-directory user-init-file) file))

(setq custom-file (relative-from-init ".emacs-custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(load (relative-from-init "functions.el"))

(package-initialize)
;; loading org files : not loading org-mode everytime, but load the .el cached file
(load-org-cached "config.org")

(message (format "%s%s" "Startup time: " (emacs-init-time)))
