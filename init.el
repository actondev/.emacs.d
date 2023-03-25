(defmacro comment (&rest a))

;; Make gc pauses faster by decreasing the threshold.
;; we later lower it
(setq gc-cons-threshold (* 64 1000 1000)
      inhibit-startup-screen t)

(setq native-comp-async-report-warnings-errors nil)

(setq custom-file (expand-file-name ".emacs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(if t
    (progn
      (load (expand-file-name "bootstrap/elpaca.el" user-emacs-directory)))
  (progn
    (load (expand-file-name "bootstrap/use-package.el" user-emacs-directory))))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-verbose 'verbose
      use-package-minimum-reported-time 0.01)

;; for load-org-cached
(load (expand-file-name "functions.el" user-emacs-directory))

(message "Bootstrapped functions & package manager in %.2f seconds"
	 (float-time (time-subtract (current-time) before-init-time)))

(let ((start-time (current-time)))
  (load-org-cached "config.org")
  (load-org-cached "elisp.org")
  (message "Loaded .org files in %.2f seconds"
	   (float-time (time-subtract (current-time) start-time))))

(defun aod/require-init-packages ()
  (message "Requiring init packages...")
  (let ((start-time (current-time))
	(packages '(
		    minions
		    doom-themes
		    doom-modeline
		    dashboard
		    company
		    which-key
		    vertico
		    ;; from elisp/
		    ;; TODO autoloads not present?
		    ;; that's why I need to require those
		    aod-do
		    pin-mode
		    nice-jumper
		    )))
    (dolist (package packages)
      (condition-case err
	  (require package)
	(error (warn "Error while requiring %s: %s" package (error-message-string err)))))
    (message "   ...requiring init packages took %.2f seconds"
	     (float-time (time-subtract (current-time) start-time)))))

(setq aod/package-after-init-hook
      (if (featurep 'elpaca)
	  'elpaca-after-init-hook
	'after-init-hook))

;; revert gc threshold (should use after-init-hook or elpaca-after-init-hook if using elpaca)
(add-hook aod/package-after-init-hook
	  #'(lambda ()
	      (setq gc-cons-threshold (* 2 1000 1000))))

(add-hook aod/package-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract (current-time) before-init-time)))
                     gcs-done)
	    (aod/require-init-packages)
	    ))
