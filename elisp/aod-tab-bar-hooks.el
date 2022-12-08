;;; aod-tab-bar-hooks.el --- Collection of hooks to show certain buffers in their own tab  -*- lexical-binding: t; -*-

(defun aod.tab-bar/make-project-tab-bar-hook (prefix)
  (lambda ()
    "Hook to move buffer to its own tab, named by `prefix: <current-project-name>`"
    (let ((is-explicit-named (alist-get 'explicit-name (tab-bar--current-tab)))
	  (current-name (alist-get 'name (tab-bar--current-tab)))
	  (wanted-name (format "%s%s" prefix
			       (cond ((fboundp 'project)
				      (project-name (project-current)))
				     ((featurep 'projectile)
				      (projectile-project-name))
				     (t (error "could not figure out project name"))))))
      (when (and is-explicit-named
		 (not (string= current-name wanted-name)))
	(message "switching to tab %s" wanted-name)
	(let ((buffer (current-buffer)))
	  (delete-window)
	  (tab-bar-switch-to-tab wanted-name)
	  ;; instead of just switching to buffer, we do some window trickery
	  ;; embark & xref seems to mess up with the current buffer
	  (switch-to-buffer-other-window buffer)
	  (delete-other-windows))))))

(provide 'aod-tab-bar-hooks)

