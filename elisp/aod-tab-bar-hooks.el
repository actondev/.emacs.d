;;; aod-tab-bar-hooks.el --- Collection of hooks to show certain buffers in their own tab  -*- lexical-binding: t; -*-

(defun aod.tab-bar/make-project-tab-bar-post-hook (prefix)
  
  (lambda ()
    "Hook to move buffer to its own tab, named by `prefix: <current-project-name>`"
    (when (project-current)
      (let ((is-explicit-named (alist-get 'explicit-name (tab-bar--current-tab)))
	    (current-name (alist-get 'name (tab-bar--current-tab)))
	    (wanted-name (format "%s%s" prefix (project-name (project-current)))))
	(when (and is-explicit-named
		   (not (string= current-name wanted-name)))
	  (message "switching to tab %s" wanted-name)
	  (let ((buffer (current-buffer)))
	    (delete-window)
	    (tab-bar-switch-to-tab wanted-name)
	    ;; instead of just switching to buffer, we do some window trickery
	    ;; embark & xref seems to mess up with the current buffer
	    (switch-to-buffer-other-window buffer)
	    (delete-other-windows)))))))

(defun aod.tab-bar/make-project-tab-bar-pre-hook (prefix)
  (lambda ()
    (when (project-current)
      "Hook to move buffer to its own tab, named by `prefix: <current-project-name>`"
      (let ((is-explicit-named (alist-get 'explicit-name (tab-bar--current-tab)))
	    (current-name (alist-get 'name (tab-bar--current-tab)))
	    (wanted-name (format "%s%s" prefix
				 (project-name (project-current)))))
	(when (and is-explicit-named
		   (not (string= current-name wanted-name)))
	  (message "switching to tab %s" wanted-name)
	  (tab-bar-switch-to-tab wanted-name))))))

(provide 'aod-tab-bar-hooks)

