;; pro tip:
;; run the following into a known project
;; 
;; (add-to-list 'aod-do/eval-safe-directories (format "%s.*" default-directory))
;; (customize-save-variable 'aod-do/eval-safe-directories aod-do/eval-safe-directories)
;; and even better, add this snippet into the dev.org of the project
;; 
;; then, in the .dir-locals.el
;; (defhydra foo ...)
;;
;; (setq-local aod-do/action #'s7bi/body)

(defcustom aod-do/eval-safe-directories ()
  "List of safe directories to perform eval in dir-locals")

(defun aod-do/eval-safep (orig-fun &rest args)
  (catch 'done
    ;; (dolist (dir aod-do/eval-safe-directories)
    ;;   (when (string-match-p dir default-directory)
    ;; 	(message "safe directory match with %s default-directory %s " dir default-directory)
    ;; 	(throw 'done t)))
    (message "Calling original hack-one-local-variable-eval-safep in default-directory %s " default-directory)
    (apply orig-fun args)))

(advice-add 'hack-one-local-variable-eval-safep :around #'aod-do/eval-safep)

(comment
 (advice-remove 'hack-one-local-variable-eval-safep :around #'aod-do/eval-safep)
 )

(defvar aod-do/saved-window-excursion
  ())

(defvar aod-do/saved-buffer
  ())

(defun aod-do/restore ()
  (interactive)
  (set-buffer aod-do/saved-buffer)
  (set-window-configuration aod-do/saved-window-excursion)
  (transient-quit-one))

(defvar-local aod-do/action
  (lambda ()
    (message "no action has been set!")))

(defvar aod-do/action-global nil
  "Set this to override the aod-do/action-interactive to call this instead of aod-do/action")

(defvar aod-do/registered-actions (list ()))

(defun aod-do/register-action (action)
  (add-to-list 'aod-do/registered-actions action))

(defun aod-do/set-global-action ()
  (interactive)
  (let ((choice
	 (completing-read "Action: "
			  (mapcar (lambda (el)
				    (if (null el)
					"nil"
				      (format "#'%s" el)))
				  aod-do/registered-actions))))
    (setq aod-do/action-global (eval (read choice)))))

(defun aod-do/action-interactive ()
  (interactive)
  (setq aod-do/saved-window-excursion (current-window-configuration))
  (setq aod-do/saved-buffer (current-buffer))
  (if aod-do/action-global
      (funcall aod-do/action-global)
    (funcall aod-do/action)))

(provide 'aod-do)

