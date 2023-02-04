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

(defvar-local aod-do/action nil)

(defvar aod-do/action-global nil
  "Set this to override the aod-do/action-interactive to call this instead of aod-do/action")

(defvar aod-do/registered-actions (list))

(defun aod-do/register-action (action)
  (add-to-list 'aod-do/registered-actions action))

(defun aod-do/read-action ()
  (if aod-do/registered-actions
      (let ((choice ; NB: intern-soft will return nil if passed empty string or if no such symbol exists
	     (intern-soft (completing-read "Action: "
					   aod-do/registered-actions
					   ))))
	(symbol-function choice))
    (progn
      (message "No action has been registered! You can register one with `aod-do/register-action'")
      nil)))

(defun aod-do/set-global-action ()
  (interactive)
  (setq aod-do/action-global (aod-do/read-action)))

(defun aod-do/action-interactive (prefix)
  (interactive "P")
  (setq aod-do/saved-window-excursion (current-window-configuration))
  (setq aod-do/saved-buffer (current-buffer))
  (cond
   ((and aod-do/action-global ; global action is bypassed with a prefix argument
	 (not prefix))
    (funcall aod-do/action-global))
   ((and aod-do/action ; local action is bypassed with a prefix argument
	 (not prefix))
    (funcall aod-do/action))
   ((eq 1 (length aod-do/registered-actions)) ; defaulting to sole action
    (let ((action (car aod-do/registered-actions)))
      (message "calling sole registered action %s" action)
      (funcall action)))
   (:else ; otherwise we prompt for action
    (when-let ((action (aod-do/read-action)))
      (funcall action)))))

(provide 'aod-do)
