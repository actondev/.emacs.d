;;; aod-command-repeat.el --- Repeat command by using the final binding keystroke  -*- lexical-binding: t; -*-

;; Example usage:
;; (aod.command-repeat/enable 'winner-undo)
;; (aod.command-repeat/disable 'winner-undo)

(defun aod.command-repeat/--advice (function &rest args)
  (apply function args)
  (let ((map (make-sparse-keymap))
	(repeat-function (lambda ()
			   (interactive)
			   (message "aod.command-repeat: repeating %s" function)
			   (apply function args))))
    (define-key map
		(vector last-command-event)
		repeat-function)
    (set-transient-map map t)))

(defun aod.command-repeat/enable (function)
  (advice-add function :around 'aod.command-repeat/--advice))

(defun aod.command-repeat/disable (function)
  (advice-remove function 'aod.command-repeat/--advice))

(provide 'aod-command-repeat)
