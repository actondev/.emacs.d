;;; eat-compilation.el --- compilation mode using eat (Emulate A Terminal)  -*- lexical-binding: t; -*-
;;; credits: https://codeberg.org/akib/emacs-eat/issues/33#issuecomment-1127805
;;; slightly adjusted to report compilation time duration similarly to normal compilation-mode

(defun eat-compilation-exit-hook (process)
  (when (boundp 'compilation--start-time)
    (compilation-insert-annotation
     " at "
     (substring (current-time-string) 0 19)
     ", duration "
     (let ((elapsed (- (float-time) compilation--start-time)))
       (cond ((< elapsed 10) (format "%.2f s" elapsed))
             ((< elapsed 60) (format "%.1f s" elapsed))
             (t (format-seconds "%h:%02m:%02s" elapsed)))))))

(defun eat-compilation-start (command &optional mode name-function highlight-regexp continue)
  (let ((name-of-mode "compilation")
        (dir default-directory)
        outbuf)
    (if (or (not mode) (eq mode t))
        (setq mode #'compilation-minor-mode)
      (setq name-of-mode (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
    (with-current-buffer
        (setq outbuf
              (get-buffer-create
               (compilation-buffer-name name-of-mode mode name-function)))
      (setq default-directory dir)
      (setq buffer-read-only nil)
      (erase-buffer)
      (compilation-insert-annotation
       "-*- mode: " name-of-mode
       "; default-directory: "
       (prin1-to-string (abbreviate-file-name default-directory))
       " -*-\n")
      (compilation-insert-annotation
       (format "%s started at %s\n\n"
               mode-name
	       (substring (current-time-string) 0 19))
       command "\n")
      (eat-mode)
      (setq compilation--start-time (float-time))
      (eat-exec outbuf "*compile*" shell-file-name nil (list "-lc" command))
      (run-hook-with-args 'compilation-start-hook (get-buffer-process outbuf))
      (eat-emacs-mode)
      (funcall mode)
      (setq next-error-last-buffer outbuf)
      (display-buffer outbuf '(nil (allow-no-window . t)))
      (when-let (w (get-buffer-window outbuf))
        (set-window-start w (point-min))))))

;;;###autoload
(define-minor-mode eat-compilation-mode
  "Toggle eat compilation mode"
  :global t
  (require 'eat)
  (add-hook 'eat-exit-hook #'eat-compilation-exit-hook)
  (advice-add #'compilation-start :override #'eat-compilation-start)
  (if eat-compilation-mode
      (progn
        (message "eat compilation: enabled")
        (advice-add #'compilation-start :override #'eat-compilation-start)
        )
    (progn
      (message "eat compilation: disabled")
      (advice-remove #'compilation-start #'eat-compilation-start))))


(provide 'eat-compilation)
