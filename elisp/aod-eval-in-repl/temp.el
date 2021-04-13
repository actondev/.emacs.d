;; notes about quickly inserting repl buffer text into the org src block

(defun aod/shell-or-term-buffers ()
  (let ((buffers)
	(modes '(shell-mode term-mode vterm-mode)))
    (dolist (buf (buffer-list) buffers)
      (with-current-buffer buf
        (when (memq major-mode modes)
          (push buf buffers))))
    buffers))

(defun aod/shell-or-term-windows ()
  (delq nil
	(mapcar (lambda (buffer)
		  (get-buffer-window buffer t))
		(aod/shell-or-term-buffers))))

(aod/shell-or-term-windows)



(cl-letf (((symbol-function 'avy-window-list) (lambda ()
						(progn
						  (message "here, called avy-window-list?")
						  (aod/shell-or-term-windows)))))
  (avy-goto-line))


(cl-letf (((symbol-function 'avy-window-list)
	   (lambda ()
	     (list (get-buffer-window "*local-bouncr-analytics-sql*")))))
  (let* ((res-buffer)
	 (beg)
	 (end))
    (save-window-excursion (setq beg (avy-goto-char-timer))
			   (message "current buffer %s" (current-buffer))
			   (setq res-buffer (current-buffer)))
    (save-window-excursion (setq end (avy-goto-char-timer)))
    (message "res buffer %s beg %s end %s" res-buffer beg end)
    (with-current-buffer res-buffer 
      (buffer-substring-no-properties (car beg)
				      (car end)))))

(cl-letf (((symbol-function 'avy-window-list)
	   (lambda ()
	     (list (get-buffer-window "*local-bouncr-analytics-sql*")))))
  (let* ((res-buffer)
	 (beg)
	 (end))
    (save-window-excursion (setq beg (avy-goto-char-timer))
			   (message "current buffer %s" (current-buffer))
			   (setq res-buffer (current-buffer)))
    (save-window-excursion (setq end (avy-goto-char)))
    (message "res buffer %s beg %s end %s" res-buffer beg end)
    (with-current-buffer res-buffer 
      (buffer-substring-no-properties (car beg)
				      (car end)))))


;; or.. advice around avy-action-goto
(let ((goto-char-original (symbol-function 'goto-char)))
  (cl-letf (((symbol-function 'goto-char) (lambda (pt)
					    ;; (message "going to %s" pt)
					    ;; (when (eq major-mode 'vterm-mode)
					    ;;   (vterm-copy-mode 1))
					    (funcall goto-char-original pt))))
    (avy-goto-char-timer)))


;; copy between two searches
