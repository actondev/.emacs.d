(setq aod.mark-on-enter/callback (lambda (beg end)))

(setq aod.mark-on-enter/mode-map (make-sparse-keymap))

(define-minor-mode aod.mark-on-enter/mode
  "Toggle org present mode. In this mode You only see the current
  heading of the org file. M-n and M-p go to next/previous
  headings (and hiding the rest)"
  :init-value nil ; wtf is this
  :lighter "present"
  :keymap aod.mark-on-enter/mode-map
  (let ((aod.mark-on-enter/-override-alist minor-mode-overriding-map-alist))
    (if aod.mark-on-enter/mode
	;; enabled
	(progn
	  ;; (setq-local aod.mark-on-enter/-override-alist minor-mode-overriding-map-alist)
	  (when (equal major-mode 'vterm-mode))
	  (vterm-copy-mode t)
	  (push `(vterm-copy-mode-map . ,aod.mark-on-enter/mode-map)
		minor-mode-overriding-map-alist)
	  (set-mark (point))
	  (message "hoo"))
      ;; disabled
      (progn
	;; hm, using the previously
	(setq minor-mode-overriding-map-alist ())
	;; (setq minor-mode-overriding-map-alist aod.mark-on-enter/-override-alist)
	))))

(defun aod.mark-on-enter/start (callback)
  (setq-local aod.mark-on-enter/callback callback)
  (aod.mark-on-enter/mode t))

;; test
;; (aod.mark-on-enter/start (lambda (beg end) (message "%s" (buffer-substring-no-properties beg end))))

(defun aod.mark-on-enter/on-enter ()
  (interactive)
  (aod.mark-on-enter/mode -1)
  (message "on enter")
  (when (region-active-p)
    (funcall aod.mark-on-enter/callback (mark) (point))))

(defun aod.mark-on-enter/stop ()
  (interactive)
  (message "stop 2!")
  (aod.mark-on-enter/mode -1))

(define-key aod.mark-on-enter/mode-map (kbd "<return>") 'aod.mark-on-enter/on-enter)
(define-key aod.mark-on-enter/mode-map (kbd "C-g") 'aod.mark-on-enter/stop)

;; with avy

(setq aod.avy/goto-choices
      (list (list ?t "char with timer" 'avy-goto-char-timer)
	    (list ?l "line" 'avy-goto-line)
	    (list ?w "word" 'avy-goto-word-1)))

(defun aod.avy/copy-yank-from (buffer)
  (interactive "bBuffer:")
  (message "buf %s" buffer)
  (let ((current-buffer (current-buffer)))
    (switch-to-buffer-other-window buffer)
    ;; hooks?
    (when (equal major-mode 'vterm-mode)
      ;; (message "vterm ??")
      (vterm-copy-mode t))
    (setq-local aod.avy/-copy-yank-buffer current-buffer)
    (setq-local aod.avy/-copy-yank-point (point)))
  ;; (let ((choice (read-multiple-choice "avy action:" aod.avy/goto-choices)))
  ;;   (call-interactively (caddr choice)))
  ;; (avy-goto-char-timer)
  (aod.mark-on-enter/start (lambda (beg end)
			     (let ((str (buffer-substring-no-properties beg end)))
			       (switch-to-buffer aod.avy/-copy-yank-buffer)
			       ;; (goto-char aod.avy/-copy-yank-point)
			       (insert str)))))


