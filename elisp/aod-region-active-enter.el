(setq aod.region-active-enter/callback (lambda (beg end)))

(setq aod.region-active-enter/mode-map (make-sparse-keymap))

(define-minor-mode aod.region-active-enter/mode
  "Toggle org present mode. In this mode You only see the current
  heading of the org file. M-n and M-p go to next/previous
  headings (and hiding the rest)"
  :init-value nil ; wtf is this
  :lighter "region-active-enter"
  :keymap aod.region-active-enter/mode-map
  (if aod.region-active-enter/mode
      ;; enabled
      (progn
	;; TODO term-mode also.. make it a generic function?
	(when (equal major-mode 'vterm-mode)
	  (vterm-copy-mode t))
	(push `(vterm-copy-mode-map . ,aod.region-active-enter/mode-map)
	      minor-mode-overriding-map-alist)
	;; (set-mark (point))
	(message "Select your region and press enter!")
	)
    ;; disabled
    (progn
      ;; better restore previous.. but it's ok for now
      (setq minor-mode-overriding-map-alist ()))))

(defun aod.region-active-enter/start (callback)
  (setq-local aod.region-active-enter/callback callback)
  (aod.region-active-enter/mode t))

;; test
;; (aod.region-active-enter/start (lambda (beg end) (message "Our region is %s" (buffer-substring-no-properties beg end))))

(defun aod.region-active-enter/on-enter ()
  (interactive)
  (aod.region-active-enter/mode -1)
  (when (region-active-p)
    (funcall aod.region-active-enter/callback (mark) (point))))

(defun aod.region-active-enter/stop ()
  (interactive)
  (aod.region-active-enter/mode -1))

(define-key aod.region-active-enter/mode-map (kbd "<return>") 'aod.region-active-enter/on-enter)
(define-key aod.region-active-enter/mode-map (kbd "C-g") 'aod.region-active-enter/stop)

;; with avy

(setq aod.avy/goto-choices
      '((?t "char with timer" nil avy-goto-char-timer)
	(?c "char" nil avy-goto-char)
	(?l "line" nil avy-goto-line)
	(?w "word" nil avy-goto-word-1)))

(defun aod/focus-buffer (buffer)
  (let ((frame (and
		(get-buffer-window buffer t)
		(window-frame (get-buffer-window buffer t)))))
    (if (and frame
	     (not (equal frame (selected-frame))))
	(progn
	  (message "raising frame")
	  (select-frame-set-input-focus frame)
	  (raise-frame frame))
      (progn
	(message "switch to other window")
	(switch-to-buffer-other-window buffer)))))

;; (aod/focus-buffer "*bouncr-docker*")
;; (aod/focus-buffer "analytics-sql-queries.org")

;; (let ((buffer "*bouncr-docker*"))
;;   (and
;;    (get-buffer-window buffer t)
;;    (window-frame (get-buffer-window buffer t))))

;; Check
;; (selected-window)
;; (select-window w)
;; what avy-copy-region calls
(defun aod.avy/copy-yank-from (buffer)
  (interactive "bBuffer:")
  (let ((current-buffer (current-buffer))
	(current-window-configuration (current-window-configuration)))
    (aod/focus-buffer buffer)
    ;; (switch-to-buffer-other-window buffer)
    ;; hooks?
    (when (equal major-mode 'vterm-mode)
      ;; (message "vterm ??")
      (vterm-copy-mode t))
    (setq-local aod.avy/-copy-yank-buffer current-buffer)
    (setq-local aod.avy/-copy-yank-point (point))
    (setq-local aod.avy/-copy-yank-wc current-window-configuration))
  (aod.region-active-enter/start (lambda (beg end)
				   (kill-ring-save beg end)
				   (let ((str (buffer-substring-no-properties beg end))
					 (orig-buffer aod.avy/-copy-yank-buffer)
					 (orig-wc aod.avy/-copy-yank-wc))
				     ;; (message "orig buffer %S" orig-buffer)
				     (aod/focus-buffer orig-buffer)
				     (insert str)
				     (set-window-configuration orig-wc)))))
