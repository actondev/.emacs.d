(defvar aod.window/placement nil
  "Set during TODO , either nil or one of 'left 'top 'right 'down")
(defvar aod.window/buffer nil
  "The buffer to place")

(defun aod.window/aw-hook (x)
  (select-window x)
  (pcase aod.window/placement
    ('left (split-window-right))
    ('top (split-window-below))
    ('down (split-window-below)
	   (windmove-down))
    ('right (split-window-right)
	    (windmove-right)))
  (setq aod.window/placement nil)
  (when aod.window/buffer
    (switch-to-buffer aod.window/buffer)
    (setq aod.window/buffer nil)))
(defun aod.window/aw-dispatch-function (x)
  (cond ((eq (aref (kbd "a") 0) x)
	 (message "Will place the window left of..")
	 (setq aod.window/placement 'left))
	((eq (aref (kbd "w") 0) x)
	 (message "Will place the window up of..")
	 (setq aod.window/placement 'up))
	((eq (aref (kbd "s") 0) x)
	 (message "Will place the window down of..")
	 (setq aod.window/placement 'down))
	((eq (aref (kbd "d") 0) x)
	 (message "Will place the window right..")
	 (setq aod.window/placement 'right))
	;; getting out
	((eq (aref (kbd "C-g") 0) x)
	 (throw 'done 'exit))
	((eq (aref (kbd "ESC") 0) x)
	 (throw 'done 'exit))
	(t (message "Unkown key %s, press C-g or ESC to exit" x))))

;;;###autoload
(defun aod.window/place-buffer (buffer)
  (interactive "bBuffer:")
  (if (<= (length (aw-window-list)) 1)
      (progn
	(message "Only 1 window is visible, placing buffer to other-window")
	(switch-to-buffer-other-window buffer))
    (progn
      (setq aod.window/buffer buffer)
      (setq aod.window/placement nil))
    (let ((aw-dispatch-always t)
	  (aw-display-mode-overlay nil)
	  (aw-dispatch-function #'aod.window/aw-dispatch-function))
      (aw-select "Use a-w-s-d to place buffer left/top/right/down of the selected window" #'aod.window/aw-hook))))

;; (comment
;;  (setq aw-display-mode-overlay nil)
;;  (setq aw-display-mode-overlay t)
;;  (setq aw-minibuffer-flag t)
;;  )

(provide 'aod-window)
