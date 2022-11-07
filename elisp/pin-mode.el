;;; pin-mode.el --- Pin windows: keep them from closing

;; Copyright (C) 2022 Christos Vagias
;; Author: Christos Vagias
;; URL: https://github.com/actondev/pin-mode
;; Filename: pin-mode.el
;; Description: Pin windows: keep them from closing
;; Created: 2022-11-07
;; Version: 0.0.1
;; Keywords: window pin

;;; Commentary:
;;
;; Install:
;;
;; (require '-pin-mode)
;;
;; Usage:
;;
;; (global-pin-mode t)
;; 
;; and then, in every buffer whose window you want pinned:
;; 
;; (pin-mode t)
;;
;; Example configuration with use-Package
;;
;; (use-package pin
;;   :load-path "elisp/"
;;   :defer 0
;;   :config
;;   (global-pin-mode 1)
;;   (when (boundp 'minions-mode)
;;     (add-to-list 'minions-prominent-modes 'pin-mode))
;;   (with-eval-after-load 'minions-mode
;;     (add-to-list 'minions-prominent-modes 'pin-mode)))
;;
;;; Code:

(defun pin-mode--update-no-delete-other-windows (&optional buffer)
  (unless buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((window (get-buffer-window)))
      (cond ((and global-pin-mode pin-mode)
	     (set-window-parameter window 'no-delete-other-windows t))
	    (t 
	     (set-window-parameter window 'no-delete-other-windows pin-mode--is-pinned-by-other))))))

(defvar-local pin-mode--is-pinned-by-other nil
  "Storing `no-delete-other-windows' value of a bufer's window before modifying it via pin-mode operations")

(defun pin-mode--init-pinned-by-other (buffer)
  (with-current-buffer buffer
    (when (and (window-parameter (get-buffer-window) 'no-delete-other-windows)
	       (not pin-mode))
      (setq-local pin-mode--is-pinned-by-other t))))

;;;###autoload
(define-minor-mode pin-mode
  "Pin mode protects a window from being deleted by `delete-other-windows'"
  :init-value nil
  :lighter "🖈" ;; pin in utf8: 📌 📍 🖈
  ;; Note: could use `(:eval (if global-pin-mode "🖈" "📌"))`
  (pin-mode--update-no-delete-other-windows))

;;;###autoload
(define-minor-mode global-pin-mode
  "Enabling pin global mode takes into account if `pin-mode' is enabled (per bufer)"
  :init-value nil
  :lighter "pin-global-mode"
  :global t
  (if global-pin-mode
      (progn ;; enabled
	(dolist (buf (buffer-list)) ;; todo for-each ?
	  (pin-mode--init-pinned-by-other buf)
	  (pin-mode--update-no-delete-other-windows buf))
	(add-hook 'buffer-list-update-hook #'pin-mode--update-no-delete-other-windows))
    (progn ;; disabled
      (dolist (buf (buffer-list))
	(pin-mode--update-no-delete-other-windows buf))
      (remove-hook 'buffer-list-update-hook #'pin-mode--update-no-delete-other-windows))))

(provide 'pin-mode)

;;; pin-mode.el ends here
