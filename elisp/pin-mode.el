;;; pin-mode.el --- Pin windows: keep them from closing

;; Copyright (C) 2022 Christos Vagias
;; Author: Christos Vagias
;; URL: https://github.com/actondev/pin-mode
;; Filename: pin-mode.el
;; Description: Pin windows: keep them from closing
;; Created: 2022-11-07
;; Version: 0.0.1
;; Keywords: window management pin

;;; Commentary:
;;
;; Install:
;;
;; (require 'pin-mode)
;;
;; Usage:
;;
;; (global-pin-mode 1)
;; 
;; and then, in every window you want pinned run:
;; 
;; (pin-window)
;;
;; Example configuration with use-Package
;;
;; (use-package pin-mode
;;   :load-path "elisp/"
;;   :defer 0
;;   :config
;;   (global-pin-mode 1)
;;   (with-eval-after-load "minions"
;;     (add-to-list 'minions-prominent-modes 'global-pin-mode)))
;;
;;; Code:

(defcustom pin-indicator "📌"
  "This string is shown in the `global-pin-mode' lighter when a buffer's window is pinned. Suggestions: 📌 📍 🖈"
  :type 'string
  :group 'pin-mode)

;;;###autoload
(define-minor-mode global-pin-mode
  "Enabling pin global mode takes into account if `pin-mode' is enabled (per bufer)"
  :init-value nil
  :lighter (:eval (if (window-parameter (selected-window) 'no-delete-other-windows) pin-indicator ""))
  :global t)

;;;###autoload
(defun pin-window (window)
  "Protects a window from being deleted with `delete-other-windows'"
  (interactive (list (selected-window)))
  (set-window-parameter window 'no-delete-other-windows t))

;;;###autoload
(defun unpin-window (window)
  "Unprotects a window from being deleted with `delete-other-windows'"
  (interactive (list (selected-window)))
  (set-window-parameter window 'no-delete-other-windows nil))

;;;###autoload
(defun unpin-all-windows ()
  "Unprotects all windows of current frame from being deleted"
  (interactive)
  (dolist (window (window-list))
    (unpin-window window)))

(provide 'pin-mode)

;;; pin-mode.el ends here
