(defun aod.org/hide-others ()
  (interactive)
  (let ((cur (point))
	(start nil)
	(end nil))
    (ignore-errors
      (org-back-to-heading t))
    (setq start (point))
    (call-interactively #'org-next-visible-heading)
    (setq end (point))
    (goto-char cur)
    (narrow-to-region start end)))

(defun aod.org/present-home ()
  (interactive)
  (widen)
  (beginning-of-buffer)
  (aod.org/hide-others)
  )

(defun aod.org/present-prev ()
  (interactive)
  (widen)
  (org-shifttab 3)
  (call-interactively #'org-previous-visible-heading)
  (org-show-entry)
  (ignore-errors
    (org-narrow-to-subtree))

  (aod.org/hide-others)
  )

(setq aod-org/present-mode-map
      ;; nil
      (make-sparse-keymap)
      )

(define-minor-mode aod-org/present-mode
  "Toggle org present mode. In this mode You only see the current
  heading of the org file. M-n and M-p go to next/previous
  headings (and hiding the rest)"
  :init-value nil ; wtf is this
  :lighter "present"
  :keymap aod-org/present-mode-map
  (if aod-org/present-mode
      ;; enabled
      (progn
	(aod.org/present-prev))
    ;; disabled
    (progn
      (widen))
    ))

(setq aod-org/present-mode-map
      ;; nil
      (make-sparse-keymap)
      )

(define-key aod-org/present-mode-map (kbd "SPC") 'aod.org/present-next)
(define-key aod-org/present-mode-map (kbd "S-SPC") 'aod.org/present-prev)
(define-key aod-org/present-mode-map (kbd "M-<") 'aod.org/present-home)

(defun aod.org/present-next ()
  (interactive)
  (message "next")
  (widen)
  (org-shifttab 3)
  (call-interactively #'org-next-visible-heading)
  (org-show-entry)
  (ignore-errors
    (org-narrow-to-subtree))

  (aod.org/hide-others)
  )

(defhydra aod.hydra/org-present
  (
   :body-pre (progn
	       (message "body-pre")
	       (display-line-numbers-mode -1)
	       (org-show-entry)
	       (aod.org/hide-others))
   ;; :pre is called before each command
   :post (progn
	   (widen)
	   (display-line-numbers-mode 1))
   :foreign-keys run)
  "Org present"
  ("q" nil "quit")
  ("S-SPC" aod.org/present-prev "Prev")
  ("<mouse-3>" aod.org/present-prev)
  ("SPC" aod.org/present-next "Next")
  ("<mouse-1>" aod.org/present-next)
  )
