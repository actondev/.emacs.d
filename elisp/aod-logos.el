;;; aod-logs.el --- simple presentation mode for org file  -*- lexical-binding: t; -*-

(setq aod-logos/present-mode-map (make-sparse-keymap))
;; Note: when using aod.logos/present-mode I had errors in emacs < 29
;; during easy-mmode--mode-docstring
;; called from define-minor-mode
(define-minor-mode aod-logos/present-mode
  "Toggle present mode"
  :init-value nil
  :lighter "present"
  :keymap aod-logos/present-mode-map
  (require 'logos)
  (if aod-logos/present-mode
      (progn
	(logos-narrow-dwim)
	(logos-focus-mode 1)
	(logos--mode 'menu-bar-mode -1)
	(logos--mode 'tool-bar-mode -1)
	(logos--mode 'display-line-numbers-mode -1)
	(logos--mode 'org-modern-mode 1)

        ;; backing up
        (setq-local aod.logos/org-hide-emphasis-markers org-hide-emphasis-markers)

        ;; while it looks cool, it messes up the table alignment, when
        ;; previously aligned *WITH* the marker
        ;; comment or uncomment next line accordingly
	;; (setq-local org-hide-emphasis-markers t)

        ;; trigger update for org-hide-emphasis-markers
	(font-lock-mode 1)
	)
    (progn
      (logos-focus-mode -1)
      ;; reverting
      (setq-local org-hide-emphasis-markers aod.logos/org-hide-emphasis-markers)
      (font-lock-mode 1)
      (widen)
      )))

(define-key aod-logos/present-mode-map (kbd "<f7>") 'logos-backward-page-dwim)
(define-key aod-logos/present-mode-map (kbd "<f9>") 'logos-forward-page-dwim)
(define-key aod-logos/present-mode-map (kbd "M-p") 'logos-backward-page-dwim)
(define-key aod-logos/present-mode-map (kbd "M-n") 'logos-forward-page-dwim)

(defun aod-logos/backward-skip-comment (&rest args)
  (when (org-in-commented-heading-p)
    (call-interactively #'logos-backward-page-dwim)))

(defun aod-logos/forward-skip-comment (&rest args)
  (when (org-in-commented-heading-p)
    (call-interactively #'logos-forward-page-dwim)))

(advice-add 'logos-backward-page-dwim :after #'aod-logos/backward-skip-comment)
(advice-add 'logos-forward-page-dwim :after #'aod-logos/forward-skip-comment)

(provide 'aod-logos)
