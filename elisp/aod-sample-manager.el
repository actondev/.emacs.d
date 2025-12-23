;;; aod-sample-manager.el --- sample manager like functionality for dired  -*- lexical-binding: t; -*-

(defcustom aod-sample-manager/play-program "play"
  "The program to use to play samples. play is part of sox package & supports flac as well, unlike aplay"
  :type 'string
  :group 'aod-sample-manager)

(defvar aod-sample-manager/play-process nil "Holds the currently running play process")

(defun aod-sample-manager/play-file (file)
  (interactive "f")
  (message "playing %s" (file-name-nondirectory file))
  (aod-sample-manager/stop)
  (setq aod-sample-manager/play-process (start-process aod-sample-manager/play-program nil aod-sample-manager/play-program (f-expand file))))

(defvar aod-sample-manager/play-mode-map (make-sparse-keymap))
(define-key aod-sample-manager/play-mode-map (kbd "n") 'aod-sample-manager/next)
(define-key aod-sample-manager/play-mode-map (kbd "p") 'aod-sample-manager/prev)
(define-key aod-sample-manager/play-mode-map (kbd "SPC") 'aod-sample-manager/play-toggle)
(define-key aod-sample-manager/play-mode-map (kbd "C-g") 'aod-sample-manager/stop)
(define-key aod-sample-manager/play-mode-map (kbd "j") 'aod-sample-manager/jump)

(defun aod-sample-manager/down-mouse (event)
  (interactive "e")
  "Make org links draggable to external applications."
  (when mark-active
    (deactivate-mark))
  (with-selected-window (posn-window (event-end event))
    (goto-char (posn-point (event-end event))))
  (when-let ((link (aod-sample-manager/get-current)))
    (aod-sample-manager/play-file link)
    ;; mouse pos: (posn-x-y (cadr event)) or (nth 2 (cadr event))
    (track-mouse
      (let ((event (read-event)))
        (when (eq (car event) 'mouse-movement)
          (aod-sample-manager/stop)
          (dnd-begin-file-drag link nil 'copy t))))))

;; Use down-mouse-1 to play, track mouse and on mouse movement stop playback & start drag-n-drop.
;; REMARK: down-mouse-1 doesn't let mouse-1 to get through when I mess with dnd etc
;; Also: drag-mouse-1 is generated on "drop", not on move.
(define-key aod-sample-manager/play-mode-map [down-mouse-1] 'aod-sample-manager/down-mouse)

(defvar aod-sample-manager/supported-extentions '("wav" "mp3" "ogg" "flac"))

;;;###autoload
(define-minor-mode aod-sample-manager/play-mode
  "Toggle play mode. use n and p to navigate files and automatically.
Use j to jump to the file in dired. You can then drag-n-drop the
file to another window. `dired-mouse-drag-files' needs to bet for
this behavior. It is enabled by default upon enabling the mode"
  :init-value nil                       ; wtf is this
  :lighter "sample-manager"
  :keymap aod-sample-manager/play-mode-map
  (if aod-sample-manager/play-mode
      (pcase major-mode
        ('dired-mode
         (setq dired-mouse-drag-files t))
        ('org-mode
         ;; (define-key org-mouse-map [mouse-2] nil) ; org-open-at-mouse
         )
        (_ (aod-sample-manager/play-mode -1)
           (user-error "play mode only works in dired or org mode")))
    (message "play mode disabled")))

(defun aod-sample-manager/supported-or-nil (path)
  (when (member (downcase (file-name-extension path)) aod-sample-manager/supported-extentions)
    path))

(defun aod-sample-manager/get-current-org ()
  (let* ((context (org-element-context))
         (type (org-element-type context))
         (path nil))
    (when (eq type 'link)
      (setq path (expand-file-name (org-element-property :path context)))
      (aod-sample-manager/supported-or-nil path))))

(defun aod-sample-manager/get-current-dired ()
  (aod-sample-manager/supported-or-nil (dired-get-filename)))

(defun aod-sample-manager/get-current ()
  (pcase major-mode
    ('dired-mode (aod-sample-manager/get-current-dired))
    ('org-mode (aod-sample-manager/get-current-org))
    (_ (user-error "unsupported mode"))))

(defun aod-sample-manager/next-dired ()
  (dired-next-line 1))

(defun aod-sample-manager/next-org ()
  (org-next-link))

(defun aod-sample-manager/next ()
  (interactive)
  (pcase major-mode
    ('dired-mode (aod-sample-manager/next-dired))
    ('org-mode (aod-sample-manager/next-org))
    (_ (user-error "unsupported mode")))
  (when-let ((file (aod-sample-manager/get-current)))
    (aod-sample-manager/play-file file)))

(defun aod-sample-manager/prev-dired ()
  (dired-next-line -1))

(defun aod-sample-manager/prev-org ()
  (org-previous-link))

(defun aod-sample-manager/prev ()
  (interactive)
  (pcase major-mode
    ('dired-mode (aod-sample-manager/prev-dired))
    ('org-mode (aod-sample-manager/prev-org))
    (_ (user-error "unsupported mode")))
  (when-let ((file (aod-sample-manager/get-current)))
    (aod-sample-manager/play-file file)))

(defun aod-sample-manager/play-toggle ()
  (interactive)
  (if (process-live-p aod-sample-manager/play-process)
      (aod-sample-manager/stop)
    (when-let ((file (aod-sample-manager/get-current)))
      (aod-sample-manager/play-file file))))

(defun aod-sample-manager/stop ()
  (interactive)
  (when (process-live-p aod-sample-manager/play-process)
    (delete-process aod-sample-manager/play-process)
    ;; (kill-process aod-sample-manager/play-process)
    ))

(defun aod-sample-manager/jump ()
  (interactive)
  (when-let ((file (aod-sample-manager/get-current)))
    (dired-jump 'other-window file)
    (aod-sample-manager/play-mode 1)))

(provide 'aod-sample-manager)
