(defun aod-repl/help-echo-src-block (window buffer char)
  (save-current-buffer
    (set-buffer buffer)
    (save-excursion
      (goto-char char)
      (format "header-args: %S" (aod.eir/opts)))))

(defvar aod-repl/font-lock-keywords 
  '(("begin_src\\|BEGIN_SRC" (0 `(face font-lock-comment-face
				       help-echo
				       ,#'aod-repl/help-echo-src-block
				       ;; or (lambda (window buffer char) ...)
				       ;; or (lambda (&rest args) ... )
				       ) prepend)))
  "The font-lock for begin_src headers.")


(defun aod-repl/-org-mode-init ()
  (message "aod-repl/-org-mode-init")
  (font-lock-add-keywords nil aod-repl/font-lock-keywords))

(defun aod-repl/-org-mode-deinit ()
  "Remove font-lock keywords for extra lisp highlithing."
  (message "aod-repl/-org-mode-deinit")
  (font-lock-remove-keywords nil aod-repl/font-lock-keywords))

(define-minor-mode aod-repl/org-mode
  "Minor mode that adds help-echo in the begin_src lines"
  :group 'aod-repl/org
  (if aod-repl/org-mode
      (aod-repl/-org-mode-init)
    (aod-repl/-org-mode-deinit))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(provide 'aod-eval-in-repl-mode)
