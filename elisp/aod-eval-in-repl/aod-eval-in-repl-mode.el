(defun aod-repl/help-echo-src-block (window buffer char)
  (save-current-buffer
    (set-buffer buffer)
    (save-excursion
      (goto-char char)
      (let ((opts (aod.eir/opts)))
	(format ":session %s
:replace %s"
		(aod.eir/-sesion-name-from-opts opts)
		(aod.eir/get-opts opts :replace))))))

(defconst aod-repl/org-src-block-regexp
  "^[ \t]*#\\+begin_src\\([^\n]*\\)?\n\\([\0-\377[:nonascii:]]+?\\)\n[ \t]*#\\+end_src[ \t]*$"
  "org src block regexp")

(defun aod-repl/org-src-block-matcher (limit)
  (let ((case-fold-search t))
    (re-search-forward
     aod-repl/org-src-block-regexp
     limit 'no-error)))

(defvar aod-repl/font-lock-keywords 
  `((aod-repl/org-src-block-matcher (0 `(face nil
					      help-echo
					      ,#'aod-repl/help-echo-src-block
					      ;; or (lambda (window buffer char) ...)
					      ;; or (lambda (&rest args) ... )
					      )
				       prepend))))

(defun aod-repl/-org-mode-init ()
  (message "aod-repl/-org-mode-init")
  ;; https://stackoverflow.com/a/10035494/8720686
  (font-lock-add-keywords nil aod-repl/font-lock-keywords 'append)
  (make-local-variable 'font-lock-extra-managed-props)
  (push 'help-echo font-lock-extra-managed-props))

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
