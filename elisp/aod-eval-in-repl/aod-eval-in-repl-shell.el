;;; aod-eval-in-repl-shell.el --- Extensible evaluation in repl with focus in org-mode  -*- lexical-binding: t; -*-

(defcustom aod.eir/shell-type 'shell
  "The default shell type to be used for newly spawned shells (ie shell, eshell, term, vterm etc). It's replaced by passing :shell-type in the org-mode src header."
  :group 'aod-eval-in-repl-shell
  :type '(choice (const :tag "shell (shell-mode)" shell)
		 (const :tag "term (term-mode)" term)
		 (const :tag "vterm (vterm-mode)" vterm)))

;; org-lint: do not trigger error for :shell-type
(require 'ob-core)
(add-to-list 'org-babel-header-arg-names 'shell-type)

(cl-defgeneric aod.eir/-start-repl-shell (shell-name shell-type)
  "Default shell creation: using shell-mode. Other implementations include term & vterm"
  (shell shell-name))

(cl-defmethod aod.eir/-start-repl-shell (shell-name (shell-type (eql term)))
  ;; make-term wraps the passed name with asterisks ie *<passed-name>*
  (make-term (aod.eir/-remove-surrounding-stars shell-name) "/bin/bash"))

(cl-defmethod aod.eir/-start-repl-shell (shell-name (shell-type (eql vterm)))
  ;; vterm messes with the window configuration
  (vterm shell-name))

(cl-defmethod aod.eir/start-repl ((lang (eql sh)) session &optional opts)
  "Starts a repl for sh/shell"
  (let ((shell-type (or (intern-soft (cdr (assq :shell-type opts)))
			aod.eir/shell-type))
	;; opts might have multiple :var keys, so we cannot use just assoc or
	;; something like this. instead, seq-filter
	;; then, org-babel-process-params is responsible for possibly evaluationg
	;; any lisp forms inside the :var
	(opts-with-vars (org-babel-process-params (seq-filter (lambda (x)
								(eq (car x) :var))
							      opts))))
    ;; TODO have a base start-repl and then wrap the result
    ;; this should be start-repl-impl
    (let ((created-buffer (save-window-excursion (aod.eir/-start-repl-shell session shell-type))))
      (let ((assignment-statement
	     (org-babel-expand-body:generic
	      "" opts (org-babel-variable-assignments:shell opts-with-vars))))
	(aod.eir/eval lang session assignment-statement opts)
	(aod.window/place-buffer created-buffer)))))

(cl-defmethod aod.eir/send-string (string &context (major-mode term-mode))
  (term-send-string (current-buffer) string))

(cl-defmethod aod.eir/send-string (string &context (major-mode vterm-mode))
  "term-send-string seems to work with vterm. Is there another command that should be used instead?"
  (when vterm-copy-mode
    (message "disabling vterm-copy-mode")
    (vterm-copy-mode -1))
  (term-send-string (current-buffer) string))


(cl-defmethod aod.eir/send-input (&context (major-mode term-mode))
  (term-send-input))

(cl-defmethod aod.eir/send-input (&context (major-mode vterm-mode))
  (vterm-send-return))

(cl-defmethod aod.eir/last-output (&context (major-mode vterm-mode))
  (vterm-previous-prompt 1)
  (let ((p (point)))
    (vterm-next-prompt 1)
    (let ((output (buffer-substring-no-properties (point) p)))
      output)))

(provide 'aod-eval-in-repl-shell)


(cl-defgeneric aod.eir/eof ((lang (eql sh)) string)
  (format "$(cat <<EOF\n%s\nEOF\n)" string))


(cl-defmethod aod.eir/send-input (&context (major-mode vterm-mode))
  (vterm-send-return))
