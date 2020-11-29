;;; aod-eval-in-repl-shell.el --- Extensible evaluation in repl with focus in org-mode  -*- lexical-binding: t; -*-

(defcustom aod.eir/shell-type 'shell
  "The default shell type to be used for newly spawned shells (ie shell, eshell, term, vterm etc). It's replaced by passing :shell-type in the org-mode src header."
  :group 'aod-eval-in-repl-shell
  :type '(choice (const :tag "shell (shell-mode)" shell)
		 (const :tag "term (term-mode)" term)
		 (const :tag "vterm (vterm-mode)" vterm)))

;;; Code:

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
			aod.eir/shell-type)))
    (aod.eir/-start-repl-shell session shell-type)))

(cl-defmethod aod.eir/send-string (string &context (major-mode term-mode))
  (term-send-string (current-buffer) string))

(cl-defmethod aod.eir/send-string (string &context (major-mode vterm-mode))
  "term-send-string seems to work with vterm. Is there another command that should be used instead?"
  (term-send-string (current-buffer) string))


(cl-defmethod aod.eir/send-input (&context (major-mode term-mode))
  (term-send-input))

(cl-defmethod aod.eir/send-input (&context (major-mode vterm-mode))
  (vterm-send-return))

(provide 'aod-eval-in-repl-shell)

