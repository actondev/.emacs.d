;;; aod-eval-in-repl-python.el --- Extensible evaluation in repl with focus in org-mode  -*- lexical-binding: t; -*-

(cl-defmethod aod.eir/start-repl ((lang (eql python)) session &optional opts)
  "Starts a python repl"
  (let ((python-shell-buffer-name (aod.eir/-remove-surrounding-stars session)))
    (run-python)))

(provide 'aod-eval-in-repl-python)
