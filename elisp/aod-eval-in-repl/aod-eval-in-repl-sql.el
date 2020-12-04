;;; aod-eval-in-repl-sql.el --- Extensible evaluation in repl with focus in org-mode  -*- lexical-binding: t; -*-

(cl-defmethod aod.eir/get-region-to-eval ((lang (eql sql)) &optional opts)
  "In sql prefer sending the current paragraph (to support long multi-linde queries)"
  (save-mark-and-excursion
    ;; if we are in the beginning of a one-line statement
    ;; going back one paragraph would maybe go out of scope
    ;; so first, end-of-line
    (end-of-line)
    (backward-paragraph)
    (set-mark (point))
    (forward-paragraph)
    (list (mark) (point))))

(provide 'aod-eval-in-repl-sql)
