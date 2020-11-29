;;; aod-eval-in-repl-sql.el --- Extensible evaluation in repl with focus in org-mode  -*- lexical-binding: t; -*-

(cl-defmethod aod.eir/get-string-to-eval ((lang (eql sql)))
  "In sql prefer sending the current paragraph (to support long multi-line queries)"
  (save-mark-and-excursion
    (backward-paragraph)
    (set-mark (point))
    (forward-paragraph)
    (s-trim (buffer-substring-no-properties (mark) (point)))))

(provide 'aod-eval-in-repl-sql)
