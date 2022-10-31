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

(defun aod.eir/sql-replace-params (text)
  (let ((values
	 (mapcar (lambda (x)
		   (replace-regexp-in-string ",$" "" (s-trim x)))
		 (split-string text "[\n\r]+"))))
    (let ((i 0))
      (mapconcat (lambda (x)
		   (setq i (+ i 1))
		   (format "$%s=%S" i x)
		   )
		 values
		 " "))))

(defun aod.eir/sql-yank-replace-params ()
  "Of internal interest. Used alongside output of typeorm's getQueryAndParameters() to
generate tha appropriate :replace statement (ie $1=\"'this'\" etc)"
  (interactive)
  (insert (aod.eir/sql-replace-params (substring-no-properties (car kill-ring)))))

(provide 'aod-eval-in-repl-sql)
