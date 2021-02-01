;;; aod-eval-in-repl-js.el --- Extensible evaluation in repl with focus in org-mode  -*- lexical-binding: t; -*-

;; for the js2-mark-defun
(require 'js2-mode)

;; js2-mark-defun apparently keeps internal state (needs js AST)
;; so it needs first the mode initialized in the buffer
(cl-defmethod aod.eir/get-region-to-eval ((lang (eql js)) &optional opts)
  "In js send the whole function definition (if there's one under cursor)"
  (let ((src (org-element-property :value (org-element-at-point)))
	(src-point (aod.eir/org-src-point))
	(src-beginning (car (org-src--contents-area (org-element-at-point)))))
    (mapcar (lambda (x)
	      ;; even if we're in the beginning of the src code
	      ;; (point) will return 1, not 0. so decreasing (to get 0-based)
	      (+ src-beginning (decf  x)))
	    (with-temp-buffer
	      (insert src)
	      (js2-mode)
	      (js2-reparse)
	      (goto-char src-point)
	      ;; in case of const foo = (x) => {
	      ;; // some body
	      ;; }
	      ;; the js2-mark-defun will mark from (x) and onwards
	      (js2-mark-defun)
	      ;; so that's why we need to mark since the beginning of the line
	      (beginning-of-line)
	      (list (point) (mark))))))

(provide 'aod-eval-in-repl-js)
