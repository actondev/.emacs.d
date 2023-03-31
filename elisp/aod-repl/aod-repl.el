;;; aod--repl.el --- Extensible evaluation in repl with focus in org-mode  -*- lexical-binding: t; -*-

;; Copyleft (C) 2023- Christos Vagias

;; Author: actondev
;; Keywords: tools, literate programming
;; URL: https://github.com/actondev/tbd

;; This file is not part of GNU Emacs.

;;; Code:

(defun aod-repl/js2-mark-node (node)
  (let* ((beg (js2-node-abs-pos node))
	 (end (+ beg (js2-node-len node))))
    (goto-char beg)
    (set-mark end)))

(defun aod-repl/js2-mark-node-at-point ()
  (interactive)
  (aod-repl/js2-mark-node (js2-node-at-point)))

(defun aod-repl/js2-mark-top-level ()
  "From js2-mark-parent-statement (expand-region/js2-mode-expansions.el)"
  (interactive)
  (named-let rec ((p0 0)
		  (p1 (point)))
    (unless (eq p0 p1)
      (beginning-of-line)
      (js2-mark-defun)
      (rec p1 (point)))))

(defcustom aod-repl/mark-statement-per-major-mode #s(hash-table data (js2-mode aod-repl/js2-mark-top-level))
  "Custom fn for marking the current statement to be evaluated for a specific major-mode"
  :type 'hash-table
  :group 'aod-repl)
(defcustom aod-repl/mark-statement nil
  "Buffer local variable to override the `aod-repl/default-mark-statement'"
  :type 'string
  :group 'aod-repl
  :local t)

(setf aod-repl/default-mark-statement #'mark-defun)

(defun aod-repl/mark-statement ()
  (interactive)
  (let ((fn (cond
	     (aod-repl/mark-statement aod-repl/mark-statement)
	     ((gethash major-mode aod-repl/mark-statement-per-major-mode) (gethash major-mode aod-repl/mark-statement-per-major-mode))
	     (:else aod-repl/default-mark-statement))))
    (call-interactively fn)))

(defun aod-repl/js-remove-const-let-var (str)
  (replace-regexp-in-string "^const\\|^let\\|^var " "" str))

(defcustom aod-repl/preprocess-string-per-major-mode
  #s(hash-table data(
		     js2-mode aod-repl/js-remove-const-let-var
		     js-mode aod-repl/js-remove-const-let-var
		     ))
  "Custom fn for processing a string before evaluating it per major-mode"
  :type 'hash-table
  :group 'aod-repl)

(defcustom aod-repl/preprocess-string nil
  "Buffer local variable"
  :type 'string
  :group 'aod-repl
  :local t)

(defun aod-repl/apply-preprocessors (fns str)
  ;; TODO handle list
  (cond
   ((functionp fns) (funcall fns str))
   ((listp fns) (let ((str str))
		  (dolist (fn fns)
		    (setq str (funcall fn str)))
		  str))
   (:else str)))

(defun aod-repl/preprocess-string (str)
  (cond
   (aod-repl/preprocess-string (aod-repl/apply-preprocessors aod-repl/preprocess-string str))
   ((gethash major-mode aod-repl/preprocess-string-per-major-mode)
    (aod-repl/apply-preprocessors (gethash major-mode aod-repl/preprocess-string-per-major-mode) str))
   (:else str)))

(defcustom aod-repl/repl-names #s(hash-table data (js2-mode "*js*"))
  "Custom fn for mark-defun for a specific major-mode"
  :type 'hash-table
  :group 'aod-repl)

(defcustom aod-repl/repl-name nil
  "Buffer local variable to override the repl name"
  :type 'string
  :group 'aod-repl
  :local t)

(defun aod-repl/repl-fallback-name ()
  (let ((major-mode-name (symbol-name major-mode)))
    (format "*%s*" (string-replace "-mode" "" major-mode-name))))

(defun aod-repl/repl-name ()
  (cond
   (aod-repl/repl-name aod-repl/repl-name)
   ((gethash major-mode aod-repl/repl-names) (gethash major-mode aod-repl/repl-names))
   (:else (aod-repl/repl-fallback-name))))

(defun aod-repl/statement-region ()
  (save-mark-and-excursion
    (aod-repl/mark-statement)
    ;; so that's why we need to mark since the beginning of the line
    (beginning-of-line)
    (list (point) (mark))))

(defun aod-repl/eval-region (start end session)
  (interactive (list (mark) (point) (aod-repl/repl-name)))
  (let* ((src (buffer-substring-no-properties start end))
	 (processed (aod-repl/preprocess-string src)))
    ;; (unless (string-equal src processed)
    ;;   (message "processed src: %s" processed))
    ;; TODO flash region
    (pulse-momentary-highlight-region start end 'next-error)
    (save-current-buffer
      (set-buffer session)
      (aod.eir/send-string processed)
      (aod.eir/send-input))))

(defun aod-repl/eval-dwim ()
  (interactive)
  (let ((region (if (region-active-p)
		    (list (mark) (point))
		  (aod-repl/statement-region))))
    (message "here region %s" region)
    (aod-repl/eval-region (car region) (cadr region) (aod-repl/repl-name))))

(defun aod-repl/eval-buffer ()
  (interactive)
  (aod-repl/eval-region (point-min) (point-max) (aod-repl/repl-name)))
