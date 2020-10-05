;; Some helper funcionts for workin with s7 namely, changing
;; "namespace" with (ns ..) forms ala clojure tyle
;; 
;; Pardon the non-elisp-idiomatic dots and slashes but I really prefer
;; this namespace/clojure sytnax

(require 'cmuscheme)

(defun aod.s7/get-ns ()
  "Get the namespace name to send on the repl.
NOTE: you need to have the (ns) in the beginning of the
line. This is to not mistake things after ;; comments"
  (save-excursion
    (beginning-of-buffer)
    ;; matching any non whitespace character after the "(ns "
    ;; \s didn't match new line for some reason..?
    (if (re-search-forward "^\(ns \\([^ \t\r\n]+\\)" nil t)
	(match-string-no-properties 1)
      nil)))

(defun aod.s7/switch-to-ns ()
  "Switches to the namespace of the currently open buffer."
  (interactive)
  (if-let ((ns (aod.s7/get-ns)))
      (progn
	(message (format "switching to ns %s" ns))
	(scheme-send-string (format "(ns %s)" ns)))
    ;; else
    (progn
      (message "switching to (rootlet)")
      (scheme-send-string "(set! *ns* (rootlet))"))))

(defun aod.s7/top-level-sexp-region ()
  "Gets the top level sexp (start end)"
  (save-excursion
    (list
     (progn (beginning-of-defun) (point))
     (progn (end-of-sexp) (point)))))

(defun aod.s7/last-sexp-region ()
  "Gets the last (previous) sexp (start end)"
  (list
   (save-excursion (backward-sexp) (point))
   (point)))

(defun aod.s7/top-level-sexp ()
  "Gets the top level sexp without any trailing new lines
This was added cause the cmuscheme had extra whitespace at the end (a new line)
And this was messing a bit with my REPL. However, have edited my cmuscheme.el now.
So this is duplicate/not necessary code now"
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (string-trim
       (buffer-substring-no-properties (point) end)))))

(defun aod.s7/send-definition ()
  "Sends the top level sexp under point.
Note: (scheme-send-definition) would send 2 things:
1. The top level sexp with a new line at the end (cause of (end-of-defun))
2. The '\n' string as well. (extra commint command)

Here we just send ounce the trimmed top level sexp"
  (interactive)
  (aod.s7/switch-to-ns)
  (let ((region (aod.s7/top-level-sexp-region)))
    (apply #'scheme-send-region region)))

(defun aod.s7/send-last-sexp ()
  "Send the last sexp"
  (interactive)
  (aod.s7/switch-to-ns)
  (let ((region (aod.s7/last-sexp-region)))
    (apply #'scheme-send-region region)))

(provide 'aod-s7)
