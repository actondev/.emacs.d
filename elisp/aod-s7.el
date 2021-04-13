;; Some helper funcionts for workin with s7 namely, changing
;; "namespace" with (ns ..) forms ala clojure tyle
;; 
;; Pardon the non-elisp-idiomatic dots and slashes but I really prefer
;; this namespace/clojure sytnax

(require 'cmuscheme)

(defgroup aod.s7 nil
  "Repl functionality for s7"
  :group 'scheme)

(defcustom aod.s7/send-whole-ns-form t
  "Send the whole ns form (including :require etc) or only the ns name.
When the former, it means the the required namespaces are in away 'reloaded' (imported
functions are updated if they got redefined in the required namespace)
When the latter, it only changes the current ns where the repl performs its evaluations."
  :type 'boolean
  :group 'cmuscheme)

(defun aod.s7/get-ns ()
  "Get the namespace name to send on the repl.
NOTE: you need to have the (ns) in the beginning of the
line. This is to not mistake things after ;; comments"
  (save-mark-and-excursion
    (beginning-of-buffer)
    ;; note: sexp-at-point returns a form! not a string
    (let ((ns-form (sexp-at-point)))
      (if (and ns-form (string-match "\(ns \\([^ \t\r\n]+\\)" (format "%s" ns-form)))
          (progn
            (if aod.s7/send-whole-ns-form
                ns-form
              (format "(ns %s)" (match-string-no-properties 1))))
        nil))))

(defun aod.s7/switch-to-ns ()
  "Switches to the namespace of the currently open buffer."
  (interactive)
  (if-let ((ns (aod.s7/get-ns)))
      (progn
	(message (format "switching to ns %s" ns))
	(scheme-send-string ns)
        )
    ;; else
    (progn
      (message "switching to (rootlet)")
      (scheme-send-string "(set! *ns* (rootlet))")
      )))

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

(defun aod.s7/defn->cpp (fn-name)
  (->> fn-name
       (replace-regexp-in-string "-" "_")
       (replace-regexp-in-string "!" "_bang")))

(provide 'aod-s7)
