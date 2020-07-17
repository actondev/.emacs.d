;; Some helper funcionts for workin with s7 namely, changing
;; "namespace" with (ns ..) forms ala clojure tyle
;; 
;; Pardon the non-elisp-idiomatic dots and slashes but I really prefer
;; this namespace/clojure sytnax

(require 'cmuscheme)

(defun aod.s7/get-ns ()
  "Get the (ns some.namespace) part to send on the repl"
  (save-excursion
    (beginning-of-buffer)
    (if (re-search-forward "^\\(\(ns .+\)\\)" nil t)
	(match-string-no-properties 1)
      nil)))

(defun aod.s7/switch-to-ns ()
  (interactive)
  (if-let ((ns (aod.s7/get-ns)))
      (progn
	(message (format "switching to ns %s" ns))
	(scheme-send-string ns))
    ;; else
    (progn (if (interactive-p)
	       (message "No (ns ..) form found!"))
	   (scheme-send-string "(set! *ns* #f)"))))

(defun aod.s7/send-definition ()
  (interactive)
  (aod.s7/switch-to-ns)
  (scheme-send-definition)
  )

(defun aod.s7/send-last-sexp ()
  (interactive)
  (aod.s7/switch-to-ns)
  (scheme-send-last-sexp)
  )

(provide 'aod-s7)
