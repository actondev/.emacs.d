(defconst aod/s7bi-pd-font-lock-keywords
  (list (list
	 (concat
	  "(\\(define\\)-\\(abstraction\\|param\\)"
	  ;; (concat "(\\(define\\)-\\(abstraction\\)"
	  ;;         ;; Any whitespace and declared object.
	  ;;         ;; The "(*" is for curried definitions, e.g.,
	  ;;         ;;  (define ((sum a) b) (+ a b))
	  "[ \t]*"
	  "\\(\\sw+\\)?")
	 ;;         "\\(\\sw+\\)?")
	 '(1 font-lock-keyword-face)
	 ;; '(1 font-lock-keyword-face)
	 '(2 font-lock-type-face)
	 '(3 font-lock-variable-name-face)
	 )
	(list
	 "(\\(obj\\|msg\\)[ \t]"
	 ;; (concat
	 ;;  "(\\(obj\\)[ \t]\"\\(.*\\)\""
	 ;;  )
	 ;;         "\\(\\sw+\\)?")
	 '(1 font-lock-type-face)
	 ;;'(2 font-lock-type-face prepend)
	 )))

(define-minor-mode aod/s7bi-pd-mode
  "Minor mode that adds help-echo in the begin_src lines"
  :group 'aod/s7bi-pd
  (if aod/s7bi-pd-mode
      (progn
	(font-lock-add-keywords nil aod/s7bi-pd-font-lock-keywords))
    (progn
      (font-lock-remove-keywords nil aod/s7bi-pd-font-lock-keywords)))
  ;; As of Emacs 24.4, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(provide 's7bi-pd)
