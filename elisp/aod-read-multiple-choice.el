;;; aod-read-multiple-choice.el --- a read-multiple-choice mod

;; from rmc.el : read-multiple-choice

;;;###autoload
(defun aod/read-multiple-choice (prompt choices &optional allow-other-char)
  "Ask user a multiple choice question.
PROMPT should be a string that will be displayed as the prompt.

CHOICES is a list of (KEY NAME &rest user data).  KEY is a
character to be entered.  NAME is a short name for the entry to
be displayed while prompting (if there's room, it might be
shortened).

The return value is the matching entry from the CHOICES list,
or the read input character (if `allow-other-char' is non-nil)

See `read-multiple-choice' for usage example
"
  (let* ((full-prompt (format
		       "%s (%s): "
		       prompt
		       (mapconcat
			(lambda (elem)
			  (let ((name (cadr elem)))
			    (format "[%c] %s" (car elem) name)))
			choices ", ")))
         tchar wrong-char answer)
    (save-window-excursion
      (save-excursion
	(while (not tchar)
	  (message "%s%s"
                   (if wrong-char
                       "Invalid choice.  "
                     "")
                   full-prompt)
          (setq tchar
		;; removed (display-popup-menus-p) and use-dialog-box check
		;; it was calling x-popup-dialog
                (condition-case nil
		    (let ((cursor-in-echo-area t))
                      (read-event))
                  (error nil)))
	  ;; removed (setq answer (lookup-key query-replace-map (vector tchar) t))
	  ;; and cond .. 'recent 'scroll-up etc
          (when (eq tchar t)
	    ;; why would that happen?
            (setq wrong-char nil
                  tchar nil))
	  ;; removed help message
          (when (and (not (eq tchar nil))
                     (not (assq tchar choices))
		     (not allow-other-char))
	    (setq tchar nil
		  wrong-char t)))))
    (or (assq tchar choices)
	(and allow-other-char tchar))))

(comment
 (aod/read-multiple-choice "Continue connecting?"
			   '((?a "always")
                             (?s "session only")
                             (?n "no")))

 (aod/read-multiple-choice "Continue connecting?"
			   '((?a "always")
                             (?s "session only")
                             (?n "no"))
			   'allow)

 (let ((choices '((?C "char (default)" avy-goto-char)
		  (?L "line" avy-goto-line)
		  (?w "word" avy-goto-word-1))))
   (let ((choice (aod/read-multiple-choice "avy action:" choices 'allow)))
     (if (listp choice)
	 (call-interactively (caddr choice))
       (progn
	 ;; (message "fallback, calling %s with %s" (caddar choices) choice)
	 (funcall (caddar choices) choice)))))
 ;; comment
 )

(provide 'aod-read-multiple-choice)
