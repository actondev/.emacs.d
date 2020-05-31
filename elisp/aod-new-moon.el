(defun aod.org/insert-new-moon-calendar (new-moon-date work-days rest-days)
  (interactive (list
		(org-read-date nil nil nil "New moon date")
		(read-number "work days:")
		(read-number "rest days:")))
  (message (format "read date %s work days %s"
		   new-moon-date
		   work-days))
  (let ((separator "|---|\n")
	(separator2 "\n|---|"))
    (insert
     "| # | 🌙 | Date | Notes | \n"
     "|---|\n"
     (mapconcat
      ;; x 0: will be for the new moon (ένη και νέα: old and new day)
      ;; x 1: new month, first day (νουμηνία: numinia)
      (lambda(x)
	(let* ((moon-phase "")
	       (period (+ work-days rest-days))
	       (period-n (mod x period))
	       ;; week day shall be 1 for first work day etc..
	       ;; keep in mind: with the following statement x 0 is mapped to work-days + rest-days
	       (week-day (1+ (mod (1- x) period)))
	       (is-first-work-day (= 1 week-day))
	       (is-first-rest-day (= (1+ work-days) week-day)))
	  ;; (message (format "x is %s week-day is %s" x week-day))
	  (format "%s | %s | %s | <%s> |"
		  (cond
		   ((= 0 x) "")
		   ;; the work rest strings are ignored upon pressing tabs
		   ;; and they are converted to horizontal delimiters
		   ;; they're here for debugging purposes
		   (is-first-work-day "|---work|\n")
		   (is-first-rest-day "|---rest|\n")
		   (t ""))
		  x
		  moon-phase
		  (tiny-date new-moon-date x))))
      (number-sequence 0 30)
      "\n"))))
