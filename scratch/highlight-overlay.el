;; https://emacsnotes.wordpress.com/2018/10/26/highlight-text-not-with-faces-but-with-other-texts/

(require 'hi-lock)

(defun my-highlight-regexp-with-string (&optional regexp string face)
  "Display each match of REGEXP as STRING with face FACE.
With a prefix arg, remove all such highlights."
  (interactive
   (unless current-prefix-arg
     (let* ((regexp (read-regexp "Regexp: "))
            (string (read-string (format "Display %s as : " (propertize regexp 'face 'fixed-pitch-serif)))))
       (list regexp string  (hi-lock-read-face-name)))))
  (if current-prefix-arg  (remove-overlays nil nil 'my-overlay t)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (re-search-forward regexp nil t)
          (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put ov 'my-overlay t)
            (overlay-put ov 'display string)
            (overlay-put ov 'face face)))))))

(defun my-remove-all-string-highlights ()
  "Remove all highlights created with `my-highlight-regexp-with-string'."
  (interactive)
  (remove-overlays nil nil 'my-overlay t))


(comment
 (my-highlight-regexp-with-string "eed990e9-9875-48d7-96c5-bee425332be0" "publisher:chip(prod)" 'highlight)

 (aod/highlight-regexp-with-string 'aod-test "eed990e9-9875-48d7-96c5-bee425332be0" "publisher:chip(prod)")
 
 (remove-overlays nil nil 'aod-test t)
 )
