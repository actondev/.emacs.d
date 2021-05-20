(defvar-local aod-do/map
  (make-sparse-keymap))
(defun aod-do/hello-world ()
  (interactive)
  (message "hello world"))

(defun aod-do/hide ()
  (interactive))

;;;###autoload
(defmacro aod-do/define (&rest body)
  (setq-local aod-do/map (make-sparse-keymap))
  (general-def
    :prefix-map 'aod-do/map
    "q" '(aod-do/hide :wk "quit"))
  `(general-def 'aod-do/map
     ,@body))

(comment
 (general-def
   :prefix-map 'aod-do/map
   "h" 'aod-do/hello-world
   "m" '(aod-do/hello-world :wk "mmap") ))

(defun make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

(define-minor-mode aod-do-mode
  "Minor mode that uses hercules to create interactive actions")

(advice-add 'aod-do-mode :around #'make-silent)
(require 'key-chord)
(key-chord-define-global "d." #'aod-do-mode)

(hercules-def
 :toggle-funs #'aod-do-mode
 :hide-funs #'aod-do/hide
 :keymap 'aod-do/map
 :transient t)

(provide 'aod-do)
