;; pro tip:
;; run the following into a known project
;; 
;; (add-to-list 'aod-do/eval-safe-directories (format "%s.*" default-directory))
;; (customize-save-variable 'aod-do/eval-safe-directories aod-do/eval-safe-directories)
;; and even better, add this snippet into the dev.org of the project
;; 
;; see https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
;; to reload dir locals

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

(defun make-silent (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest args) nil)))
    (apply func args)))

(define-minor-mode aod-do-mode
  "Minor mode that uses hercules to create interactive actions")

;; disabling the message of "enabled aod-do mode"
(advice-add 'aod-do-mode :around #'make-silent)
(require 'key-chord)
(key-chord-define-global "/d" #'aod-do-mode)

(hercules-def
 :toggle-funs #'aod-do-mode
 :hide-funs #'aod-do/hide
 :keymap 'aod-do/map
 :transient t)

(defcustom aod-do/eval-safe-directories ()
  "List of safe directories to perform eval in dir-locals")

(defun aod-do/eval-safep (orig-fun &rest args)
  (catch 'done
    (dolist (dir aod-do/eval-safe-directories)
      (when (string-match-p dir default-directory)
	(message "safe directory match with %s" dir)
	(throw 'done t)))
    (apply orig-fun args)))

(advice-add 'hack-one-local-variable-eval-safep :around #'aod-do/eval-safep)

(comment
 (advice-remove 'hack-one-local-variable-eval-safep :around #'aod-do/eval-safep)
 )

(provide 'aod-do)
